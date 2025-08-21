test_that("ReverseProxy initialization works", {
  # Test default initialization
  proxy <- ReverseProxy$new("http://localhost:8080")
  expect_equal(proxy$name, "firestorm_http://localhost:8080/")

  # Test with custom root
  proxy <- ReverseProxy$new("http://localhost:8080", root = "/api")
  expect_s3_class(proxy, "ReverseProxy")
  expect_equal(proxy$.__enclos_env__$private$root_regex, "^/api/")

  # Test with except
  proxy <- ReverseProxy$new(
    "http://localhost:8080",
    root = "/api",
    except = "/test"
  )
  expect_s3_class(proxy, "ReverseProxy")
  expect_equal(proxy$.__enclos_env__$private$root_regex, "^/api/(?!test/)")
})

test_that("ReverseProxy handles URL formatting during initialization", {
  # Test with trailing slash in target
  proxy1 <- ReverseProxy$new("http://localhost:8080/")
  expect_equal(proxy1$name, "firestorm_http://localhost:8080/")

  # Test without trailing slash in target
  proxy2 <- ReverseProxy$new("http://localhost:8080")
  expect_equal(proxy2$name, "firestorm_http://localhost:8080/")

  # Test with different root formats
  proxy3 <- ReverseProxy$new("http://localhost:8080", root = "api")
  proxy4 <- ReverseProxy$new("http://localhost:8080", root = "/api")

  expect_equal(proxy3$.__enclos_env__$private$root_regex, "^/api/")
  expect_equal(proxy4$.__enclos_env__$private$root_regex, "^/api/")
})

test_that("ReverseProxy can forward HTTP requests", {
  skip_if_not_installed("fiery")

  # Create a target server
  target_port <- httpuv::randomPort()
  target_app <- fiery::Fire$new(port = target_port)

  # Add a simple handler to verify forwarding works
  target_app$on("request", function(request, ...) {
    response <- request$respond()
    response$status <- 200
    response$body <- paste0("Received request at path: ", request$path)
    response$type <- "text/plain"
  })

  # Create the proxy
  proxy <- ReverseProxy$new(
    paste0("http://127.0.0.1:", target_port),
    root = "/api",
    except = "no_proxy"
  )
  proxy_port <- httpuv::randomPort()
  proxy_app <- fiery::Fire$new(port = proxy_port)
  proxy_app$attach(proxy)

  # Add handler to verify non-matching paths aren't forwarded
  proxy_app$on("request", function(request, ...) {
    response <- request$respond()
    if (response$status != 200) {
      response$status <- 200
      response$body <- "Proxy handler reached"
      response$type <- "text/plain"
    }
  })

  # Start the servers in non-blocking mode
  target_app$start(block = FALSE)
  on.exit(target_app$stop())

  # Test proxy forwarding
  rook <- fiery::fake_request(paste0(
    "http://127.0.0.1:",
    proxy_port,
    "/api/test"
  ))
  res <- proxy_app$test_request(rook)
  promise_impl <- attr(res, "promise_impl", exact = TRUE)
  while (promise_impl$status() == "pending") {
    later::run_now()
    Sys.sleep(0.01)
  }
  res <- promise_impl$.__enclos_env__$private$value

  # Verify response
  expect_equal(res$status, 200)
  expect_match(rawToChar(res$body), "Received request at path: /test")

  # Test root handling
  rook <- fiery::fake_request(paste0("http://127.0.0.1:", proxy_port, "/test"))
  res <- proxy_app$test_request(rook)

  # Verify response
  expect_equal(res$status, 200)
  expect_match(res$body, "Proxy handler reached")

  # Test except handling
  rook <- fiery::fake_request(paste0(
    "http://127.0.0.1:",
    proxy_port,
    "/api/no_proxy/test"
  ))
  res <- proxy_app$test_request(rook)

  # Verify response
  expect_equal(res$status, 200)
  expect_match(res$body, "Proxy handler reached")
})

test_that("ReverseProxy forwards request headers", {
  skip_if_not_installed("fiery")

  # Create a target server that echoes headers
  target_port <- httpuv::randomPort()
  target_app <- fiery::Fire$new(port = target_port)

  # Add a handler that returns the headers
  target_app$on("request", function(request, ...) {
    response <- request$respond()
    response$status <- 200
    response$body <- list(
      via = request$headers$via,
      x_forwarded_host = request$headers$x_forwarded_host,
      x_forwarded_proto = request$headers$x_forwarded_proto,
      x_forwarded_for = request$headers$x_forwarded_for
    )
    response$format(json = reqres::format_json())
  })

  # Create a proxy server
  proxy_port <- httpuv::randomPort()
  proxy_app <- fiery::Fire$new(port = proxy_port)

  # Create proxy with trust = TRUE
  proxy <- ReverseProxy$new(
    paste0("http://localhost:", target_port),
    trust = TRUE
  )
  proxy_app$attach(proxy)

  # Start servers in non-blocking mode
  target_app$start(block = FALSE)
  on.exit(target_app$stop())

  # Send a request with some headers
  rook <- fiery::fake_request(
    paste0("http://127.0.0.1:", proxy_port, "/test"),
    headers = list(
      X_Forwarded_For = "10.0.0.1"
    )
  )

  # Verify headers were forwarded and modified correctly
  res <- proxy_app$test_request(rook)
  promise_impl <- attr(res, "promise_impl", exact = TRUE)
  while (promise_impl$status() == "pending") {
    later::run_now()
    Sys.sleep(0.01)
  }
  res <- promise_impl$.__enclos_env__$private$value
  headers <- reqres::parse_json()(res$body)

  expect_match(headers$via, "HTTP/1.1 firestorm")
  expect_equal(headers$x_forwarded_host, paste0("127.0.0.1:", proxy_port))
  expect_equal(headers$x_forwarded_proto, "http")
  expect_match(headers$x_forwarded_for[1], "10.0.0.1")
})

test_that("ReverseProxy can handle WebSocket connections", {
  skip_if_not_installed("fiery")
  skip_if_not_installed("later")

  # Create a target server with websocket support
  target_port <- httpuv::randomPort()
  target_app <- fiery::Fire$new(port = target_port)

  # Add WebSocket handler
  target_app$on(
    "message",
    function(server, message, id, ...) {
      # Echo the message back with a prefix
      server$send(paste0("Echo: ", message), id)
    }
  )

  # Create a proxy server
  proxy_port <- httpuv::randomPort()
  proxy_app <- fiery::Fire$new(port = proxy_port)

  # Create proxy with websocket support
  proxy <- ReverseProxy$new(
    paste0("http://127.0.0.1:", target_port),
    root = "/ws"
  )
  proxy_app$attach(proxy)

  # Test WebSocket connection through the proxy
  ws_url <- paste0("ws://127.0.0.1:", proxy_port, "/ws/")
  client <- websocket::WebSocket$new(ws_url, autoConnect = FALSE)

  # Start servers in non-blocking mode
  target_app$start(block = FALSE)
  proxy_app$start(block = FALSE)
  on.exit({
    client$close()
    target_app$stop()
    proxy_app$stop()
  })


  # Setup message collection
  received_messages <- list()
  client$onMessage(function(event) {
    received_messages[[length(received_messages) + 1]] <<- event$data
  })

  # Wait for connection to establish
  client$connect()
  while(client$readyState() == 0) {
    Sys.sleep(0.2)
    later::run_now()
  }
  Sys.sleep(2)
  later::run_now()

  # Send a test message
  client$send("test message")

  # Wait for response
  for (i in 1:10) {
    if (length(received_messages) > 0) {
      break
    }
    Sys.sleep(0.2)
    later::run_now()
  }

  # Verify we got the response
  expect_length(received_messages, 1)
  expect_equal(received_messages[[1]], "Echo: test message")
})
