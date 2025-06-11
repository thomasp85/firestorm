load_balance <- function(host, port, servers, strategy = c("roundrobin", "hash", "least_con"), trust = FALSE, reconnect = NULL) {
  url <- paste0("http://", hosts, ":", ports)

  httpuv::startServer(
    host = host,
    port = port,
    app = list(
      call = function(req) {
        request$trust <- private$trust
        headers <- request$headers
        new_headers <- list(
          "Via" = paste0(c(headers$Via, "HTTP/1.1 firestorm"), collapse = ", "),
          "X-Forwarded-Host" = request$host,
          "X-Forwarded-Proto" = request$protocol,
          "X-Forwarded-For" = paste0(c(headers$X_Forwarded_For, request$origin$REMOTE_ADDR), collapse = ",")
        )
        url <- paste0(private$target, sub(private$root_regex, "", request$path))
        request$forward(url, headers = new_headers, return = function(...) private$continue)
      }
    )
  )
}
