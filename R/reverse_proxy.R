#' Fiery plugin for a single reverse proxy
#'
#' This plugin establishes a reverse proxy between the server and a target
#' origin server. It supports both HTTP and WebSocket connections and sets all
#' the relevant headers for tracking the request along the proxy chain.
#'
#' @usage NULL
#' @format NULL
#'
#' @section Initialization:
#' A new 'ReverseProxy'-object is initialized using the \code{new()} method on
#' the generator:
#'
#' \strong{Usage}
#' \tabular{l}{
#'  \code{route <- ReverseProxy$new(target, root = "/", continue = FALSE, trust = FALSE)}
#' }
#'
#' @export
#'
ReverseProxy <- R6::R6Class(
  "ReverseProxy",
  public = list(
    #' @description Create a new reverse proxy
    #' @param target The URL to the origin server being proxied
    #' @param root The root path the reverse proxy should respond to. Only
    #' requests to subsets of the root path will be proxied, and the root will
    #' be stripped from the URL path before being forwarded
    #' @param continue Should handling of the request be passed on to other
    #' handlers in the fiery app
    #' @param trust Are requests coming from a trusted source
    #'
    initialize = function(target, root = "/", continue = FALSE, trust = FALSE) {
      private$target <- paste0(sub("/$", "", target), "/")
      private$root_regex <- paste0("^/", sub("^/", "", root))
      private$continue <- continue
      private$trust <- trust
    },
    #' @description Hook for attaching the plugin to a fiery app. Should not be
    #' called directly
    #' @param app The fiery app to attach to
    #' @param ... Ignored
    #'
    on_attach = function(app, ...) {
      app$on("request", function(request, ...) {
        private$http_forward(request)
      })
      app$on("websocket-opened", function(server, connection, ...) {
        private$ws_forward(connection, server)
      })
    }
  ),
  active = list(
    #' @field name The name used by default for the plugin
    name = function() {
      paste0("firestorm_", private$target)
    }
  ),
  private = list(
    target = NULL,
    root_regex = "^/",
    continue = FALSE,
    trust = FALSE,

    http_forward = function(request) {
      if (!grepl(private$root_regex, request$path)) {
        return()
      }
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
    },
    ws_forward = function(client, app) {
      if (!grepl(private$root_regex, paste0("/", sub("^/", "", client$request$PATH_INFO)))) {
        return()
      }
      protocols <- unlist(strsplit(client$request$HTTP_SEC_WEBSOCKET_PROTOCOL %||% "", ",\\s?"))
      headers <- list()
      if (!is.null(client$request$HTTP_HOST)) {
        headers$Host <- client$request$HTTP_HOST
      }
      if (!is.null(client$request$HTTP_ORIGIN)) {
        headers$Origin <- client$request$HTTP_ORIGIN
      }
      if (!is.null(client$request$HTTP_COOKIE)) {
        headers$Cookie <- client$request$HTTP_COOKIE
      }
      headers$`X-Forwarded-For` = paste0(
        c(client$request$HTTP_X_FORWARDED_FOR, client$request$REMOTE_ADDR),
        collapse = ", "
      )
      headers$`X-Forwarded-Proto` = if (private$trust && !is.null(client$request$HTTP_X_FORWARDED_PROTO)) {
        client$request$HTTP_X_FORWARDED_PROTO
      } else {
        "http"
      }
      headers$`X-Forwarded-Host` = if (private$trust && !is.null(client$request$HTTP_X_FORWARDED_HOST)) {
        client$request$HTTP_X_FORWARDED_HOST
      } else {
        client$request$HTTP_HOST
      }

      server <- websocket::WebSocket$new(
        url = sub("^http", "ws", private$target),
        protocols = protocols,
        headers = headers,
        autoConnect = FALSE
      )

      msg_buffer <- list()
      flush_buffer <- function() {
        for (msg in msg_buffer) {
          server$send(msg)
        }
        msg_buffer <<- list()
      }

      client$onMessage(function(binary, message) {
        if (server$readyState() == 0) {
          msg_buffer[length(msg_buffer) + 1] <<- message
        } else {
          server$send(message)
        }
      })
      client$onClose(function() {
        server$close()
      })
      server$onOpen(function(event) {
        flush_buffer()
      })
      server$onMessage(function(message) {
        client$send(message$data)
      })
      server$onClose(function(event) {
        cli::cli_inform(paste0(event$reason, " (", event$code, ")"))
        client$close()
      })
      server$onError(function(event) {
        cli::cli_abort(event$message)
      })
      server$connect()
    }
  )
)
