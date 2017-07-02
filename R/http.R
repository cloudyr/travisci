#' @title Travis-CI HTTP Requests
#' @description This is the workhorse function for executing API requests for Travis-CI.
#' @details This is mostly an internal function for executing API requests. In almost all cases, users do not need to access this directly.
#' @param verb A character string containing an HTTP verb, defaulting to \dQuote{GET}.
#' @param path A character string with the API endpoint (should begin with a slash).
#' @param query A list specifying any query string arguments to pass to the API.
#' @param body A character string of request body data.
#' @param base A character string specifying the base URL for the API.
#' @param token A character string containing a Travis-CI API token. If missing, defaults to value stored in environment variable \env{TRAVIS_CI_TOKEN}.
#' @param ... Additional arguments passed to an HTTP request function, such as \code{\link[httr]{GET}}.
#' @return A list.
#' @export
travisHTTP <- function(verb = "GET",
                       path = "", 
                       query = list(),
                       body = "",
                       base = c("https://api.travis-ci.org", "https://api.travis-ci.com"),
                       token = Sys.getenv("TRAVIS_CI_TOKEN"),
                       ...) {
    base <- match.arg(base)
    url <- paste0(base, path)
    h <- httr::add_headers("Accept" = "application/vnd.travis-ci.2+json", 
                           "Authorization" = paste("token", token))
    if (!length(query)) query <- NULL
    if (verb == "GET") {
        r <- httr::GET(url, query = query, h, ...)
    } else if (verb == "DELETE") {
        r <- httr::DELETE(url, query = query, h, ...)
        s <- httr::http_status(r)
        if (s$category %in% c("Success","success")) {
            return(TRUE)
        } else {
            message(s$message)
            return(FALSE)
        }
    } else if (verb == "POST") {
        if(body == "") {
          r <- httr::POST(url, query = query, h, ...)
        } else {
          r <- httr::POST(url, body = body, query = query, h, ...)
        }
    } else if (verb == "PUT") {
        if(body == "") {
          r <- httr::PUT(url, query = query, h, ...)
        } else {
          r <- httr::PUT(url, body = body, query = query, h, ...)
        }
    } else if (verb == "PATCH") {
        if(body == "") {
            r <- httr::PATCH(url, query = query, h, ...)
        } else {
            r <- httr::PATCH(url, body = body, query = query, h, ...)
        }
    } else {
        stop("Unrecognized HTTP verb")
    }
    httr::stop_for_status(r)
    out <- httr::content(r, "parsed", encoding = "UTF-8")
    if (!is.null(names(out)) && "error" %in% names(out)) {
        warning(out$error$message)
    }
    out
}
