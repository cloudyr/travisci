#' @title Get Hooks
#' @description Retrieve a list of Travis-CI web hooks.
#' @details \code{get_hooks} retrieves a list of hooks. \code{enable_hook} and \code{disable_hook} enable and disable hooks, respectively.
#' @param hook A numeric hook ID, or an object of class \dQuote{travis_hook}.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get list of hooks
#' get_hooks()
#'
#' # disable a hook
#' disable_hook("12345")
#'
#' # enable a hook
#' enable_hook("1235")
#' }
#' @export
get_hooks <- function(...) {
    out <- travisHTTP("GET", path = paste0("/hooks"), ...)$hooks
    lapply(out, `class<-`, "travis_hook")
}

print.travis_hook <- function(x, ...) {
    cat("Hook (", x$id, "): ", x$name, "\n", sep = "")
    cat("Owner: ", x$owner_name, "\n", sep = "")
    cat("Description: ", x$description, "\n", sep = "")
    cat("Active:  ", as.character(x$active), "\n", sep = "")
    cat("Private: ", as.character(x$active), "\n", sep = "")
    cat("Admin:   ", as.character(x$active), "\n", sep = "")
    invisible(x)
}

#' @export
#' @rdname get_hooks
enable_hook <- function(hook, ...) {
    if (inherits(hook, "travis_hook")) {
        hook <- hook$id
    }
    travisHTTP("PUT", path = paste0("/hooks/"), 
               body = list(hook = list(id = hook, active = "true")), 
               encode = "json", ...)$result
}

#' @export
#' @rdname get_hooks
disable_hook <- function(hook, ...) {
    if (inherits(hook, "travis_hook")) {
        hook <- hook$id
    }
    travisHTTP("PUT", path = paste0("/hooks/"), 
               body = list(hook = list(id = hook, active = "false")), 
               encode = "json", ...)$result
}
