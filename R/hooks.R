#' @title Get Hooks
#' @description Retrieve a list of Travis-CI web hooks.
#' @details \code{get_hooks} retrieves a list of hooks. \code{enable_hook} and \code{disable_hook} enable and disable hooks, respectively.
#' @param hook A numeric hook ID.
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
    travisHTTP("GET", path = paste0("/hooks"), ...)
}

#' @export
#' @rdname get_hooks
enable_hook <- function(hook, ...) {
    travisHTTP("PUT", path = paste0("/hooks/"), 
               body = list(hook = list(id = hook, active = "true")), 
               encode = "json", ...)
}

#' @export
#' @rdname get_hooks
disable_hook <- function(hook, ...) {
    travisHTTP("PUT", path = paste0("/hooks/"), 
               body = list(hook = list(id = hook, active = "false")), 
               encode = "json", ...)
}
