#' @title GitHub Requests
#' @description Examine requests from GitHub to Travis CI
#' @details This can examine requests made by GitHub to initiate builds on Travis-CI, perhaps when a push fails to execute a build.
#' @param request Optionally, a numeric request ID.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get a list of requests
#' get_requests()
#'
#' # get a specific request
#' get_requests(12345)
#' }
#' @seealso \code{\link{get_permissions}} to check GitHub permissions
#' @export
get_requests <- function(request, ...) {
    if (missing(request)) {
        travisHTTP("GET", path = paste0("/requests"), ...)
    } else {
        travisHTTP("GET", path = paste0("/requests/", request), ...)
    }
}
