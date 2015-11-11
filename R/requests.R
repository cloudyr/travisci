#' @title GitHub Requests
#' @description Examine requests from GitHub to Travis CI
#' @details This can examine requests made by GitHub to initiate builds on Travis-CI, perhaps when a push fails to execute a build.
#' @param repo Optionally, a numeric repository ID (such as returned by this function) or a character string specifying a GitHub repository \dQuote{slug} (e.g., \samp{ghusername/ghreponame}). Must specify either \code{repo} or \code{request}.
#' @param request Optionally, a numeric request ID. Must specify either \code{repo} or \code{request}.
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
get_requests <- function(repo, request, ...) {
    if (missing(request)) {
        if (is.numeric(repo)) {
            query <- list(slug = repo)
        } else {
            query <- list(repository_id = repo)
        }
        travisHTTP("GET", path = paste0("/requests"), query = query, ...)
    } else {
        travisHTTP("GET", path = paste0("/requests/", request), ...)
    }
}
