#' @title Get and Delete Cache
#' @description Retrieve the Travis-CI cache, or delete it.
#' @details \code{get_caches} retrieves caches for a repository. \code{delete_caches} deletes all caches for a repository.
#' @param repo A numeric repository ID (such as returned by this function), a character string specifying a GitHub repository \dQuote{slug} (e.g., \samp{ghusername/ghreponame}), or an object of class \dQuote{travis_repo}.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get caches for a repo
#' get_caches("cloudyr/travisci")
#'
#' # delete caches
#' delete_caches("cloudyr/travisci")
#' }
#' @export
get_caches <- function(repo, ...) {
    if (inherits(repo, "travis_build")) {
        repo <- repo$id
    }
    travisHTTP("GET", path = paste0("/repos/", repo, "/caches"), ...)
}

#' @export
#' @rdname get_caches
delete_caches <- function(repo, ...) {
    if (inherits(repo, "travis_build")) {
        repo <- repo$id
    }
    travisHTTP("DELETE", path = paste0("/repos/", repo, "/caches"), ...)
}
