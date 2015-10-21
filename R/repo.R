#' @title Get Repo
#' @description Retrieve a Travis-CI repository
#' @details This retrieves a list of details about a given repository.
#' @param repo A numeric repository ID (such as returned by this function) or a character string specifying a GitHub repository \dQuote{slug} (e.g., \samp{ghusername/ghreponame}).
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_builds}}
#' @examples
#' \dontrun{
#' # get a repo
#' get_repo(repo = "cloudyr/travisci")
#'
#' # get a specific branch
#' get_branch(repo = "cloudyr/travisci", "master")
#'
#' # get recent builds for that repo
#' get_builds(repo = "cloudyr/travisci")
#' }
#' @seealso \code{\link{get_builds}}, \code{\link{get_branch}}
#' @export
get_repo <- function(repo, ...) {
    structure(travisHTTP("GET", path = paste0("/repos/", repo), ...)$repo, class = "travis_repo")
}

#' @title Get Branch
#' @description Retrieve branches for a repo
#' @details This can retrieve a list of recent branches for a given repo, or if \code{branch} is specified, details about a specific branch.
#' @param repo Optionally, a numeric repository ID (such as returned by this function) or a character string specifying a GitHub repository \dQuote{slug} (e.g., \samp{ghusername/ghreponame}).
#' @param branch A character string specifying the name of a branch.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_builds}}
#' @examples
#' \dontrun{
#' # get branches for a given repo
#' get_branch(repo = "cloudyr/travisci")
#'
#' # get a specific branch
#' get_branch(repo = "cloudyr/travisci", "master")
#' }
#' @seealso \code{\link{get_repo}}, \code{\link{get_builds}}
#' @export
get_branch <- function(repo, branch, ...) {
    if (!missing(branch)) {
        travisHTTP("GET", path = paste0("/repos/", repo, "/branches"), ...)
    } else {
        travisHTTP("GET", path = paste0("/repos/", repo, "/branches/", branch), ...)
    }
    
}

#' @title Get/Set Repo Settings
#' @description Get or set repository settings
#' @details \code{get_repo_settings} retrieves Travis-CI settings for a given repository. \code{set_repo_settings} modifies those settings based upon a list of input values. Use \code{\link{get_env_vars}} and \code{\link{add_env_vars}} to modify environment variables used in a build. Most of these features can also be modified using a \samp{.travis.yml} file in the GitHub repository itself (see \href{http://docs.travis-ci.com/user/languages/r/}{Building an R Project} in the Travis CI documentation for details).
#' @param repo A numeric repository ID (such as returned by this function) or a character string specifying a GitHub repository \dQuote{slug} (e.g., \samp{ghusername/ghreponame}).
#' @param settings A list containing named settings and their values. See results of \code{get_repo_settings} and \href{http://docs.travis-ci.com/api/#settings:-general}{the API documentation} for details.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_builds}}
#' @examples
#' \dontrun{
#' # get settings
#' get_repo_settings(repo = "cloudyr/travisci")
#'
#' # specify new settings
#' set_repo_settings(repo = "cloudyr/travisci", list("builds_only_with_travis_yml" = FALSE))
#' }
#' @seealso \code{\link{get_repo}}, \code{\link{get_env_vars}}
#' @export
get_repo_settings <- function(repo, ...) {
    travisHTTP("GET", path = paste0("/repos/", repo, "/settings"), ...)
}

#' @rdname get_repo_settings
#' @export
set_repo_settings <- function(repo, settings = list(), ...) {
    # builds_only_with_travis_yml
    # build_pushes
    # build_pull_requests
    # maximum_number_of_builds
    travisHTTP("PATCH", path = paste0("/repos/", repo, "/settings"), 
               body = list(settings = settings), encode = "json", ...)
}