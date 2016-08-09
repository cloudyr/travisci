#' @title Get Repo
#' @description Retrieve a Travis-CI repository
#' @details This retrieves a list of details about a given repository.
#' @param repo A numeric repository ID (such as returned by this function), a character string specifying a GitHub repository \dQuote{slug} (e.g., \samp{ghusername/ghreponame}), or an object of class \dQuote{travis_repo}.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_builds}}
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
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
    if (inherits(repo, "travis_repo")) {
        repo <- slug_to_id(repo)
    }
    structure(travisHTTP("GET", path = paste0("/repos/", repo), ...)$repo, class = "travis_repo")
}

#' @export
print.travis_repo <- function(x, ...) {
    cat("Repo (", x$id, "): ", x$slug, "\n", sep = "")
    cat("Active: ", as.character(x$active), "\n", sep = "")
    cat("Description: ", x$description, "\n", sep = "")
    cat("Language: ", x$github_language, "\n", sep = "")
    cat("Last Build (", x$last_build_id, ") Status: ", x$last_build_state, "\n", sep = "")
    cat("Last Build Finished: ", x$last_build_finished_at, "\n\n", sep = "")
    invisible(x)
}

#' @title Get Branch
#' @description Retrieve branches for a repo
#' @details This can retrieve a list of recent branches for a given repo, or if \code{branch} is specified, details about a specific branch.
#' @param repo A numeric repository ID (such as returned by this function), a character string specifying a GitHub repository \dQuote{slug} (e.g., \samp{ghusername/ghreponame}), or an object of class \dQuote{travis_repo}.
#' @param branch Optionally, a character string specifying the name of a branch.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_builds}}
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get branches for a given repo
#' get_branch(repo = "cloudyr/travisci")
#'
#' # get a specific branch
#' get_branch(repo = "cloudyr/travisci", "master")
#' }
#' @seealso \code{\link{get_repo}}, \code{\link{get_builds}}
#' @export
get_branch <- function(repo, branch, ...) {
    repo <- slug_to_id(repo)
    if (missing(branch)) {
        out <- travisHTTP("GET", path = paste0("/repos/", repo, "/branches"), ...)
        structure(list(branches = lapply(out$branches, `class<-`, "travis_branch"),
                       commits = lapply(out$commits, `class<-`, "travis_commit")))
    } else {
        out <- travisHTTP("GET", path = paste0("/repos/", repo, "/branches/", branch), ...)
        structure(list(branch = `class<-`(out$branches, "travis_branch"),
                       commits = `class<-`(out$commits, "travis_commit")))
    }
}

#' @export
print.travis_branch <- function(x, ...) {
    cat("Branch (", x$id, ") state: ", x$state, "\n", sep = "")
    cat("Repo:   ", x$repository_id, "\n", sep = "")
    cat("Commit: ", x$commit_id, "\n", sep = "")
    cat("Started:  ", x$started_at, "\n", sep = "")
    cat("Finished: ", x$finished_at, "\n", sep = "")
    invisible(x)
}

#' @title Get/Set Repo Settings
#' @description Get or set repository settings
#' @details \code{get_repo_settings} retrieves Travis-CI settings for a given repository. \code{set_repo_settings} modifies those settings based upon a list of input values. Use \code{\link{get_env_vars}} and \code{\link{add_env_vars}} to modify environment variables used in a build. Most of these features can also be modified using a \samp{.travis.yml} file in the GitHub repository itself (see \href{http://docs.travis-ci.com/user/languages/r/}{Building an R Project} in the Travis CI documentation for details).
#' @param repo A numeric repository ID (such as returned by this function), a character string specifying a GitHub repository \dQuote{slug} (e.g., \samp{ghusername/ghreponame}), or an object of class \dQuote{travis_repo}.
#' @param settings A list containing named settings and their values. See results of \code{get_repo_settings} and \href{http://docs.travis-ci.com/api/#settings:-general}{the API documentation} for details. The response from either \code{get_repo_settings} or \code{set_repo_settings} can be passed directly as the value of this argument.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list of settings.
#' @seealso \code{\link{get_builds}}
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get settings
#' g <- get_repo_settings(repo = "cloudyr/travisci")
#'
#' # pass modified settings list
#' g[[1]] <- TRUE
#' set_repo_settings(repo = "cloudyr/travisci", g)
#'
#' # specify new settings individually
#' set_repo_settings(repo = "cloudyr/travisci", list("builds_only_with_travis_yml" = FALSE))
#' }
#' @seealso \code{\link{get_repo}}, \code{\link{get_env_vars}}
#' @export
get_repo_settings <- function(repo, ...) {
    repo <- slug_to_id(repo)
    travisHTTP("GET", path = paste0("/repos/", repo, "/settings"), ...)$settings
}

#' @rdname get_repo_settings
#' @export
set_repo_settings <- function(repo, settings = list(), ...) {
    # builds_only_with_travis_yml
    # build_pushes
    # build_pull_requests
    # maximum_number_of_builds
    s <- c("builds_only_with_travis_yml",
           "build_pushes",
           "build_pull_requests",
           "maximum_number_of_builds")
    settings <- settings[names(settings) %in% s]
    repo <- slug_to_id(repo)
    travisHTTP("PATCH", path = paste0("/repos/", repo, "/settings"), 
               body = list(settings = settings), encode = "json", ...)$settings
}
