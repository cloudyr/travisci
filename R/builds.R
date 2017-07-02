#' @title Get Builds
#' @description Retrieve Travis Builds
#' @details This can retrieve a list of recent builds (across all repos), recent builds for a specific repo (if \code{repo} is specified), or information about a specific build if \code{build} is (or both \code{repo} and \code{build} are) specified.
#' @param repo Optionally, a numeric repository ID (such as returned by this function), a character string specifying a GitHub repository \dQuote{slug} (e.g., \samp{ghusername/ghreponame}), or an object of class \dQuote{travis_repo}.
#' @param build Optionally, a numeric value specifying a build number, or an object of class \dQuote{travis_build}.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{cancel_build}}, \code{\link{restart_build}}
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get all recent builds
#' g <- get_builds()
#' 
#' # get builds for a specific repo
#' get_builds(repo = "cloudyr/travisci")
#' get_builds(repo = g[[1]]$repository_id)
#'
#' # get a specific build
#' get_builds(build = g[[1]]$id)
#' }
#' @seealso \code{\link{cancel_build}}, \code{\link{get_repo}}, \code{\link{get_branch}}
#' @export
get_builds <- function(repo, build, ...) {
    if (!missing(repo) & !missing(build)) {
        repo <- slug_to_id(repo)
        if (inherits(build, "travis_build")) {
            build <- build$id
        }
        out <- travisHTTP("GET", path = paste0("/repos/", repo, "/builds/", build), ...)
        structure(list(build = `class<-`(out$builds, "travis_build"), 
                       commit = `class<-`(out$commit, "travis_commit"),
                       jobs = lapply(out$jobs, `class<-`, "travis_job"),
                       annotations = out$annotations))
    } else if (!missing(repo)) {
        repo <- slug_to_id(repo)
        out <- travisHTTP("GET", path = paste0("/repos/", repo, "/builds"), ...)
        structure(list(builds = lapply(out$builds, `class<-`, "travis_build"), 
                       commits = lapply(out$commits, `class<-`, "travis_commit")))
    } else if (!missing(build)) {
        if (inherits(build, "travis_build")) {
            build <- build$id
        }
        out <- travisHTTP("GET", path = paste0("/builds/", build), ...)
        structure(list(build = `class<-`(out$builds, "travis_build"), 
                       commit = `class<-`(out$commit, "travis_commit"),
                       jobs = lapply(out$jobs, `class<-`, "travis_job"),
                       annotations = out$annotations))
    } else {
        out <- travisHTTP("GET", path = "/builds", ...)
        structure(list(builds = lapply(out$builds, `class<-`, "travis_build"), 
                       commits = lapply(out$commits, `class<-`, "travis_commit")))
        
    }
}

#' @export
print.travis_build <- function(x, ...) {
    cat("Build (", x$id, ") state: ", x$state, "\n", sep = "")
    cat("Type:   ", x$event_type, "\n", sep = "")
    if (x$pull_request) {
        cat("  PR (", x$pull_request_number, "): ", x$pull_request_title, "\n", sep = "")
    }
    cat("Repo:   ", x$repository_id, "\n", sep = "")
    cat("Commit: ", x$commit_id, "\n", sep = "")
    cat("Started:  ", x$started_at, "\n", sep = "")
    cat("Finished: ", x$finished_at, "\n", sep = "")
    invisible(x)
}

#' @export
print.travis_commit <- function(x, ...) {
    cat("Commit (", x$id, "): ", x$message, "\n", sep = "")
    cat("Author: ", x$author_name, " (", x$author_email, ")\n", sep = "")
    cat("SHA: ", x$sha, "\n", sep = "")
    cat("Commited: ", x$committed_at, "\n", sep = "")
    invisible(x)
}

#' @export
print.travis_job <- function(x, ...) {
    cat("Job (", x$id, ") state: ", x$state, "\n", sep = "")
    cat("Repo (", x$repository_id, "): ", x$repository_slug, "\n", sep = "")
    cat("Build: ", x$build_id, ", Commit: ", x$commit_id, "\n", sep = "")
    cat("Started:  ", x$started_at, "\n", sep = "")
    cat("Finished: ", x$finished_at, "\n", sep = "")
    invisible(x)
}

#' @title Cancel and Restart Builds
#' @description Cancel and restart Travis-CI builds
#' @details \code{cancel_build} will cancel a given build. \code{restart_build} will restart a cancelled build.
#' @param build A numeric value specifying a build number or an object of class \dQuote{travis_build}. (For \code{restart_last_build}, this can also be a character string specifying a GitHub repository, such as \dQuote{user/repo}.)
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return For \code{cancel_build} and \code{restart_build}, a logical that is \code{TRUE} if the operation succeeded. For \code{restart_last_build}, the build number is stored in the \code{build_id} attributes.
#' @seealso \code{\link{get_builds}}
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get all recent builds for this repo
#' g <- get_builds(repo = "cloudyr/travisci")
#' 
#' # cancel most recent build
#' cancel_build(g[[1]]$id)
#'
#' # restart that build
#' restart_build(g[[1]]$id)
#' }
#' @export
cancel_build <- function(build, ...) {
    if (inherits(build, "travis_build")) {
        build <- build$id
    }
    out <- travisHTTP("POST", path = paste0("/builds/", build, "/cancel"), ...)
    if (is.null(out)) {
        TRUE
    } else {
        FALSE
    }
}

#' @rdname cancel_build
#' @export
restart_build <- function(build, ...) {
    if (inherits(build, "travis_build")) {
        build <- build$id
    }
    out <- travisHTTP("POST", path = paste0("/builds/", build, "/restart"), ...)
    if (out$result) {
        TRUE
    } else {
        FALSE
    }
}

#' @rdname cancel_build
#' @export
restart_last_build <- function(build, ...) {
    if (inherits(build, "travis_build")) {
        b <- build$id
    } else if (is.numeric(build)) {
        b <- build
    } else {
        b <- get_builds(build)$builds[[1]]$id
    }
    structure(restart_build(b), build_id = b)
}
