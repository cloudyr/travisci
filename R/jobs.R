#' @title Get Job
#' @description Retrieve a Travis-CI job
#' @details \code{get_job} retrieves a list of details about a given job. \code{cancel_job} cancels and \code{restart_job} restarts a given job.
#' @param job A numeric job ID.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get a job
#' get_job("12345")
#'
#' # cancel a job
#' cancel_job("12345")
#'
#' # restart a job
#' restart_job("1235")
#' }
#' @seealso \code{\link{get_annotations}}, \code{\link{get_logs}}
#' @export
get_job <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job), ...)
}

# print.travis_job <- function(x, ...) {}


#' @export
#' @rdname get_job
cancel_job <- function(job, ...) {
    travisHTTP("POST", path = paste0("/jobs/", job, "/cancel"), ...)
}

#' @export
#' @rdname get_job
restart_job <- function(job, ...) {
    travisHTTP("POST", path = paste0("/jobs/", job, "/restart"), ...)
}


#' @title Job Annotations
#' @description Retrieve and create job Travis CI annotations
#' @details \code{get_annotations} retrieves any annotations attached to a Travis CI job. \code{create_annotation} creates a new annotation for a job. Note: As of October, 2015 support for annotations is considered \dQuote{experimental}.
#' @param job A numeric job ID.
#' @param body A list of arguments specifying the annotation. See \href{http://docs.travis-ci.com/api/#annotations}{API documentation} for details.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_jobs}}
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get annotations for a job
#' get_annotations(12345)
#'
#' # create a new annotation
#' create_annotations(12345, body = list(description = "job description", status = "passed"))
#' }
#' @seealso \code{\link{get_jobs}}, \code{\link{get_logs}}
#' @export
get_annotations <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job, "/annotations"), ...)
}

#' @export
#' @rdname get_annotations
create_annotation <- function(job, body = list(), ...) {
    travisHTTP("GET", path = paste0("/jobs/", job, "/annotations"), body = body, encode = "json", ...)
}


#' @title Job Logs
#' @description Retrieve logs for Travis CI jobs
#' @details This can retrieve logs for a given job or a specific log by its job ID.
#' @param job A numeric job ID. Must specify \code{job} or \code{log}.
#' @param log A numeric log ID. Must specify \code{job} or \code{log}.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_jobs}}
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get logs for a job
#' get_logs(job = 12345)
#' }
#' @seealso \code{\link{get_jobs}}, \code{\link{get_annotations}}
#' @export
get_logs <- function(job, log, ...) {
    if (!missing(log)) {
        travisHTTP("GET", path = paste0("/logs/", log), ...)
    } else {
        travisHTTP("GET", path = paste0("/jobs/", job, "/logs"), ...)
    }
}
