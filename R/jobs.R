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
#' @export
get_job <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job), ...)
}

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
