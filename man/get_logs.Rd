% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobs.R
\name{get_logs}
\alias{get_logs}
\title{Job Logs}
\usage{
get_logs(job, log, ...)
}
\arguments{
\item{job}{A numeric job ID, or an object of class \dQuote{travis_job}. Must specify \code{job} or \code{log}.}

\item{log}{A numeric log ID. Must specify \code{job} or \code{log}.}

\item{...}{Additional arguments passed to \code{\link{travisHTTP}}.}
}
\value{
A list.
}
\description{
Retrieve logs for Travis CI jobs
}
\details{
This can retrieve logs for a given job or a specific log by its job ID.
}
\examples{
\dontrun{
# authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
auth_travis()

# get logs for a job
get_logs(job = 12345)
}
}
\seealso{
\code{\link{get_job}}

\code{\link{get_job}}, \code{\link{get_annotations}}
}
