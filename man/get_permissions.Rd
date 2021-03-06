% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/account.R
\name{get_permissions}
\alias{get_permissions}
\title{Get GitHub Permissions}
\usage{
get_permissions(...)
}
\arguments{
\item{...}{Additional arguments passed to \code{\link{travisHTTP}}.}
}
\value{
A list.
}
\description{
Retrieve GitHub permissions that have been authorized to Travis-CI.
}
\details{
This can be useful for checking what access rights have been granted to Travis-CI by GitHub. If for some reason Travis-CI isn't working, this might be useful for troubleshooting but probably not otherwise.
}
\examples{
\dontrun{
# authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
auth_travis()

# get travis accounts
get_permissions()
}
}
\seealso{
\code{\link{get_accounts}}, \code{\link{get_users}}, \code{\link{get_requests}} (to troubleshoot specific code pushes)
}
