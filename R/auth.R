#' @title Authorize Travis-CI API
#' @description Initialize a Travis-CI API token
#' @details Using the Travis-CI API requires an API token (see \href{http://docs.travis-ci.com/api/#authentication}{API documentation} for details). This function implements a sort of handshake that uses a GitHub personal access token to generate a Travis token (to store in an environment variable, \env{TRAVIS_CI_TOKEN}) or, alternatively, uses a GitHub username and password pair to generate such a token, conduct the handshake, and then cleanup the generated, temporary token. The easiest way to use the function is to set the \env{GITHUB_TOKEN} environment variable, then simply call \code{auth_travis()} with no arguments, but see examples.
#' @param username A character string containing a GitHub username.
#' @param password A character string containing a GitHub password for account \code{username}.
#' @param setenv A logical indicating whether to set the \env{TRAVIS_CI_TOKEN} environment variable. If \code{FALSE}, this will have to be passed to all other functions.
#' @param clean A logical specifying whether to delete the temporarily generated GitHub personal access token from the GitHub account. Deafult is \code{TRUE} if \code{username} is specified. This is ignored if passing a GitHub token directly.
#' @param gh_token An optional character string  specifying a GitHub personal access token. This is ignored if \code{username} is supplied.
#' @param base A character string specifying the base URL for the API. By default this is \url{https://api.travis-ci.org}, but may need to be changed if using a Travis Pro or enterprise account.
#' @return A character string containing the Travis-CI API token, invisibly.
#' @examples
#' \dontrun{
#' # authenticate using GitHub login credentials
#' auth_travis("username", "password")
#'
#' # authenticate using a GitHub personal access token
#' auth_travis(gh_token = "example token")
#'
#' # set GITHUB_TOKEN environment variable to authenticate
#' Sys.setenv("GITHUB_TOKEN" = "example token")
#' auth_travis()
#' }
#'
#' @seealso \code{\link{travisHTTP}}
#' @export
auth_travis <- 
function(username, 
         password, 
         setenv = TRUE, 
         clean = !missing(username), 
         gh_token = Sys.getenv("GITHUB_TOKEN"), 
         base = c("https://api.travis-ci.org", "https://api.travis-ci.com")) {
    gh <- gh_token
    if (!missing(username)) {
        b <- list(scopes = c("read:org", "user:email", "repo_deployment", "repo:status", "write:repo_hook"), 
                  note = "temporary token to auth against travis")
        p <- httr::POST("https://api.github.com/authorizations", 
                        httr::authenticate(username, password), 
                        body = b, encode = "json")
        gh <- httr::content(p)$token
        if (clean) {
            on.exit(httr::DELETE(paste0("https://api.github.com/authorizations/", 
                                 httr::content(p)$id), 
                                 httr::authenticate(username, password), encode = "json"))
        }
    }
    base <- match.arg(base)
    r <- httr::POST(paste0(base, "/auth/github"), 
                    httr::add_headers(Accept = "application/vnd.travis-ci.2+json"),
                    body = list(github_token = gh), encode = "json")
    travis_token <- jsonlite::fromJSON(httr::content(r, "text"))$access_token
    if (setenv) {
        Sys.setenv("TRAVIS_CI_TOKEN" = travis_token)
    }
    return(invisible(travis_token))
}
