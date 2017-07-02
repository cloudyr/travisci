#' @title Travis-CI API Client
#' @description This package provides functionality for interacting with the Travis-CI API. Travis-CI is a continuous integration service that allows for automated testing of software each time that software is publicly committed to a repository on GitHub. Setting up Travis-CI is quite simple, requiring only a GitHub account, some public (or private) repository hosted on GitHub, and logging into to Travis to link it to that repository. Travis-CI provides \href{http://docs.travis-ci.com/user/languages/r/}{straightforward documentation} for configuring your repository to interact with Travis via a \samp{.travis.yml} file added to your repository.
#'
#' Once you have your Travis-CI account configured online, you can use this package to interact with and perform all operations on your Travis builds that you would normally perform by the Travis website. This includes monitoring builds, modifying build environment settings and environment variables, and cancelling or restarting builds.
#'
#' Before you can use the package, you need to authenticate against the Travis-CI API using \code{\link{auth_travis}}, using your GitHub username and password, a GitHub personal access token, or a previously generated Travis API token (see examples, below).
#'
#' @examples
#' \dontrun{
#' # authenticate using your GitHub login credentials
#' auth_travis("username", "password")
#' 
#' # authenticate using a GitHub token explicitly
#' auth_travis(gh_token = "githubtokenvalue")
#'
#' # authenticate using a stored environment variables
#' Sys.setenv("GITHUB_TOKEN" = "githubtokenvalue")
#' 
#' # check to see if you've authenticated correctly
#' get_accounts()
#' }
#'
#' @docType package
#' @seealso \code{\link{auth_travis}}, \code{\link{get_accounts}}, \code{\link{get_repo}}
#' @name travisci
NULL

#' @title Get Broadcasts
#' @description Retrieve Travis-CI broadcasts
#' @details This is not particularly useful from an R perspective, but it will retrieve \dQuote{broadcasts} or news from Travis-CI.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get broadcasts
#' get_broadcasts()
#' }
#' @export
get_broadcasts <- function(...) {
    travisHTTP("GET", path = "/broadcasts")
}

#' @title Get Travis Accounts
#' @description Retrieve GitHub accounts linked to the authenticated Travis user.
#' @details This is probably the closest thing to a \dQuote{Hello World!} on the API. It provides information about what accounts have been linked between GitHub and Travis, such as organization accounts that the user is a member of.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get travis accounts
#' get_accounts()
#' }
#' @seealso \code{\link{get_users}}, \code{\link{get_permissions}}
#' @export
get_accounts <- function(...) {
    lapply(travisHTTP("GET", path = "/accounts", ...)$accounts, `class<-`, "travis_account")
}

#' @export
print.travis_account <- function(x, ...) {
    cat("Account (", x$id, "): ", x$name, "\n", sep = "")
    cat("Type: ", x$type, "\n", sep = "")
    cat("Login: ", x$login, "\n", sep = "")
    cat("Repos: ", x$repos_count, "\n", sep = "")
    invisible(x)
}

#' @title Get GitHub Permissions
#' @description Retrieve GitHub permissions that have been authorized to Travis-CI.
#' @details This can be useful for checking what access rights have been granted to Travis-CI by GitHub. If for some reason Travis-CI isn't working, this might be useful for troubleshooting but probably not otherwise.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get travis accounts
#' get_permissions()
#' }
#' @seealso \code{\link{get_accounts}}, \code{\link{get_users}}, \code{\link{get_requests}} (to troubleshoot specific code pushes)
#' @export
get_permissions <- function(...) {
    travisHTTP("GET", path = "/users/permissions", ...)
}


#' @title Travis CI Users
#' @description Retrieve and sync Travis CI users
#' @details \code{get_users} retrieves information about GitHub users attached to a Travis account. \code{sync_users} syncs Travis's local cache of users against GitHub.
#' @param user Optionally, an integer specifying a user ID, or a character string specifying a user login, or a \dQuote{travis_user} object. If missing, all users are returned.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # sync users
#' sync_users()
#' 
#' # get travis users
#' get_users()
#'
#' # get specific user
#' get_user("userid")
#' }
#' @seealso \code{\link{get_accounts}}, \code{\link{get_permissions}}
#' @export
get_users <- function(user, ...) {
    if (!missing(user)) {
        if (inherits(user, "travis_user")) {
            user <- user$id
        } else if (inherits(user, "character")) {
            users <- get_users(...)
            logins <- lapply(users, `[[`, "login")
            user <- users[[which(logins == user)]][["id"]]
        }
        out <- travisHTTP("GET", path = paste0("/users/", user), ...)
    } else {
        out <- travisHTTP("GET", path = paste0("/users"), ...)
    }
    lapply(out, `class<-`, "travis_user")
}

#' @export
print.travis_user <- function(x, ...) {
    cat("User (", x$id, "): ", x$name, "\n", sep = "")
    cat("Login: ", x$login, "\n", sep = "")
    cat("Email: ", x$email, "\n", sep = "")
    cat("Correct scopes: ", as.character(x$correct_scopes), "\n", sep = "")
    invisible(x)
}

#' @export
#' @rdname get_users
sync_users <- function(...) {
    travisHTTP("POST", path = paste0("/users/sync"), ...)$result
}
