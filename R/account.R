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
#' @export
get_accounts <- function(...) {
    travisHTTP("GET", path = "/accounts", ...)
}

#' @title Travis CI Users
#' @description Retrieve and sync Travis CI users
#' @details \code{get_users} retrieves information about GitHub users attached to a Travis account. \code{sync_users} syncs Travis's local cache of users against GitHub.
#' @param user Optionally, a character string specifying a user. If missing, all users are returned.
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
#' @export
get_users <- function(user, ...) {
    if (!missing(user)) {
        travisHTTP("GET", path = paste0("/users/", user), ...)
    } else {
        travisHTTP("GET", path = paste0("/users"), ...)
    }
}

#' @export
#' @rdname get_users
sync_users <- function(...) {
    travisHTTP("POST", path = paste0("/users/sync"), ...)
}
