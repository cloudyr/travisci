#' @title Get Travis Accounts
#' @description Retrieve GitHub accounts linked to the authenticated Travis user.
#' @details This is probably the closest thing to a \dQuote{Hello World!} on the API. It provides information about what accounts have been linked between GitHub and Travis, such as organization accounts that the user is a member of.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @export
get_accounts <- function(...) {
    travisHTTP("GET", path = "/accounts", ...)
}

get_users <- function(user, ...) {
    if (!missing(user)) {
        travisHTTP("GET", path = paste0("/users/", user), ...)
    } else {
        travisHTTP("GET", path = paste0("/users"), ...)
    }
}

sync_users <- function(...) {
    travisHTTP("POST", path = paste0("/users/sync"), ...)
}



get_annotations <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job, "/annotations"), ...)
}

create_annotation <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job, "/annotations"), ...)
}

get_broadcasts <- function(...) {
    travisHTTP("GET", path = "/broadcasts")
}



get_caches <- function(repo, ...) {
    travisHTTP("GET", path = paste0("/repos/", repo, "/caches"), ...)
}

delete_caches <- function(repo, ...) {
    travisHTTP("DELETE", path = paste0("/repos/", repo, "/caches"), ...)
}


get_hooks <- function(...) {
    travisHTTP("GET", path = paste0("/hooks"), ...)
}

enable_hook <- function(hook, ...) {
    travisHTTP("PUT", path = paste0("/hooks/"), 
               body = list(hook = list(id = hook, active = "true")), 
               encode = "json", ...)
}

disable_hook <- function(hook, ...) {
    travisHTTP("PUT", path = paste0("/hooks/"), 
               body = list(hook = list(id = hook, active = "false")), 
               encode = "json", ...)
}


get_job <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job), ...)
}
cancel_job <- function(job, ...) {
    travisHTTP("POST", path = paste0("/jobs/", job, "/cancel"), ...)
}

restart_job <- function(job, ...) {
    travisHTTP("POST", path = paste0("/jobs/", job, "/restart"), ...)
}


get_logs <- function(log, ...) {
    travisHTTP("GET", path = paste0("/logs/", log), ...)
}


get_permissions <- function(...) {
    travisHTTP("GET", path = "/users/permissions", ...)
}


get_requests <- function(request, ...) {
    if (missing(request)) {
        travisHTTP("GET", path = paste0("/requests"), ...)
    } else {
        travisHTTP("GET", path = paste0("/requests/", request), ...)
    }
}


# lint
