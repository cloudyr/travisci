#' @title Manage environment variables
#' @description These functions retrieve and modify environment variables for the Travis-CI build environment, as a possible alternative to specifying them in a \samp{.travis.yml} file.
#' @details This can be useful for checking, creating, updating, or deleting environment variables used as build settings. See href{http://docs.travis-ci.com/user/environment-variables/}{the API documentation} for full details.
#' @param repo A character string specifying a repo slug (i.e., \samp{cloudyr/travisci}) a numeric Travis-CI repository ID, or an object of class \dQuote{travis_repo}. If a slug is used, it will be implicitly converted to a repository ID in some cases where only the latter is accepted.
#' @param id An alphanumeric ID for a given environment variable or an object of class \dQuote{travis_envvar}, for example as returned by one of these functions.
#' @param evar A list of environment variables as key-value pairs.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list of objects of class \dQuote{travis_envvar}, except for \code{delete_env_vars}, which returns a logical.
#' @seealso \code{\link{get_encryption_key}}, for handling encrypted environment variables
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get environment variables for a repo
#' get_env_vars("cloudyr/travisci")
#'
#' # get a specific environment variable based on its ID
#' get_env_vars("cloudyr/travisci", id = 12345)
#'
#' # set environment variables
#' e <- add_env_vars("cloudyr/travisci", var = list(VAR1 = "value1", VAR2 = "value2"))
#'
#' # update an environment variable
#' update_env_vars("cloudyr/travisci", id = e$id, 
#'                  var = list(VAR1 = "newvalue")
#' 
#' # delete an environment variable
#' delete_env_vars("cloudyr/travisci", id = e$id)
#' 
#' }
#' @export
get_env_vars <- function(repo, id, ...) {
    if (missing(id)) {
        out <- travisHTTP("GET", path = paste0("/settings/env_vars"), query = list(repository_id = slug_to_id(repo)), ...)
    } else {
        out <- travisHTTP("GET", path = paste0("/settings/env_vars/", id), query = list(repository_id = slug_to_id(repo)), ...)
    }
    if (length(out[[1]])) {
        lapply(out[[1]], `class<-`, "travis_envvar")
    } else {
        out[[1]]
    }
}

#' @export
#' @rdname get_env_vars
add_env_vars <- function(repo, evar, public = TRUE, ...) {
    out <- travisHTTP("POST", path = paste0("/settings/env_vars"), query = list(repository_id = slug_to_id(repo)), 
               body = list("env_var" = list(name = names(evar), value = as.character(evar), public = public)),
               encode = "json", ...)
    list(structure(out[[1]], class = "travis_envvar"))
}

#' @export
#' @rdname get_env_vars
update_env_vars <- function(repo, id, evar, public, ...) {
    if (inherits(id, "travis_envvar")) {
        repo <- id[["repository_id"]]
        if (missing(evar)) {
            b <- list("env_var" = list(name = id[["name"]], value = id[["value"]]))
        } else {
            b <- list("env_var" = list(name = names(evar), value = as.character(evar)))
        }
        if (missing(public)) {
            b[[1]][["public"]] <- id[["public"]]
        } else {
            b[[1]][["public"]] <- public
        }
        id <- id[["id"]]
    } else {
        b <- list("env_var" = list(name = names(evar), value = as.character(evar), public = public))
    }
    out <- travisHTTP("PATCH", path = paste0("/settings/env_vars/", id), query = list(repository_id = slug_to_id(repo)), 
               body = b,
               encode = "json", ...)
    list(structure(out[[1]], class = "travis_envvar"))
}

#' @export
#' @rdname get_env_vars
delete_env_var <- function(repo, id, ...) {
    if (inherits(id, "travis_envvar")) {
        repo <- id[["repository_id"]]
        id <- id[["id"]]
    }
    travisHTTP("DELETE", path = paste0("/settings/env_vars/", id), query = list(repository_id = slug_to_id(repo)), ...)
}

#' @export
print.travis_envvar <- function(x, ...) {
    cat("Repo:", x[["repository_id"]], "\n")
    cat("Environment Variable (id):", x[["id"]], "\n")
    cat(x[["name"]], ": ", x[["value"]], "\n", sep = "")
    cat("Public?", x[["public"]], "\n")
    invisible(x)
}

#' @title Manage encrypted environment variables
#' @description Retrieve and refresh the public key used for encryption.
#' @details These functions retrieve and refresh the public key used for encrypting secure environment variables for the Travis-CI build environment. This is probably more easily handled using the travis command-line tools, but it is also an implemented feature of the API and the functionality may be expanded in the future. See href{http://docs.travis-ci.com/user/encryption-keys/}{the API documentation} for full details. \code{get_encryption_key} retrieves the current, repository-specific public key. \code{set_encryption_key} resets the key and returns its value.
#' @param repo A character string specifying a repo slug (i.e., \samp{cloudyr/travisci}), a numeric Travis-CI repository ID, or an object of class \dQuote{travis_repo}. If a slug is used, it will be implicitly converted to a repository ID in some cases where only the latter is accepted.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_env_vars}}
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # get current public key
#' get_encryption_key("cloudyr/travisci")
#' 
#' # reset public key and retrieve the new value
#' set_encryption_key("cloudyr/travisci")
#' }
#' @export
get_encryption_key <- function(repo, ...) {
    travisHTTP("GET", path = paste0("/repos/", slug_to_id(repo), "/key"), ...)
}

#' @export
#' @rdname get_encryption_key
set_encryption_key <- function(repo, ...) {
    travisHTTP("POST", path = paste0("/repos/", slug_to_id(repo), "/key"), ...)
}
