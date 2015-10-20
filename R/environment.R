#' @title Manage environment variables
#' @description These functions retrieve and modify environment variables for the Travis-CI build environment, as a possible alternative to specifying them in a \samp{.travis.yml} file.
#' @details This can be useful for checking, creating, updating, or deleting environment variables used as build settings. See href{http://docs.travis-ci.com/user/environment-variables/}{the API documentation} for full details.
#' @param repo A character string specifying a repo slug (i.e., \samp{cloudyr/travisci}) or a numeric Travis-CI repository ID. If a slug is used, it will be implicitly converted to a repository ID in some cases where only the latter is accepted.
#' @param id A numeric ID for a given environment variable, for example as returned by this function with this argument missing.
#' @param var A list of environment variables as key-value pairs.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_encryption_key}}
#' @export
get_env_vars <- function(repo, id, ...) {
    if (is.character(repo)) {
        repo <- slug_to_id(repo, ...)
    }
    if (missing(id)) {
        travisHTTP("GET", path = paste0("/settings/env_vars?repository_id=", repo), ...)
    } else {
        travisHTTP("GET", path = paste0("/settings/env_vars/", id, "?repository_id=", repo), ...)
    }
}

#' @export
#' @rdname get_env_vars
add_env_vars <- function(repo, var = list(), ...) {
    travisHTTP("POST", path = paste0("/settings/env_vars?repository_id=", repo), 
               body = list(env_var = var), encode = "json", ...)
}

#' @export
#' @rdname get_env_vars
update_env_vars <- function(repo, id, var = list(), ...) {
    travisHTTP("PATCH", path = paste0("/settings/env_vars/", id, "?repository_id=", repo), 
               body = list(env_var = var), encode = "json", ...)
}

#' @export
#' @rdname get_env_vars
delete_env_vars <- function(repo, id, ...) {
    travisHTTP("DELETE", path = paste0("/settings/env_vars/", id, "?repository_id=", repo), ...)
}


#' @title Manage encrypted environment variables
#' @description Retrieve and refresh the public key used for encryption.
#' @details These functions retrieve and refresh the public key used for encrypting secure environment variables for the Travis-CI build environment. This is probably more easily handled using the travis command-line tools, but it is also an implemented feature of the API and the functionality may be expanded in the future. See href{http://docs.travis-ci.com/user/encryption-keys/}{the API documentation} for full details.
#' @param repo A character string specifying a repo slug (i.e., \samp{cloudyr/travisci}) or a numeric Travis-CI repository ID. If a slug is used, it will be implicitly converted to a repository ID in some cases where only the latter is accepted.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @seealso \code{\link{get_env_vars}}
#' @export
get_encryption_key <- function(repo, ...) {
    travisHTTP("GET", path = paste0("/repos/", repo, "/key"), ...)
}

#' @export
#' @rdname get_encryption_key
set_encryption_key <- function(repo, ...) {
    travisHTTP("POST", path = paste0("/repos/", repo, "/key"), ...)
}
