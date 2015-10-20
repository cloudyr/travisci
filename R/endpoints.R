get_accounts <- function(...) {
    travisHTTP("GET", path = "/accounts", ...)
}

get_annotations <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job, "/annotations"), ...)
}

create_annotation <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job, "/annotations"), ...)
}

get_branch <- function(repo, branch, ...) {
    if (!missing(branch)) {
        travisHTTP("GET", path = paste0("/repos/", repo, "/branches"), ...)
    } else {
        travisHTTP("GET", path = paste0("/repos/", repo, "/branches/", branch), ...)
    }
    
}

get_broadcasts <- function(...) {
    travisHTTP("GET", path = "/broadcasts")
}

get_builds <- function(repo, build, ...) {
    if (!missing(repo) & !missing(build)) {
        travisHTTP("GET", path = paste0("/repos/", repo, "/builds/", build), ...)
    } else if(!missing(repo)) {
        travisHTTP("GET", path = paste0("/repos/", repo, "/builds"), ...)
    } else if(!missing(build)) {
        travisHTTP("GET", path = paste0("/builds/", build), ...)
    } else {
        travisHTTP("GET", path = "/builds", ...)
    }
}

cancel_build <- function(build, ...) {
    travisHTTP("POST", path = paste0("/builds/", build, "/cancel"), ...)
}

restart_build <- function(build, ...) {
    travisHTTP("POST", path = paste0("/builds/", build, "/restart"), ...)
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


get_repo <- function(repo, ...) {
    structure(travisHTTP("GET", path = paste0("/repos/", repo), ...)$repo, class = "travis_repo")
}


get_requests <- function(request, ...) {
    if (missing(request)) {
        travisHTTP("GET", path = paste0("/requests"), ...)
    } else {
        travisHTTP("GET", path = paste0("/requests/", request), ...)
    }
}

get_repo_settings <- function(repo, ...) {
    travisHTTP("GET", path = paste0("/repos/", repo, "/settings"), ...)
}

set_repo_settings <- function(repo, settings = list(), ...) {
    # builds_only_with_travis_yml
    # build_pushes
    # build_pull_requests
    # maximum_number_of_builds
    travisHTTP("PATCH", path = paste0("/repos/", repo, "/settings"), 
               body = list(settings = settings), encode = "json", ...)
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


# lint

