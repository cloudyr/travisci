get_annotations <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job, "/annotations"), ...)
}

create_annotation <- function(job, ...) {
    travisHTTP("GET", path = paste0("/jobs/", job, "/annotations"), ...)
}

get_broadcasts <- function(...) {
    travisHTTP("GET", path = "/broadcasts")
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
