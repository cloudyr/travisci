slug_to_id <- function(slug, ...) {
    if (inherits(slug, "travis_repo")) {
        slug[["id"]]
    } else if (is.numeric(slug)) {
        slug
    } else if (is.character(slug)) {
        get_repo(slug, ...)[["id"]]
    } else {
        stop("Cannot coerce value to repo id")
    }
}
