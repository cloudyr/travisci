#' @title Linting
#' @description Lint a .travis.yml file
#' @details This can validate a \samp{.travis.yml} file and identify possible errors.
#' @param file A character string specifying a path to a \samp{.travis.yml} file.
#' @param ... Additional arguments passed to \code{\link{travisHTTP}}.
#' @return A list.
#' @examples
#' \dontrun{
#' # authenticate based on Sys.setenv("GITHUB_TOKEN" = "sometoken")
#' auth_travis()
#'
#' # validate .travis.yml
#' travis_lint("./.travis.yml")
#' }
#' @seealso \code{\link{get_permissions}} to check GitHub permissions
#' @export
travis_lint <- function(file, ...) {
    travisHTTP("GET", path = paste0("/requests"), 
               body = list(content = httr::upload_file(file)), ...)
}
