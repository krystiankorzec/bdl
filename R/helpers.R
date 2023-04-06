#' Set environment variable with BDL API token
#'
#' @param token a string with BDL API personal token
#' @return nothing
#' @examples
#' set_api_token(token = "123456qwerty)
#' @export
set_api_token <- function(token = NULL) {
  if (is.null(token)) {
    token <- askpass::askpass("Please enter your API token")
  }
  Sys.setenv("BDL_TOKEN" = token)
}