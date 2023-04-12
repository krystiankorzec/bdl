#' Get all subject's variables
#'
#' @param an id of a subjects that has variables
#' @param rate maximum number of requests per second 60/60 by default
#' @return a tibble with variables and their metadata
#' @examples
#' get_subject_variables("P1754")
#' @export
get_subject_variables <- function(subject_id, rate = 60/60){
  contents <- define_bdl_request() %>%
    httr2::req_url_path_append("Variables") %>%
    httr2::req_url_query(`subject-id` = subject_id) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  output <- loop_over_links(contents, rate) %>% purrr::map_dfr(unlist)
  output
}