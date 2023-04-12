#' Get Polish NUTS classification 
#'
#' @param level (0 to 6) a numeric value
#' @param rate maximum number of requests per second 60/60 by default
#' @return a tibble with NUTS classification
#' @examples
#' get_nuts_by_level(level = 3)
#' @export
get_nuts_by_level <- function(level, rate = 60/60){
  contents <- define_bdl_request() %>%
    httr2::req_url_path_append("Units") %>%
    httr2::req_url_query(level = level) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  output <- loop_over_links(contents, rate) %>% purrr::map_dfr(unlist)
  output
}