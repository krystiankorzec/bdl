extract_data <- function(lol){
  lol$values %>% purrr::map_dfr(unlist) %>%
    dplyr::mutate(nuts_name = lol$name,
                  nuts_id = lol$id) %>%
    dplyr::select(nuts_id, nuts_name, dplyr::everything())
}
#' Get data by variable for a NUTS level
#'
#' @param variable_id an id of variable, you can list variables for each
#' subject using function `get_subject_variables()`
#' @param unit_level a NUTS level (0 to 6) a numeric value
#' @param rate maximum number of requests per second 60/60 by default
#' @return a tibble data and their NUTS ids and names (nuts_id, nuts_name)
#' @examples
#' get_data("1609969", 3)
#' @export
get_data <- function(variable_id, unit_level, rate = 60/60){
  contents <- define_bdl_request() %>%
    httr2::req_url_path_append("data","by-variable",variable_id) %>%
    httr2::req_url_query(`unit-level` = unit_level) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  
  loop_over_links(contents, rate) %>% 
    purrr::map_dfr(~extract_data(.))
}