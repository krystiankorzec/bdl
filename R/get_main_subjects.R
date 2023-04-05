#' Get all main subjects of BDL API
#'
#' @return a list with subjects names, ids (tibble) and other subcategories
#' @examples
#' get_main_subjects()
#' @export
get_main_subjects <- function(){
  contents <- define_bdl_request() %>%
    httr2::req_url_path_append("subjects") %>%
    httr2::req_url_query(format = "json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  results <- contents[["results"]]
  links <- contents[["links"]]
  while(links$self != links$last){
    contents <- httr2::request(links$`next`) %>%
      add_bdl_token() %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
    results <- c(results, contents[["results"]])
    links <- contents[["links"]]
  }
  list(
    categories = tibble::tibble(
      category = purrr::map_chr(results,"name"),
      id = purrr::map_chr(results,"id")
      ),
    all = results
  )
}
