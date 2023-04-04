#' Get all main subjects of BDL API
#'
#' @return a list with subjects names, ids and other subcategories
#' @examples
#' get_main_subjects()
#' @export
get_main_subjects <- function(){
  contents <- define_bdl_request() %>%
    req_url_path_append("subjects") %>%
    req_url_query(format = "json") %>%
    req_perform() %>%
    resp_body_json()
  results <- contents[["results"]]
  links <- contents[["links"]]
  while(links$self != links$last){
    contents <- request(links$`next`) %>%
      req_perform %>%
      resp_body_json()
    results <- c(results, contents[["results"]])
    links <- contents[["links"]]
  }
  list(
    name = map_chr(results,"name"),
    id = map_chr(results, "id"),
    all = results
  )
}
