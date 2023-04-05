get_subject_by_id <- function(id){
  contents <- define_bdl_request() %>%
    req_url_path_append("subjects") %>%
    req_url_query(`parent-id` = id) %>%
    req_perform() %>%
    resp_body_json()
  results <- contents[["results"]]
  links <- contents[["links"]]
  while(links$self != links$last){
    contents <- request(links$`next`) %>% 
      add_bdl_token()
      req_perform %>%
      resp_body_json()
    results <- c(results, contents[["results"]])
    links <- contents[["links"]]
  }
  list(
    df = tibble::tibble(
      parent_id = id,
      name = purrr::map_chr(results, "name"),
      id = purrr::map_chr(results, "id")
    ),
    resp = results
  )
}
#' Get all subcategories of main subject
#'
#' @param main_subject_id main subject id e.g K8
#' @param sleep_time time in seconds between consecutive queries
#' @return a tibble with all subcategories and variables
#' @examples
#' get_main_subjects("K8")
#' @export
get_subject_subcategories <- function(main_subject_id, 
                                      sleep_time = 1){
  output <- list()
  i <- 1
  x <- get_subject_by_id(id = main_subject_id)
  output[[i]] <- x
  condition <- sum(map_lgl(x$resp, "hasVariables")) == 0
  while(condition){
    f <- function(id){Sys.sleep(sleep_time); get_subject_by_id(id)}
    x <- map(x$df$id, ~f(.))
    i <- i+1
    output[[i]] <- list(df = map_dfr(x, "df"),
                        resp = map(x, "resp"))
    has_variables <- map(x, ~map_lgl(.$resp, "hasVariables"))
    condition <- sum(unlist(has_variables)) == 0 
  }
  output
  dfs_list <- map(output, "df")
  my_left_join <- purrr::partial(dplyr::left_join, by = c("id" = "parent_id"))
  Reduce(my_left_join, dfs_list)
}