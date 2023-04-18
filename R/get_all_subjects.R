#' Get lower level of subject
#'
#' @return a tibble with high level id (parent_id), name of lower level (name) and
#' its id
#' @param id subject id
#' @param rate maximum number of requests per second 60/60 by default
#' @examples
#' get_subject_by_id("K8")
#' @export
get_subject_by_id <- function(id, rate = 60/60){
  contents <- define_bdl_request() %>%
    httr2::req_url_path_append("subjects") %>%
    httr2::req_url_query(`parent-id` = id) %>%
    httr2::req_throttle(rate) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  results <- loop_over_links(contents, rate = rate)
  tibble::tibble(
    parent_id = id,
    name = purrr::map_chr(results, "name"),
    id = purrr::map_chr(results, "id")
  )
}
#' Get all main subjects of BDL API
#'
#' @return a tibble with main subjects ids and their lower levels
#' @examples
#' get_main_subjects(verbose = TRUE)
#' @param rate maximum number of requests per second 60/60 by default
#' @param verbose if true then print some info
#' @export
get_all_subjects <- function(rate = 60/60, verbose = TRUE){
  contents <- define_bdl_request() %>%
    httr2::req_url_path_append("subjects") %>%
    httr2::req_url_query(format = "json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
  results <- loop_over_links(contents, rate)
    main_subjects <- tibble::tibble(
      id = purrr::map_chr(results,"id"),
      name = purrr::map_chr(results,"name"),
      children = purrr::map(results,"children") 
    )
  # Iterate over main subjects to list all categories  
  output <- list()
  for(i in 1:length(main_subjects$children)){
    if (verbose) {
      n_vars <- length(main_subjects$children[[i]])
      print(paste(main_subjects$name[i], "#categories:", n_vars))
    }
    output[[i]] <- purrr::map(main_subjects$children[[i]],
                              ~get_subject_by_id(id = .))
  }
  main_subjects %>%
    tidyr::unnest("children") %>%
    dplyr::mutate(children = unlist(children)) %>%
    dplyr::rename(main_subject_name = name,
                  main_subject_id = id) %>%
    dplyr::left_join(
      purrr::map_dfr(output, rbind),
      by = c("children" = "parent_id")
    ) %>%
    dplyr::rename(variables_id = id,
                  subject_name = name,
                  subject_id = children)
}
