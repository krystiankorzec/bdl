get_token <- function(){
  token <- Sys.getenv("BDL_TOKEN")
  if (identical(token, "")){
    return(NULL)
  }else{
    return(token)
  }
}
define_bdl_request <- function(bdl_api_url = "https://bdl.stat.gov.pl/api/v1",
                               token = get_token()){
  httr2::request(bdl_api_url) %>%
    {if(!is.null(token)) httr2::req_headers(., `X-ClientId` = token) else . }
    
}
add_bdl_token <- function(req, token = get_token()){
  req %>%
    {if(!is.null(token)) httr2::req_headers(., `X-ClientId` = token) else . }
}
loop_over_links <- function(contents, rate){
  results <- contents[["results"]]
  links <- contents[["links"]]
  if (is.null(links)){
    warning("Empty links in contents variable!")
    return(NULL)
  }
  while(links$self != links$last){
    contents <- httr2::request(links$`next`) %>% 
      add_bdl_token() %>%
      httr2::req_throttle(rate) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()
    results <- c(results, contents[["results"]])
    links <- contents[["links"]]
  }
  results
}
