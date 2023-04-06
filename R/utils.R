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
