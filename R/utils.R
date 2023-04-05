define_bdl_request <- function(bdl_api_url = "https://bdl.stat.gov.pl/api/v1",
                               token = httr2::secret_decrypt(config::get("secret_scrambled"), "BDL_KEY")){
  httr2::request(bdl_api_url) %>%
    httr2::req_headers(`X-ClientId` = token)
}
add_bdl_token <- function(req, 
                          token = httr2::secret_decrypt(config::get("secret_scrambled"), "BDL_KEY")){
  req %>%
    httr2::req_headers(`X-ClientId` = token)
}
