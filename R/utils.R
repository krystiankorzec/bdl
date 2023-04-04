define_bdl_request <- function(bdl_api_url = "https://bdl.stat.gov.pl/api/v1"){
  httr2::request(bdl_api_url)
}
