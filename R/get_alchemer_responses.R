# getting responses from a designated alchemer survey id
#' Get Alchemer Responses
#'
#' @param id Alchemer survey ID
#' @param page Alchemer survey page
#' @param resultsperpage Number of results per page
#'
#' @returns Dataframe of Alchemer survey data
#' @export
#'
#' @examples
get_alchemer_responses <- function(id, page, resultsperpage) {
  if (Sys.getenv('ALCHEMER_KEY') == "") {
    print("ERROR: Missing Alchemer authentication from alchemer_auth() function")
  } else if (stringr::str_length(Sys.getenv('ALCHEMER_KEY') > 0)) {
    response <- httr::GET(url = stringr::str_glue("https://api.alchemer.com/v5/survey/{id}/surveyresponse?api_token={Sys.getenv('ALCHEMER_KEY')}&api_token_secret={Sys.getenv('ALCHEMER_SECRET')}&page={page}&resultsperpage={resultsperpage}")) |>
      httr::content("parsed")
    responses <- as.data.frame(do.call(rbind, response$data))
  } else {
    print("ERROR: Failed to get Alchemer responses")
  }
}
