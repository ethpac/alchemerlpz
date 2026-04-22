# get the total number of responses for a survey id
#' Total Responses to Alchemer Survey
#'
#' @param id Alchemer survey ID
#'
#' @returns Number of responses to the given survey
#' @export
#'
#' @examples
total_alchemer_responses <- function(id) {
  if (Sys.getenv('ALCHEMER_KEY') == "") {
    print("ERROR: Missing Alchemer authentication from alchemer_auth() function")
  } else if (stringr::str_length(Sys.getenv('ALCHEMER_KEY') > 0)) {
    response <- httr::GET(url = stringr::str_glue("https://api.alchemer.com/v5/survey/{id}?api_token={Sys.getenv('ALCHEMER_KEY')}&api_token_secret={Sys.getenv('ALCHEMER_SECRET')}")) |>
      httr::content()
    sum(response$data$statistics$Partial, response$data$statistics$Disqualified, response$data$statistics$Complete)
  } else {
    print("ERROR: Failed to get Alchemer responses")
  }
}
