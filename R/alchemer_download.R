# function alchemer_download to get the whole survey
# that way i dont need page, resultsperpage
# with argument to input a partial survey and get any new responses
# (no way to check that the prev responses were from same id tho)
#' Download Alchemer Survey Responses
#'
#' @param id Alchemer survey ID
#' @param prev A previous download of the same survey
#' @param nresults How many responses to download at once
#' @param silent Silence progress messages
#'
#' @returns Dataframe of all responses to a given Alchemer survey
#' @export
#'
#' @examples
alchemer_download <- function(id, prev = NA, nresults = 500, silent = FALSE) {
  page_total <- ceiling(total_alchemer_responses(id)/nresults)
  if (is.data.frame(prev)) {
    page <- ceiling(nrow(prev)/nresults)
    rows <- nresults
    while (rows == nresults) {
      if (silent == FALSE) {
        print(paste0("Exporting page ", page, "/", page_total," of survey ID ", id, " from Alchemer"))
      }
      export <- get_alchemer_responses(id, page, nresults)
      prev <- dplyr::bind_rows(prev, export |> dplyr::filter(id > parse_number(tail(prev$id, 1)[[1]])))
      rows <- nrow(export)
      page <- page + 1
    }
    if (silent == FALSE) {
      print(paste0("Done exporting from Alchemer"))
    }
    return(prev)
  } else if (is.na(prev)) {
    page <- 1
    rows <- nresults
    compiled <- data.frame()
    while (rows == nresults) {
      if (silent == FALSE) {
        print(paste0("Exporting page ", page, "/", page_total," of survey ID ", id, " from Alchemer"))
      }
      export <- get_alchemer_responses(id, page, nresults)
      compiled <- dplyr::bind_rows(compiled, export)
      rows <- nrow(export)
      page <- page + 1
    }
    if (silent == FALSE) {
      print(paste0("Done exporting from Alchemer"))
    }
    return(compiled)
  } else {
    print("ERROR: Previous responses not in correct format")
  }
}
