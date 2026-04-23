# cleans up the raw alchemer export into a usable form
# able to decide whether or not to keep test data, disqualified, or metadata
# fixes timezone for date submitted
# titles questions based on their actual titles
# altho for checkboxes, has to append option number for those
# TODO: arg to instead try and use the checkbox text rather than the option number?
#' Clean Alchemer Responses
#'
#' @param df Raw dataframe exported via alchemer_download()
#' @param keep_test Keep test responses? (default false)
#' @param keep_dq Keep disqualified responses? (default false)
#' @param keep_metadata Keep metadata aside from response ID and date submitted? (default false)
#'
#' @returns Cleaned, unnested, formatted, retitled dataframe of Alchemer responses
#' @export
#'
#' @examples
alchemer_clean <- function(df, keep_test = FALSE, keep_dq = FALSE, keep_metadata = FALSE) {

  # decide whether to keep test data, dq'd responses, and metadata besides status and submission date
  if (keep_test == FALSE) {
    df <- df |>
      dplyr::filter(is_test_data == 0)
  }
  if (keep_dq == FALSE) {
    df <- df |>
      dplyr::filter(status != "Disqualified")
  }
  if (keep_metadata == FALSE) {
    df <- df |>
      dplyr::select(id, status, date_submitted, survey_data)
  }

  # initial unnest
  df <- df |>
    tidyr::unnest_wider(c(id, status, date_submitted), "_")

  # get proper timezone (defaults to UTC if not EST/EDT which it should be)
  timesplit <- df |> utils::head(1) |> dplyr::pull(date_submitted_1) |> stringr::str_split(" ")
  bad_timezone <- timesplit[[1]][3]
  if (bad_timezone %in% c("EST", "EDT")) {
    timezone <- "America/New_York"
  } else {
    timezone <- "UTC"
  }

  # format response id and date time submitted, fix status name
  df <- df |>
    dplyr::mutate(response_id = readr::parse_number(id_1)) |>
    dplyr::mutate(date_time_submitted = lubridate::parse_date_time(date_submitted_1, orders = "ymd HMS", tz = timezone)) |>
    dplyr::rename(status = status_1)

  # unnest survey data
  df <- df |>
    tidyr::unnest_wider(survey_data)

  # unnest question cols
  list_columns <- df |> purrr::keep(is.list) |> names()
  for (list_column in list_columns) {
    df <- df |>
      tidyr::unnest_wider(dplyr::all_of(list_column), names_sep = "_")
  }

  # get question titles
  questions <- df |>
    dplyr::select(tidyselect::ends_with("_question")) |>
    tidyr::pivot_longer(cols = everything(), names_to = "item", values_to = "title") |>
    dplyr::mutate(qnum = readr::parse_number(item)) |>
    stats::na.omit() |>
    unique() |>
    dplyr::mutate(clean_title = janitor::make_clean_names(title)) |>
    dplyr::select(qnum, clean_title)

  # renaming variables using the question titles
  df <- df |>
    dplyr::select(response_id, date_time_submitted, status, tidyselect::matches("_answer|_options")) |>
    dplyr::rename_with(~purrr::map_chr(readr::parse_number(.),
                         \(x) {
                           questions |>
                             dplyr::filter(qnum == x) |>
                             dplyr::pull(clean_title)}),
                tidyselect::ends_with("_answer")) |>
    dplyr::rename_with(~purrr::map_chr(readr::parse_number(.),
                         \(x) {
                           questions |>
                             dplyr::filter(qnum == x) |>
                             dplyr::pull(clean_title)}),
                       tidyselect::ends_with("_options")) |>
    dplyr::select(!(tidyselect::ends_with("_answer_id")))

  # cleaning up the list column ones
  # finds these by looking for digit at end of variable name
  df <- df |>
    tidyr::unnest_wider(tidyselect::everything(), names_sep = "_") |>
    dplyr::rename_with(~gsub("_1$", "", .x)) |>
    dplyr::mutate(dplyr::across(tidyselect::matches("\\d{5}$"), ~ purrr::map(.x, \(y) purrr::pluck(y, "answer")))) |>
    tidyr::unnest_wider(tidyselect::everything(), names_sep = "_") |>
    dplyr::rename_with(~gsub("_1$", "", .x)) |>
    dplyr::mutate(dplyr::across(tidyselect::matches("\\d{5}$"), ~ ifelse(. == "NULL", NA, .)))

  # get the cols to return
  return(df |>
           dplyr::select(response_id, date_time_submitted, status, tidyselect::everything()))
}
