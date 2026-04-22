# set key & secret values
#' Authenticate Alchemer API
#'
#' @param key Your Alchemer API v5 key
#' @param secret Your Alchemer API v5 secret
#'
#' @returns Environment variables
#' @export
#'
#' @examples
alchemer_auth <- function(key, secret) {
  Sys.setenv(ALCHEMER_KEY = key, ALCHEMER_SECRET = secret)
}
