#' @title calculate
#' @name calculate
#' @description cscale, sscale
#' @param cscale
#' @param sscale

#' @export cscale
cscale <- function(x) {
  x %>%
    scale(center = T, scale = F)
}

#' @export sscale
sscale <- function(x) {
  x %>%
    scale(center = F, scale = T)
}
