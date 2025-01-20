#' @title calculate
#' @name calculate
#' @description cscale, sscale, minmax, outliers_iqr, outliers_q99
#' @param cscale
#' @param sscale
#' @param minmax
#' @param outliers_iqr
#' @param outliers_q99

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

#' @export minmax
minmax <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#' @export outliers_iqr
outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25) # 计算第一四分位数
  q3 <- quantile(x, 0.75) # 计算第三四分位数
  iqr <- IQR(x) # 计算四分位间距
  upper_limit <- q3 + 1.5 * iqr # 计算离群值上限
  lower_limit <- q1 - 1.5 * iqr # 计算离群值下限

  # 将超过上限的离群值替换为Q3+1.5*IQR
  x[x > upper_limit] <- upper_limit
  # 将超过下限的离群值替换为Q1-1.5*IQR
  x[x < lower_limit] <- lower_limit
  return(x)
}

#' @export outliers_q99
outliers_q99 <- function(x) {
  q1 <- quantile(x, 0.01)
  q9 <- quantile(x, 0.99)
  x[x > q9] <- q9
  x[x < q1] <- q1
  return(x)
}

#' @export outliers_3sig
outliers_3sig <- function(x) {
  mean_x <- mean(x, na.rm = T)
  sd_x <- sd(x, na.rm = T)
  upper_limit <- mean_x + 3 * sd_x # 计算离群值上限
  lower_limit <- mean_x - 3 * sd_x # 计算离群值下限
  x[x > upper_limit] <- upper_limit
  x[x < lower_limit] <- lower_limit
  return(x)
}
