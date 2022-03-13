#' @title Yuri
#'
#' @description cols, fills, t_theme, Yuri_theme, outer.IQR, Up
#'
#' @param cols
#'
#' @param fills
#'
#' @param t_theme
#'
#' @param Yuri_theme
#'
#' @param outer.IQR
#'
#' @param Up
#'
#' @return NULL
#'
#' @examples
#'
#' @export cols
cols <- c("#696AAD", "#FC766A", "#5BB3B0", "#95AAD3", "#EBBF57", "#E2654D", "#00997B", "#F8CDCD", "#34558B", "#B067A1", "#D85A7B", "#C34E7C")

#' @export fills
fills <- c("#696AAD", "#FC766A", "#5BB3B0", "#95AAD3", "#EBBF57", "#E2654D", "#00997B", "#F8CDCD", "#34558B", "#B067A1", "#D85A7B", "#C34E7C")

#' @export t_theme
windowsFonts(A = windowsFont("Source Han Sans CN"))#
t_theme <- theme(text = element_text(family = "Source Han Sans CN", color = 1), #
                 plot.title = element_text(size = 23, hjust = 0.5, angle = 0, lineheight = 1, margin = margin(0, 1, 0, 0)), #
                 plot.subtitle = element_text(size = 17, angle = 0, lineheight = 1, margin = margin(0, 0, 2, 0)), #
                 plot.caption = element_text(size = 14, angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), #
                 plot.tag = element_text(size = 15, angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), #
                 legend.title = element_text(size = 12, margin = margin(0, 0, 0, 0)), #
                 legend.text = element_text(size = 12, margin = margin(0, 0, 0, 0)), #
                 strip.text = element_text(size = 15, hjust = 0, margin = margin(0, 0, 0, 0)), #
                 axis.text.y = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 0, margin = margin(0, 1, 0, 0)), #
                 axis.title.y = element_text(size = 16, vjust = 0.5, hjust = 0.5, angle = 90, margin = margin(0, 3, 0, 0)), #
                 axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(1, 0, 0, 0)), #
                 axis.title.x = element_text(size = 15, vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(2, 0, 0, 0)), #
                 panel.background = element_rect(fill = NA), #
                 panel.border = element_rect(color = 1, fill = NA, size = 1), #
                 plot.background = element_rect(fill = NA, color = NA), #
                 legend.background = element_rect(fill = NA, color = NA), #
                 legend.key = element_rect(fill = NA, color = NA), #
                 strip.background = element_rect(fill = NA), #
                 axis.line = element_line(color = 1, size = 1, lineend = "square"), #
                 axis.ticks.x = element_line(color = 1, size = 1), #
                 axis.ticks.y = element_line(color = 1, size = 1), #
                 axis.ticks.length = unit(.15, "cm"), #刻度线长???
                 panel.grid.major.x = element_line(color = "#96999C", size = 0.5, linetype = 3), #
                 panel.grid.major.y = element_line(color = "#96999C", size = 0.5, linetype = 3), #
                 panel.grid.minor.x = element_line(color = "#96999C", size = 0.25, linetype = 3), #
                 panel.grid.minor.y = element_line(color = "#96999C", size = 0.25, linetype = 3), #
                 plot.margin = margin(t = 0.5, r = 0.5, b = 0.1, l = 0.5, unit = "cm"), #
                 panel.spacing = unit(.25, "cm"), #
                 legend.margin = margin(t = 0, r = 0, b = 0, l = -9), #
                 legend.position = ("right"), #
                 legend.key.height = unit(0.5, "cm"), #
                 legend.key.width = unit(0.5, "cm"), #
                 legend.key.size = unit(0.5, "cm"), #
                 legend.title.align = (0), #
                 legend.text.align = (0), #
                 legend.direction = "vertical", #
                 legend.justification = c(1, 1), #
                 legend.box = "horizontal", #
                 legend.box.just = (0.5), #
                 plot.caption.position = "panel",
                 plot.tag.position = "topleft",
                 plot.title.position = "plot",
                 #legend.key = element_rect(fill = "group", color = "group"), #
)

#' @export outer.IQR
outer.IQR <- function(x, multiple = 1.5, replace = FALSE, revalue = NA) {
  q <- quantile(x, na.rm = T)#
  IQR <- q[4]-q[2]
  x1 <- which(x<q[2]-multiple*IQR|x>q[4]+multiple*IQR)
  x2 <- x[x1]
  if(length(x2)>0)outlier <- data.table(location = x1, value = x2)
  else outlier <- data.table(location = 0, value = 0)
  if (replace == TRUE) {
    x[x1] <- revalue
  }
  return(list(new.value = x, outlier = outlier))
}

#' @export Yuri_theme
Yuri_theme <- theme(text = element_text(color = 1), #
                    plot.title = element_text(hjust = 0.5, angle = 0, lineheight = 1, margin = margin(0, 1, 0, 0)), #
                    plot.subtitle = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 2, 0)), #
                    plot.caption = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), #
                    plot.tag = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), #
                    legend.title = element_text(margin = margin(0, 0, 0, 0)), #
                    legend.text = element_text(margin = margin(0, 0, 0, 0)), #
                    strip.text = element_text(hjust = 0, margin = margin(0, 0, 0, 0)), #
                    axis.text.y = element_text(vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(0, 1, 0, 0)), #
                    axis.title.y = element_text(vjust = 0.5, hjust = 0.5, angle = 90, margin = margin(0, 3, 0, 0)), #
                    axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(1, 0, 0, 0)), #
                    axis.title.x = element_text(vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(2, 0, 0, 0)), #
                    panel.background = element_rect(fill = NA), #
                    panel.border = element_rect(color = 1, fill = NA, size = 1), #
                    plot.background = element_rect(fill = NA, color = NA), #
                    legend.background = element_rect(fill = NA, color = NA), #
                    legend.key = element_rect(fill = NA, color = NA), #
                    strip.background = element_rect(fill = NA), #
                    axis.line = element_line(color = 1, size = 1, lineend = "square"), #
                    axis.ticks.x = element_line(color = 1, size = 1), #
                    axis.ticks.y = element_line(color = 1, size = 1), #
                    #axis.ticks.length = unit(.15, "cm"), #
                    panel.grid.major.x = element_line(color = "#96999C", size = 0.5, linetype = 3), #
                    panel.grid.major.y = element_line(color = "#96999C", size = 0.5, linetype = 3), #
                    panel.grid.minor.x = element_line(color = "#96999C", size = 0.25, linetype = 3), #
                    panel.grid.minor.y = element_line(color = "#96999C", size = 0.25, linetype = 3), #
                    plot.margin = margin(t = 0.5, r = 0.5, b = 0.1, l = 0.5, unit = "cm"), #
                    #panel.spacing = unit(.25, "cm"), #
                    legend.margin = margin(t = 0, r = 0, b = 0, l = -9), #
                    legend.position = ("right"), #
                    #legend.key.height = unit(0.5, "cm"), #
                    #legend.key.width = unit(0.5, "cm"), #
                    #legend.key.size = unit(0.5, "cm"), #
                    legend.title.align = (0), #
                    legend.text.align = (0), #
                    legend.direction = "vertical", #
                    legend.justification = c(1, 1), #
                    legend.box = "horizontal", #
                    legend.box.just = (0.5), #
                    plot.caption.position = "panel",
                    plot.tag.position = "topleft",
                    plot.title.position = "plot",
                    #legend.key = element_rect(fill = "group", color = "group"), #
)

#' @export Up
Up <- print("rvcheck::update_all()")

