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
#' @param cclean
#'
#' @return NULL
#'
#' @examples
#' @export col1
col1 <- c("#440154", "#470E61", "#481B6D", "#482576", "#46307E", "#443B84", "#404688", "#3C508B", "#38598C", "#33628D", "#2F6B8E", "#2C738E", "#287C8E", "#25838E", "#228C8D", "#1F948C", "#1E9D89", "#20A486", "#26AD81", "#31B57B", "#3FBC73", "#4FC46A", "#61CB5F", "#75D054", "#8BD646", "#A2DA37", "#B9DE28", "#D1E11C", "#E8E419", "#FDE725")

#' @export cols
cols <- c("#696AAD", "#FC766A", "#5BB3B0", "#95AAD3", "#EBBF57", "#E2654D", "#00997B", "#F8CDCD", "#34558B", "#B067A1", "#D85A7B", "#C34E7C")

#' @export fills
fills <- c("#696AAD", "#FC766A", "#5BB3B0", "#95AAD3", "#EBBF57", "#E2654D", "#00997B", "#F8CDCD", "#34558B", "#B067A1", "#D85A7B", "#C34E7C")

#' @export t_theme
windowsFonts(A = windowsFont("Source Han Sans CN")) # 字体设置
t_theme <- theme(
  text = element_text(family = "Source Han Sans CN", color = 1), # 全字体
  plot.title = element_text(size = 23, hjust = 0.5, angle = 0, lineheight = 1, margin = margin(0, 1, 0, 0)), # 标题
  plot.subtitle = element_text(size = 17, angle = 0, lineheight = 1, margin = margin(0, 0, 2, 0)), # 副标题
  plot.caption = element_text(size = 14, angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), # 说明文字
  plot.tag = element_text(size = 15, angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), # 标签
  legend.title = element_text(size = 12, margin = margin(0, 0, 0, 0)), # 图例标题
  legend.text = element_text(size = 12, margin = margin(0, 0, 0, 0)), # 图例文字
  strip.text.x = element_text(size = 15, hjust = 0.5, margin = margin(0, 0, 0, 0)), # 水平分面标签文本
  strip.text.y = element_text(size = 15, vjust = 0.5, margin = margin(0, 0, 0, 0)), # 竖直分面标签文本
  axis.text.y = element_text(size = 14, vjust = 0.5, hjust = 1, angle = 0, margin = margin(0, 1, 0, 0)), # Y轴数字标
  axis.title.y = element_text(size = 16, vjust = 0.5, hjust = 0.5, angle = 90, margin = margin(0, 3, 0, 0)), # Y轴标题
  axis.text.x = element_text(size = 14, vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(1, 0, 0, 0)), # X轴数字标
  axis.title.x = element_text(size = 15, vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(2, 0, 0, 0)), # X轴标题
  panel.background = element_rect(fill = NA), # 绘图区域背景
  panel.border = element_rect(color = 1, fill = NA, size = 1), # 绘图区边框
  plot.background = element_rect(fill = NA, color = NA), # 文字区域+图像背景
  legend.background = element_rect(fill = NA, color = NA), # 图例背景
  legend.key = element_rect(fill = NA, color = NA), # 图例符号背景
  strip.background = element_rect(fill = NA), # 分面标签背景
  axis.line = element_line(color = 1, size = 1, lineend = "square"), # 坐标轴
  axis.ticks.x = element_line(color = 1, size = 0.5), # X轴刻度线
  axis.ticks.y = element_line(color = 1, size = 0.5), # Y轴刻度线
  axis.ticks.length = unit(.15, "cm"), # 刻度线长度
  panel.grid.major.x = element_line(color = "#96999C", size = 0.5, linetype = 3), # 主竖线
  panel.grid.major.y = element_line(color = "#96999C", size = 0.5, linetype = 3), # 主横线
  panel.grid.minor.x = element_line(color = "#96999C", size = 0.25, linetype = 3), # 次竖线
  panel.grid.minor.y = element_line(color = "#96999C", size = 0.25, linetype = 3), # 次横线
  plot.margin = margin(t = 0.5, r = 0.5, b = 0.1, l = 0.5, unit = "cm"), # 图边界(距离可以为0或者为负值)
  panel.spacing.x = unit(.25, "cm"), # 竖直分面绘图区之间的间距
  panel.spacing.y = unit(.25, "cm"), # 水平分面绘图区之间的间距
  legend.margin = margin(t = 0, r = 0, b = 0, l = -9), # 图例边界, unit = "cm"
  legend.position = ("right"), # 图例位置left, right, bottom, top, c(0.9, 0.9)
  legend.key.height = unit(0.5, "cm"), # 图例符号高度
  legend.key.width = unit(0.5, "cm"), # 图例符号宽度
  legend.key.size = unit(0.5, "cm"), # 图例符号大小
  legend.title.align = (0), # 图例标题对齐方式(0为左齐, 1为右齐, 0.5居中)
  legend.text.align = (0), # 图例文字标签对齐方式(0为左齐, 1为右齐, 0.5居中)
  legend.direction = "vertical", # 图例排列方向"horizontal"(水平一行)
  legend.justification = c(1, 1), # 图例位置	center或两数字向量
  legend.box = "vertical", # 多图例的排列方式"horizontal"
  legend.box.just = (0.5), # 多图例居中方式
  plot.caption.position = "panel",
  plot.tag.position = "topleft",
  plot.title.position = "plot",
  # aspect.ratio(7/5),
  # legend.key = element_rect(fill = "group", color = "group"),#图例符号
)

#' @export outer.IQR
outer.IQR <- function(x, multiple = 1.5, replace = FALSE, revalue = NA) {
  q <- quantile(x, na.rm = T) #
  IQR <- q[4] - q[2]
  x1 <- which(x < q[2] - multiple * IQR | x > q[4] + multiple * IQR)
  x2 <- x[x1]
  if (length(x2) > 0) {
    outlier <- data.table(location = x1, value = x2)
  } else {
    outlier <- data.table(location = 0, value = 0)
  }
  if (replace == TRUE) {
    x[x1] <- revalue
  }
  return(list(new.value = x, outlier = outlier))
}

#' @export Yuri_theme
Yuri_theme <- theme(
  text = element_text(family = "Source Han Sans CN", color = 1), # 全字体
  plot.title = element_text(size = 12, hjust = 0.5, angle = 0, lineheight = 1, margin = margin(0, 1, 0, 0)), # 标题
  plot.subtitle = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 2, 0)), # 副标题
  plot.caption = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), # 说明文字
  plot.tag = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), # 标签
  legend.title = element_text(margin = margin(0, 0, 0, 0)), # 图例标题
  legend.text = element_text(margin = margin(0, 0, 0, 0)), # 图例文字
  strip.text.x = element_text(hjust = 0.5, margin = margin(0, 0, 1, 0)), # 水平分面标签文本
  strip.text.y = element_text(vjust = 0.5, margin = margin(0, 0, 0, 1)), # 竖直分面标签文本
  axis.text.y = element_text(vjust = 0.5, hjust = 1, angle = 0, margin = margin(0, 1, 0, 0)), # Y轴数字标
  axis.title.y = element_text(vjust = 0.5, hjust = 0.5, angle = 90, margin = margin(0, 3, 0, 0)), # Y轴标题
  axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(1, 0, 0, 0)), # X轴数字标
  axis.title.x = element_text(vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(2, 0, 0, 0)), # X轴标题
  panel.background = element_rect(fill = NA), # 绘图区域背景
  panel.border = element_rect(color = NA, fill = NA, size = 1), # 绘图区边框
  plot.background = element_rect(fill = NA, color = NA), # 文字区域+图像背景
  legend.background = element_rect(fill = NA, color = NA), # 图例背景
  legend.key = element_rect(fill = NA, color = NA), # 图例符号背景
  strip.background = element_rect(fill = NA), # 分面标签背景
  axis.line = element_line(color = "#96999C", size = 1, lineend = "square"), # 坐标轴
  axis.ticks.x = element_line(color = "#96999C", size = 0.5, lineend = "square"), # X轴刻度线
  axis.ticks.y = element_line(color = "#96999C", size = 0.5, lineend = "square"), # Y轴刻度线
  axis.ticks.length = unit(.15, "cm"), # 刻度线长度
  panel.grid.major.x = element_line(color = "#96999C", size = 0.5, linetype = 3, lineend = "square"), # 主竖线
  panel.grid.major.y = element_line(color = "#96999C", size = 0.5, linetype = 3, lineend = "square"), # 主横线
  panel.grid.minor.x = element_line(color = "#96999C", size = 0.25, linetype = 3, lineend = "square"), # 次竖线
  panel.grid.minor.y = element_line(color = "#96999C", size = 0.25, linetype = 3, lineend = "square"), # 次横线
  plot.margin = margin(t = 0.5, r = 0.5, b = 0.1, l = 0.5, unit = "cm"), # 图边界(距离可以为0或者为负值)
  panel.spacing.x = unit(.15, "cm"), # 竖直分面绘图区之间的间距
  panel.spacing.y = unit(.15, "cm"), # 水平分面绘图区之间的间距
  legend.margin = margin(t = 0, r = 0, b = 0, l = -9), # 图例边界, unit = "cm"
  legend.position = ("right"), # 图例位置left, right, bottom, top, c(0.9, 0.9)
  legend.key.height = unit(0.5, "cm"), # 图例符号高度
  legend.key.width = unit(0.5, "cm"), # 图例符号宽度
  legend.key.size = unit(0.5, "cm"), # 图例符号大小
  legend.title.align = (0), # 图例标题对齐方式(0为左齐, 1为右齐, 0.5居中)
  legend.text.align = (0), # 图例文字标签对齐方式(0为左齐, 1为右齐, 0.5居中)
  legend.direction = "vertical", # 图例排列方向"horizontal"(水平一行)
  legend.justification = c(1, 1), # 图例位置	center或两数字向量
  legend.box = "vertical", # 多图例的排列方式	"horizontal"
  legend.box.just = (0.5), # 多图例居中方式
  plot.caption.position = "panel",
  plot.tag.position = "topleft",
  plot.title.position = "plot",
  # aspect.ratio = 7/5,
  # legend.key = element_rect(fill = "group", color = "group"),#图例符号
)

#' @export Up
Up <- print("rvcheck::update_all()")

#' @export cclean
clean <- print("styler:::style_active_file()")
