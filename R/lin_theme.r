#' @title lin_theme
#' @name lin_theme
#' @description lin_theme, cols, fills
#' @param lin_theme
#' @param col1
#' @param col2
#' @param col3
#' @param col4
#' @param col5
#' @param col6
#' @param cols
#' @param fills

#' @export col1
col1 <- c("#90c9e7", "#219ebc", "#136783", "#02304a", "#feb705", "#ff9e02", "#fa8600")

#' @export col2
col2 <- c("#db3124", "#fc8c5a", "#ffdf92", "#e6f1f3", "#90bee0", "#4b74b2")

#' @export col3
col3 <- c("#264653", "#287271", "#2a9d8c", "#8ab07d", "#e9c46b", "#f3a261", "#e66f51")

#' @export col4
col4 <- c("#a40545", "#f46f44", "#fdd985", "#e9f5a1", "#7fcba4", "#4b65af")

#' @export col5
col5 <- c("#023047", "#0e5b76", "#1a86a3", "#46acca", "#9bcfe8", "#f3f9fc", "#ffca5f", "#fea809", "#fd9802", "#fb8402")

#' @export col6
col6 <- c("#001219", "#006073", "#099396", "#91d3c0", "#ebd7a5", "#ee9b00", "#cc6602", "#bc3e03", "#ae2012", "#9b2227")

#' @export cols
cols <- c("#3266B3", "#BE3455", "#01847F", "#F09839", "#6395EC", "#87A931", "#696AAD", "#FC766A", "#92B558", "#95AAD3", "#E34F33", "#385F32")

#' @export fills
fills <- c("#3266B3", "#BE3455", "#01847F", "#F09839", "#6395EC", "#87A931", "#696AAD", "#FC766A", "#92B558", "#95AAD3", "#E34F33", "#385F32")

#' @export lin_theme
lin_theme <- theme(
  # 全字体
  text = element_text(color = "#000000", family = "Arial"),
  # 标题
  plot.title = element_text(size = 14, hjust = 0.5, angle = 0, lineheight = 1, margin = margin(0, 1, 0, 0)),
  # 副标题
  plot.subtitle = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 2, 0)),
  # 说明文字
  plot.caption = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)),
  # 标签
  plot.tag = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)),
  # 图例标题
  legend.title = element_text(margin = margin(0, 0, 0, 0)),
  # 图例文字
  legend.text = element_text(margin = margin(0, 0, 0, 0)),
  # 水平分面标签文本
  strip.text.x = element_text(hjust = 0.5, margin = margin(0, 0, 1, 0)),
  # 竖直分面标签文本
  strip.text.y = element_text(size = 15, vjust = 0.5, margin = margin(0, 0, 0, 1)),
  # Y轴数字标
  axis.text.y = element_text(size = 17, vjust = 0.5, hjust = 1, angle = 0, margin = margin(0, 3, 0, 0)),
  # Y轴标题
  axis.title.y = element_text(size = 20, vjust = 0.5, hjust = 0.5, angle = 90, margin = margin(0, 3, 0, 0)),
  # X轴数字标
  axis.text.x = element_text(size = 17, vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(2, 0, 0, 0)),
  # X轴标题
  axis.title.x = element_text(size = 20, vjust = 0.5, hjust = 0.5, angle = 0, margin = margin(2, 0, 0, 0)),
  # 绘图区域背景
  panel.background = element_rect(fill = NA),
  # 绘图区边框
  panel.border = element_rect(color = NA, fill = NA, linewidth = 0),
  # 文字区域+图像背景
  plot.background = element_rect(fill = NA, color = NA),
  # 图例背景
  legend.background = element_rect(fill = NA, color = NA),
  # 图例符号背景
  legend.key = element_rect(fill = NA, color = NA),
  # 分面标签背景
  strip.background = element_rect(fill = NA),

  # 坐标轴
  axis.line = element_line(color = 1, linewidth = 1, lineend = "square"),
  # X轴刻度线
  axis.ticks.x = element_line(color = 1, linewidth = 1),
  # Y轴刻度线
  axis.ticks.y = element_line(color = 1, linewidth = 1),
  # 刻度线长度
  axis.ticks.length = unit(-0.2, "cm"),
  # 主竖线
  panel.grid.major.x = element_line(color = NA, linewidth = 0.5, linetype = 3),
  # 主横线
  panel.grid.major.y = element_line(color = NA, linewidth = 0.5, linetype = 3),
  # 次竖线
  panel.grid.minor.x = element_line(color = NA, linewidth = 0.25, linetype = 3),
  # 次横线
  panel.grid.minor.y = element_line(color = NA, linewidth = 0.25, linetype = 3),

  # 图边界(距离可以为0或者为负值)
  plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
  # 竖直分面绘图区之间的间距
  panel.spacing.x = unit(.15, "cm"),
  # 水平分面绘图区之间的间距
  panel.spacing.y = unit(.15, "cm"),
  # 图例边界, unit = "cm"
  legend.margin = margin(t = 0, r = 0, b = 0, l = 0), #-9
  # 图例位置left, right, bottom, top, c(0.9, 0.9)
  legend.position = ("right"),
  # 图例符号高度
  # legend.key.height = unit(0.5, "cm"),
  # 图例符号宽度
  # legend.key.width = unit(0.5, "cm"),
  # 图例符号大小
  # legend.key.size = unit(0.5, "cm"),
  # 图例标题对齐方式(0为左齐, 1为右齐, 0.5居中)
  legend.title.align = (0),
  # 图例文字标签对齐方式(0为左齐, 1为右齐, 0.5居中)
  legend.text.align = (0),
  # 图例排列方向"horizontal"(水平一行)
  legend.direction = "vertical",
  # 图例位置	center或两数字向量
  legend.justification = c(1, 1),
  # 多图例的排列方式	"horizontal"
  legend.box = "vertical",
  # 多图例居中方式
  legend.box.just = (0.5),
  plot.caption.position = "panel",
  plot.tag.position = "topleft",
  plot.title.position = "plot",
  # aspect.ratio = 7/5,
  # 图例符号
  # legend.key = element_rect(fill = "group", color = "group"),
)
