#' @title packages
#' @name packages
#' @description lrp, Up, time
#' @param lrp
#' @param Up
#' @param time

#' @export lrp
lrp <- print(c(
  "library('glmnet') # LASSO回归",
  "library('rms')",
  "library('tidymodels')",
  "library('customLayout') # 图片排版",
  "library('gtsummary') # 基线资料表",
  "library('scales') # 缩放刻度",
  "library('namedropR') # 参考文献",
  "library('lavaan') # 结构方程模型",
  "library('semPlot') # 结构方程模型可视化",
  "library('lavaanPlot')",
  "library('shiny') # 交互网页",
  "library('rvcheck') # 更新包",
  "library('circlize') # 圈图",
  "library('ComplexHeatmap') # 热图*",
  "library('ggwordcloud') # 词云",
  "library('ggrepel') # 文本防重叠",
  "library('survminer') # 生存曲线",
  "library('survival') # 生存曲线",
  "library('Cairo') # 渲染",
  "library('furrr') # purrr",
  "library('ggpmisc') # 拟合模型相关的注释和绘图",
  "library('ggforce') # 形状曲线",
  "library('gghalves') # 裁剪图形",
  "library('ggbreak') # 坐标轴截断",
  "library('ggsignif') # 添加P值",
  "library('mice') # 多重插补",
  "library('skimr') # 用最少的代码来获知数据框的方方面面",
  "library('DMwR2') # 使用局部异常因子法(LOF法)检测异常值",
  "library('future') # 多核计算",
  "library('kableExtra') # 表格+",
  "library('jiebaR') # 分词",
  "library('readtext') # 读取文本pdf,docx,xml,json",
  "library('tokenizers') # 文本切分",
  "library('hunspell') # 拼写检查",
  "library('udpipe') # 词形还原",
  "library('textreadr') # rtf,html,docx",
  "library('tidytext') #"
))

#' @export Up
Up <- print("rvcheck::update_all()")

#' @export time
time <- function(x) {
  options(timeout = 2000)
  return(print("timeout is 2000"))
}
