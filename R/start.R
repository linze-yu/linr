#' @title start
#' @name start
#' @description Start_creating
#' @param Start_creating

#' @export Start_creating
Start_creating <- function(x) {
  showtext_auto()
  font_add("HOS", "C:/Windows/Fonts/HarmonyOS Sans SC/HarmonyOS_Sans_SC_Regular.ttf")
  showtext_opts(dpi = 400)
  font_families()
  if (x == "mlr3verse") {
    library("tidymodels")
    library("mlr3verse")
    library("GenSA")
    library("mlr3extralearners")
    future::plan(list("multisession", "sequential"), workers = 50)
  }
  if (x == "tidymodels") {
    library("tidymodels")
    library("vip")
    all_cores <- parallel::detectCores(logical = F)
    registerDoParallel(cores = all_cores)
  }
  if (x == "EDA") {
    library("tidymodels")
    library("mice")
    library("gtsummary")
    library("glmnet")
    library("rms")
    library("nomogramFormula")
    library("survminer")
    library("survival")
    library("ComplexHeatmap") #
    library("circlize") #
  }
  return(print("Everything is ready!"))
}
