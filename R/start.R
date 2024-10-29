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
    library("mlr3proba")
    library("survival")
    library("survminer")
    library("distr6")
    future::plan(list(
      tweak(multisession, workers = 6), # availableCores() %/% 4
      tweak(multisession, workers = 3)
    ))
    options(mlr3.allow_utf8_names = T)
    # options("OMP_THREAD_LIMIT" = 1)
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
