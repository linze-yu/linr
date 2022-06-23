#' @title linr
#'
#' @description cols, fills, lin_theme, stdca, dca, Up
#'
#' @param cols
#'
#' @param fills
#'
#' @param lin_theme
#'
#' @param stdca
#'
#' @param dca
#'
#' @param Up
#'
#' @return NULL
#'
#' @examples

#' @export cols
cols <- c("#00997B", "#FC766A", "#696AAD", "#EBBF57", "#C34E7C", "#92B558", "#BD3645", "#34558B", "#F3E04E", "#6B5B95", "#4DB6AD", "#E34F33", "#95AAD3", "#DECEBB", "#975A58", "#5BB3B0", "#9D363F", "#6364A5", "#E2654D", "#F8CDCD", "#7FC9CB", "#D85A7B", "#98B2D1", "#B067A1")

#' @export fills
fills <- c("#00997B", "#FC766A", "#696AAD", "#EBBF57", "#C34E7C", "#92B558", "#BD3645", "#34558B", "#F3E04E", "#6B5B95", "#4DB6AD", "#E34F33", "#95AAD3", "#DECEBB", "#975A58", "#5BB3B0", "#9D363F", "#6364A5", "#E2654D", "#F8CDCD", "#7FC9CB", "#D85A7B", "#98B2D1", "#B067A1")

#' @export lin_theme
lin_theme <- theme(
  text = element_text(family = "Source Han Sans CN-Regular", color = 1), # 全字体
  plot.title = element_text(size = 12, hjust = 0.5, angle = 0, lineheight = 1, margin = margin(0, 1, 0, 0)), # 标题
  plot.subtitle = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 2, 0)), # 副标题
  plot.caption = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), # 说明文字
  plot.tag = element_text(angle = 0, lineheight = 1, margin = margin(0, 0, 0, 0)), # 标签
  legend.title = element_text(margin = margin(0, 0, 0, 0)), # 图例标题
  legend.text = element_text(margin = margin(0, 0, 0, 0)), # 图例文字
  strip.text.x = element_text(hjust = 0.5, margin = margin(0, 0, 1, 0)), # 水平分面标签文本
  strip.text.y = element_text(vjust = 0.5, margin = margin(0, 0, 0, 1)), # 竖直分面标签文本
  axis.text.y = element_text(vjust = 0.5, hjust = 1, angle = 0, margin = margin(0, 2, 0, 0)), # Y轴数字标
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
  axis.ticks.x = element_line(color = "#96999C", size = 1, lineend = "square"), # X轴刻度线
  axis.ticks.y = element_line(color = "#96999C", size = 1, lineend = "square"), # Y轴刻度线
  axis.ticks.length = unit(.15, "cm"), # 刻度线长度
  panel.grid.major.x = element_line(color = NA, size = 0.5, linetype = 3, lineend = "square"), # 主竖线"#96999C"
  panel.grid.major.y = element_line(color = NA, size = 0.5, linetype = 3, lineend = "square"), # 主横线"#96999C"
  panel.grid.minor.x = element_line(color = NA, size = 0.25, linetype = 3, lineend = "square"), # 次竖线"#96999C"
  panel.grid.minor.y = element_line(color = NA, size = 0.25, linetype = 3, lineend = "square"), # 次横线"#96999C"
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

#' @export stdca
stdca <- function(data, outcome, ttoutcome, timepoint, predictors, xstart = 0.01, xstop = 0.99, xby = 0.01,
                  ymin = -0.05, probability = NULL, harm = NULL, graph = TRUE, intervention = FALSE,
                  interventionper = 100, smooth = FALSE, loess.span = 0.10, cmprsk = FALSE) {
  # LOADING REQUIRED LIBRARIES
  require(survival)
  require(stats)

  # ONLY KEEPING COMPLETE CASES
  data <- data[complete.cases(data[c(outcome, ttoutcome, predictors)]), c(outcome, ttoutcome, predictors)]

  # outcome MUST BE CODED AS 0 AND 1
  if ((length(data[!(data[outcome] == 0 | data[outcome] == 1), outcome]) > 0) & cmprsk == FALSE) {
    stop("outcome must be coded as 0 and 1")
  }

  # data MUST BE A DATA FRAME
  if (class(data) != "data.frame") {
    stop("Input data must be class data.frame")
  }

  # xstart IS BETWEEN 0 AND 1
  if (xstart < 0 | xstart > 1) {
    stop("xstart must lie between 0 and 1")
  }

  # xstop IS BETWEEN 0 AND 1
  if (xstop < 0 | xstop > 1) {
    stop("xstop must lie between 0 and 1")
  }

  # xby IS BETWEEN 0 AND 1
  if (xby <= 0 | xby >= 1) {
    stop("xby must lie between 0 and 1")
  }

  # xstart IS BEFORE xstop
  if (xstart >= xstop) {
    stop("xstop must be larger than xstart")
  }

  # STORING THE NUMBER OF PREDICTORS SPECIFIED
  pred.n <- length(predictors)

  # IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A T OR F
  if (length(probability) > 0 & pred.n != length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }


  # IF harm SPECIFIED ENSURING THAT EACH PREDICTOR HAS A SPECIFIED HARM
  if (length(harm) > 0 & pred.n != length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }

  # INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  if (length(harm) == 0) {
    harm <- rep(0, pred.n)
  }
  if (length(probability) == 0) {
    probability <- rep(TRUE, pred.n)
  }

  # THE PREDICTOR NAMES CANNOT BE EQUAL TO all OR none.
  if (length(predictors[predictors == "all" | predictors == "none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }

  # CHECKING THAT EACH probability ELEMENT IS EQUAL TO T OR F,
  # AND CHECKING THAT PROBABILITIES ARE BETWEEN 0 and 1
  # IF NOT A PROB THEN CONVERTING WITH A COX REGRESSION
  for (m in 1:pred.n) {
    if (probability[m] != TRUE & probability[m] != FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m] == TRUE & (max(data[predictors[m]]) > 1 | min(data[predictors[m]]) < 0)) {
      stop(paste(predictors[m], "must be between 0 and 1 OR sepcified as a non-probability in the probability option", sep = " "))
    }
    if (probability[m] == FALSE) {
      model <- NULL
      pred <- NULL
      model <- coxph(Surv(data.matrix(data[ttoutcome]), data.matrix(data[outcome])) ~ data.matrix(data[predictors[m]]))
      surv.data <- data.frame(0)
      pred <- data.frame(1 - c(summary(survfit(model, newdata = surv.data), time = timepoint)$surv))
      names(pred) <- predictors[m]
      data <- cbind(data[names(data) != predictors[m]], pred)
      print(paste(predictors[m], "converted to a probability with Cox regression. Due to linearity and proportional hazards assumption, miscalibration may occur.", sep = " "))
    }
  }

  #########  CALCULATING NET BENEFIT   #########
  N <- dim(data)[1]

  # getting the probability of the event for all subjects
  # this is used for the net benefit associated with treating all patients
  if (cmprsk == FALSE) {
    km.cuminc <- survfit(Surv(data.matrix(data[ttoutcome]), data.matrix(data[outcome])) ~ 1)
    pd <- 1 - summary(km.cuminc, times = timepoint)$surv
  } else {
    require(cmprsk)
    cr.cuminc <- cuminc(data[[ttoutcome]], data[[outcome]])
    pd <- timepoints(cr.cuminc, times = timepoint)$est[1]
  }

  # creating dataset that is one line per threshold for the treat all and treat none strategies;
  # CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all AND none STRATEGY
  nb <- data.frame(seq(from = xstart, to = xstop, by = xby))
  names(nb) <- "threshold"
  interv <- nb
  error <- NULL

  nb["all"] <- pd - (1 - pd) * nb$threshold / (1 - nb$threshold)
  nb["none"] <- 0

  # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  for (m in 1:pred.n) {
    nb[predictors[m]] <- NA

    for (t in 1:length(nb$threshold)) {
      # calculating number of true and false postives;
      px <- sum(data[predictors[m]] > nb$threshold[t]) / N

      if (px == 0) {
        error <- rbind(error, paste(predictors[m], ": No observations with risk greater than ", nb$threshold[t] * 100, "%", sep = ""))
        break
      } else {
        # calculate risk using Kaplan Meier
        if (cmprsk == FALSE) {
          km.cuminc <- survfit(Surv(data.matrix(data[data[predictors[m]] > nb$threshold[t], ttoutcome]), data.matrix(data[data[predictors[m]] > nb$threshold[t], outcome])) ~ 1)
          pdgivenx <- (1 - summary(km.cuminc, times = timepoint)$surv)
          if (length(pdgivenx) == 0) {
            error <- rbind(error, paste(predictors[m], ": No observations with risk greater than ", nb$threshold[t] * 100, "% that have followup through the timepoint selected", sep = ""))
            break
          }
          # calculate risk using competing risk
        } else {
          cr.cuminc <- cuminc(data[[ttoutcome]][data[[predictors[m]]] > nb$threshold[t]], data[[outcome]][data[[predictors[m]]] > nb$threshold[t]])
          pdgivenx <- timepoints(cr.cuminc, times = timepoint)$est[1]
          if (is.na(pdgivenx)) {
            error <- rbind(error, paste(predictors[m], ": No observations with risk greater than ", nb$threshold[t] * 100, "% that have followup through the timepoint selected", sep = ""))
            break
          }
        }
        # calculating NB based on calculated risk
        nb[t, predictors[m]] <- pdgivenx * px - (1 - pdgivenx) * px * nb$threshold[t] / (1 - nb$threshold[t]) - harm[m]
      }
    }
    interv[predictors[m]] <- (nb[predictors[m]] - nb["all"]) * interventionper / (interv$threshold / (1 - interv$threshold))
  }
  if (length(error) > 0) {
    print(paste(error, ", and therefore net benefit not calculable in this range.", sep = ""))
  }

  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED
  for (m in 1:pred.n) {
    if (smooth == TRUE) {
      lws <- loess(data.matrix(nb[!is.na(nb[[predictors[m]]]), predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]), "threshold"]), span = loess.span)
      nb[!is.na(nb[[predictors[m]]]), paste(predictors[m], "_sm", sep = "")] <- lws$fitted

      lws <- loess(data.matrix(interv[!is.na(nb[[predictors[m]]]), predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]), "threshold"]), span = loess.span)
      interv[!is.na(nb[[predictors[m]]]), paste(predictors[m], "_sm", sep = "")] <- lws$fitted
    }
  }


  # PLOTTING GRAPH IF REQUESTED
  if (graph == TRUE) {
    require(graphics)

    # PLOTTING INTERVENTIONS AVOIDED IF REQUESTED
    if (intervention == TRUE) {
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- NULL
      legendcolor <- NULL
      legendwidth <- NULL
      legendpattern <- NULL

      # getting maximum number of avoided interventions
      ymax <- max(interv[predictors], na.rm = TRUE)

      # INITIALIZING EMPTY PLOT WITH LABELS
      plot(x = nb$threshold, y = nb$all, type = "n", xlim = c(xstart, xstop), ylim = c(ymin, ymax), xlab = "Threshold probability", ylab = paste("Net reduction in interventions per", interventionper, "patients"))

      # PLOTTING INTERVENTIONS AVOIDED FOR EACH PREDICTOR
      for (m in 1:pred.n) {
        if (smooth == TRUE) {
          lines(interv$threshold, data.matrix(interv[paste(predictors[m], "_sm", sep = "")]), col = m, lty = 2)
        } else {
          lines(interv$threshold, data.matrix(interv[predictors[m]]), col = m, lty = 2)
        }

        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    } else {
      # PLOTTING NET BENEFIT IF REQUESTED
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- c("None", "All")
      legendcolor <- c(17, 8)
      legendwidth <- c(2, 2)
      legendpattern <- c(1, 1)

      # getting maximum net benefit
      ymax <- max(nb[names(nb) != "threshold"], na.rm = TRUE)

      # inializing new benfit plot with treat all option
      plot(x = nb$threshold, y = nb$all, type = "l", col = 8, lwd = 2, xlim = c(xstart, xstop), ylim = c(ymin, ymax), xlab = "Threshold probability", ylab = "Net benefit")
      # adding treat none option
      lines(x = nb$threshold, y = nb$none, lwd = 2)
      # PLOTTING net benefit FOR EACH PREDICTOR
      for (m in 1:pred.n) {
        if (smooth == TRUE) {
          lines(nb$threshold, data.matrix(nb[paste(predictors[m], "_sm", sep = "")]), col = m, lty = 2)
        } else {
          lines(nb$threshold, data.matrix(nb[predictors[m]]), col = m, lty = 2)
        }
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    }
    # then add the legend
    legend("topright", legendlabel, cex = 0.8, col = legendcolor, lwd = legendwidth, lty = legendpattern)
  }

  # RETURNING RESULTS
  results <- list()
  results$N <- N
  results$predictors <- data.frame(cbind(predictors, harm, probability))
  names(results$predictors) <- c("predictor", "harm.applied", "probability")
  results$interventions.avoided.per <- interventionper
  results$net.benefit <- nb
  results$interventions.avoided <- interv
  return(results)
}

#' @export dca
dca <- function(data, outcome, predictors, xstart = 0.01, xstop = 0.99, xby = 0.01,
                ymin = -0.05, probability = NULL, harm = NULL, graph = TRUE, intervention = FALSE,
                interventionper = 100, smooth = FALSE, loess.span = 0.10) {
  # LOADING REQUIRED LIBRARIES
  require(stats)

  # data MUST BE A DATA FRAME
  if (class(data) != "data.frame") {
    stop("Input data must be class data.frame")
  }

  # ONLY KEEPING COMPLETE CASES
  data <- data[complete.cases(data[append(outcome, predictors)]), append(outcome, predictors)]

  # outcome MUST BE CODED AS 0 AND 1
  if (max(data[[outcome]]) > 1 | min(data[[outcome]]) < 0) {
    stop("outcome cannot be less than 0 or greater than 1")
  }
  # xstart IS BETWEEN 0 AND 1
  if (xstart < 0 | xstart > 1) {
    stop("xstart must lie between 0 and 1")
  }

  # xstop IS BETWEEN 0 AND 1
  if (xstop < 0 | xstop > 1) {
    stop("xstop must lie between 0 and 1")
  }

  # xby IS BETWEEN 0 AND 1
  if (xby <= 0 | xby >= 1) {
    stop("xby must lie between 0 and 1")
  }

  # xstart IS BEFORE xstop
  if (xstart >= xstop) {
    stop("xstop must be larger than xstart")
  }

  # STORING THE NUMBER OF PREDICTORS SPECIFIED
  pred.n <- length(predictors)

  # IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A YES OR NO
  if (length(probability) > 0 & pred.n != length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }

  # IF harm SPECIFIED ENSURING THAT EACH PREDICTOR HAS A SPECIFIED HARM
  if (length(harm) > 0 & pred.n != length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }

  # INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  if (length(harm) == 0) {
    harm <- rep(0, pred.n)
  }
  if (length(probability) == 0) {
    probability <- rep(TRUE, pred.n)
  }


  # CHECKING THAT EACH probability ELEMENT IS EQUAL TO YES OR NO,
  # AND CHECKING THAT PROBABILITIES ARE BETWEEN 0 and 1
  # IF NOT A PROB THEN CONVERTING WITH A LOGISTIC REGRESSION
  for (m in 1:pred.n) {
    if (probability[m] != TRUE & probability[m] != FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m] == TRUE & (max(data[predictors[m]]) > 1 | min(data[predictors[m]]) < 0)) {
      stop(paste(predictors[m], "must be between 0 and 1 OR sepcified as a non-probability in the probability option", sep = " "))
    }
    if (probability[m] == FALSE) {
      model <- NULL
      pred <- NULL
      model <- glm(data.matrix(data[outcome]) ~ data.matrix(data[predictors[m]]), family = binomial("logit"))
      pred <- data.frame(model$fitted.values)
      pred <- data.frame(pred)
      names(pred) <- predictors[m]
      data <- cbind(data[names(data) != predictors[m]], pred)
      print(paste(predictors[m], "converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur.", sep = " "))
    }
  }

  # THE PREDICTOR NAMES CANNOT BE EQUAL TO all OR none.
  if (length(predictors[predictors == "all" | predictors == "none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }

  #########  CALCULATING NET BENEFIT   #########
  N <- dim(data)[1]
  event.rate <- colMeans(data[outcome])

  # CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all AND none STRATEGY
  nb <- data.frame(seq(from = xstart, to = xstop, by = xby))
  names(nb) <- "threshold"
  interv <- nb

  nb["all"] <- event.rate - (1 - event.rate) * nb$threshold / (1 - nb$threshold)
  nb["none"] <- 0

  # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  for (m in 1:pred.n) {
    for (t in 1:length(nb$threshold)) {
      # COUNTING TRUE POSITIVES AT EACH THRESHOLD
      tp <- mean(data[data[[predictors[m]]] >= nb$threshold[t], outcome]) * sum(data[[predictors[m]]] >= nb$threshold[t])
      # COUNTING FALSE POSITIVES AT EACH THRESHOLD
      fp <- (1 - mean(data[data[[predictors[m]]] >= nb$threshold[t], outcome])) * sum(data[[predictors[m]]] >= nb$threshold[t])
      # setting TP and FP to 0 if no observations meet threshold prob.
      if (sum(data[[predictors[m]]] >= nb$threshold[t]) == 0) {
        tp <- 0
        fp <- 0
      }

      # CALCULATING NET BENEFIT
      nb[t, predictors[m]] <- tp / N - fp / N * (nb$threshold[t] / (1 - nb$threshold[t])) - harm[m]
    }
    interv[predictors[m]] <- (nb[predictors[m]] - nb["all"]) * interventionper / (interv$threshold / (1 - interv$threshold))
  }

  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED
  for (m in 1:pred.n) {
    if (smooth == TRUE) {
      lws <- loess(data.matrix(nb[!is.na(nb[[predictors[m]]]), predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]), "threshold"]), span = loess.span)
      nb[!is.na(nb[[predictors[m]]]), paste(predictors[m], "_sm", sep = "")] <- lws$fitted

      lws <- loess(data.matrix(interv[!is.na(nb[[predictors[m]]]), predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]), "threshold"]), span = loess.span)
      interv[!is.na(nb[[predictors[m]]]), paste(predictors[m], "_sm", sep = "")] <- lws$fitted
    }
  }

  # PLOTTING GRAPH IF REQUESTED
  if (graph == TRUE) {
    require(graphics)

    # PLOTTING INTERVENTIONS AVOIDED IF REQUESTED
    if (intervention == TRUE) {
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- NULL
      legendcolor <- NULL
      legendwidth <- NULL
      legendpattern <- NULL

      # getting maximum number of avoided interventions
      ymax <- max(interv[predictors], na.rm = TRUE)

      # INITIALIZING EMPTY PLOT WITH LABELS
      plot(x = nb$threshold, y = nb$all, type = "n", xlim = c(xstart, xstop), ylim = c(ymin, ymax), xlab = "Threshold probability", ylab = paste("Net reduction in interventions per", interventionper, "patients"))

      # PLOTTING INTERVENTIONS AVOIDED FOR EACH PREDICTOR
      for (m in 1:pred.n) {
        if (smooth == TRUE) {
          lines(interv$threshold, data.matrix(interv[paste(predictors[m], "_sm", sep = "")]), col = m, lty = 2)
        } else {
          lines(interv$threshold, data.matrix(interv[predictors[m]]), col = m, lty = 2)
        }

        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    } else {
      # PLOTTING NET BENEFIT IF REQUESTED

      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- c("None", "All")
      legendcolor <- c(17, 8)
      legendwidth <- c(2, 2)
      legendpattern <- c(1, 1)

      # getting maximum net benefit
      ymax <- max(nb[names(nb) != "threshold"], na.rm = TRUE)

      # inializing new benfit plot with treat all option
      plot(x = nb$threshold, y = nb$all, type = "l", col = 8, lwd = 2, xlim = c(xstart, xstop), ylim = c(ymin, ymax), xlab = "Threshold probability", ylab = "Net benefit")
      # adding treat none option
      lines(x = nb$threshold, y = nb$none, lwd = 2)
      # PLOTTING net benefit FOR EACH PREDICTOR
      for (m in 1:pred.n) {
        if (smooth == TRUE) {
          lines(nb$threshold, data.matrix(nb[paste(predictors[m], "_sm", sep = "")]), col = m, lty = 2)
        } else {
          lines(nb$threshold, data.matrix(nb[predictors[m]]), col = m, lty = 2)
        }
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    }
    # then add the legend
    legend("topright", legendlabel, cex = 0.8, col = legendcolor, lwd = legendwidth, lty = legendpattern)
  }

  # RETURNING RESULTS
  results <- list()
  results$N <- N
  results$predictors <- data.frame(cbind(predictors, harm, probability))
  names(results$predictors) <- c("predictor", "harm.applied", "probability")
  results$interventions.avoided.per <- interventionper
  results$net.benefit <- nb
  results$interventions.avoided <- interv

  return(results)
}

#' @export Up
Up <- print("rvcheck::update_all()")
