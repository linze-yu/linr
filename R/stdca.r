#' @title stdca
#' @name stdca
#' @description stdca
#' @param stdca

#' @export stdca
stdca <- function(data, outcome, ttoutcome, timepoint, predictors,
                  xstart = 0.001, xstop = 0.999, xby = 0.001,
                  ymin = -0.05, probability = NULL, harm = NULL,
                  intervention = F, # graph = F,
                  interventionper = 100, smooth = T,
                  loess.span = 0.10, cmprsk = F) {
  # LOADING REQUIRED library
  # 加载所需的库
  # require(survival)
  # require(stats)

  # data MUST BE A data.frame
  # 数据必须是data.frame
  if (class(data) != "data.frame") {
    stop("Input data must be class data.frame")
  }

  # ONLY KEEPING COMPLETE CASES
  # 只保留完整案例
  data <- data[
    complete.cases(data[c(outcome, ttoutcome, predictors)]),
    c(outcome, ttoutcome, predictors)
  ]

  # outcome MUST BE CODED AS 0 AND 1
  #  outcome必须编码为0和1
  if ((length(data[!(data[outcome] == 0 | data[outcome] == 1), outcome]) > 0) & cmprsk == F) {
    stop("outcome must be coded as 0 and 1")
  }

  # xstart IS BETWEEN 0 AND 1
  # xstart 在 0 和 1 之间
  if (xstart < 0 | xstart > 1) {
    stop("xstart must lie between 0 and 1")
  }

  # xstop IS BETWEEN 0 AND 1
  # xstop 在 0 和 1 之间
  if (xstop < 0 | xstop > 1) {
    stop("xstop must lie between 0 and 1")
  }

  # xby IS BETWEEN 0 AND 1
  # xby 在 0 和 1 之间
  if (xby <= 0 | xby >= 1) {
    stop("xby must lie between 0 and 1")
  }

  # xstart IS BEFORE xstop
  # xstart 在 xstop 之前
  if (xstart >= xstop) {
    stop("xstop must be larger than xstart")
  }

  # STORING THE NUMBER OF predictorS SPECIFIED
  # 计算使用了几个predictor
  pred.n <- length(predictors)

  # IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A T OR F
  # 如果probability指定确保每个预测器都表示为真T或假F
  if (length(probability) > 0 & pred.n != length(probability)) {
    # 指定probability的数量必须与正在检查的predictor的数量相同
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }

  # IF harm SPECIFIED ENSURING THAT EACH predictor HAS A SPECIFIED harm
  # 如果harm特定确保每个预测器都有指定的harm
  if (length(harm) > 0 & pred.n != length(harm)) {
    # 指定的harm的数量必须与正在检查的predictor的数量相同
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }

  # INITIALIZING DEFAULT VALUES FOR probabilityS AND harmS IF NOT SPECIFIED
  # 如果未指定,则初始化probability和harm的默认值
  if (length(probability) == 0) {
    probability <- rep(F, pred.n)
  }
  if (length(harm) == 0) {
    harm <- rep(0, pred.n)
  }

  # CHECKING THAT EACH probability ELEMENT IS EQUAL TO T OR F,
  # 检查每个概率元素是否等于T或F
  # AND CHECKING THAT probabilityS ARE BETWEEN 0 and 1
  # 检查概率是否在0和1之间
  # IF NOT A PROB THEN CONVERTING WITH A COX REGRESSION
  # 如果不是一个概率问题,那么可以使用Cox回归模型进行转换
  for (m in 1:pred.n) {
    if (probability[m] != T & probability[m] != F) {
      # probability向量的每个元素必须是T或F
      stop("Each element of probability vector must be T or F")
    }
    if (probability[m] == T & (max(data[predictors[m]]) > 1 | min(data[predictors[m]]) < 0)) {
      # predictor向量必须在0和1之间,或者在probability选项中指定为非概率值。
      stop(paste0(predictors[m], "must be between 0 and 1 OR sepcified as a non-probability in the probability option"))
    }
    if (probability[m] == F) {
      model <- NULL
      pred <- NULL
      model <- coxph(Surv(data.matrix(data[ttoutcome]), data.matrix(data[outcome])) ~
        data.matrix(data[predictors[m]]))
      surv.data <- data.frame(0)
      pred <- data.frame(1 - c(summary(survfit(model, newdata = surv.data),
        time = timepoint, extend = T
      )$surv))
      names(pred) <- predictors[m]
      data <- cbind(data[names(data) != predictors[m]], pred)
      # predictor用Cox回归转换成概率。由于线性和比例风险假设,可能会发生误校准。
      print(paste0(predictors[m], "converted to a probability with Cox regression. Due to linearity and proportional hazards assumption, miscalibration may occur."))
    }
  }

  # THE predictor NAMES CANNOT BE EQUAL TO all OR none.
  # predictor的名称不能是all或none
  if (length(predictors[predictors == "all" | predictors == "none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }

  ################## CALCULATING NET BENEFIT #################
  ######################## 计算净获益 ########################

  N <- nrow(data)

  # getting the probability of the event for all subjects
  # this is used for the net benefit associated with treating all patients
  # 计算所有受试者的事件概率,这用于与治疗所有患者相关的净效益
  if (cmprsk == F) {
    km.cuminc <- survfit(Surv(data.matrix(data[ttoutcome]), data.matrix(data[outcome])) ~ 1)
    pd <- 1 - summary(km.cuminc, times = timepoint, extend = T)$surv
  } else {
    require(cmprsk)
    cr.cuminc <- cuminc(data[[ttoutcome]], data[[outcome]])
    pd <- timepoints(cr.cuminc, times = timepoint)$est[1]
  }

  # creating dataset that is one line per threshold for the treat all and treat none strategies
  # 创建数据集,每个threshold对应一行,分别对应all和none策略
  # CREATING data.frame THAT IS ONE LINE PER threshold PER all AND none STRATEGY
  # 创建一个data.frame,每个threshold对应一个all和none策略的单行数据
  nb <- data.frame(seq(from = xstart, to = xstop, by = xby))
  names(nb) <- "threshold"
  interv <- nb
  error <- NULL

  nb["all"] <- pd - (1 - pd) * nb$threshold / (1 - nb$threshold)
  nb["none"] <- 0

  # CYCLING THROUGH EACH predictor AND CALCULATING NET BENEFIT
  # 循环遍历每个predictor并计算净获益
  for (m in 1:pred.n) {
    nb[predictors[m]] <- NA

    for (r in 1:length(nb$threshold)) {
      # calculating number of T and F postives
      # 计算真正例和假正例的数量
      px <- sum(data[predictors[m]] > nb$threshold[r]) / N

      if (px == 0) {
        error <- rbind(error, paste(predictors[m], ": No observations with risk greater than ", nb$threshold[r] * 100, "%", sep = " "))
        break
      } else {
        # calculate risk using Kaplan-Meier
        # 使用Kaplan-Meier方法计算风险
        if (cmprsk == F) {
          km.cuminc <- survfit(Surv(data.matrix(data[data[predictors[m]] > nb$threshold[r], ttoutcome]), data.matrix(data[data[predictors[m]] > nb$threshold[r], outcome])) ~ 1)
          pdgivenx <- (1 - summary(km.cuminc, times = timepoint, extend = T)$surv)
          if (length(pdgivenx) == 0) {
            error <- rbind(error, paste0(predictors[m], ": No observations with risk greater than ", nb$threshold[r] * 100, "% that have followup through the timepoint selected", sep = " "))
            break
          }
          # calculate risk using competing risk
          # 使用竞争风险计算风险
        } else {
          cr.cuminc <- cuminc(data[[ttoutcome]][data[[predictors[m]]] > nb$threshold[r]], data[[outcome]][data[[predictors[m]]] > nb$threshold[r]])
          pdgivenx <- timepoints(cr.cuminc, times = timepoint)$est[1]
          if (is.na(pdgivenx)) {
            error <- rbind(error, paste(predictors[m], ": No observations with risk greater than ", nb$threshold[r] * 100, "% that have followup through the timepoint selected", sep = " "))
            break
          }
        }
        # calculating NB based on calculated risk
        # 根据计算出的风险计算NB
        nb[r, predictors[m]] <- pdgivenx * px - (1 - pdgivenx) * px * nb$threshold[r] / (1 - nb$threshold[r]) - harm[m]
      }
    }
    interv[predictors[m]] <- (nb[predictors[m]] - nb["all"]) * interventionper / (interv$threshold / (1 - interv$threshold))
  }
  if (length(error) > 0) {
    print(paste(error, ", and therefore net benefit not calculable in this range.", sep = " "))
  }

  # CYCLING THROUGH EACH predictor AND smooth NET BENEFIT AND INTERVENTIONS AVOIDED
  # 循环遍历每个predictor和smooth净收益以及避免的干预措施
  for (m in 1:pred.n) {
    if (smooth == T) {
      lws <- loess(data.matrix(nb[!is.na(nb[[predictors[m]]]), predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]), "threshold"]), span = loess.span)
      nb[!is.na(nb[[predictors[m]]]), paste0(predictors[m], "_sm")] <- lws$fitted

      lws <- loess(data.matrix(interv[!is.na(nb[[predictors[m]]]), predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]), "threshold"]), span = loess.span)
      interv[!is.na(nb[[predictors[m]]]), paste0(predictors[m], "_sm")] <- lws$fitted
    }
  }

  ###
  # graph
  ###

  # RETURNING RESULTS
  # 返回结果
  results <- list()
  results$N <- N
  results$predictors <- data.frame(cbind(predictors, harm, probability)) %>% data.table()
  names(results$predictors) <- c("predictor", "harm.applied", "probability")
  results$interventions.avoided.per <- interventionper
  results$net.benefit <- nb %>% data.table()
  results$interventions.avoided <- interv %>% data.table()
  return(results)
}
