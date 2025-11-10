#' @title dca
#' @name dca
#' @description dca
#' @param dca

#' @export dca
dca <- function(data, outcome, predictors,
                xstart = 0.001, xstop = 0.999, xby = 0.001,
                ymin = -0.05, w = NULL, probability = T, harm = NULL,
                intervention = F, # graph = F,
                interventionper = 100, smooth = T,
                loess.span = 0.10) {
  # LOADING REQUIRED libraryS
  # 加载所需的库
  # require(stats)

  # data MUST BE A data.frame
  # 数据必须是data.frame
  if (class(data) != "data.frame") {
    stop("Input data must be class data.frame")
  }

  # ONLY KEEPING COMPLETE CASES
  # 仅保留完整案例
  data <- data[
    complete.cases(data[append(outcome, predictors)]),
    append(outcome, predictors)
  ]

  # outcome MUST BE CODED AS 0 AND 1
  # outcome 必须编码为0和1
  if (max(data[[outcome]]) > 1 | min(data[[outcome]]) < 0) {
    stop("outcome cannot be less than 0 or greater than 1")
  }
  # xstart IS BETWEEN 0 AND 1
  # xstart 在0和1之间
  if (xstart < 0 | xstart > 1) {
    stop("xstart must lie between 0 and 1")
  }

  # xstop IS BETWEEN 0 AND 1
  # xstop 在0和1之间
  if (xstop < 0 | xstop > 1) {
    stop("xstop must lie between 0 and 1")
  }

  # xby IS BETWEEN 0 AND 1
  # xby 在0和1之间
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

  # IF probability SPECIFIED ENSURING THAT EACH predictor IS INDICATED AS A YES OR NO
  # 如果probability是指定的,确保每个predictor被标示为YES或NO
  if (length(probability) > 0 & pred.n != length(probability)) {
    # 指定probability的数量必须与正在检查的predictor的数量相同
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }

  # IF harm SPECIFIED ENSURING THAT EACH predictor HAS A SPECIFIED harm
  # 如果harm是指定的,确保每个predictor都有对应的harm
  if (length(harm) > 0 & pred.n != length(harm)) {
    # 指定的harm的数量必须与正在检查的predictor的数量相同
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }

  # INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS AND w IF NOT SPECIFIED
  # 如果未指定,初始化probabilities和harms的默认值
  if (length(harm) == 0) {
    harm <- rep(0, pred.n)
  }
  if (length(probability) == 0) {
    probability <- rep(F, pred.n)
  }

  # CHECKING THAT EACH probability ELEMENT IS EQUAL TO YES OR NO,
  # 检查每个probability是否等于T或F。
  # AND CHECKING THAT probabilityS ARE BETWEEN 0 and 1
  # 检查probabilityS是否在0和1之间
  # IF NOT A PROB THEN CONVERTING WITH A LOGISTIC REGRESSION
  # 如果不是一个概率问题,那么使用逻辑回归进行转换
  for (m in 1:pred.n) {
    if (probability[m] != T & probability[m] != F) {
      # probability向量的每个元素必须是T或F
      stop("Each element of probability vector must be T or F")
    }
    if (probability[m] == T & (max(data[predictors[m]]) > 1 | min(data[predictors[m]]) < 0)) {
      # predictor向量必须在0和1之间,或者在probability选项中指定为非概率值。
      stop(paste(predictors[m], "must be between 0 and 1 OR sepcified as a non-probability in the probability option", sep = " "))
    }
    if (probability[m] == F) {
      model <- NULL
      pred <- NULL
      model <- glm(data.matrix(data[outcome]) ~ data.matrix(data[predictors[m]]), family = binomial("logit"))
      pred <- data.frame(model$fitted.values)
      pred <- data.frame(pred)
      names(pred) <- predictors[m]
      data <- cbind(data[names(data) != predictors[m]], pred)
      # predictor转换为逻辑回归的概率。由于线性假设,可能会发生校准误差。
      print(paste0(predictors[m], "converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur."))
    }
  }

  # THE predictor NAMES CANNOT BE EQUAL TO all OR none.
  # predictor名称不能是all或none
  if (length(predictors[predictors == "all" | predictors == "none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }

  ################# CALCULATING NET BENEFIT #################
  ####################### 计算净获益 ########################

  N <- nrow(data)
  event.rate <- colMeans(data[outcome])

  # CREATING data.frame THAT IS ONE LINE PER threshold PER all AND none STRATEGY
  # 创建一个data.frame,每个阈值对应一行,分别对应"all"和"none"。
  nb <- data.frame(seq(from = xstart, to = xstop, by = xby))
  names(nb) <- "threshold"
  interv <- nb
  # nb["all"] <- event.rate - (1 - event.rate) * nb$threshold / (1 - nb$threshold)
  # nb["none"] <- 0
  nb %>%
    mutate_dt(all = event.rate - (1 - event.rate) * nb$threshold / (1 - nb$threshold)) %>%
    mutate_dt(none = 0) %>%
   #mutate_dt() %>%
    data.frame() -> nb

  # OLD:Net Benefit = TPrate − FPrate × ( 1 − T )
  # NEW:Net Benefit = sensitivity × prevalence − ( 1 − specificity ) × ( 1 − prevalence ) × w
  # sensitivity=sens(),敏感性,真阳性率,所有实际为阳性的样本中,模型预测为阳性的比例
  # specificity=spec(),特异性,真阴性率,所有实际为阴性的样本中,模型预测为阴性的比例
  # prevalence,流行率,所有样本中实际为阳性的比例
  # w,在阈值概率下的获益与损失的比值,可以理解为一个副作用很强的干预对于阳性病人干预的获益是对阴性病人干预损失的倍数

  # True Positive(TP):真正类;样本的真实是正类,将其识别为正类
  # False Negative(FN):假负类;样本的真实是正类,将其识别为负类
  # False Positive(FP):假正类;样本的真实是负类,将其识别为正类
  # True Negative(TN):真负类;样本的真实是负类,将其识别为负类

  # CYCLING THROUGH EACH predictor AND CALCULATING NET BENEFIT
  # 循环遍历每个predictor并计算净获益
  for (m in 1:pred.n) {
    for (r in 1:length(nb$threshold)) {
      w <- nb$threshold[r] / (1 - nb$threshold[r])
      nb[r, "w"] <- w
      # 计算在该阈值下被判断为正例的数量的数量
      TFP <- data[[predictors[m]]] >= nb$threshold[r]
      sum_T <- sum(TFP)
      # 计算在该阈值下被判断为反例的数量的数量
      TFN <- data[[predictors[m]]] < nb$threshold[r]
      sum_N <- sum(TFN)

      # COUNTING T POSITIVES AT EACH threshold
      # 在每个阈值下计算真正例TP/N真阳性率
      TP <- mean(data[TFP, outcome]) * sum_T

      # COUNTING F POSITIVES AT EACH threshold
      # 在每个阈值下计算误报数FP/N假阳性率
      FP <- (1 - mean(data[TFP, outcome])) * sum_T

      # COUNTING T NEGATIVE AT EACH threshold
      # 在每个阈值下计算真负例TN/N真阴性率
      TN <- (1 - mean(data[TFN, outcome])) * sum_N

      # COUNTING T NEGATIVE AT EACH threshold
      # 在每个阈值下计算真负例FN/N假阴性率
      FN <- mean(data[TFN, outcome]) * sum_N

      # setting TP and FP to 0 if no observations meet threshold prob.
      # 如果没有任何观测值满足阈值概率,则将真正例(TP)和假正例(FP)设为0
      if (sum_T == 0) {
        TP <- 0
        FP <- 0
      }
      if (sum_N == 0) {
        TN <- 0
        FN <- 0
      }
      TP_rate <- TP / N
      FP_rate <- FP / N
      TN_rate <- TN / N
      FN_rate <- FN / N
      #
      nb[r, "TP"] <- TP
      nb[r, "FP"] <- FP
      nb[r, "TN"] <- TN
      nb[r, "FN"] <- FN
      #
      nb[r, "TP_rate"] <- TP_rate
      nb[r, "FP_rate"] <- FP_rate
      nb[r, "TN_rate"] <- TN_rate
      nb[r, "FN_rate"] <- FN_rate
      # CALCULATING NET BENEFIT
      # 计算净获益
      ##################################### TP_rate − FP_rate × ( 1 − T )
      nb[r, predictors[m]] <- TP_rate - FP_rate * w - harm[m]
      ##################################### TP_rate × event.rate − (1 − TN_rate) × (1 − event.rate) × w
      nb[r, paste0(predictors[m], "_N")] <- TP_rate * event.rate - FN_rate * (1 - event.rate) * w - harm[m]
    }
    interv[predictors[m]] <- (nb[predictors[m]] - nb["all"]) * interventionper / (interv$threshold / (1 - interv$threshold))
    interv[paste0(predictors[m], "_N")] <- (nb[paste0(predictors[m], "_N")] - nb["all"]) * interventionper / (interv$threshold / (1 - interv$threshold))
  }

  # CYCLING THROUGH EACH predictor AND smooth NET BENEFIT AND INTERVENTIONS AVOIDED
  # 循环遍历每个predictor,平滑净获益和避免的干预措施
  for (m in 1:pred.n) {
    if (smooth == T) {
      lws <- loess(data.matrix(nb[!is.na(nb[[predictors[m]]]), predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]), "threshold"]), span = loess.span)
      nb[!is.na(nb[[predictors[m]]]), paste0(predictors[m], "_sm")] <- lws$fitted
      lws <- loess(data.matrix(interv[!is.na(nb[[predictors[m]]]), predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]), "threshold"]), span = loess.span)
      interv[!is.na(nb[[predictors[m]]]), paste0(predictors[m], "_sm")] <- lws$fitted

      lws <- loess(data.matrix(nb[!is.na(nb[[paste0(predictors[m], "_N")]]), paste0(predictors[m], "_N")]) ~ data.matrix(nb[!is.na(nb[[paste0(predictors[m], "_N")]]), "threshold"]), span = loess.span)
      nb[!is.na(nb[[paste0(predictors[m], "_N")]]), paste0(paste0(predictors[m], "_N"), "_sm")] <- lws$fitted
      lws <- loess(data.matrix(interv[!is.na(nb[[paste0(predictors[m], "_N")]]), paste0(predictors[m], "_N")]) ~ data.matrix(interv[!is.na(nb[[paste0(predictors[m], "_N")]]), "threshold"]), span = loess.span)
      interv[!is.na(nb[[paste0(predictors[m], "_N")]]), paste0(paste0(predictors[m], "_N"), "_sm")] <- lws$fitted
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
  # results$plot<-p

  return(results)
}
