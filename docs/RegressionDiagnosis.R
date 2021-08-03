

#' Title
#'
#' @param Observed 
#' @param Model 
#' @param K 
#' @param Name 
#' @param Form 
#' @param kutuf 
#' @param TTy 
#' @param Data 
#'
#' @return
#' @export
#'
#' @examples
#' 
ModelSelection <- function(Observed, yvalue, Model, K, Name, Form, kutuf, TTy) {
  Predy = 0
  Preds = 0
  library(Metrics)
  Predy <- if (Name == "ARIMA") {
    Model[["fitted"]]
  } else if (Form == "ALM") {
    Model[["fitted"]]
  } else if (Form == "ARDL") {
    c(0, 0, 0, Model[["fitted.values"]])
  } else if (Name == "Values") {
    Model 
  } else {
    fitted.values(Model)
  }
  
  Preds <- if (Form == "LM") {
    fitted.values(Model)
  } else if (Form == "ALM") {
    0
  } else if (Form == "N-LM") {
    0
  } else if (Form == "GLM" & Name != "Log") {
    KK <- broom::augment(Model, data = Observed) %>%
      mutate(prob = 1/(1 + exp(-.fitted)), Observed)%>%
      mutate(Predicted = ifelse(prob > 0.5, 1, 0))
    KK$Predicted
  } else {
    predict(Model, type = "response")
  }
  
  ppk <- if (sum(Preds) == 0) 1 else 2
  
  RD01 <- signif(ifelse(Name == "ARIMA",  Model$aic,
                       ifelse(Name == "SMOOTH"| Name == "Values", 0, AIC(Model))), 2)
  RD02 <- signif(ifelse(Name == "ARIMA",  Model$bic,
                       ifelse(Name == "SMOOTH"| Name == "Values", 0, BIC(Model))), 2)
  RD03 <- signif(ifelse(Name == "ARIMA" |
                         Name == "SMOOTH"| Form == "GLM"| Form == "ALM"|
                         Name == "Values"| Name == "Logit", 0, summary(Model)$r.squared), 2)
  RD04 <- signif(ifelse(Name == "ARIMA" | Name == "SMOOTH"| Form == "GLM"| 
                         Form == "ALM"| Name == "Values"| Name == "Logit", 0, 
                       summary(Model)$adj.r.squared), 2)
  RD05 = signif(accuracy(yvalue, Preds), 2)
  RD06 = signif(sum(ae(yvalue, Preds)), 2)
  RD07 = signif(sum(ape(yvalue, Predy)), 2)
  RD08 = signif(apk(actual = yvalue, predicted = Preds, k = K), 2)
  RD09 = signif(ifelse(Form == "LM"| Form == "ALM" |Form == "ARDL" |
                        TTy == "Number" | Name == "nil" & ppk == 1, auc(yvalue, Predy),
                      ModelMetrics::auc(yvalue, Preds)), 2)
  RD10 = signif(bias(yvalue, Preds), 2)
  RD11 = signif(ifelse(Form == "LM" | TTy == "Number"| Form == "ALM",
                      ce(yvalue, Predy), ModelMetrics::ce(Model)), 2)
  RD12 = signif(ifelse(Form == "LM" | Form == "ALM", f1(yvalue, Predy), 
                      ModelMetrics::f1Score(yvalue, Preds, 
                                            cutoff = kutuf)), 2)
  RD13 = signif(sum(na.omit((ll(yvalue, Preds))), 2))
  RD14 = signif(ifelse(Form == "LM" | Form == "ALM", logLoss(yvalue, Predy),
                      ModelMetrics::logLoss(yvalue, Preds)), 2)
  RD15 = if(Form == "GLM"){
    signif(ModelMetrics::mae(actual = yvalue, predicted = Preds), 2)
  } else if (Form == "LM"| TTy == "Number"| Form == "ALM"){
    signif(ModelMetrics::mae(actual = yvalue, predicted = Preds), 2)
  } else {
    signif(ModelMetrics::mae(actual = yvalue, predicted = Predy), 2)
  }

  RD16 = signif(mape(yvalue, Predy), 2)
  RD17 = signif(ifelse(Name != "Values", 
                       mapk(actual = Observed, predicted = Model, k = K),
                       0), 2)
  RD18 = signif(mase(yvalue, Preds), 2)
  RD19 = signif(mdae(yvalue, Predy), 2)
  RD20 = signif(ifelse(Form == "LM" | Form == "ALM", mse(yvalue, Predy),
                      ModelMetrics::mse(yvalue, Preds)), 2)
  RD21 = signif(msle(yvalue, Preds), 2)
  RD22 = signif(percent_bias(yvalue, Predy), 2)
  RD23 = signif(ifelse(Form == "LM" | Form == "ALM", precision(yvalue, Predy),
                      ModelMetrics::precision(yvalue, Preds,
                                              cutoff = kutuf)), 2)
  RD24 = signif(rae(yvalue, Predy), 2)
  RD25 = signif((ifelse(Form == "LM" | Form == "ALM", recall(yvalue, Predy),
                       ModelMetrics::recall(yvalue, Preds,
                                            cutoff = kutuf))), 2)
  RD26 = signif(ifelse(Form == "LM" | Form == "ALM", rmse(yvalue, Predy),
                      ModelMetrics::rmse(yvalue, Preds)), 2)
  RD27 = signif(rmsle(yvalue, Preds), 2)
  RD28 = signif(rrse(yvalue, Preds), 2)
  RD29 = signif(rse(yvalue, Preds), 2)
  RD30 = signif(sum(se(yvalue, Preds)), 2)
  RD31 = signif(sum(sle(yvalue, Preds)), 2)
  RD32 = signif(smape(yvalue, Predy), 2)
  RD33 = signif(sse(yvalue, Preds), 2)
  ptp  = diff(yvalue, lag = 1) / diff(Predy, lag = 1)
  ptpe = ifelse(ptp > 0, 0, 1)
  RD34 = sum(ptpe)
  #RD35 = randtests::turning.point.test(Observed)
  #if (ppk != 2) RD36 = randtests::turning.point.test(Predy) else RD36 = randtests::turning.point.test(Preds)
  
  WLE  = if (Name == "ARIMA" | Name == "SMOOTH"| Name == "Values"|
             Name == "EssemWet"| Name == "Logit"| Form == "GLM") {
    0
  } else if (Form == "ALM") {
    signif(max(summary(wle::mle.cp(Model, data = Observed))$cp))
  } else {
    signif(max(summary(wle::mle.cp(Model))$cp))
  }
  
  RD37 = WLE
  RD38 <- ifelse(ppk == 1 & Name == "QUADRATIC", signif(qpcR::PRESS(Model, verbose = FALSE)$P.square, 2), 0)
  
  RD39 = signif(ifelse(Form == "LM"| TTy == "Number" | Form == "ALM", 
                      ModelMetrics::brier(yvalue, Preds),
                      ModelMetrics::brier(Model)), 0)
  RD40 = signif(ifelse(Form == "LM"| TTy == "Number" | Form == "ALM", 
                      ModelMetrics::gini(yvalue, Predy),
                      ModelMetrics::gini(Model)), 0)
  RD41 = signif(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::kappa(yvalue, Preds, 
                                          cutoff = kutuf)), 0)
  RD42 = signif(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::sensitivity(yvalue, Predy,
                                                cutoff = kutuf)), 0)
  RD43 = signif(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::specificity(yvalue, Preds, 
                                                cutoff = kutuf)), 0)
  RD44 = signif(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::fScore(yvalue, Preds, 
                                           cutoff = kutuf, beta = 1)), 0)
  RD45 = signif(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::mcc(yvalue, Preds, cutoff = kutuf)), 0)
  RD46 = signif(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::tnr(yvalue, Preds, cutoff = kutuf)), 0)
  RD47 = signif(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::tpr(yvalue, Preds, cutoff = kutuf)), 0)
  RD48 = signif(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::ppv(yvalue, Preds, cutoff = kutuf)), 0)
  RD49 = signif(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::npv(yvalue, Preds, cutoff = kutuf)), 0)
  
  results <- list(
    "Absolute Error" = RD06,
    "Absolute Percent Error" = RD07,
    "Accuracy" = RD05,
    "Adjusted R Square" = RD04,
    "Akaike's Information Criterion AIC" = RD01,
    "Allen's Prediction Sum-Of-Squares (PRESS, P-Square)" = RD38,
    "Area under the ROC curve (AUC)" = RD09,
    "Average Precision at k" = RD08,
    "Bias" = RD10,
    "Brier score" = RD39,
    "Classification Error" = RD11,
    "F1 Score" = RD12,
    "fScore" = RD44,
    "GINI Coefficient" = RD40,
    "kappa statistic" = RD41,
    "Log Loss" = RD13,
    "Mallow's cp" = RD37,
    "Matthews Correlation Coefficient" = RD45,
    "Mean Log Loss" = RD14,
    "Mean Absolute Error" = RD15,
    "Mean Absolute Percent Error" = RD16,
    "Mean Average Precision at k" = RD17,
    "Mean Absolute Scaled Error" = RD18,
    "Median Absolute Error" = RD19,
    "Mean Squared Error" = RD20,
    "Mean Squared Log Error" = RD21,
    "Model turning point error" = RD34,
    "Negative Predictive Value" = RD49,
    #    "Observed turning point error" = RD35$tp,
    "Percent Bias" = RD22,
    "Positive Predictive Value" = RD48,
    "Precision" = RD23,
    "R Square" = RD03,
    "Relative Absolute Error" = RD24,
    "Recall" = RD25,
    "Root Mean Squared Error" = RD26,
    "Root Mean Squared Log Error" = RD27,
    "Root Relative Squared Error" = RD28,
    "Relative Squared Error" = RD29,
    "Schwarz's Bayesian criterion BIC" = RD02,
    "Sensitivity" = RD42,
    "specificity" = RD43,
    "Squared Error" = RD30,
    "Squared Log Error" = RD31,
    "Symmetric Mean Absolute Percentage Error" = RD32,
    "Sum of Squared Errors" = RD33,
    "True negative rate" = RD46,
    "True positive rate" = RD47
    #   "Turning point error using random tests" = RD36$tp
  )
  return(results)
}
