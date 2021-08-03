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
ModelSelection <- function(Observed, Model, K, Name, Form, kutuf, TTy, Data) {
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
    0
  } else if (Form == "ALM") {
    0
  } else if (Form == "N-LM") {
    0
  } else {
    predict(Model, type = 'response')
  }
  
  ppk <- if(Preds == 0) 1 else 2
  
  RD01 <- round(ifelse(Name == "ARIMA",  Model$aic,
                       ifelse(Name == "SMOOTH"| Name == "Values", 0, AIC(Model))), 2)
  RD02 <- round(ifelse(Name == "ARIMA",  Model$bic,
                       ifelse(Name == "SMOOTH"| Name == "Values", 0, BIC(Model))), 2)
  RD03 <- round(ifelse(Name == "ARIMA" |
                         Name == "SMOOTH"| Form == "GLM"| Form == "ALM"|
                         Name == "Values"| Form == "Logit", 0, summary(Model)$r.squared), 2)
  RD04 <- round(ifelse(Name == "ARIMA" | Name == "SMOOTH"| Form == "GLM"| 
                         Form == "ALM"| Name == "Values"| Form == "Logit", 0, 
                       summary(Model)$adj.r.squared), 2)
  RD05 = round(accuracy(Observed, Predy), 2)
  RD06 = round(sum(ae(Observed, Predy)), 2)
  RD07 = round(sum(ape(Observed, Predy)), 2)
  RD08 = round(apk(actual = Observed, predicted = Predy, k = K), 2)
  RD09 = round(ifelse(Form == "LM"| Form == "ALM" |Form == "ARDL" |
                        TTy == "Number" | Name == "nil" & ppk == 1, auc(Observed, Predy),
                      ModelMetrics::auc(Observed, Preds)), 2)
  RD10 = round(bias(Observed, Predy), 2)
  RD11 = round(ifelse(Form == "LM" | TTy == "Number"| Form == "ALM",
                      ce(Observed, Predy), ModelMetrics::ce(Model)), 2)
  RD12 = round(ifelse(Form == "LM" | Form == "ALM", f1(Observed, Predy), 
                      ModelMetrics::f1Score(Observed, Preds, 
                                            cutoff = kutuf)), 2)
  RD13 = round(sum(na.omit((ll(Observed, Predy))), 2))
  RD14 = round(ifelse(Form == "LM" | Form == "ALM", logLoss(Observed, Predy),
                      ModelMetrics::logLoss(Observed, Preds)), 2)
  RD15 = round(ifelse(Form == "LM"| TTy == "Number"| Form == "ALM",
                      mae(Observed, Predy), ModelMetrics::mae(Model)), 2)
  RD16 = round(mape(Observed, Predy), 2)
  RD17 = round(mapk(actual = Observed, predicted = Predy, k = K), 2)
  RD18 = round(mase(Observed, Predy), 2)
  RD19 = round(mdae(Observed, Predy), 2)
  RD20 = round(ifelse(Form == "LM" | Form == "ALM", mse(Observed, Predy),
                      ModelMetrics::mse(Observed, Preds)), 2)
  RD21 = round(msle(Observed, Predy), 2)
  RD22 = round(percent_bias(Observed, Predy), 2)
  RD23 = round(ifelse(Form == "LM" | Form == "ALM", precision(Observed, Predy),
                      ModelMetrics::precision(Observed, Preds,
                                              cutoff = kutuf)), 2)
  RD24 = round(rae(Observed, Predy), 2)
  RD25 = round((ifelse(Form == "LM" | Form == "ALM", recall(Observed, Predy),
                       ModelMetrics::recall(Observed, Preds,
                                            cutoff = kutuf))), 2)
  RD26 = round(ifelse(Form == "LM" | Form == "ALM", rmse(Observed, Predy),
                      ModelMetrics::rmse(Observed, Preds)), 2)
  RD27 = round(rmsle(Observed, Predy), 2)
  RD28 = round(rrse(Observed, Predy), 2)
  RD29 = round(rse(Observed, Predy), 2)
  RD30 = round(sum(se(Observed, Predy)), 2)
  RD31 = round(sum(sle(Observed, Predy)), 2)
  RD32 = round(smape(Observed, Predy), 2)
  RD33 = round(sse(Observed, Predy), 2)
  ptp  = diff(Observed, lag = 1) / diff(Predy, lag = 1)
  ptpe = ifelse(ptp > 0, 0, 1)
  RD34 = sum(ptpe)
  #RD35 = randtests::turning.point.test(Observed)
  #if (ppk != 2) RD36 = randtests::turning.point.test(Predy) else RD36 = randtests::turning.point.test(Preds)
  
  WLE  = if (Name == "ARIMA" | Name == "SMOOTH"| Name == "Values"|
             Name == "EssemWet"| Form == "Logit") {
    0
  } else if (Form == "ALM") {
    round(max(summary(wle::mle.cp(Model, data = Data))$cp))
  } else {
    round(max(summary(wle::mle.cp(Model))$cp))
  }
  
  RD37 = WLE
  RD38 <- if (ppk == 1 & Name == "QUADRATIC") round(qpcR::PRESS(Model, verbose = FALSE)$P.square, 2) else 0
  
  RD39 = round(ifelse(Form == "LM"| TTy == "Number" | Form == "ALM", 
                      ModelMetrics::brier(Observed, Predy),
                      ModelMetrics::brier(Model)), 0)
  RD40 = round(ifelse(Form == "LM"| TTy == "Number" | Form == "ALM", 
                      ModelMetrics::gini(Observed,
                                         Predy),
                      ModelMetrics::gini(Model)), 0)
  RD41 = round(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::kappa(Observed, Preds, 
                                          cutoff = kutuf)), 0)
  RD42 = round(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::sensitivity(Observed, Preds,
                                                cutoff = kutuf)), 0)
  RD43 = round(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::specificity(Observed, Preds, 
                                                cutoff = kutuf)), 0)
  RD44 = round(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::fScore(Observed, Preds, 
                                           cutoff = kutuf, beta = 1)), 0)
  RD45 = round(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::mcc(Observed, Preds, cutoff = kutuf)), 0)
  RD46 = round(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::tnr(Observed, Preds, cutoff = kutuf)), 0)
  RD47 = round(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::tpr(Observed, Preds, cutoff = kutuf)), 0)
  RD48 = round(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::ppv(Observed, Preds, cutoff = kutuf)), 0)
  RD49 = round(ifelse(Form == "LM" | Form == "ALM", 0, 
                      ModelMetrics::npv(Observed, Preds, cutoff = kutuf)), 0)
  
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
