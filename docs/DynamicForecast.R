

#' Title
#'
#' @param Data 
#' @param BREAKS 
#' @param MaximumDate 
#' @param Trend 
#'
#' @return
#' @export
#'
#' @examples
DynamicForecast <- function(Data, BREAKS, MaximumDate, Trend) {
  Data$Day <- ss <- seq(1:length(Data$Case))
  fit01  <- lm(Case ~ splines::bs(Day, knots = NULL), data = Data)
  fit10   <- lm(Case ~ splines::bs(Day, knots = BREAKS),
                data = Data)
  fit11  <- stats::smooth.spline(Data$Day, Data$Case)
  fita1  <- forecast::auto.arima(Data$Case)
  fitpi1 <- stats::lm(Case ~ Day + I(Day^2), data = Data)
  Dss19 <- seq(Data$Day[1], by = 1, length.out = length(Data$Day))
  MaximumDate <- as.Date(MaximumDate)
  
  if (Trend == "Day") {
    Dsf19 <- seq(as.Date(MaximumDate + lubridate::days(1)),
                 by = "day", length.out = length(Data$Case))
    Dsf19day01 <- format(Dsf19[1], format = "%b %d, %y")
    Dsf19daylast <- format(Dsf19[length(Dsf19)], format = "%b %d, %y")
  } else if (Trend == "Month") {
    Dsf19 <- seq(as.Date(MaximumDate + lubridate::month(1)),
                 by = "month", length.out = length(Data$Case))
    Dsf19day01 <- zoo::as.yearmon(Dsf19[1], "%b %y")
    Dsf19daylast <- zoo::as.yearmon(Dsf19[length(Dsf19)], "%b %y")
  } else {
    Dsf19 <- seq(as.Date(MaximumDate + lubridate::years(1)),
                 by = "year", length.out = length(Data$Case))
    Dsf19day01 <- format(as.Date(Dsf19[1]), "%Y")
    Dsf19daylast <- format(as.Date(Dsf19[length(Dsf19)]), "%Y")
  }
  
  Title <- paste(Dsf19day01, "-", Dsf19daylast,
                 collapse="")
  Without.knots <- fitted.values(fit01)
  With.knots <- fitted.values(fit10)
  Smooth <- fitted.values(fit11)
  ARIMA <- fita1[["fitted"]]
  Quadratic <- fitted.values(fitpi1)
  
  kk91   <- forecast::forecast(Without.knots,  h = length(Dsf19))
  kk091  <- forecast::forecast(With.knots,  h = length(Dsf19))
  kk191  <- forecast::forecast(Smooth,  h = length(Dsf19))
  kk1091 <- forecast::forecast(Quadratic, h = length(Dsf19))
  kk291  <- forecast::forecast(fita1, h = length(Dsf19))
  kk3091 <- (Without.knots + With.knots +
               Smooth + Quadratic +
               ARIMA)/5
  kk3191 <- forecast::forecast(kk3091, h = length(Dsf19))
  kk4091 <- lm(Data$Day~Without.knots * With.knots * Smooth * Quadratic *
                 ARIMA)
  kk4191 <- forecast::forecast(fitted.values(kk4091), h = length(Dsf19))
  kk6091 <- lm(Data$Day~Without.knots + With.knots + Smooth + Quadratic +
                 ARIMA)
  kk6191 <- forecast::forecast(fitted.values(kk6091), h = length(Dsf19))
  
  KK91 <- as.data.frame(cbind("Date" = Dsf19,"Day" = ss, "Without Knots" =
                                kk91[["mean"]], "Smooth spline" =
                                kk091[["mean"]], "With Knots" =
                                kk191[["mean"]], "Polynomial" =
                                kk1091[["mean"]], "Lower ARIMA" =
                                kk291[["lower"]], "Upper ARIMA" =
                                kk291[["upper"]]))
  KK91 <- KK91[,-c(7,9)]
  names(KK91) <- c("Date", "Day", "Without Knots", "Smooth spline",
                   "With Knots", "Polynomial", "Lower ARIMA", "Upper ARIMA")
  #KK91$Date <- as.character(KK91$Date)
  
  RMSE91 <- c("Without knots" = Metrics::rmse(Data$Case,
                                              Without.knots),
              "Smooth Spline" = Metrics::rmse(Data$Case, With.knots),
              "With knots" = Metrics::rmse(Data$Case, Smooth),
              "Polynomial" = Metrics::rmse(Data$Case, Quadratic),
              "Lower ARIMA" = Metrics::rmse(Data$Case, ARIMA),
              "Upper ARIMA" = Metrics::rmse(Data$Case, ARIMA))
  
  #RMSE <- 1/RMSE
  RMSE_weight91 <- as.list(RMSE91 / sum(RMSE91))
  KK91$Date <- as.Date(KK91$Date, origin = "1970-01-01")
  KK91$`Ensembled with equal weight` <- kk3191[["mean"]]
  KK91$`Ensembled based on weight` <- kk4191[["mean"]]
  KK91$`Ensembled based on summed weight` <- kk6191[["mean"]]
  P_weight91 <- (Without.knots * RMSE_weight91$`Without knots`) +
    (With.knots * RMSE_weight91$`Smooth Spline`) +
    (Smooth * RMSE_weight91$`With knots`) +
    (Quadratic * RMSE_weight91$Polynomial) +
    (ARIMA * RMSE_weight91$`Lower ARIMA`)
  
  kk5191 <- forecast::forecast(P_weight91, h = length(Dsf19))
  KK91$`Ensembled based on weight of fit` <- kk5191[["mean"]]
  RMSE91$`Ensembled with equal weight` <- Metrics::rmse(Data$Case, kk3091)
  RMSE91$`Ensembled based on weight` <- Metrics::rmse(Data$Case,
                                                      fitted.values(kk4091))
  RMSE91$`Ensembled based on summed weight` <- Metrics::rmse(Data$Case,
                                                             fitted.values(kk6091))
  RMSE91$`Ensembled based on weight of fit` <- Metrics::rmse(Data$Day, P_weight91)
  DDf91 <- c("Without knots", "Smooth Spline",
             "With knots", "Quadratic Polynomial",
             "Lower ARIMA", "Upper ARIMA",
             "Ensembled with equal weight",
             "Ensembled based on weight",
             "Ensembled based on summed weight",
             "Ensembled based on weight of fit" )
  Forcasts91 <- colSums(KK91[,-c(1,2)])
  Fore_f91 <- as.data.frame(cbind("Model" = DDf91,
                                  "Case" =
                                    formattable::comma(round(Forcasts91, 0))))
  RMSE_f91 <- c(
    "Without knots" =
      round(RMSE91$`Without knots`, 2),
    "Smooth Spline"  =
      round(RMSE91$`Smooth Spline`, 2),
    "With knots"  =
      round(RMSE91$`With knots`, 2),
    "Polynomial"  =
      round(RMSE91$Polynomial, 2),
    "Lower ARIMA"  =
      round(RMSE91$`Lower ARIMA`, 2),
    "Upper ARIMA"  =
      round(RMSE91$`Upper ARIMA`, 2),
    "Ensembled with equal weight"  =
      round(RMSE91$`Ensembled with equal weight`, 2),
    "Ensembled based on weight"  =
      round(RMSE91$`Ensembled based on weight`, 2),
    "Ensembled based on weight"  =
      round(RMSE91$`Ensembled based on summed weight`, 2),
    "Ensembled based on weight of fit"  =
      round(RMSE91$`Ensembled based on weight of fit`, 2)
  )
  RMSE_f91 <- cbind("Models" = DDf91, "RMSE" = RMSE_f91)
  
  KK191 <- KK91 %>%
    tidyr::pivot_longer(-c(Date, Day), names_to = "Models",
                        values_to = "Forecast")
  KK191$Date <- as.Date(KK191$Date)
  #KK911$Date <- as.character(as.Date(KK191$Date, origin = "2021-02-09"))
  KK0091 <- ggplot2::ggplot(KK191) +
    aes(x = Date, y = Forecast, colour = Models, group = Models) +
    geom_line(size = 1L) +
    scale_color_hue() +
    theme_bw() +
    theme() +
    labs(title = Title,
         subtitle = " ",
         caption = " ")
  results <- list(
    "Spline without knots" = fit01,
    "Spline with knots" = fit10,
    "Smooth Spline" = fit11,
    "ARIMA" = fita1,
    "Quadratic" = fitpi1,
    "Ensembled with equal weight" = kk3091,
    "Ensembled based on weight" = kk4091,
    "Ensembled based on summed weight" = kk6091,
    "Ensembled based on weight of fit" = P_weight91,
    "Forecast" = Fore_f91,
    "RMSE"     = RMSE_f91,
    "Plot"     = KK0091,
    "Date"     = Title
  )
  return(results)
}
