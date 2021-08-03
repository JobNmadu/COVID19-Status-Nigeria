
library(tidyverse)    # main package
library(zoo)          # was used for moving average
library(gghighlight)  # for highlight specif plot at each facet
library(ggthemes)     # may be needed for specif need
library(ftplottools)  #
library(coronavirus)  # used for corona dataset

library(readxl)
cumDaty <- read_excel("cumDat.xlsx")
cumDaty$Date <- as.Date(cumDaty$Date)
options(scipen = 999, digits = 2)
cumDat <- as.data.frame(cumDaty)
cumDat$Month <- factor(cumDat$Month, levels = month.name)
data <- cumDat 

library(splines)
ss <- seq(1:length(cumper$DailyCase))
Dss <- seq(cumDat$Date[1], by = "day", length.out = length(cumper$DailyCase))
Dsf <- seq(as.Date(Dss[length(Dss)] + lubridate::days(1)), by = "day", length.out = length(cumper$DailyCase))
day01 <- format(Dss[1], format = "%B %d, %Y")
daylast <- format(Dss[length(Dss)], format = "%B %d, %Y")
casesstarts <- paste("Starting from", day01, "to", daylast, collapse=" ")
casesstarts1 <- paste("Number of days from", day01, "to", daylast, collapse=" ")
dayfo <- format(Dsf[1], format = "%B %d, %Y")
lastdayfo <- format(Dsf[length(Dsf)], format = "%B %d, %Y")
forcaststarts <- paste("Starting from", dayfo, "to", lastdayfo, collapse=" ")


z.s <- as.data.frame(cbind("Day" = ss, "Case" = cumper$DailyCase), "Day1" = Dss)
monthlims<-range(z.s[,1])
month.grid<-seq(from=monthlims[1], to = monthlims[2])
niz <- as.data.frame(z.s)

fit0<-lm(niz[,2] ~ bs(niz[,1], knots =NULL), data = niz)
fit<-lm(niz[,2] ~ bs(niz[,1], knots = c(34,57,78,104,138,157)), data = niz)
fit1<-smooth.spline(z.s[,1],z.s[,2])
fita <- forecast::auto.arima(z.s$Case)


 kk <- forecast::forecast(fitted.values(fit), h = length(Dsf))
 kk0 <- forecast::forecast(fitted.values(fit0), h = length(Dsf))
 kk1 <- forecast::forecast(fitted.values(fit1), h = length(Dsf))
 kk2 <- forecast::forecast(fita, h = length(Dsf))

 KK <- as.data.frame(cbind("Date" = Dsf,"Day" = ss, "Without Knots" = kk0[["mean"]], "With Knots" = kk[["mean"]], "Smooth spline" = kk1[["mean"]], "Lower ARIMA" = kk2[["lower"]], "Upper ARIMA" = kk2[["upper"]]))

 KK <- KK[,-c(7,8)]
names(KK) <- c("Date", "Day", "Without Knots", "With Knots", "Smooth spline", "Lower ARIMA", "Upper ARIMA") 
colSums(KK)
KK$Date <- as.Date(KK$Date)
 
 KK1 <- KK %>%
   pivot_longer(-c(Date, Day), names_to = "Models",
                values_to = "Forecast")
KK1$Date <- as.Date(KK1$Date) 

 library(gganimate)
 library(magick)
 library(transformr)
 library(gifski)
 library(lubridate)
 library(grid)
 library(scales)

  KK2 <- KK1 %>%
   group_by(Date) %>%
   mutate(Value_lbl = paste0("  ", Forecast)) %>%
   group_by(Models) %>% 
   ungroup()
 
  anim <-  ggplot(KK2, aes(Date, Forecast,
                 colour = Models, frame = Date)) +
   geom_line() +
   geom_line(size=1.2) + 
   scale_colour_manual(values=c("green", "red", "blue", "pink", "gold")) +
   theme_minimal() +
   geom_text(aes(y=Forecast, label = Value_lbl, hjust=0)) + # Adds changing text
   transition_reveal(Date) +
 coord_cartesian(clip = 'off') + 
   labs(title = forcaststarts,
        y = "", 
        caption = "Source: coronavirus package, last updated 10/05/2020") + 
 #  scale_x_date(limits = c(dayfo,lastdayfo)) + #I add 2 weeks to x-axis so you can see the numbers
#   scale_y_continuous(breaks = c(0,1,2,3),
#                      labels = c(1,10,100,1000)) +
   theme(axis.title.x = element_text(face="bold", size=14),
         axis.title.y = element_text(face="bold", size=14),
         plot.caption = element_text(hjust=4.8, face="italic", color="#333333"),
#         legend.position = c(.05, .95),
         legend.justification = c("right", "top"))
 animate(anim, 
         nframes = 500,
         fps = 10,
         end_pause = 20)
 anim_save("forecast.gif", animation = last_animation())
 
 animate(anim,
         nframes = 500,
         fps = 10,
         end_pause = 20,
         renderer = ffmpeg_renderer(), 
         width = 800,
         height = 450)
 anim_save("forecast.mp4", animation = last_animation())
 