s1 <- seq(ss[length(ss)]-14)
for (i in 1:14){
  ssk[i] = s1+i
  as.list(nizk[1:ssk,]) = niz[1:ssk,]
}

result <- sapply(names(iris)[1 : 4], 
                 function(x) { 
                   lapply(names(iris)[1 : 4], 
                          function(y) {
                            if (x != y) {
                              model <- lm(as.formula(paste0(y, "~", x)), iris) 
                              return(list(x = x, 
                                          y = y, 
                                          r.squared = summary(model)$r.squared, 
                                          coefficients =  summary(model)$coefficients))
                            }
                          })
                 })

lood.model <- function(data = niz){
  for (i in 1:15){
    DDDD[i] <- niz[1: length(niz$Day)-i,]
    model[i] <- lm(niz[,2] ~ bs(niz[,1], 
                                knots = c(34,57,78,104,138,157)),
                   data = DDDD[i])
    Fit[i] <- fitted.values(model[i])
    Forecast[i] <- forecast::forecast(fitted.values(model[i]),
                                      h = (length(Dsf)-i))
    }
  return ("Model" = model,
          "Fits" = Fit,
          "Forecast" = Forecast)
}

bt.cor <- npboot.function(abc)
bootmean <- mean(bt.cor)
