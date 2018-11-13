library(simts)
library(pageviews)

#Plots
mobile = project_pageviews(platform = "mobile-app",
                           start = "2018010100",
                           end = "2019010100")
mobile_ts = gts(mobile$views)

desktop = project_pageviews(platform = "desktop",
                            start = "2018031500",
                            end = "2019010100")
desktop_ts = gts(desktop$views)

silvio = article_pageviews(article = "Silvio_Berlusconi",
                           start = "2018080100",
                           end = "2019010100")
silvio_ts = gts(silvio$views)

beyonce = article_pageviews(article = "Beyonce",
                            start = "2018090800",
                            end = "2019010100")
beyonce_ts = gts(beyonce$views)

chomsky = article_pageviews(article = "Noam_Chomsky",
                            start = "2017060100",
                            end = "2019010100")
chomsky_ts = gts(chomsky$views)

lazio = article_pageviews(article = "SS_Lazio",
                          start = "2018070100",
                          end = "2019010100")
lazio_ts = gts(lazio$views)

thanksgiving = article_pageviews(article = "Thanksgiving",
                                 start = "2017110700",
                                 end = "2017113000")
thanksgiving_ts = gts(thanksgiving$views)

#Set xts to whichever time series you want to select from
xts = mobile_ts

#Set parameters to current best model; OR all zeros to start from scratch
ar = 0
i = 0
ma = 0
sar = 0
si = 0
sma = 0
s = 0
best_aic = estimate(SARIMA(ar, i, ma, sar, si, sma, s), xts, method = "mle")$mod$aic

#Stupid loop that runs forever, must be manually terminated when you're bored of running R scripts.
#Warning: sometimes just straight-up crashes R, make sure everything's saved before running.
#Algorithm based on stochastic hill-climbing, but much jankier.
#Prints parameters and AIC every time it finds an improvement.
#Also warning: iterations can take a loooooooooooooooooooooooooooooooooooooooooooooooooooooooong time.
#You can uncomment some of the cat statements if you want to make sure it's working;
#Just make sure you read the correct best model at the end.
#Theoretically converges to optimal model, but I wouldn't count on it.

while (T) {
  #Perturbs each parameter by a random integer, tests to see if it's found a better model.
  ar_d = rpois(1, 2) * sample(c(-1, 1), 1)
  ma_d = rpois(1, 2) * sample(c(-1, 1), 1)
  sar_d = rpois(1, 2) * sample(c(-1, 1), 1)
  sma_d = rpois(1, 2) * sample(c(-1, 1), 1)
  s_d = rpois(1, 2) * sample(c(-1, 1), 1)
  while (s + s_d < 0) {
    s_d = rpois(1, 2) * sample(c(-1, 1), 1)
  }
  #The magic: if the model it's testing throws an error, it backs out gracefully and tries a new one.
  tryCatch({
    #cat(c(ar + ar_d, 0, ma + ma_d, sar + sar_d, 0, sma + sma_d, s + s_d, '\n'))
    current_AIC = estimate(SARIMA(ar + ar_d, 0, ma + ma_d, sar + sar_d, 0, sma + sma_d, s + s_d),
                           xts,
                           method = "mle")$mod$aic
    #cat('no error\n')
    if (current_AIC < best_aic) {
      best_aic = current_AIC
      ar = ar + ar_d
      i = 0
      ma = ma + ma_d
      sar = sar + sar_d
      si = 0
      sma = sma + sma_d
      s = s + s_d
      #cat(c(best_aic, '\n'))
      cat(c(ar, i, ma, sar, si, sma, s, best_aic, '\n'))
    }
  }, error = function(err) {
    return(0)
  })
}
