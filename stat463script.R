#Packages
library(forecast463)
library(simts)
library(pageviews)
library(gmailr)

#Plots
mobile = project_pageviews(platform = "mobile-app", start = "2018010100", end = "2019010100")
mobile_ts = gts(mobile$views)
plot(mobile_ts)

desktop = project_pageviews(platform = "desktop", start = "2018031500", end = "2019010100")
desktop_ts = gts(desktop$views)
plot(desktop_ts)

silvio = article_pageviews(article = "Silvio_Berlusconi", start = "2018080100", end = "2019010100")
silvio_ts = gts(silvio$views)
plot(silvio_ts)

beyonce = article_pageviews(article = "Beyonce", start = "2018090800", end = "2019010100")
beyonce_ts = gts(beyonce$views)
plot(beyonce_ts)

chomsky = article_pageviews(article = "Noam_Chomsky", start = "2017060100", end = "2019010100")
chomsky_ts = gts(chomsky$views)
plot(chomsky_ts)

lazio = article_pageviews(article = "SS_Lazio", start = "2018070100", end = "2019010100")
lazio_ts = gts(lazio$views)
plot(lazio_ts)

thanksgiving = article_pageviews(article = "Thanksgiving", start = "2017110700", end = "2017113000")
thanksgiving_ts = gts(thanksgiving$views)
plot(thanksgiving_ts)

#Predictions using AR and SARIMA models
#mobile_selection = select(AR(23), mobile_ts)
mobile_estimate = estimate(AR(18), mobile_ts)
check(model = mobile_estimate)
mobile_prediction = predict(mobile_estimate, n.ahead = 30, level = 0.95)

#desktop_selection = select(AR(5), desktop_ts)
desktop_estimate = estimate(SARIMA(ar = 1, i = 0, ma = 0, sar = 4, si = 0, sma = 0, s = 7), desktop_ts,
                            method = "rgmwm")
check(model = desktop_estimate)
desktop_prediction = predict(desktop_estimate, n.ahead = 30, level = 0.95)

#silvio_selection = select(AR(20), silvio_ts)
silvio_estimate = estimate(SARIMA(ar = 0, i = 0, ma = 0, sar = 3, si = 0, sma = 1, s = 11), silvio_ts,
                           method = "rgmwm")
check(model = silvio_estimate)
silvio_prediction = predict(silvio_estimate, n.ahead = 30, level = 0.95)

#beyonce_selection = select(AR(12), beyonce_ts)
beyonce_estimate = estimate(SARIMA(ar = 1, i = 0, ma = 0, sar = 2, si = 0, sma = 0, s = 15), beyonce_ts,
                                    method = "rgmwm")
check(model = beyonce_estimate)
beyonce_prediction = predict(beyonce_estimate, n.ahead = 30, level = 0.95)

#chomsky_selection = select(AR(17), chomsky_ts)
chomsky_estimate = estimate(SARIMA(ar = 3, i = 0, ma = 0, sar = 4, si = 0, sma = 0, s = 10), chomsky_ts,
                            method = "rgmwm")
check(model = chomsky_estimate)
chomsky_prediction = predict(chomsky_estimate, n.ahead = 30, level = 0.95)

#lazio_selection = select(AR(24), lazio_ts)
lazio_estimate = estimate(SARIMA(ar = 3, i = 0, ma = 0, sar = 2, si = 0, sma = 0, s = 9), lazio_ts,
                          method = "rgmwm")
check(model = lazio_estimate)
lazio_prediction = predict(lazio_estimate, n.ahead = 30, level = 0.95)

thanks_estimate = estimate(SARIMA(ar = 0, i = 0, ma = 1, sar = 1, si = 0, sma = 0, s = 22), thanksgiving_ts,
                          method = "rgmwm")
check(model = thanks_estimate)
thanks_prediction = predict(thanks_estimate, n.ahead = 30, level = 0.95)

#Compiling predictions into list
mobile_object = list(mobile_pred = as.numeric(mobile_prediction$pred), 
                     mobile_ci = as.numeric(mobile_prediction$CI0.95))
desktop_object = list(desktop_pred = as.numeric(desktop_prediction$pred), 
                      desktop_ci = as.numeric(desktop_prediction$CI0.95))
silvio_object = list(silvio_pred = as.numeric(silvio_prediction$pred), 
                     silvio_ci = as.numeric(silvio_prediction$CI0.95))
beyonce_object = list(beyonce_pred = as.numeric(beyonce_prediction$pred), 
                      beyonce_ci = as.numeric(beyonce_prediction$CI0.95))
chomsky_object = list(chomsky_pred = as.numeric(chomsky_prediction$pred), 
                      chomsky_ci = as.numeric(chomsky_prediction$CI0.95))
lazio_object = list(lazio_pred = as.numeric(lazio_prediction$pred), 
                    lazio_ci = as.numeric(lazio_prediction$CI0.95))
thanks_object = list(thanks_pred = as.numeric(thanks_prediction$pred), 
                     thanks_ci = as.numeric(thanks_prediction$CI0.95))

prediction = list(mobile = mobile_object, desktop = desktop_object, 
                  silvio = silvio_object, beyonce = beyonce_object, 
                  chomsky = chomsky_object, lazio = lazio_object, 
                  thanks = thanks_object)

# Check prediction object
prediction_OK = check_prediction(prediction = prediction)

# Check credentials
group = 5 # insert group number
from = "psu.forecasting.group.5@gmail.com" # insert group gmail address
key = "gkutbiFMJq8720E" # insert group unique key
credential_OK = check_credentials(group = group, from = from, key = key)

suppressPackageStartupMessages(library(gmailr))

group = 5 # example group number
from = "psu.forecasting.group.5@gmail.com" # example gmail address used by group number X to send their forecasts
key_group = "gkutbiFMJq8720E" # example of unique key identifier for the group
date = Sys.Date() # time at which the forecast is sent

send_prediction = function(group, prediction, to, from, key, date = Sys.Date()){
  send_message(mime(
    To = to,
    From = from,
    Subject = paste("[STAT 463] Group ", group, sep = ""),
    body = paste(key, date, paste(unlist(prediction), collapse = ","), sep = ";")))
}

#to = "psu.forecasting.instructors@gmail.com"
send_prediction(group = group, prediction = prediction, to = to, from = from, 
                key = key_group, date = date)

to = "jyangjayy@live.com"
# If checks OK: submit forecasts
send_prediction(group = group, prediction = prediction, to = to, from = from, 
                key = key_group, date = date)
