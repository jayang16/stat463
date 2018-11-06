devtools::install_github("SMAC-Group/forecast463")

library(forecast463)
use_secret_file('stat463.json')

sender = "psu.forecasting.group.5@gmail.com"
receiver = "jyangjayy@live.com"

send_message(mime(
  To = receiver,
  From = sender,
  Subject = "Hi",
  body = "Hello"))

group = 5
from = "psu.forecasting.group.5@gmail.com"
key = "gkutbiFMJq8720E"
credential_OK = check_credentials(group = group, from = from, key = key)

#Visualizations of all time series

library(simts)
library(pageviews)

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

thanksgiving = article_pageviews(article = "Thanksgiving", start = "2017100100", end = "2019010100")
thanksgiving_ts = gts(thanksgiving$views)
plot(thanksgiving_ts)

#Predictions using just AR(p) models

mobile_selection = select(AR(23), mobile_ts)
mobile_estimate = estimate(AR(18), mobile_ts)
check(model = mobile_estimate)
predict(mobile_estimate)

desktop_selection = select(AR(5), desktop_ts)
desktop_estimate = estimate(AR(5), desktop_ts)
check(model = desktop_estimate)
predict(desktop_estimate)

silvio_selection = select(AR(20), silvio_ts)
silvio_estimate = estimate(AR(9), silvio_ts)
check(model = silvio_estimate)
predict(silvio_estimate)

beyonce_selection = select(AR(12), beyonce_ts)
beyonce_estimate = estimate(AR(2), beyonce_ts)
check(model = beyonce_estimate)
predict(beyonce_estimate)

chomsky_selection = select(AR(17), chomsky_ts)
chomsky_estimate = estimate(AR(9), chomsky_ts)
check(model = chomsky_estimate)
predict(chomsky_estimate)

lazio_selection = select(AR(24), lazio_ts)
lazio_estimate = estimate(AR(7), lazio_ts)
check(model = lazio_estimate)
predict(lazio_estimate)
