install.packages("gmailr")

library(gmailr)
use_secret_file('stat463.json')

sender = "psu.forecasting.group.5@gmail.com"
receiver = "jyangjayy@live.com"

send_message(mime(
  To = receiver,
  From = sender,
  Subject = "Hi",
  body = "Hello"))