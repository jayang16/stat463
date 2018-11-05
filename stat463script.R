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
