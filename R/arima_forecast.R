require(forecast)
require(tidyhydat)

debits_journaliers<-hy_daily_flows(station_number ="02QA036")

#transformer les donnees en time series

debits_journaliers$Value %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

#a explorer: https://otexts.com/fpp2/arima-r.html
