install.packages("fpp3")
library(fpp3)

install.packages("readabs")

library(readabs)
cpidata_full <- read_abs("6401.0", tables = "9", check_local=FALSE) %>%
  mutate(Quarter = yearquarter (date)) %>%
  as_tsibble(
    index = Quarter,
    key = c(series_id)
  )

install.packages("tidyverse")
library(tidyverse)
cpidata <- cpidata_full %>%
  filter(!str_detect(`series`, "All groups")) %>%
  filter(!str_detect(`series`, "Furn")) %>%
  filter(!str_detect(`series`, "Insurance")) %>%
  filter(!str_detect(`series`, "Financial services")) %>%
  filter(!str_detect(`series`, "Deposit")) %>%
  filter(!str_detect(`series`, "Health"))

set.seed(5421374)
myseries <- cpidata %>%
  filter(`series_id` == sample(`series_id`, 1)) %>%
  filter(!is.na(value))

myylab <- substr(myseries$series[1], 1, 6)
myxlab <- "Quarter"
mytitle <- paste0(c("CPI: "),
                  substr(myseries$series[1], 18, nchar(myseries$series[1])-2))
myseries %>%
  autoplot(value) +
  theme(title = element_text(size = 10)) +
  labs(y = myylab,
       x = myxlab,
       title = mytitle)
#b) 
myseries %>%
  autoplot(value) +
  theme(title = element_text(size = 10)) +
  labs(y = myylab,
       x = myxlab,
       title = mytitle)


myseries %>% 
  autoplot(box_cox(value, 0)) + 
  theme(title = element_text(size = 10)) +
  labs(y = myylab,
       x = myxlab,
       title = mytitle,
       subtitle = "With a Log Transformation")

# Split of training and test datasets:
myseries_tr <-myseries %>%
  filter(year(Quarter) < 2010)   

myseries_te <- myseries %>%
  filter (year(Quarter) >=2010 ) 

autoplot(myseries_tr, log(value)) + 
  autolayer(myseries_te, log(value), colour = "red") + 
  labs(y = myylab,
       x = myxlab,
       title = mytitle,
       subtitle = "Split of Train and Test Datasets")

#d) 
fit_R <- myseries_tr %>% 
  model(ETS(log(value))) 
fit_R

fit_AAN <-myseries_tr %>%
  model("R reported" = ETS(log(value) ~ error("A") + trend("A") + season("N")))

# Residuals 
ETS_res <- myseries_tr %>% 
  model("R reported" = ETS(log(value)~ error("A") + trend("A") + season("N")))
ETS_res %>% 
  gg_tsresiduals()

#Ljung-Box test 

fit_AAN%>% report()
augment(fit_AAN) %>% 
  features(.innov,ljung_box,lag=10)



#e) 

#Forecast 1 
fc1<-myseries %>%
  filter(year(Quarter) < 2010) %>%
  model("R reported" = ETS(log(value)~ error("A") + trend("A") + season("N"))) %>%
  forecast(h = 4)

fc1 %>%
  autoplot(filter_index(myseries,"2000 Q1"~ "2010 Q4"),level = NULL) +
  autolayer(fc1)+ 
  labs(y = myylab,
       x = myxlab,
       title = mytitle,
       subtitle = "Short Term Forecast: 1 Year Ahead ")

#Forecast 2 
fc2<-myseries %>%
  filter(year(Quarter) < 2010) %>%
  model("R reported" = ETS(log(value)~ error("A") + trend("A") + season("N"))) %>%
  forecast(h = 20)

fc2 %>%
  autoplot(filter_index(myseries,"2000 Q1"~"2014 Q4"),level = NULL) +
  autolayer(fc2)+ 
  labs(y = myylab,
       x = myxlab,
       title = mytitle,
       subtitle = "Medium Term Forecast: 5 Years Ahead ")

#Forecast 3
fc3<-myseries %>%
  filter(year(Quarter) < 2010) %>%
  model("R reported" = ETS(log(value)~ error("A") + trend("A") + season("N"))) %>%
  forecast(h = 56)

fc3 %>%
  autoplot(filter_index(myseries_tr,"2000 Q1"~.),level = NULL) +
  autolayer(fc3)+ 
  labs(y = myylab,
       x = myxlab,
       title = mytitle,
       subtitle = "Long Term Forecast: 14 Years Ahead ")
#f) 
accuracy(fit_AAN)
fc1 %>% accuracy(myseries)
fc2 %>% accuracy(myseries)
fc3 %>% accuracy(myseries)

#g) 
autoplot(myseries,log(value))

myseries %>% 
  features(log(value), unitroot_ndiffs)

myseries %>% 
  autoplot((log(value)) %>% difference(1)) + 
  labs(y = "Value after Differencing",
       x = myxlab,
       title = mytitle,
       subtitle = "Differencing Applied")

myseries %>% mutate(log_value = box_cox(value,0) %>% difference(1)) %>% 
  features(log_value, unitroot_kpss)

myseries %>% mutate(log_value = difference(log(value),1)) %>% 
  features(log_value, unitroot_kpss)


#h)
fitA <- myseries %>% 
  model(auto = ARIMA(log(value), stepwise = FALSE, approx = FALSE)) %>%
report(fitA)



fitA %>% 
  select(auto) %>% 
  gg_tsresiduals()

                                                             
fitA %>% report()
augment(fitA) %>% 
  features(.innov,ljung_box,lag=8, dof= 6)



#i) 
STL_ARIMA <- myseries_tr %>% 
  model(
    ETS_AAN = ETS(log(value) ~ error("A") + trend("A") + season("N")),
    STL_dcm = decomposition_model(STL(log(value)), ARIMA(season_adjust))
    ) %>% 
  forecast(h = 56)

# Model Comparisons 
STL_ARIMA %>% 
  autoplot(filter_index(myseries,"2000"~.),level = 95)+ 
  labs(y = myylab,
       x = myxlab,
       title = mytitle,
       subtitle = "Forecast of an STL-ARIMA model and an ETS model")

  
STL_ARIMA %>% accuracy(myseries)


