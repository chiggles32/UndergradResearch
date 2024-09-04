#Prepping futures import data

library(tidyverse)
library(jsonlite)
library(lubridate)
library(quantmod)

data = read.table(file.choose(),sep = ",", header = F)

colnames(data) = c("D/T", "Open", "High", "Low", "Close", "Volume")

data = data |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F) |>
    filter(hms(Time) < hms("09:31:00") & hms(Time) > hms("00:00:00")) 

x = data[,1]

data = data |>
  select(Open, High, Low, Close, Volume)

#consolidating to 10 min intervals

data = data |>
  as.xts(ymd_hms(x))

data = data |>
  to.period(period = "minutes", k = 30, indexAt = "startof")

x = index(data)

datat = as_tibble(data)

datat = datat |>
  mutate("D/T" = x) |>
    separate("D/T", c("Date","Time"), sep = " ", remove = F)

#calculating factors/indicators

datat = datat |>
  group_by(Date) |>
    mutate("Return" = Delt(data.Close),"CO" = wilderSum((sign(Return)*data.Volume),n=3)/runMean(data.Volume,n=3))


##Selecting CO data points i.e. 9:30 since indexed at end

CO_data = datat |>
  filter(Time == "09:00:00") |>
    select(Date, CO)
CO_data



#making list of days


data_list = datat |>
  group_by(Date) |>
    group_split()




#Gets us to interval returns

#Tidying Index ETF Data

etf = read.table(file.choose(),sep = ",", header = F)

colnames(etf) = c("D/T", "Open", "High", "Low", "Close", "Volume")

etf = etf |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F) |>
    filter(hms(Time) >= hms("09:30:00") & hms(Time) < hms("12:00:00"))


int_ret = etf|>
  filter(Time %in% c("09:30:00", "11:55:00")) |>
    group_by(Date) |>
      mutate(Int_Ret = Close/lag(Close)-1)

int_ret2 = etf|>
  filter(Time %in% c("09:30:00", "10:00:00")) |>
    group_by(Date) |>
      mutate(Int_Ret = log(Close/lag(Close)))

int_ret3 = etf|>
  filter(Time %in% c("09:30:00", "10:30:00")) |>
    group_by(Date) |>
      mutate(Int_Ret = Close/lag(Close)-1)

int_ret4 = etf|>
  filter(Time %in% c("09:30:00", "11:00:00")) |>
    group_by(Date) |>
      mutate(Int_Ret = Close/lag(Close)-1)

Ret_data = int_ret |>
  filter(Time == "11:55:00")|>
    select(Date,Int_Ret)
Ret_data

Ret_data2 = int_ret2 |>
  filter(Time == "10:00:00")|>
    select(Date,Int_Ret)

Ret_data3 = int_ret3 |>
  filter(Time == "10:30:00")|>
    select(Date,Int_Ret)

Ret_data4 = int_ret4 |>
  filter(Time == "11:00:00")|>
    select(Date,Int_Ret)


##Initial Analysis

merged_data = merge(Ret_data,CO_data)
merged_data2 = merge(Ret_data2,CO_data)
merged_data3 = merge(Ret_data3,CO_data)
merged_data4 = merge(Ret_data4,CO_data)

ggplot(data = merged_data2,
  mapping = aes(x = CO, y = Int_Ret))+
  geom_point()+
  geom_smooth()


cor(merged_data2[,3],merged_data2[,2])

fit = lm(merged_data2[,3]~ merged_data2[,2])
summary(fit)
plot(residuals(fit))

slimmed = merged_data2 |>
  filter(CO > 2 | CO < -2)
 

fit = lm(slimmed[,3]~ slimmed[,2])
summary(fit)
plot(residuals(fit))

