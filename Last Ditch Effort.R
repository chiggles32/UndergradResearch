library(tidyverse)
library(jsonlite)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(tibbletime)
library(purrr)
library(ggridges)
library(gtsummary)
library(TTR)





##work on flow

fut = read.table(file.choose(),sep = ",", header = F)
etf = read.table(file.choose(),sep = ",", header = F)

colnames(fut) = c("D/T", "Open", "High", "Low", "Close", "Volume")
colnames(etf) = c("D/T", "Open", "High", "Low", "Close", "Volume")

ret_time = format(seq(as.POSIXct("09:30:00", format = "%H:%M:%S"), by = "30 min", length.out = 13), format = "%H:%M:%S")


fut = fut |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F)|>
    filter(hms(Time) >= hms("00:00:00") & hms(Time) < hms("09:30:00"))

etf = etf |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F) |>
    filter(hms(Time) >= hms("09:30:00") & hms(Time) < hms("16:00:00"))

etf = etf |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F) |>
    filter(hms(Time) %in% hms(ret_time))


x = unique(fut[,2])
y = unique(etf[,2])
z = intersect(x,y)

fut = fut |>
  filter(Date %in% z) |> 
    mutate(Weekday = wday(as.Date(Date), label = T), Months = month(as.Date(Date)), Year = year(as.Date(Date)) )


    

etf = etf |>
  filter(Date %in% z) |> 
    mutate(Weekday = wday(as.Date(Date), label = T), Months = month(as.Date(Date)), Year = year(as.Date(Date)) )

letf = etf |>
  group_by(Date) |>
     group_split()

lfut = fut |>
  group_by(Date) |>
     group_split()

fut_ret = fut |>
  filter(Time %in% c("00:00:00", "09:00:00")) |>
      group_by(Date) |>
        mutate(Return = Delt(Close)) |>
          filter(Time == "09:00:00")|>
            select(Date, Weekday, Return)

int_ret = etf |>
    filter(Time %in% c("09:30:00", "11:59:00")) |>
      group_by(Date) |>
        mutate(Int_Ret = Delt(Close)) |>
          filter(Time == "11:59:00")|>
            select(Date, Weekday, Int_Ret) 

mcloset = etf |>
  group_by(Date) |>
    slice_tail(n = 1)|>
      pull(Time)

full_int_ret = etf |>
    filter(Time %in% c("09:30:00", "15:59:00")) |>
      group_by(Date) |>
        mutate(Int_Ret = Delt(Close)) |>
          filter(Time == "15:59:00")|>
            select(Date, Weekday, Int_Ret) 


 
###Returns are correct
rolled_sd = rollify(sd,5)

co_fut = fut |>
    group_by(Date) |>
        mutate(Return = Delt(Close), CO = wilderSum((sign(Return)*Volume*Close),n=5)/runMean(Volume*Close,n=5),
          rsd = rolled_sd(Return), MODCO = CO/rsd)

##Selected Times

sfut = co_fut |>
  filter(Time == "09:00:00") 

###Ridgeline of Day of the Week

sfut |>
  ggplot( aes(y = Weekday, x = CO, fill = Weekday)) +
    geom_density_ridges(alpha = .6, stat = "binline", bins = 20)

int_ret |>
  ggplot( aes(y = Weekday, x = Int_Ret, fill = Weekday)) +
    geom_density_ridges(alpha = .6, stat = "binline", bins = 20) +
      xlab("Open to Noon Return")

###Pairing Data

comb = inner_join(int_ret, sfut, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)

comb1 = inner_join(full_int_ret, sfut, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)

###Day of the Week Scatters
## four variables of interest - CO, futures returns, interval returns (morning/full)

comb |>
  ggplot(aes( x = CO, y = Int_Ret, color = Weekday.x)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm, color = "black") +
     facet_wrap(~Weekday.x) +
       labs(title = "CO and Midday Returns", subtitle = "S&P 500 from 2005 - 2023",
         y = "Return", fill = "Weekday")

comb1 |>
  ggplot(aes( x = Return, y = Int_Ret, color = Weekday.x)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm, color = "black") +
     facet_wrap(~Weekday.x) 
       

###Full Scatter

comb1 |>
  ggplot(aes( x = CO, y = Int_Ret)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm) +
       labs( title = "S&P 500 CO and Full Trading Day Return",
       x = "Continuing Overconfidence", y = "Daily Return",
       subtitle = "CO from 7:00 to 9:30 and Returns from 9:30 to 4:00")

comb |>
  ggplot(aes( x = CO, y = Int_Ret)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm) +
       labs( title = "S&P 500 CO and Half Trading Day Return",
       x = "Continuing Overconfidence", y = "Daily Return",
       subtitle = "CO from 7:00 to 9:30 and Returns from 9:30 to 12:00")



###Regression Models
### Work on regression with days as factor

morning = lm(Int_Ret ~ CO, data = comb)
summary(morning)
tbl1 = tbl_regression(morning)


full_day = lm(Int_Ret ~ CO, data = comb1)
tbl2 = tbl_regression(full_day)

modify_caption(tbl_stack(list(tbl1,tbl2),group_header = c("Midday","Full Day")), "S&P 500")



#Need to finish this script then apply to other data set
#Overlapping Distribution of returns?
#Overall Scatterplot with regression 
#Evolution of relationship over the years to note on publishing in 2014
#Initial Graphic depicting the theoretical relationship

   






sdata = data |>
  filter(Date %in% sampledays) |>
    group_by(Date) |>
        mutate(Return = Delt(Close), CO = wilderSum((sign(Return)*Volume*Close),n=5)/runMean(Volume*Close,n=5),
          rsd = rolled_sd(Return), MODCO = CO/rsd) |>
            group_split()



par(mfrow = c(4,3))

for (i in 1:4){
  plot(lfut[[i]]$Close, typ = 'l')
  plot(lfut[[i]]$Volume, typ = 'l')
  plot(letf[[i]]$Close, typ = 'l')
  }


reg_mod = c()
for (i in 1:length(lfut)){
	reg_mod[i] = coef(lm(seq_along(lfut[[i]]$Close)~ lfut[[i]]$Close))[2] 
}

###L/S portfolio

weight = abs(comb$CO^2)/5 + .05
weight1 = abs(comb$CO)

signal = ifelse(comb$CO > 0, 1,
  ifelse(comb$CO < 0, -1, 0))

signal1 = ifelse(comb$CO > 1, 1,
  ifelse(comb$CO < -1, -1, 0))

signal2 = ifelse(comb$CO > 2, 1,
  ifelse(comb$CO < -2, -1, 0))

returns = comb$Int_Ret*signal

returns = as.xts(returns, as.Date(unique(comb$Date)))
chart.CumReturns(returns, main = "Long/Short Strategy - S&P 500")

par(mfrow = c(3,1))

hist(comb1$Int_Ret, breaks = 30, main = "Full Day Return S&P 500", ylab = "", xlab = "Return", sub = "2005 - 2021")
hist(comb$Int_Ret, breaks = 40, main = "Midday Return S&P 500", ylab = "", xlab = "Return", sub = "2005 - 2021")
hist(comb$CO, breaks = 30, main = "S&P 500 CO", ylab = "", xlab = "CO", sub = "2005 - 2021")




