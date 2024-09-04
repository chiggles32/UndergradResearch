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
  filter(Date %in% z & Date > as.Date("2008-10-01")) |> 
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
  filter(Time %in% c("00:00:00", "09:25:00")) |>
      group_by(Date) |>
        mutate(Return = Delt(Close)) |>
          filter(Time == "09:25:00")|>
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

min_int_ret = etf |>
    filter(Time %in% c("09:30:00", "09:31:00")) |>
      group_by(Date) |>
        mutate(Int_Ret = Delt(Close)) |>
          filter(Time == "09:31:00")|>
            select(Date, Weekday, Int_Ret) 

min5_int_ret = etf |>
    filter(Time %in% c("09:30:00", "09:35:00")) |>
      group_by(Date) |>
        mutate(Int_Ret = Delt(Close)) |>
          filter(Time == "09:35:00")|>
            select(Date, Weekday, Int_Ret) 

min10_int_ret = etf |>
    filter(Time %in% c("09:30:00", "09:40:00")) |>
      group_by(Date) |>
        mutate(Int_Ret = Delt(Close)) |>
          filter(Time == "09:40:00")|>
            select(Date, Weekday, Int_Ret) 


 
###Returns are correct
rolled_sd = rollify(sd,5)

co_fut = fut |>
    group_by(Date) |>
        mutate(Return = Delt(Close), CO = wilderSum((sign(Return)*Volume*Close),n=6)/runMean(Volume*Close,n=6),
          rsd = rolled_sd(Return), MODCO = CO/rsd)

co_fut1 = fut |>
    group_by(Date) |>
        mutate(Return = Delt(Close), CO = wilderSum((sign(Return)*Volume*Close),n=12)/runMean(Volume*Close,n=12),
          rsd = rolled_sd(Return), MODCO = CO/rsd)

co_fut2 = fut |>
    group_by(Date) |>
        mutate(Return = Delt(Close), CO = wilderSum((sign(Return)*Volume*Close),n=18)/runMean(Volume*Close,n=18),
          rsd = rolled_sd(Return), MODCO = CO/rsd)

##Selected Times

sfut = co_fut |>
  filter(Time == "09:25:00") 

sfut1 = co_fut1 |>
  filter(Time == "09:25:00") 

sfut2 = co_fut2 |>
  filter(Time == "09:25:00")

############################
#Hist of CO


sfut2 |>
  ggplot(aes(x = CO)) +
    geom_histogram(alpha = .7) 

###Ridgeline of Day of the Week

sfut2 |>
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

combmin = inner_join(min_int_ret, sfut, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)

combmin5 = inner_join(min5_int_ret, sfut, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)

combmin10 = inner_join(min10_int_ret, sfut, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)

#######################################
#Combinations of CO 2 hour
#################################

comb = inner_join(int_ret, sfut2, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)

comb1 = inner_join(full_int_ret, sfut2, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)

combmin = inner_join(min_int_ret, sfut2, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)

combmin5 = inner_join(min5_int_ret, sfut2, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)

combmin10 = inner_join(min10_int_ret, sfut2, by = "Date") |>
  select(Date, Weekday.x, Int_Ret, CO) |>
    inner_join(fut_ret, by = "Date") |>
      select(Date, Weekday.x, Int_Ret, CO, Return)
##############################################################

###Day of the Week Scatters
## four variables of interest - CO, futures returns, interval returns (morning/full)

comb |>
  ggplot(aes( x = CO, y = Int_Ret, color = Weekday.x)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm, color = "black") +
       facet_wrap(~Weekday.x) +
         labs(title = "CO and Midday Returns", subtitle = "Russell 2000 from 2008 - 2023",
         y = "Return", fill = "Weekday")

comb1 |>
  ggplot(aes( x = CO, y = Int_Ret, color = Weekday.x)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm, color = "black") +
     facet_wrap(~Weekday.x) +
       labs(title = "CO and Full Day Returns", subtitle = "Russell 2000 from 2008 - 2023",
         y = "Return", fill = "Weekday")

combmin |>
  ggplot(aes( x = CO, y = Int_Ret, color = Weekday.x)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm, color = "black") +
     facet_wrap(~Weekday.x) +
       labs(title = "CO and 1 Minute Returns", subtitle = "Russell 2000 from 2008 - 2023",
         y = "Return", fill = "Weekday")

###Full Scatter

comb1 |>
  ggplot(aes( x = CO, y = Int_Ret)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm) +
       labs( title = "Russell 2000 CO and Full Trading Day Return",
       x = "Continuing Overconfidence", y = "Daily Return",
       subtitle = "CO from 7:00 to 9:30 and Returns from 9:30 to 4:00")

comb |>
  ggplot(aes( x = CO, y = Int_Ret)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm) +
       labs( title = "Russell 2000 CO and Half Trading Day Return",
       x = "Continuing Overconfidence", y = "Daily Return",
       subtitle = "CO from 7:00 to 9:30 and Returns from 9:30 to 12:00")

combmin |>
  ggplot(aes( x = CO, y = Int_Ret)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm) +
       labs( title = "Russell 2000 CO and 1 Minute Return",
       x = "Continuing Overconfidence", y = "Return",
       subtitle = "CO from 7:00 to 9:30 and Returns from 9:30 to 9:31")

combmin5 |>
  ggplot(aes( x = CO, y = Int_Ret)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm) +
       labs( title = "S&P 500 CO and 5 Minute SPY Return",
       x = "Continuing Overconfidence", y = "Return",
       subtitle = "5 Minute CO from 9:00 to 9:30 and Returns from 9:30 to 9:35")

combmin10 |>
  ggplot(aes( x = CO, y = Int_Ret)) +
   geom_point(size = 3) + 
     geom_smooth(method = lm) +
       labs( title = "Russell 2000 CO and 10 Minute Return",
       x = "Continuing Overconfidence", y = "Return",
       subtitle = "CO from 7:00 to 9:30 and Returns from 9:30 to 9:40")

###Regression Models
### Work on regression with days as factor

morning = lm(Int_Ret ~ CO, data = comb)
summary(morning)
 tbl1 = tbl_regression(morning)

full_day = lm(Int_Ret ~ CO, data = comb1)
 tbl2 = tbl_regression(full_day)

modify_caption(tbl_stack(list(tbl1,tbl2),group_header = c("Midday","Full Day")), "Russell 2000")

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
chart.CumReturns(returns, main = "Long/Short Strategy - Russell 2000")

par(mfrow = c(3,1))

hist(comb1$Int_Ret, breaks = 30, main = "Full Day Return Russell 2000", ylab = "", xlab = "Return", sub = "2008 - 2021")
hist(comb$Int_Ret, breaks = 40, main = "Midday Return Russell 2000", ylab = "", xlab = "Return", sub = "2008 - 2021")
hist(comb$CO, breaks = 30, main = "Russell 2000 CO", ylab = "", xlab = "CO", sub = "2008 - 2021")


