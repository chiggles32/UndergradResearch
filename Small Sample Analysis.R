library(tidyverse)
library(jsonlite)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(tibbletime)
library(purrr)

data = read.table(file.choose(),sep = ",", header = F)

colnames(data) = c("D/T", "Open", "High", "Low", "Close", "Volume")

data = data |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F)|>
    filter(hms(Time) >= hms("00:00:00") & hms(Time) < hms("09:30:00"))



##etf

etf = read.table(file.choose(),sep = ",", header = F)

colnames(etf) = c("D/T", "Open", "High", "Low", "Close", "Volume")

etf = etf |>
  separate("D/T", c("Date","Time"), sep = " ", remove = F) |>
    filter(hms(Time) >= hms("09:30:00") & hms(Time) < hms("12:00:00"))

rolled_sd = rollify(sd,5)

x = unique(data[,2])
y = unique(etf[,2])
z = intersect(x,y)
q = length(z)

sampledaynum = sample(q,1000,replace = F)
sampledays = z[sampledaynum]

sdata = data |>
  filter(Date %in% sampledays) |>
    group_by(Date) |>
        mutate(Return = Delt(Close), CO = wilderSum((sign(Return)*Volume*Close),n=5)/runMean(Volume*Close,n=5),
          rsd = rolled_sd(Return), MODCO = CO/rsd) |>
            group_split()

##cleaning sdata?

for (i in 1:1000){
  sdata[[i]]$MODCO[is.na(sdata[[i]]$MODCO) | sdata[[i]]$MODCO =="Inf"] = 0
  }


int_ret = etf |>
  filter(Date %in% sampledays)|>
    filter(Time %in% c("09:30:00", "11:55:00")) |>
      group_by(Date) |>
        mutate(Int_Ret = Delt(Close)) |>
          filter(Time == "11:55:00")|>
            select(Int_Ret) |>
              as.data.frame()
          

##Regression coef

get_coefficients = function(data){
  reg_mod = c()
  for (i in 1:1000){
  	reg_mod[i] = coef(lm(data[[i]]$MODCO ~ seq_along(data[[i]]$MODCO), na.action = "na.exclude"))[2]
  	}
  coeff <<- reg_mod
  }

get_coefficients(sdata)

test = cbind(int_ret,coeff)
plot(test[1:100,3],test[1:100,2])
hist(coeff, breaks = 30)

##plotting
setf = etf |>
  filter(Date %in% sampledays)|>
    group_by(Date) |>
      group_split()


par(mfrow = c(4,3))

for (i in 1:4){
  plot(sdata[[i]]$Close, typ = 'l')
  plot(sdata[[i]]$MODCO, typ = 'l')
  plot(setf[[i]]$Close, typ = 'l')
  }



column_name <- "MODCO"


coefficients_list <- lapply(sdata, function(df) {
 
  model <- lm(df[[column_name]] ~ seq_along(df[[column_name]])

return(coef(model))
})

