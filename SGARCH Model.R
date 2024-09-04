SGARCH Model

s =
ugarchspec(mean.model=list(armaOrder = c(0,0)),
  variance.model = list(model = 'sGARCH'),distribution.model='norm')

m = ugarchfit(return, spec = s)

plot(m, which= 'all')

f = ugarchforecast(fitORspec = m, n.ahead = 20)

##variants##

v = sqrt(252)*sigma(m)
w = .05/v
plot(merge(v,w),multi.panel=T)


#GARCH Student T Distribution

s =
ugarchspec(mean.model=list(armaOrder = c(0,0)),
  variance.model = list(model = 'sGARCH'),distribution.model='sstd')

m = ugarchfit(return, spec = s)

plot(m, which= 'all')
  
##gjrGarch

s =
ugarchspec(mean.model=list(armaOrder = c(0,0)),
  variance.model = list(model = 'gjrGARCH'),distribution.model='sstd')

m = ugarchfit(return, spec = s)

plot(m, which= 'all')

#AR(1) JR-GARCH   (adds another coefficient)

s =
ugarchspec(mean.model=list(armaOrder = c(1,0)),
  variance.model = list(model = 'gjrGARCH'),distribution.model='sstd')

m = ugarchfit(return, spec = s)

plot(m, which= 'all')


## GJR-GARCH in mean

s =
ugarchspec(mean.model=list(armaOrder = c(0,0), archm = T, archpow = 2),
  variance.model = list(model = 'gjrGARCH'),distribution.model='sstd')

m = ugarchfit(return, spec = s)

plot(m, which= 'all')


#simulating prices with garch


sfinal = s
setfixed(sfinal) = as.list(coef(m))

f2008 = ugarchforecast(return['/2008-12'], fitORspec = sfinal, n.ahead = 252)

f2022 = ugarchforecast(return['2021'], fitORspec = sfinal, n.ahead = 252)

par(mfrow = c(2,1))
plot(sigma(f2008))
plot(sigma(f2022))

sim = ugarchpath(spec = sfinal, m.sim = 5, n.sim = 25)
plot.zoo(fitted(sim))
plot.zoo(sigma(sim))

p = 38.965*apply(fitted(sim),2,'cumsum')+38.965
matplot(p, typ = 'l')




