packages <- c("tidyverse", "forecast", "tseries", "svars", "vars", "urca", "tsDyn", "gogarch", "plm", "punitroots")
lapply(packages, require, character.only = TRUE)


setwd("C:/Users/jpbra/Desktop/uni/SEM3/advanced econometrics")

# load the "joined" data set
data <- read_csv("project/data/data.join.csv") 

# the dates seem to be read in as d_m_y which could cause problems for plotting

data$Date <- dmy(data$Date)

# for convenience let's extract columns from the data frame
# also make a data frame for each "triple" of variables

LXUSJP <- ts(log(data$XUSJP))
LXUKJP <- ts(log(data$XUKJP))
LXUKUS <- ts(log(data$XUKUS))
LUSCPI <- ts(log(data$USACPI))
LUKCPI <- ts(log(data$GBRCPI))
LJPCPI <- ts(log(data$JPNCPI))

df1 <- data.frame(x = LXUSJP, y = LUSCPI, z = LJPCPI)
df2 <- data.frame(x = LXUKJP, y = LUKCPI, z = LJPCPI)
df3 <- data.frame(x = LXUKUS, y = LUKCPI, z = LUSCPI)

# do some basic plots etc

ggplot() +
  geom_line(data = data, aes(x = Date, y = log(XUKJP) - log(XUKJP)[1], 
                             colour = "JP-UK Exchange Rate")) +
  geom_line(data = data, aes(x = Date, y = log(JPNCPI) - log(JPNCPI)[1], 
                             colour = "JP CPI")) + 
  geom_line(data = data, aes(x = Date, y = log(GBRCPI) - log(GBRCPI)[1],   
                             colour = "UK CPI")) + ylab("Change since 1970")

ggplot() +
  geom_line(data = data, aes(x = Date, y = log(XUSJP) - log(XUSJP)[1], 
                             colour = "JP-US Exchange Rate")) +
  geom_line(data = data, aes(x = Date, y = log(JPNCPI) - log(JPNCPI)[1], 
                             colour = "JP CPI")) + 
  geom_line(data = data, aes(x = Date, y = log(USACPI) - log(USACPI)[1],   
                             colour = "US CPI")) + ylab("Change since 1970")

ggplot() +
  geom_line(data = data, aes(x = Date, y = log(XUKUS) - log(XUKUS)[1], 
                             colour = "UK-US Exchange Rate")) +
  geom_line(data = data, aes(x = Date, y = log(GBRCPI) - log(GBRCPI)[1], 
                             colour = "UK CPI")) + 
  geom_line(data = data, aes(x = Date, y = log(USACPI) - log(USACPI)[1],   
                             colour = "US CPI")) + ylab("Change since 1970")


#data$LXUKJP_ma <- ma(log(data$XUKJP), order=7)
#data$LXUKJP_ma30 <- ma(log(data$XUKJP), order=30)

# see, below, I have to ADD an NA into the diff() object to make it the same length

# ggplot() + 
#  geom_line(data = data, aes(x = Date, y = c(NA, diff(log(XUSJP), 1)), colour = "First difference")) + 
#  geom_line(data = data, aes(x = Date, y = log(XUSJP) - 5.6, colour = "Exchange rate"))

# and so on




# a battery of tests:
# first let's plot ACF/PACF for each series

Acf(LXUSJP, main="")
Pacf(LXUSJP, main="")

Acf(LXUKJP, main="")
Pacf(LXUKJP, main="")

Acf(LXUKUS, main="")
Pacf(LXUKUS, main="")

Acf(LUSCPI, main="")
Pacf(LUSCPI, main="")

Acf(LUKCPI, main="")
Pacf(LUKCPI, main="")

Acf(LJPCPI, main="")
Pacf(LJPCPI, main="")

# ACF's are all decaying slowly, so look non-stationary

# now do ADF tests on each series

summary(ur.df(LXUSJP, type="none", lags=50))
summary(ur.df(LXUSJP, type="none", selectlags ="BIC"))
summary(ur.df(LXUSJP, type="none", lags=trunc(length(LXUSJP -1)^(1/3))))
adf.test(LXUSJP)
adfTest(LXUSJP, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")

summary(ur.df(LXUKJP, type="none", lags=50))
summary(ur.df(LXUKJP, type="none", selectlags ="BIC"))
summary(ur.df(LXUKJP, type="none", lags=trunc(length(LXUKJP -1)^(1/3))))
adf.test(LXUKJP)
adfTest(LXUKJP, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")

summary(ur.df(LXUKUS, type="drift", lags=50))
summary(ur.df(LXUKUS, type="drift", selectlags ="BIC"))
summary(ur.df(LXUKUS, type="drift", lags=trunc(length(LXUKUS -1)^(1/3))))
adf.test(LXUKUS)
adfTest(LXUKUS, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")

summary(ur.df(LUSCPI, type="drift", lags=50))
summary(ur.df(LUSCPI, type="drift", selectlags ="BIC"))
summary(ur.df(LUSCPI, type="drift", lags=trunc(length(LUSCPI -1)^(1/3))))
adf.test(LUSCPI)
adfTest(LUSCPI, lags=trunc(length(LUSCPI -1)^(1/3)), type="c")

summary(ur.df(LUKCPI, type="drift", lags=50))
summary(ur.df(LUKCPI, type="drift", selectlags ="BIC"))
summary(ur.df(LUKCPI, type="drift", lags=trunc(length(LUKCPI -1)^(1/3))))
adf.test(LUKCPI)
adfTest(LUKCPI, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")

summary(ur.df(LJPCPI, type="drift", lags=50))
summary(ur.df(LJPCPI, type="drift", selectlags ="BIC"))
summary(ur.df(LJPCPI, type="drift", lags=trunc(length(LJPCPI -1)^(1/3))))
adf.test(LJPCPI)
adfTest(LJPCPI, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")



# PP test:

PP.test(LZJPUS, type="Z_alpha")
PP.test(LXUSJP, type="Z(t_alpha)")
pp.test(LXUSJP) # type is Z_alpha



# KPSS tests on each series:
# generally, a longer lag seems to reduce value of the test statistic
# it makes you less likely to reject the null

summary(ur.kpss(LXUSJP, type="mu", lags ="long")) # this does 18 lags
summary(ur.kpss(LXUSJP, type="mu", lags ="short")) # this does 6 lags
summary(ur.kpss(LXUSJP, type="mu", use.lag=6)) #trunc(length(LXUSJP -1)^(1/3))
kpss.test(LXUSJP)

summary(ur.kpss(LXUKJP, type="tau", lags="long"))
summary(ur.kpss(LXUKJP, type="tau", lags="short"))
summary(ur.kpss(LXUKJP, type="mu", use.lag=6)) #trunc(length(LXUKJP -1)^(1/3))
summary(ur.kpss(LXUKJP, type="mu", use.lag=2)) #trunc((4*length(LXUKJP)/100)^(2/9))
kpss.test(LXUKJP)

summary(ur.kpss(LXUKUS, type="tau", lags="long"))
summary(ur.kpss(LXUKUS, type="tau", lags="short"))
summary(ur.kpss(LXUKUS, type="mu", use.lag=6)) #trunc(length(LXUKUS -1)^(1/3))
kpss.test(LXUKUS)

summary(ur.kpss(LUSCPI, type="tau", lags="long"))
summary(ur.kpss(LUSCPI, type="tau", lags="short"))
summary(ur.kpss(LUSCPI, type="mu", use.lag=6)) #trunc(length(LUSCPI_d1 -1)^(1/3))
kpss.test(LUSCPI)

summary(ur.kpss(LUKCPI, type="mu", lags="long"))
summary(ur.kpss(LUKCPI, type="mu", lags="short"))
summary(ur.kpss(LUKCPI, type="mu", use.lag=6)) #trunc(length(LUKCPI_d1 -1)^(1/3))
kpss.test(LUKCPI)

summary(ur.kpss(LJPCPI, type="tau", lags="long"))
summary(ur.kpss(LJPCPI, type="tau", lags="short"))
summary(ur.kpss(LJPCPI, type="mu", use.lag=6)) #trunc(length(LJPCPI_d1 -1)^(1/3))
kpss.test(LJPCPI)

# all the series seem to be non-stationary according to the KPSS test
# and four out of six series are non-stationary according to the ADF test


# let's now check that the first difference of each series is stationary:

LXUSJP_d1 <- diff(LXUSJP, differences=1)
LXUKJP_d1 <- diff(LXUKJP, differences=1)
LXUKUS_d1 <- diff(LXUKUS, differences=1)
LUSCPI_d1 <- diff(LUSCPI, differences=1)
LUKCPI_d1 <- diff(LUKCPI, differences=1)
LJPCPI_d1 <- diff(LJPCPI, differences=1)

# plot these

plot(LXUSJP_d1)
plot(LXUKJP_d1)
plot(LXUKUS_d1)
plot(LUSCPI_d1)
plot(LUKCPI_d1)
plot(LJPCPI_d1)

# they certainly look stationary, but let's test:

summary(ur.df(LXUSJP_d1, type="none", lags=50))
summary(ur.df(LXUSJP_d1, type="none", selectlags="BIC"))
summary(ur.df(LXUSJP_d1, type="none", lags=trunc(length(LXUSJP_d1 -1)^(1/3))))
adf.test(LXUSJP_d1)
adfTest(LXUSJP_d1, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")

summary(ur.df(LXUKJP_d1, type="none", lags=50))
summary(ur.df(LXUKJP_d1, type="none", selectlags="BIC"))
summary(ur.df(LXUKJP_d1, type="none", lags=trunc(length(LXUKJP_d1 -1)^(1/3))))
adf.test(LXUKJP_d1)
adfTest(LXUKJP_d1, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")

summary(ur.df(LXUKUS_d1, type="none", lags=50))
summary(ur.df(LXUKUS_d1, type="none", selectlags="BIC"))
summary(ur.df(LXUKUS_d1, type="none", lags=trunc(length(LXUKUS_d1 -1)^(1/3))))
adf.test(LXUKUS_d1)
adfTest(LXUKUS_d1, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")

summary(ur.df(LUSCPI_d1, type="trend", lags=50))
summary(ur.df(LUSCPI_d1, type="trend", selectlags="BIC"))
summary(ur.df(LUSCPI_d1, type="trend", lags=trunc(length(LUSCPI_d1 -1)^(1/3))))
adf.test(LUSCPI_d1)
adfTest(LUSCPI_d1, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")

summary(ur.df(LUKCPI_d1, type="trend", lags=50))
summary(ur.df(LUKCPI_d1, type="trend", selectlags="BIC"))
summary(ur.df(LUKCPI_d1, type="trend", lags=trunc(length(LUKCPI_d1 -1)^(1/3))))
adf.test(LUKCPI_d1)
adfTest(LUKCPI_d1, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")

summary(ur.df(LJPCPI_d1, type="none", lags=50))
summary(ur.df(LJPCPI_d1, type="none", selectlags="BIC"))
summary(ur.df(LJPCPI_d1, type="none", lags=trunc(length(LJPCPI_d1 -1)^(1/3))))
adf.test(LJPCPI_d1)
adfTest(LJPCPI_d1, lags=trunc(length(LXUKUS -1)^(1/3)), type="c")


# PP


summary(ur.kpss(LXUSJP_d1, type="tau", lags ="long"))
summary(ur.kpss(LXUSJP_d1, type="tau", lags ="short"))
summary(ur.kpss(LXUSJP_d1, type="mu", use.lag=6)) #trunc(length(LXUSJP_d1 -1)^(1/3))
kpss.test(LXUSJP_d1)

summary(ur.kpss(LXUKJP_d1, type="tau", lags="long"))
summary(ur.kpss(LXUKJP_d1, type="tau", lags="short"))
summary(ur.kpss(LXUKJP_d1, type="mu", use.lag=6)) #trunc(length(LXUKJP_d1 -1)^(1/3))
kpss.test(LXUKJP_d1)

summary(ur.kpss(LXUKUS_d1, type="tau", lags="long"))
summary(ur.kpss(LXUKUS_d1, type="tau", lags="short"))
summary(ur.kpss(LXUKUS_d1, type="mu", use.lag=6)) #trunc(length(LXUKUS_d1 -1)^(1/3))
kpss.test(LXUKUS_d1)

summary(ur.kpss(LUSCPI_d1, type="tau", lags="long"))
summary(ur.kpss(LUSCPI_d1, type="tau", lags="short"))
summary(ur.kpss(LUSCPI_d1, type="mu", use.lag=6)) #trunc(length(LUSCPI_d1 -1)^(1/3))
kpss.test(LUSCPI_d1)

summary(ur.kpss(LUKCPI_d1, type="tau", lags="long"))
summary(ur.kpss(LUSCPI_d1, type="tau", lags="short"))
summary(ur.kpss(LUKCPI_d1, type="mu", use.lag=6)) #trunc(length(LUKCPI_d1 -1)^(1/3))
kpss.test(LUKCPI_d1)

summary(ur.kpss(LJPCPI_d1, type="tau", lags="long"))
summary(ur.kpss(LJPCPI_d1, type="tau", lags="short"))
summary(ur.kpss(LJPCPI_d1, type="mu", use.lag=6)) #trunc(length(LJPCPI_d1 -1)^(1/3))
kpss.test(LJPCPI_d1)


# all of the first differences are stationary at conventional significance levels acc. to ADF
# KPSS says first three are stationary and second three aren't

# conclusion: there's evidence that the series are all I(1), let's put it that way


# something to note about lags: ur.df and ur.kpss use 1 by default i think..
# well ur.df does anyway. ur.kpss allows for either custom lag lengths or
# "short", "long", which are based on formulas (... is that Newey-West? or something else?). kpss.test is based on the same formulas.



#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################



# let's now do the Engle-Granger procedure

lm_fit1 <- lm(z ~ x + y, data=df1)
e1 <- ts(lm_fit1$residuals)

lm_fit2 <- lm(z ~ x + y, data=df2)
e2 <- ts(lm_fit2$residuals)

lm_fit3 <- lm(z ~ x + y, data=df3)
e3 <- ts(lm_fit3$residuals)

plot(e1)
plot(e2)
plot(e3)

summary(ur.df(e1, type="trend", lags=50))
summary(ur.df(e1, type="trend", selectlags="BIC"))
summary(ur.df(e1, type="trend", lags=trunc(length(e1 -1)^(1/3))))
adf.test(e1)
adfTest(e1, lags=trunc(length(e1 -1)^(1/3)), type="c")

summary(ur.df(e2, type="none", lags=50))
summary(ur.df(e2, type="none", selectlags="BIC"))
summary(ur.df(e2, type="none", lags=trunc(length(e2 -1)^(1/3))))
adf.test(e2)
adfTest(e2, lags=trunc(length(e2 -1)^(1/3)), type="c")

summary(ur.df(e3, type="none", lags=50))
summary(ur.df(e3, type="none", selectlags="BIC"))
summary(ur.df(e3, type="none", lags=trunc(length(e3 -1)^(1/3))))
adf.test(e3)
adfTest(e3, lags=trunc(length(e3 -1)^(1/3)), type="c")


# PP

summary(ur.kpss(e1, type="tau", lags ="long"))
summary(ur.kpss(e1, type="tau", lags ="short"))
summary(ur.kpss(e1, type="mu", use.lag=6)) #trunc(length(e1 -1)^(1/3))
kpss.test(e1)

summary(ur.kpss(e2, type="tau", lags ="long"))
summary(ur.kpss(e2, type="tau", lags ="short"))
summary(ur.kpss(e2, type="mu", use.lag=6)) #trunc(length(e2 -1)^(1/3))
kpss.test(e2)

summary(ur.kpss(e3, type="tau", lags ="long"))
summary(ur.kpss(e3, type="tau", lags ="short"))
summary(ur.kpss(e3, type="mu", use.lag=6)) #trunc(length(e3 -1)^(1/3))
kpss.test(e3)


# first two: reject H_0 --> residuals are stationary
# third one seems to indicate that the residuals are non-stationary
# some conflicting results again

# note we don't need to test differences because we are only interested in whether 
# the residuals are stationary or not. if they're not stationary we don't care whether
# they're I(1) or I(2) etc.


# if we wanted we could do the WHOLE set of tests two more times
# using the residuals from the two other sets of regressions 

# regression set 2:

lm_fit_12 <- lm(x ~ y + z, data=df1)
e_12 <- ts(lm_fit_12$residuals)

lm_fit_22 <- lm(x ~ y + z, data=df2)
e_22 <- ts(lm_fit_22$residuals)

lm_fit_32 <- lm(x ~ y + z, data=df3)
e_32 <- ts(lm_fit_32$residuals)


# tests:

summary(ur.df(e_12, type="trend", lags=50))
summary(ur.df(e_12, type="trend", selectlags="BIC"))
summary(ur.df(e_12, type="trend", lags=trunc(length(e_12 -1)^(1/3))))
adf.test(e_12)
adfTest(e_12, lags=trunc(length(e_12 -1)^(1/3)), type="c")

summary(ur.df(e_22, type="none", lags=50))
summary(ur.df(e_22, type="none", selectlags="BIC"))
summary(ur.df(e_22, type="none", lags=trunc(length(e_22 -1)^(1/3))))
adf.test(e_22)
adfTest(e_22, lags=trunc(length(e_22 -1)^(1/3)), type="c")

summary(ur.df(e_32, type="none", lags=50))
summary(ur.df(e_32, type="none", selectlags="BIC"))
summary(ur.df(e_32, type="none", lags=trunc(length(e_32 -1)^(1/3))))
adf.test(e_32)
adfTest(e_32, lags=trunc(length(e_32 -1)^(1/3)), type="c")


# PP

summary(ur.kpss(e_12, type="tau", lags ="long"))
summary(ur.kpss(e_12, type="tau", lags ="short"))
summary(ur.kpss(e_12, type="mu", use.lag=6)) #trunc(length(e_12 -1)^(1/3))
kpss.test(e_12)

summary(ur.kpss(e_22, type="tau", lags ="long"))
summary(ur.kpss(e_22, type="tau", lags ="short"))
summary(ur.kpss(e_22, type="mu", use.lag=6)) #trunc(length(e_22 -1)^(1/3))
kpss.test(e_22)

summary(ur.kpss(e_32, type="tau", lags ="long"))
summary(ur.kpss(e_32, type="tau", lags ="short"))
summary(ur.kpss(e_32, type="mu", use.lag=6)) #trunc(length(e_32 -1)^(1/3))
kpss.test(e_32)


# regression set 3:

lm_fit_13 <- lm(y ~ x + z, data=df1)
e_13 <- ts(lm_fit_13$residuals)

lm_fit_23 <- lm(y ~ x + z, data=df2)
e_23 <- ts(lm_fit_23$residuals)

lm_fit_33 <- lm(y ~ x + z, data=df3)
e_33 <- ts(lm_fit_33$residuals)


# tests


# we can also test the STRONG form  of PPP:

u1 <- LJPCPI - LXUSJP - LUSCPI # or u1 <- df1$z - df1$x - df1$y
u2 <- LJPCPI - LXUKJP - LUKCPI # or u2 <- df2$z - df2$x - df2$y
u3 <- LUSCPI - LXUKUS - LUKCPI # or u3 <- df3$z - df3$x - df3$y

plot(u1)
plot(u2)
plot(u3)

summary(ur.df(u1, type="none", lags=50))
summary(ur.df(u1, type="none", selectlags="BIC"))
summary(ur.df(u1, type="none", lags=trunc(length(u1 -1)^(1/3))))
adf.test(u1)
adfTest(u1, lags=trunc(length(u1 -1)^(1/3)), type="c")

summary(ur.df(u2, type="none", lags=50))
summary(ur.df(u2, type="none", selectlags="BIC"))
summary(ur.df(u2, type="none", lags=trunc(length(u2 -1)^(1/3))))
adf.test(u2)
adfTest(u2, lags=trunc(length(u2 -1)^(1/3)), type="c")

summary(ur.df(u3, type="none", lags=50))
summary(ur.df(u3, type="none", selectlags="BIC"))
summary(ur.df(u3, type="none", lags=trunc(length(u3 -1)^(1/3))))
adf.test(u3)
adfTest(u3, lags=trunc(length(u3 -1)^(1/3)), type="c")


# PP

summary(ur.kpss(u1, type="tau", lags ="long"))
summary(ur.kpss(u1, type="tau", lags ="short"))
summary(ur.kpss(u1, type="mu", use.lag=6)) #trunc(length(u1 -1)^(1/3))
kpss.test(u1)

summary(ur.kpss(u2, type="tau", lags ="long"))
summary(ur.kpss(u2, type="tau", lags ="short"))
summary(ur.kpss(u2, type="mu", use.lag=6)) #trunc(length(u2 -1)^(1/3))
kpss.test(u2)

summary(ur.kpss(u3, type="tau", lags ="long"))
summary(ur.kpss(u3, type="tau", lags ="short"))
summary(ur.kpss(u3, type="mu", use.lag=6)) #trunc(length(u3 -1)^(1/3))
kpss.test(u3)


# conclusion?

# another way of testing strong PPP is to test the multiple hypothesis
#   H_0: beta_1 = 1, beta_2 = -1
# for the regressions earlier. cf. Hamilton 19.3





# now, let's do the Johansen FIML approach
# first fit a VAR in levels for each series

var_fit1 <- VAR(df1, type="both", lag.max=8, ic="SC")
var_fit2 <- VAR(df2, type="both", lag.max=8, ic="SC")
var_fit3 <- VAR(df3, type="both", lag.max=8, ic="SC")

# optimal lag length is 2 for all three models
# trend + intercept are significant in all three
# (note, a different method for fitting VARs is lineVar in tsDyn)

# then do the lambda-trace test

johansen_test1 <- ca.jo(df1, type="trace", ecdet="trend", K=2, spec="longrun")
johansen_test2 <- ca.jo(df2, type="trace", ecdet="trend", K=2, spec="longrun")
johansen_test3 <- ca.jo(df3, type="trace", ecdet="trend", K=2, spec="longrun")


# evidence of cointegration... at least one maybe 2 CI vectors

# [we can fit a VECM and do the GC stuff here too]
# [pfaff's packages offer SVECM only, i think you need tsDyn for VECM]



# finally panel data:
# create a time series using the residuals from the previous regressions

e <- ts(data.frame(e1 = e1, e2 = e2, e3 = e3))

# do IPS and LL tests on the residual panel data

IPS <- purtest(e, test = "ips", exo = "intercept", lags="SIC", pmax=5)
summary(IPS)

LLC <- purtest(e, test = "levinlin", exo="trend", lags="SIC", pmax=5)
summary(LLC)

MW <- purtest(e, test = "madwu", exo="intercept", lags="SIC", pmax=5)
summary(MW)

# at any rate, we reject the null hypothesis that all the series have unit roots
# that's good to know, i guess!

# final point: we could do panel data tests on the deviations u_t as a way of testing
# strong PPP. also could do the regressions in different orders etc.

u <- ts(data.frame(u1 = u1, u2 = u2, u3 = u3))


LLC <- purtest(u, test = "levinlin", exo="trend", lags="SIC", pmax=5)
summary(LLC)

IPS <- purtest(u, test = "ips", exo = "intercept", lags="SIC", pmax=5)
summary(IPS)

MW <- purtest(u, test = "madwu", exo="intercept", lags="SIC", pmax=5)
summary(MW)

# LLC says don't reject H_0
