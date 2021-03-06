library(tidyverse)

ps5_1 <- data.frame(
  decade=c("1960s", "1970s", "1980s", "1990s"),
  r=c(0.024, 0.026, 0.029, 0.035),
  G_lower = 25,
  G_upper = 33
)
ps5_1 <- ps5_1 %>% mutate(lower_bound=exp(r*G_lower), upper_bound=exp(r*G_upper))

ps5_4 <- tibble(
  x=c(15, 20, 25, 30, 35, 40, 45),
  nBx=c(7150, 21910, 25305, 14825, 9935, 3625, 1420),
  nDx=c(578, 502, 1034, 659, 638, 441, 638),
  nKx=c(48564, 67096, 80746, 53670, 51975, 32022, 32307),
  nLx=c(337775, 321570, 306003, 287031, 270049, 253276, 232925)
)





# Problem 5a
# This script is modified from what Robert sent out

# This script reads in the Swedish cohort life tables for spain and estimates 
# Gompertz mortality parameters for each decadal birth cohort from 1750 to 1910

# first, read in the cohort life tables for spain from the HMD.
# Note that these life tables use "." for NA
# I had downloaded the life tables from HMD prior to this. 
spain.lt = read.table("spain_deathrates_1x10.txt",skip=1, header=T,na.string=".")

# check to see if the data got read in properly
head(spain.lt)

# Okay, now we're going to set up a big empty matrix to hold the mortality rates
# and one smaller empty matrix to hold the results from our regressions

# First, set up a vector of ages from 40 to 85, by 5
age=seq(40,85,by=5)

# Create names for the column headings
# Starting at 1830, ignoring the 1827-1829 cohort
mx.names=paste("mx",seq(1830,1910,by=10),sep="")
mx.names

# Create a data frame to hold all the mx values
Mx=data.frame(cbind(age,matrix(0,nr=10,nc=9)))
Mx
names(Mx)=c("age",mx.names)
Mx

# Now extract the mx values from the Swedish life table file and stick them into Mx
for(i in 1:10) Mx[i,2:10] = spain.lt$Total[spain.lt$Age==age[i]]
Mx


# Gompertz model for ages 40-85 is mu(x)=alpha * exp(beta*x)
# Or log(mu(x)) = log(alpha) + beta*x
# so for each column, regress log(mu(x)) on x. Intercept will be log(alpha), slope is beta
# Let's create the smaller empty matrix to hold the parameter estimates from the regressions

year=seq(1830,1910,by=10)
Gompertz.params = data.frame(year,matrix(0,nrow=length(year),ncol=3))
names(Gompertz.params)= c("years","log.alpha","beta","alpha")

# Now get log(alpha) and beta and stick them into Gompertz.params
# There are 17 decades so we'll do this 17 times.
# lm does the regression, coef grabs the coefficients
for(i in 1:17) Gompertz.params[i,2:3] = coef(lm(log(Mx[,i+1])~Mx[,1]))

Gompertz.params[,"alpha"]=exp(Gompertz.params[,"log.alpha"])
Gompertz.params


# OK, now plot the parameters alpha and beta
with(Gompertz.params,plot(alpha,beta))

# plot log(alpha) and beta, and label with the year
with(Gompertz.params,plot(log.alpha,beta,type="n"))
with(Gompertz.params,text(log.alpha,beta,year))
Gompertz_cohort <- Gompertz.params
cohort <- ggplot() + geom_text(data=Gompertz_cohort, mapping = aes(x=log.alpha,y=beta, label=year)) 


# Problem 5b
spain.lt = read.table("spain_perioddata_1x10.txt",skip=1, header=T,na.string=".")
head(spain.lt)
age=seq(40,85,by=5)
mx.names=paste("mx",seq(1910,1990,by=10),sep="")
mx.names
Mx=data.frame(cbind(age,matrix(0,nr=10,nc=9)))
Mx
names(Mx)=c("age",mx.names)
Mx
for(i in 1:10) Mx[i,2:10] = spain.lt$mx[spain.lt$Age==age[i]]
Mx
year=seq(1910,1990,by=10)
Gompertz.params = data.frame(year,matrix(0,nrow=length(year),ncol=3))
names(Gompertz.params)= c("years","log.alpha","beta","alpha")
for(i in 1:17) Gompertz.params[i,2:3] = coef(lm(log(Mx[,i+1])~Mx[,1]))
Gompertz.params[,"alpha"]=exp(Gompertz.params[,"log.alpha"])
Gompertz.params
with(Gompertz.params,plot(alpha,beta))
with(Gompertz.params,plot(log.alpha,beta,type="n"))
with(Gompertz.params,text(log.alpha,beta,year))
Gompertz_period <- Gompertz.params

period <- ggplot() + geom_text(data=Gompertz_period, mapping = aes(x=log.alpha,y=beta, label=year), color="red") 

library(gridExtra)
grid.arrange(cohort, period)
