library(tidyverse)

# Reads in a life table and returns the Gompertz parameters for each age interval 
# df: a life table with years, ages, and nmx
# start: start year
# end: end year
# interval: year intervals 
# age_interval


Gompertz <- function(df, start, end, year_interval, age_interval = 1, senescense_begin = 40, senescence_end = 85) {
  # DEFINE START YEAR, END YEAR, AND INTERVAL LENGTH
  START_YEAR <- start
  END_YEAR <- end
  INTERVAL <- year_interval
  NUM_COHORTS <- (END_YEAR - START_YEAR)/INTERVAL 
 
  # If age intervals are not 1, break them into ranges
  # Make sure the limits are what you specified!
  life_table <- df %>%
    separate(Year, into = c("start_year", "end_year"), sep = "-", remove = TRUE)  %>%
    separate(Age, into = c("start_age", "end_age"), sep = "-", remove = TRUE) %>%
    filter(start_year >= START_YEAR)
  life_table
  
  
  # check to see if the data got read in properly
  # head(life_table)
  
  # First, set up a vector of ages from 40 to 85, by 5
  age=seq(senescense_begin, senescence_end, by=age_interval)
  # age
  
  # Create names for the column headings
  mx.names=paste("mx",seq(START_YEAR,END_YEAR,by=INTERVAL),sep="")
  # mx.names
  
  # Create a data frame to hold all the mx values
  Mx=data.frame(cbind(age,matrix(0,nr=length(age),nc=NUM_COHORTS+ 1)))
  # Mx
  names(Mx)=c("age",mx.names)
  # Mx
  
  # Now extract the mx values from the Swedish life table file and stick them into Mx
  # Confusingly, there are two sources of the mx data with different names; this if/else will determine what the name of mortality rate column is. 
  varname <- ""
  if("Total" %in% colnames(life_table)){
    varname <- "Total"
  } else {
    varname <- "mx"
  }
  # varname
  
  for(i in age) {
    temp <- life_table %>% subset(start_age==i) %>% select(varname)
    # print(temp[,1])
    Mx[age==i,0:NUM_COHORTS+2] <- head(temp[,1], n=(NUM_COHORTS+2))
  }
  # Mx
  # Gompertz model for ages 40-85 is mu(x)=alpha * exp(beta*x)
  # Or log(mu(x)) = log(alpha) + beta*x
  # so for each column, regress log(mu(x)) on x. Intercept will be log(alpha), slope is beta
  # Let's create the smaller empty matrix to hold the parameter estimates from the regressions
  
  year=seq(START_YEAR,END_YEAR,by=INTERVAL)
  Gompertz.params = data.frame(year,matrix(0,nrow=length(year),ncol=3))
  names(Gompertz.params)= c("years","log.alpha","beta","alpha")
  # row.names(Gompertz.params) <- Gompertz.params$years
  # Gompertz.params
  
  # Now get log(alpha) and beta and stick them into Gompertz.params
  # There are 17 decades so we'll do this 17 times.
  # lm does the regression, coef grabs the coefficients
  for(i in 1:nrow(Gompertz.params)) {
    Gompertz.params[i,2:3] = coef(lm(log(Mx[,i+1])~Mx[,1]))
  }
  
  Gompertz.params[,"alpha"]=exp(Gompertz.params[,"log.alpha"])
  # Gompertz.params
  
  # OK, now plot the parameters alpha and beta
  # with(Gompertz.params,plot(alpha,beta))
  
  # plot log(alpha) and beta, and label with the year
  # with(Gompertz.params,plot(log.alpha,beta,type="n"))
  # with(Gompertz.params,text(log.alpha,beta,year))
  
  return(params = Gompertz.params)
}


# USAGE

# Gompertz(df = lifetable, start = start year, end = end year, interval = table interval)
# Returns a table of the Gompertz parameters for each age specified by start, end, and interval parameters

# spain = read.table("spain_deathrates_1x10.txt",skip=1, header=T,na.string=".")
# spain_gompertz <- Gompertz(df = spain , start = 1830, end = 1910, year_interval = 10)
# spain_gompertz
# spain_gompertz %>% ggplot() + geom_text(mapping = aes(x=log.alpha,y=beta, label=years), color="red") 
 
spain_cohort_5x10 = read.table("spain_cohort_5x10.txt",skip=1, header=T,na.string=".")
spain_gompertz_5x10 <- Gompertz(df = spain_cohort_5x10, start = 1830, end = 1910, year_interval = 10, age_interval = 5)
spain_gompertz_5x10
spain_gompertz_5x10 %>% ggplot() + geom_text(mapping = aes(x=log.alpha,y=beta, label=years), color="red") 
 
spain_cohort_1x1 = read.table("spain_cohort_1x1.txt",skip=1, header=T,na.string=".")
spain_gompertz_1x1 <- Gompertz(df = spain_cohort_1x1, start = 1830, end = 1910, year_interval = 1, age_interval = 1)
spain_gompertz_1x1
spain_gompertz_1x1 %>% ggplot() + geom_text(mapping = aes(x=log.alpha,y=beta, label=years), color="red") 

# spain_2 = read.table("spain_fltper_1x10.txt",skip=1, header=T,na.string=".")
# spain_2_gompertz <- Gompertz(df = spain_2 , start = 1830, end = 1910, interval = 10)
# test2 <- spain_2_gompertz %>% ggplot() + geom_text(mapping = aes(x=log.alpha,y=beta, label=years), color="red") 

# spain_period = read.table("spain_perioddata_1x10.txt",skip=1, header=T,na.string=".")
# spain__period_gompertz <- Gompertz(df = spain_period , start = 1910, end = 2010, interval = 10)
# spain_period_gompertz
# spain_period_gompertz %>% ggplot() + geom_text(mapping = aes(x=log.alpha,y=beta, label=years), color="red") 

# usa <- read.table("usa.txt",skip=1, header=T,na.string=".")
# usa_gompertz <- Gompertz(df= usa, start = 1940, end = 2010, interval = 5)
# usa_gompertz %>% ggplot() + geom_text(mapping = aes(x=log.alpha,y=beta, label=years), color="red")

# library(gridExtra)
# grid.arrange(test1, test2)
