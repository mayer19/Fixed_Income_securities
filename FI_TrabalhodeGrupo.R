rm(list=ls(all=TRUE))
graphics.off()
close.screen(all = TRUE)
erase.screen()
windows.options(record=TRUE)

### Reading the data files ###

portfolio=read.csv(file='fi_group_bonds.txt',header=FALSE,sep=" ")# bonds
nrow(portfolio)

####Question2
####Question2a)
portfolio_q2 <- head(portfolio, - 1) 
portfolio_q2
nrow(portfolio_q2)
price=portfolio_q2[2]
coupon=portfolio_q2[3]
matu=portfolio_q2[1]

###getting first zero(mat 1)
zero_1=price[1,1]/(100+coupon[1,1])
zero_1

1- (1-zero_1)/4



#interpolating zeros for mat 0.25 
#considering zero rate for moment 0 = 1
zero_0.25<- 1+ ((zero_1-1)/(1-0))*(0.25-0) #inter with 0 and 1
zero_0.25

###getting second zero(mat 1.25)
zero_1.25<-(price[2,1]-coupon[2,1]*zero_0.25)/(100+coupon[2,1])
zero_1.25

#interpolating and extrapolating zeros for mat 0.75 ; 1.75 ;
zero_0.75<-zero_0.25+ ((zero_1-zero_0.25)/(1-0.25))*(0.75-0.25) 
                              #inter with 0.25 and 1
zero_0.75

zero_1.75<- zero_1 + ((1.75-1)/(1.25-1)) * (zero_1.25-zero_1)
zero_1.75

###getting third zero(mat 2.75)

zero_2.75<-(price[3,1]-coupon[3,1]*zero_0.75-coupon[3,1]*zero_1.75)/(100+coupon[3,1])
zero_2.75

#interpolating and extrapolating zeros for mat 2 ; 3 

zero_2<- zero_1.75+(2-1.75)*((zero_2.75-zero_1.75)/(2.75-1.75))  #inter with 1.75 and 2.75
zero_2

zero_3<-zero_2 + ((3-2)/(2.75-2)) * (zero_2.75-zero_2)
zero_3

###getting fourth zero(mat 4)

zero_4<-(price[4,1]-coupon[4,1]*zero_1-coupon[3,1]*zero_2 -coupon[3,1]*zero_3)/(100+coupon[4,1])
zero_4


# 1. Direct Method

coupon[]

get_zero_coupon <- function(coupons=c(1.5,1,1,2.25,2.5,2.75,3,3.25,3.5,3.75),
                            BondPrices=c(96.6,85.7595148,92.9494812,90.24,89.74,90.04,91.09,92.82,95.19,98.14),
                            nominal_value=100){
  
  #We assume both coupons and BondPrices vectors are arranged to 1 year increasing maturity.
  price_matrix <- matrix(0,nrow=length(coupons),ncol=length(coupons))
  
  #Assign the coupons for each year
  for(i in 1:length(coupons)){
    price_matrix[i,1:i] <- coupons[i]
  }
  
  #Add the maturity nominal value
  diag(price_matrix) <- diag(price_matrix) + nominal_value
  
  #Solve the system of equations to get B(0,t)
  zero_coupon_prices <- solve(price_matrix, BondPrices)
  
  #Get zero coupon yields R(0,t)
  zero_coupon_yields <- (1/zero_coupon_prices)^(1/1:length(coupons))-1
  
  return(list(B0t=zero_coupon_prices, R0t=zero_coupon_yields))
}

get_zero_coupon()



# ---------------------------------------------------------------------------------------------------
# Question 1 
#Estimating & Simulating Portfolio returns
# ---------------------------------------------------------------------------------------------------
install.packages("lubridate")
install.packages("Thermimage")
library(lubridate)
graphics.off()
close.screen(all = TRUE) 
erase.screen()
windows.options(record = TRUE)

library(pacman)
if (!require("pacman")) {install.packages("pacman")}
p_load(tidyverse)


# ---------------------------------------------------------------------------------------------------
# a) accrued interest
# ---------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------
# b) Simulating Inflation
# ---------------------------------------------------------------------------------------------------

##Creating Function that maekes the simulations

gbm_vec <- function(nsim, t, mu, sigma, S0, dt) {
  
  # matrix of random draws - one for each day for each simulation
  epsilon <- matrix(rnorm(t*nsim), ncol = nsim, nrow = t)  
  
  # get GBM and convert to price paths
  gbm <- exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
  gbm <- apply(rbind(rep(S0, nsim), gbm), 2, cumprod)
  
  return(gbm)
}

#Inputs for the Function

issue_date<- as.Date('2020-07-31')  
set_date<-as.Date('2020-09-21')
mat<-5
mat_date=as.Date('2025-07-21')
M <- 1
n <- 365
dt <- M/n
day_dif<- as.numeric(difftime(mat_date,set_date,'days'))
nsim <- 1000
t <- day_dif
mu <- 0.05321
sigma <- 0.06358
S0 <- 251.14721

#Feeding the inputs to the function
gbm <- gbm_vec(nsim, t, mu, sigma, S0,dt)

#Setting in df format

gbm_df <- as.data.frame(gbm) %>%
  mutate(ix = 1:nrow(gbm)) %>%
  pivot_longer(-ix, names_to = 'sim', values_to = 'inflation')

#Plotting the paths

gbm_df %>%
  ggplot(aes(x=ix, y=inflation, color=sim)) +
  geom_line() +
  theme(legend.position = 'none')

#Plotting the distribution

data.frame(inflation = gbm[t, ]) %>%
  ggplot(aes(x = inflation)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, col='lightblue') +
  geom_density(col='red') + 
  ggtitle('terminal inflation distribution')


# ---------------------------------------------------------------------------------------------------
# c) ILB cash flows
# ---------------------------------------------------------------------------------------------------

day_dif

daily_inflation<-meanEveryN(gbm_df[,3,1],1000)

tail(data.frame(daily_inflation))

