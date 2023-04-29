#######################################
###### up(telcom)



library(lnmixsurv)
library(ggplot2)
library(prodlim)
library(purrr)
library(dplyr)
library(tidyr)
require(ggsurvfit)
require(censored)
require(prodlim)
library(tidyverse)
library(plotly)
library(scales)
library(survival)
library(bayesplot)
require(gridExtra)
library(knitr)



df<- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")
head(df)
dim(df)
#[1] 7043   21

range(df$`tenure`)  ## min and max
### removendo tempos iguais a zero
df<- df %>%  filter(tenure >0)
dim(df)
range(df$`tenure`)

### criando uma coluna de status - censored or churned
churn = c("Yes")
df$status = ifelse(df$Churn %in% churn, 1, 0) 
#status = censoring status (0 se o individuo foi censurado, 
#1 caso contrario)

#status <- df$status
#t.duration <- df$`tenure`  ### time duration (in months)

### SeniorCitizen
df$SeniorCitizen<- as.character(df$SeniorCitizen)
df$SeniorCitizen[df$SeniorCitizen == '0'] <- 'No'
df$SeniorCitizen[df$SeniorCitizen == '1'] <- 'Yes'


### taxa de falha empirica 
tx.emp <- function(t, s) {
  aux <- c()
  for (i in 1:length(t)) {
    x <- (s[i] - s[i+1])/((t[i+1]-t[i])*s[i])
    aux <- c(aux,x)
  }
  return(aux)
}


### survival plot 
grad <- seq(0,75,20)
grad[(grad/5)%%2 != 0] <- ""
point <- format_format(big.mark = " ", decimal.mark = ".", scientific = FALSE)





df2<- df

### StreamingTV: agrupando no and no internet service
df2$StreamingTV<- as.character(df2$StreamingTV)
df2$StreamingTV[df2$StreamingTV == "No internet service"] <- 'No'

#df2$DeviceProtection<- df$DeviceProtection

### PaymentMethod: agrupando Bank transfer, Credit card e Mailed Check
df2$PaymentMethod<- as.character(df2$PaymentMethod)
df2$PaymentMethod[df2$PaymentMethod == 'Bank transfer (automatic)'] <- 'others'
df2$PaymentMethod[df2$PaymentMethod == 'Credit card (automatic)'] <- 'others'
df2$PaymentMethod[df2$PaymentMethod == 'Mailed check'] <- 'others'

df2$InternetService<- as.character(df2$InternetService)
df2$InternetService[df2$InternetService == 'DSL'] <- 'others'
df2$InternetService[df2$InternetService == 'No'] <- 'others'

df2$Contract<- as.character(df2$Contract)
df2$Contract[df2$Contract == 'One year'] <- 'One year +'
df2$Contract[df2$Contract == 'Two year'] <- 'One year +'
