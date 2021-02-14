
install_or_load_pack <- function(pack){
  
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  
  if (length(create.pkg))
    
    install.packages(create.pkg, dependencies = TRUE)
  
  sapply(pack, require, character.only = TRUE)
  
}


#### Codigo para calcular las medidas de riesgo para el MXN
library(QRM)
library(timeSeries)

install_or_load_pack(c("xts","tidyverse","tidyquant","ggplot2","dplyr"))

MXN <- tq_get('MXN=X', from = "2019-01-01",to = "2021-02-10", get = "stock.prices")

MXN %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  theme_classic() +
  labs(x = 'Date',
       y = "Adjusted Price",
       title = "MXN price chart") +
  scale_y_continuous(breaks = seq(0,300,10))


Dates<- MXN %>% select("date")
close_price<- MXN %>% select("close")

MXN_levels <- timeSeries(close_price, Dates, units="MXN")

MXN.ret <- returns(MXN_levels,percentage=TRUE)
#el peso mexicano cotiza sobre número de pesos para un dólar

VaR.hist95 <- quantile(MXN.ret, prob=0.95,names=FALSE)
VaR.norm95 <- mean(MXN.ret)+sd(MXN.ret)*qnorm(0.95)


ES.hist95 <- mean(MXN.ret[MXN.ret >= VaR.hist95])
ES.norm95 <- mean(MXN.ret)+sd(MXN.ret)*dnorm(qnorm(0.95))/(1-0.95)

t.fit <- fit.st(MXN.ret)
nu <- as.double(t.fit$par.ests[1])
mu <- as.double(t.fit$par.ests[2])
sigma <- as.double(t.fit$par.ests[3])


ES.st95 <- ESst(0.95,mu=mu,sd=sigma,df=nu)
VaR.st95 <- mu+sigma*qt(0.95,df=nu)
