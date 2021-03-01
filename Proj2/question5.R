library(knitr)
library(readr)
library(tidyverse)

data <- read.delim("Projekt2_Grupp9.txt", sep = ";") %>% 
  mutate(ClaimYear = floor((ClaimDay-1)/365) + 1,
         PaymentYear = floor((PaymentDay-1)/365) + 1) %>% 
  rbind(c(1,1,1,0,1,10)) %>% 
  rbind(c(1,1,1,0,10,19)) #Added to deal with aggregation in a simple manner


t <- data %>% 
  group_by(ClaimYear, PaymentYear) %>% 
  summarize(Total = sum(ClaimCost)) %>% 
  mutate(DevelopmentYear = PaymentYear-ClaimYear + 1) %>% 
  select(-PaymentYear) %>% 
  mutate(Total = cumsum(Total)) %>% 
  spread(value = Total, key = DevelopmentYear) %>% 
  ungroup() 

ct <- t %>% 
  filter(ClaimYear >= 11) %>% 
  select(-ClaimYear) %>% 
  as.matrix()





