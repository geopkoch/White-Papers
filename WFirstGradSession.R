#                                                                               
#                          W in first grad course                     
#                                                                               
# Purpose: What is the likelihood that a student comes back after W in first Grad course
# Created by: Geoff Koch                                                        
# Created on: 5/17/2019                                                   
# Validated by: Matt Kline                                                   
# Validated on: 5/22/2019                                                     
#################################################################################


source("P:/APUS_Corporate/Institutional Research/Data Team/Scripts/Common Code/R Scripts/Functions.R")
if (!require("pacman")) install.packages("pacman")
p_load(dplyr,magrittr,tidyr,readr,stringr,RODBC,ggplot2,data.table,broom,dummies,purrr,janitor,oddsratio,pander)

mypath <- "INTERNAL PATH"
# 
# this baby takes a long time...writing to csv afterward to reduce load time on rerun.
# mydata <- queryDB("INTERNAL PATH/Impact of W on Grad Persistence.sql",
#                   path = TRUE,
#                   cache = "mydata",
#                   odbc = "APUS_DW")
# 
# fwrite(mydata, file = paste0(getwd(),"/wpersistencedata.csv"))

mydata <- read.csv(paste0(mypath,"/wpersistencedata.csv"))

mydata <- mydata %>%
  mutate(fStartdate = as.Date(fStart),
         fStartyear  = year(as.Date(fStart)),
         isNRAfterFirst = case_when(NetRegs12Month > 0 ~ 1,
                                    TRUE ~ 0))


######## ODDS OF NET REG AFTER W IN FIRST COURSE ############



#calculate the odds of a student having more net registrations after having a W in first course

myglm <- tidy(glm(isNRAfterFirst ~ isWFrst , family = "binomial",data = mydata))


#this is the odds ratio
exp(myglm %>% filter(term == "isWFrst") %>% select(2))

#cohen's D on if they NR at all
do.cohen.d(mydata %>% filter(isWFrst == 1) %>% select(isNRAfterFirst),
           mydata %>% filter(isWFrst == 0) %>% select(isNRAfterFirst),
           1, 1)


t.test(isWFrst ~ isNRAfterFirst, data = mydata) #difference in means


# with an odds ratio of X, we can say the odds of having a net registration after W in first course are about 1 to (1/X).
# in other words, you are about (1-X)% less likely to have a subsequent net registration if you have a W in first grad course.
# additionally, having a W in a first course has a negligible/small/medium/large effect and is/is not statistically significant on the student's likelihood to net register

#test this for each school
model.schools <- mydata %>%
  nest(-ScTitle) %>% 
  mutate(fit = map(data, ~ glm(isNRAfterFirst ~ isWFrst, family = "binomial", data = .)),
         results = map(fit, tidy)) %>%
  unnest(results) 

model.schools %>% filter(term == "isWFrst") %>% select(1,3) %>%
  mutate(odds.ratio = exp(estimate))

#SIGNIF DIFF BETWEEN SCHOOLS?


#now to determine if a W has an impact on number of net registrations in the first year afterward

netregs.schoolyear <- mydata %>%
  nest(-ScTitle,-fStartyear) %>% 
  mutate(fit = map(data, ~ lm(NetRegs12Month ~ isWFrst, data = .)),
         results = map(fit, glance)) %>%
  unnest(results) %>%
  select(-data,-fit) #low r-squared for each school in each year...other variables needed

#cohen's d on if W has impact of number of NR in 12 months after
do.cohen.d(mydata %>% filter(isWFrst == 1) %>% select(NetRegs12Month),
           mydata %>% filter(isWFrst == 0) %>% select(NetRegs12Month),
           1, 1) #LARGE impact

t.test(mydata %>% filter(isWFrst == 1) %>% select(NetRegs12Month),
           mydata %>% filter(isWFrst == 0) %>% select(NetRegs12Month))
#diff in means stat sig




