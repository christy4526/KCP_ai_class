setwd("C:/projects/Rproject")

datas <- read.csv("가계동향조사_가구주.csv")

library(Hmisc)
library(prettyR)
library(psych)
library(nortest)
library(dplyr)
describe(datas)

View(datas)

######### 가구원수에 따른 엥겔지수 차이 #########
# 정규성 검증
datas2 <- datas[ ! (datas$취업인원수==6) , ]
tapply(datas2$엥겔지수, datas2$취업인원수, nortest::ad.test)

# 등분산성검증 
bartlett.test(datas2$엥겔지수, datas2$취업인원수, data=datas2)

#ANOVA
datas.lm <- lm(datas2$엥겔지수 ~ datas2$취업인원수)
anova(datas.lm)

