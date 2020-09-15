setwd("C:/projects/Rproject")
library(plotly)
library(ggplot2)


engel.j <- read.csv("가계동향조사_J개인.csv", head=TRUE)
### PIE 그래프
################################################################################
# 거처구분
hlabel <- c('단독주택', '아파트', '연립주택', '다세대주택', '비주거용건물', 
            '오피스텔')
hnums <- c(57, 150, 7, 23, 2, 5)

#입주형태
h2label <- c('자기집', '무상주택', '사택', '전세', '영구임대', 
             '보증부 월세', '보증금 없는 월세')
h2nums <- c(129, 6, 1, 60, 3, 42, 3)

# 자동차 보유댓수
clabel <- c('0대', '1대', '2대', '3대')
cnums <- c(52, 145, 46, 1)

# 연차 구분
ylabel <- c('1:28 이하', '2:29~33', '3:34~39', '4:39~43', '5:44~48', '6:49 이상')
ynums <- c(41, 43, 47, 34, 40, 39)

# 거처구분 그래프
fig <- plot_ly(engel.j, labels = ~hlabel, values = ~hnums, type = 'pie')
fig <- fig %>% layout(title = 'J산업군 거처 구분',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      legend = list(traceorder='reversed'))

fig

# 입주형태 그래프
fig <- plot_ly(engel.j, labels = ~h2label, values = ~h2nums, type = 'pie')
fig <- fig %>% layout(title = 'J산업군 입주 형태',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

# 자동차 보유 댓수
fig <- plot_ly(engel.j, labels = ~clabel, values = ~cnums, type = 'pie')
fig <- fig %>% layout(title = 'J산업군 자동차 보유 댓수',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig

# 입주형태 그래프
fig <- plot_ly(engel.j, labels = ~ylabel, values = ~ynums, type = 'pie')
fig <- fig %>% layout(title = 'J산업군 연차 구분',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig


################################################################################
#################### J 산업군 연차별 거처구분
yhome1 <- read.csv("년차별 거처구분_J개인.csv", head=TRUE)

tapply(engel.j$거처구분, engel.j$years, shapiro.test)
bartlett.test(engel.j$거처구분, engel.j$years)

oneway.test(거처구분 ~ years, data=engel.j, var.equal=FALSE) # p > 0.05
#yhome1.lm <- lm(engel ~ years, data=engel.j)
#anova(yhome1.lm)
#summary(yhome1.lm)

View(yhome1)
fig <- plot_ly(engel.j, x = ~yhome1[1], y = ~yhome1$X1, type = 'bar', name = hlabel[1])
fig <- fig %>% add_trace(y = ~yhome1$X2, name = hlabel[2])
fig <- fig %>% add_trace(y = ~yhome1$X3, name = hlabel[3])
fig <- fig %>% add_trace(y = ~yhome1$X4, name = hlabel[4])
fig <- fig %>% add_trace(y = ~yhome1$X5, name = hlabel[5])
fig <- fig %>% add_trace(y = ~yhome1$X6, name = hlabel[6])
fig <- fig %>% layout(xaxis = list(title = '년차', range(1,6)),
                      yaxis = list(title = '수'),
                      barmode = 'stack')
#fig <- plot_ly(engel.j, x = ~yhome1[1], y = ~yhome1$X1, type = 'bar', name = ylabel[1])
#fig <- fig %>% add_trace(y = ~yhome1$X2, name = ylabel[2])
#fig <- fig %>% add_trace(y = ~yhome1$X3, name = ylabel[3])
#fig <- fig %>% add_trace(y = ~yhome1$X4, name = ylabel[4])
#fig <- fig %>% add_trace(y = ~yhome1$X5, name = ylabel[5])
#fig <- fig %>% add_trace(y = ~yhome1$X6, name = ylabel[6])
#fig <- fig %>% layout(xaxis = list(title = '거처구분'),
#                      yaxis = list(title = '수'),
#                      barmode = 'stack')

fig

#################### J 산업군 연차별 입주형태
yhome2 <- read.csv("년차별 입주형태_J개인.csv", head=TRUE)

tapply(engel.j$입주형태, engel.j$years, shapiro.test)
bartlett.test(engel.j$입주형태, engel.j$years)

yhome2.lm <- lm(입주형태 ~ years, data=engel.j) # P < 0.05
anova(yhome2.lm)
summary(yhome2.lm)

fig <- plot_ly(engel.j, x = ~yhome2[1], y = ~yhome1$X1, type = 'bar', name = h2label[1])
fig <- fig %>% add_trace(y = ~yhome2$X2, name = h2label[2])
fig <- fig %>% add_trace(y = ~yhome2$X3, name = h2label[3])
fig <- fig %>% add_trace(y = ~yhome2$X4, name = h2label[4])
fig <- fig %>% add_trace(y = ~yhome2$X5, name = h2label[5])
fig <- fig %>% add_trace(y = ~yhome2$X6, name = h2label[6])
fig <- fig %>% add_trace(y = ~yhome2$X7, name = h2label[7])
fig <- fig %>% layout(xaxis = list(title = '년차', range(1,6)),
                      yaxis = list(title = '수'),
                      barmode = 'stack')
fig

#################### J 산업군 연차별 자동차 보유댓수
ycar <- read.csv("년차별 자동차수_J개인.csv", head=TRUE)

tapply(engel.j$자동차_보유댓수, engel.j$years, shapiro.test)
bartlett.test(engel.j$자동차_보유댓수, engel.j$years)

ycar.lm <- lm(자동차_보유댓수 ~ years, data=engel.j) # P < 0.05
anova(ycar.lm)  # P < 0.05
summary(ycar.lm)

fig2 <- plot_ly(engel.j, x = ~ycar[1], y = ~ycar$X3, type = 'bar', name = clabel[4])
fig2 <- fig2 %>% add_trace(y = ~ycar$X2, name = clabel[3])
fig2 <- fig2 %>% add_trace(y = ~ycar$X1, name = clabel[2])
fig2 <- fig2 %>% add_trace(y = ~ycar$X0, name = clabel[1])
fig2 <- fig2 %>% layout(xaxis = list(title = '년차', range(1,6)),
                      yaxis = list(title = '자동차 수'),
                      barmode = 'stack')
fig2
################################################################################
#################### J 산업군 연차별 엥겔지수
# 연차구분 : years

tapply(engel.j$engel, engel.j$years, shapiro.test)
bartlett.test(engel.j$engel, engel.j$years)

jengel.lm <- lm(engel ~ years, data=engel.j)
anova(jengel.lm)
summary(jengel.lm)

plot_ly(diamonds, y = ~engel.j$engel, x = ~engel.j$years, type = "box")


################################################################################
####################J  산업군 연차별 여가지수
plot_ly(diamonds, y = ~engel.j$freetime, x = ~engel.j$years, type = "box")
tapply(engel.j$freetime, engel.j$years, shapiro.test)
bartlett.test(engel.j$freetime, engel.j$years)

##### 이상치 제거
# engel2 <- engel %>% filter(freetime <= 20)
engel_ft <- engel.j %>% filter(years == 1 & freetime <12 |
                                      years == 2 & freetime <19 |
                                      years == 3 & freetime <18 |
                                      years == 4 & freetime <15 |
                                      years == 5 & freetime <18 |
                                      years == 6 & freetime <19)
# total : 222 --> ANOVA P-value < 0.05
plot_ly(diamonds, y = ~engel_ft$freetime, x = ~engel_ft$years, type = "box")

tapply(engel_ft$freetime, engel_ft$years, shapiro.test)
bartlett.test(engel_ft$freetime, engel_ft$years)

View(engel_ft)
library(psych)
length(engel_ft)

jengel.lm <- lm(freetime ~ years, data=engel_ft)
anova(jengel.lm)
#oneway.test(freetime ~ years, data=engel2, var.equal=FALSE)
model <- aov(freetime ~ years, data=engel_ft)
comparison <- LSD.test(model, "years", p.adj="bonferroni", group=T)
comparison
groupA <- subset(engel_ft, years==6)
groupB <- subset(engel_ft, years==1)
mean(groupA$freetime)
mean(groupB$freetime)
mean(engel_ft$freetime)
library(agricolae)
summary(jengel.lm)


################################################################################
################## J 산업군 연차별 행복지수
# engel$happy[is.infinite(engel$happy)] <- 0
plot_ly(diamonds, y = ~engel.j$happy, x = ~engel.j$years, type = "box")

engel_hp <- engel.j %>% filter(years == 1 & happy <293 |
                                      years == 2 & happy <317 |
                                      years == 3 & happy <158 |
                                      years == 4 & happy <235 |
                                      years == 5 & happy <194 |
                                      years == 6 & happy <193)
plot_ly(diamonds, y = ~engel_hp$happy, x = ~engel_hp$years, type = "box")

engel_hp <- engel.j %>% filter(years == 1 & happy <195 |
                               years == 2 & happy <249 |
                               years == 3 & happy <158 |
                               years == 4 & happy <235 |
                               years == 5 & happy <117 |
                               years == 6 & happy <101)

tapply(engel_hp$happy, engel_hp$years, shapiro.test)
bartlett.test(engel_hp$happy, engel_hp$years) # p-value < 0.05

oneway.test(happy ~ years, data=engel_hp, var.equal=FALSE)

model <- aov(happy ~ years, data=engel_hp)
comparison <- LSD.test(model, "years", p.adj="bonferroni", group=T)
comparison
groupA <- subset(engel_hp, years==2)
groupB <- subset(engel_hp, years==6)
mean(groupA$freetime)
mean(groupB$freetime)
mean(engel_hp$freetime)


################################################################################
# 연차별 평균
engel_mean <- read.csv("가계동향조사_J평균.csv", head=TRUE)

plot_ly(diamonds, y = ~engel_mean$happy, x = ~engel_mean$years, type = "bar",
        color = ~engel_mean$years)
plot_ly(diamonds, y = ~engel_mean$engel, x = ~engel_mean$years, type = "bar",
        color = ~engel_mean$years)
plot_ly(diamonds, y = ~engel_mean$freetime, x = ~engel_mean$years, type = "bar",
        color = ~engel_mean$years)

fig <- plot_ly(engel_mean, x = ~years, y = ~happy, type = 'bar', name = '행복지수')
fig <- fig %>% add_trace(y = ~engel, name = '엥겔지수')
fig <- fig %>% add_trace(y = ~freetime, name = '여가지수')
fig <- fig %>% layout(barmode = 'group',xaxis = list(title = '연차'),
                      yaxis = list(title = 'count'))
fig

fig2 <- plot_ly(engel_mean, x = ~years, y = ~engel, type = 'bar', name = '엥겔지수',
                marker = list(color = 'rgb(135, 153, 236)'))
fig2 <- fig2 %>% add_trace(y = ~freetime, name = '여가지수',
                           marker = list(color = 'rgb(5, 238, 236)'))
fig2 <- fig2 %>% layout(barmode = 'group',xaxis = list(title = '연차'),
                        yaxis = list(title = 'count'))
fig2


################################################################################  
# 종사상 지위에 따른 여가 지수(J 산업군에서)  
#     ------> P값 > 0.05 , 그래프 X
#     ------> 종사상지위 3,4,6,7의 수가 너무 적음
plot_ly(diamonds, y = ~engel.j$freetime, x = ~engel.j$종사상지위, type = "box")
engel_wtype <- engel.j %>% filter(freetime < 30)
engel_wtype <- engel_wtype %>% filter(종사상지위 != 3)
engel_wtype <- engel_wtype %>% filter(종사상지위 != 4)
engel_wtype <- engel_wtype %>% filter(종사상지위 != 6)
engel_wtype <- engel_wtype %>% filter(종사상지위 != 7)
# total : 222 --> ANOVA P-value < 0.05
plot_ly(diamonds, y = ~engel_wtype$freetime, x = ~engel_wtype$종사상지위, type = "box")

#shapiro.test(engel_wtype$freetime)
#shapiro.test(engel_wtype$종사상지위)
tapply(engel_wtype$freetime, engel_wtype$종사상지위, shapiro.test)
bartlett.test(engel_wtype$freetime, engel_wtype$종사상지위)

View(engel_wtype)
length(engel_wtype)

jengel.lm <- lm(freetime ~ 종사상지위, data=engel_wtype)
anova(jengel.lm)
oneway.test(freetime ~ 종사상지위, data=engel_wtype, var.equal=FALSE)
model <- aov(freetime ~ years, data=engel_ft)


################################################################################  
###거처구분 : 5가 2개 --> 제외
###### 거처구분에 따른 엥겔 지수
# -----> p > 0.05
htype <- engel.j %>% filter(거처구분 != 5)
tapply(htype$engel, htype$거처구분, shapiro.test)
bartlett.test(htype$engel, engel_home$거처구분)

htype.lm <- lm(engel ~ 거처구분, data=htype)
anova(htype.lm)   
#summary(jengel.lm)

fig <- plot_ly(diamonds, y = ~htype$engel, x = ~htype$거처구분, type = "box")
fig <- fig %>% layout(barmode = 'group',xaxis = list(title = '거처구분'),
                      yaxis = list(title = '엥겔지수'))
fig

############ 거처구분에 따른 여가 지수
# -----> p < 0.05
tapply(htype$freetime, htype$거처구분, shapiro.test)
bartlett.test(htype$freetime, engel_home$거처구분)

oneway.test(freetime ~ 거처구분, data=htype, var.equal=FALSE)# p < 0.05
#htypef.lm <- lm(freetime ~ 거처구분, data=htype)
#anova(htypef.lm) 
#summary(jengel.lm)

fig <- plot_ly(diamonds, y = ~htype$freetime, x = ~htype$거처구분, type = "box")
fig <- fig %>% layout(barmode = 'group',xaxis = list(title = '거처구분'),
                      yaxis = list(title = '여가지수'))
fig

################ 거처구분에 따른 행복 지수
# -------> p < 0.05
tapply(htype$happy, htype$거처구분, shapiro.test)
bartlett.test(htype$happy, engel_home$거처구분)

oneway.test(happy ~ 거처구분, data=htype, var.equal=FALSE)# p < 0.05
#htypef.lm <- lm(freetime ~ 거처구분, data=htype)
#anova(htypef.lm) 
#summary(jengel.lm)

fig <- plot_ly(diamonds, y = ~htype$happy, x = ~htype$거처구분, type = "box")
fig <- fig %>% layout(barmode = 'group',xaxis = list(title = '거처구분'),
                      yaxis = list(title = '행복지수'))
fig


########## 거처구분 평균 엥겔, 여가, 행복 지수
home_mean <- read.csv("거처구분별 J평균.csv", head=TRUE)

#plot_ly(diamonds, y = ~home_mean$happy, x = ~home_mean$htype, type = "bar",
#        color = ~home_mean$htype, name = '행복지수')
#plot_ly(diamonds, y = ~home_mean$engel, x = ~home_mean$htype, type = "bar",
#        color = ~home_mean$htype)
#plot_ly(diamonds, y = ~home_mean$freetime, x = ~home_mean$htype, type = "bar",
#        color = ~home_mean$htype)

fig2 <- plot_ly(home_mean, x = ~htype, y = ~engel, type = 'bar', name = '엥겔지수',
                marker = list(color = 'rgb(135, 153, 236)'))
fig2 <- fig2 %>% add_trace(y = ~freetime, name = '여가지수',
                           marker = list(color = 'rgb(5, 238, 236)'))
#fig2 <- fig2 %>% add_trace(y = ~happy, name = '행복지수')
fig2 <- fig2 %>% layout(barmode = 'group',
                        xaxis = list(title = '거처구분',
                                     ticktext = list('단독주택', '아파트', '연립주택', 
                                                     '다세대주택', '비주거용건물', 
                                                     '오피스텔'), 
                                     tickvals = list(1, 2, 3, 4, 5, 6),
                                     tickmode = "array"),
                        yaxis = list(title = '지수'))
fig2


################################################################################  
# 입주 형태에 따른 엥겔 지수
# 입주형태 : 3은 1개, 5,7은 3개
engel_home <- engel.j %>% filter(입주형태 != 3)
tapply(engel_home$engel, engel_home$입주형태, shapiro.test)
bartlett.test(engel_home$engel, engel_home$입주형태)

jengel.lm <- lm(engel ~ 입주형태, data=engel_home)
anova(jengel.lm)
#summary(jengel.lm)

fig <- plot_ly(diamonds, y = ~engel_home$engel, x = ~engel_home$입주형태, type = "box")
fig <- fig %>% layout(barmode = 'group',xaxis = list(title = '입주형태'),
                        yaxis = list(title = '엥겔지수'))


# 입주 형태에 따른 여가 지수
tapply(engel_home$freetime, engel_home$입주형태, shapiro.test)
bartlett.test(engel_home$freetime, engel_home$입주형태)

oneway.test(freetime ~ 입주형태, data=engel_home, var.equal=FALSE)
#jengel.lm <- lm(freetime ~ 입주형태, data=engel_home)
#anova(jengel.lm)

fig <- plot_ly(diamonds, y = ~engel_home$freetime, x = ~engel_home$입주형태, type = "box")
fig <- fig %>% layout(barmode = 'group',xaxis = list(title = '입주형태'),
                      yaxis = list(title = '여가지수'))
fig


# 입주 형태에 따른 행복 지수
engel_home2 <- engel.j %>% filter(happy < 353)
tapply(engel_home2$happy, engel_home2$입주형태, shapiro.test)
bartlett.test(engel_home2$happy, engel_home2$입주형태)

oneway.test(happy ~ 입주형태, data=engel_home2, var.equal=FALSE)
jengel.lm <- lm(happy ~ 입주형태, data=engel_home2)
anova(jengel.lm)
summary(jengel.lm)

fig <- plot_ly(diamonds, y = ~engel_home2$happy, x = ~engel_home2$입주형태, type = "box")
fig <- fig %>% layout(barmode = 'group',xaxis = list(title = '입주형태'),
                      yaxis = list(title = '행복지수'))
fig


###### 입주형태 평균 엥겔, 여가, 행복 지수
home_mean2 <- read.csv("입주형태별 J평균.csv", head=TRUE)

fig2 <- plot_ly(home_mean2, x = ~htype, y = ~engel, type = 'bar', name = '엥겔지수',
                marker = list(color = 'rgb(135, 153, 236)'))
fig2 <- fig2 %>% add_trace(y = ~freetime, name = '여가지수',
                           marker = list(color = 'rgb(5, 238, 236)'))
#fig2 <- fig2 %>% add_trace(y = ~happy, name = '행복지수')
fig2 <- fig2 %>% layout(barmode = 'group',
                        xaxis = list(title = '입주형태',
                                     ticktext = list('자기집', '무상주택', '전세', 
                                                     '영구임대', '보증부 월세', 
                                                     '보증금 없는 월세'), 
                                     tickvals = list(1, 2, 3, 4, 5, 6),
                                     tickmode = "array"),
                        yaxis = list(title = '지수'))
fig2


# 자동차 보유댓수별 평균 엥겔, 여가, 행복 지수
car_mean <- read.csv("자동차수별 J평균.csv", head=TRUE)

fig2 <- plot_ly(car_mean, x = ~cnum, y = ~engel, type = 'bar', name = '엥겔지수',
                marker = list(color = 'rgb(135, 153, 236)'))
fig2 <- fig2 %>% add_trace(y = ~freetime, name = '여가지수',
                           marker = list(color = 'rgb(5, 238, 236)'))
#fig2 <- fig2 %>% add_trace(y = ~happy, name = '행복지수')
fig2 <- fig2 %>% layout(barmode = 'group',
                        xaxis = list(title = '자동차 보유 댓수',
                                     ticktext = list('0대', '1대', '2대', '3대'), 
                                     tickvals = list(0, 1, 2, 3),
                                     tickmode = "array"),
                        yaxis = list(title = '지수'))
fig2


################################################################################  
######## 년차에 따른 복권
# ----------> p > 0.05
engel_lottery <- engel.j %>% filter(복권 < 2000)
plot_ly(engel_lottery, y = ~복권, x = ~years, type = 'box', 
               color = ~years, colors = 'Paired')
tapply(engel_lottery$복권, engel_lottery$years, shapiro.test)
bartlett.test(engel_lottery$복권, engel_lottery$years)

oneway.test(복권 ~ years, data=engel_lottery, var.equal=FALSE)


######## 년차에 따른 주류
# ----------> p > 0.05
engel_alch <- engel.j %>% filter(주류 < 6000)
plot_ly(engel_alch, y = ~주류, x = ~years, type = 'box', 
        color = ~years, colors = 'Paired')
tapply(engel_alch$주류, engel_alch$years, shapiro.test)
bartlett.test(engel_alch$주류, engel_alch$years)

yalch.lm <- lm(주류 ~ years, data=engel_alch)
anova(yalch.lm)

yalch
plot_ly(engel_alch, y = ~주류, x = ~years, type = 'box', 
        color = ~years, colors = 'Paired')


######## 년차에 따른 담배
# ----------> p > 0.05
engel_sigar <- engel.j %>% filter(담배 < 8000)
plot_ly(engel_sigar, y = ~담배, x = ~years, type = 'box', 
        color = ~years, colors = 'Paired')
tapply(engel_sigar$담배, engel_sigar$years, shapiro.test)
bartlett.test(engel_sigar$담배, engel_sigar$years)

oneway.test(담배 ~ years, data=engel_sigar, var.equal=FALSE)


################################################################################  
# 입주형태에 따른 복권/주류/담배
# 범주 - 연속형
engel_leisure <- engel.j %>% filter(입주형태 != 3)
engel_leisure <- engel_leisure %>% filter(복권 < 5350)
tapply(engel_leisure$복권, engel_leisure$입주형태, shapiro.test)
bartlett.test(engel_leisure$복권, engel_leisure$입주형태)

oneway.test(happy ~ 입주형태, data=engel_leisure, var.equal=FALSE)
jengel.lm <- lm(happy ~ 입주형태, data=engel_leisure)
anova(jengel.lm)
summary(jengel.lm)

# BOX
#fig <- plot_ly(engel_leisure, y = ~복권, x = ~입주형태, type = 'box', 
#               mode = 'markers', size = ~복권, color = ~입주형태, colors = 'Paired')
# SCCATER
fig <- plot_ly(engel_leisure, y = ~복권, x = ~입주형태, type = 'scatter', 
               mode = 'markers', size = ~복권, color = ~입주형태, colors = 'Paired')
fig <- fig %>% layout(barmode = 'group',xaxis = list(title = '입주형태'),
                      yaxis = list(title = '복권'))
fig


# 거처구분에 따른 복권/주류/담배
engel_leisure <- engel.j %>% filter(입주형태 != 3)
engel_leisure <- engel_leisure %>% filter(복권 < 5350)
tapply(engel_leisure$복권, engel_leisure$입주형태, shapiro.test)
bartlett.test(engel_leisure$복권, engel_leisure$입주형태)

oneway.test(happy ~ 입주형태, data=engel_leisure, var.equal=FALSE)
jengel.lm <- lm(happy ~ 입주형태, data=engel_leisure)
anova(jengel.lm)
summary(jengel.lm)

# BOX
#fig <- plot_ly(engel_leisure, y = ~복권, x = ~입주형태, type = 'box', 
#               mode = 'markers', size = ~복권, color = ~입주형태, colors = 'Paired')
# SCCATER
fig <- plot_ly(engel_leisure, y = ~복권, x = ~입주형태, type = 'scatter', 
               mode = 'markers', size = ~복권, color = ~입주형태, colors = 'Paired')
fig <- fig %>% layout(barmode = 'group',xaxis = list(title = '입주형태'),
                      yaxis = list(title = '복권'))
fig




  
  
  
  
  
  
  
  
  