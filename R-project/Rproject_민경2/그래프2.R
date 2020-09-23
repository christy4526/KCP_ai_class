setwd("C:/projects/Rproject")
library(gplots)
library(rgl)
library(ca)

each <- read.csv("가계동향조사_J개인.csv", head=TRUE)

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

# 배우자 유무
wlabel <- c('있음(동거)', '있음(비동거)', '없음')

################################################################################  
######## 엥겔지수와 배우자 유무
#  -------> p < 0.05
tapply(each$engel, each$배우자유무, shapiro.test)
bartlett.test(each$engel, each$배우자유무)

wedding.lm <- lm(engel ~ 배우자유무, data=each)
anova(wedding.lm)
summary(wedding.lm)

fig <- plot_ly(diamonds, y = ~each$engel, x = ~each$배우자유무, type = "box", name = '배우자 유무')
fig <- fig %>% layout(barmode = 'group',
                        xaxis = list(title = '배우자유무',
                                     ticktext = list('있음(동거)', '있음(비동거)', '없음'), 
                                     tickvals = list(1, 2, 3),
                                     tickmode = "array"),
                        yaxis = list(title = '엥겔지수'))
fig

######## 여가지수와 배우자 유무
#  -------> p > 0.05
weach <- each %>% filter(freetime < 25)
tapply(weach$freetime, weach$배우자유무, shapiro.test)
bartlett.test(weach$freetime, weach$배우자유무)

wedding.lm <- lm(freetime ~ 배우자유무, data=weach)
anova(wedding.lm)
summary(wedding.lm)

fig <- plot_ly(diamonds, y = ~weach$freetime, x = ~weach$배우자유무, type = "box", name = '배우자 유무')
fig <- fig %>% layout(barmode = 'group',
                      xaxis = list(title = '배우자유무',
                                   ticktext = list('있음(동거)', '있음(비동거)', '없음'), 
                                   tickvals = list(1, 2, 3),
                                   tickmode = "array"),
                      yaxis = list(title = '여가지수'))
fig


######## 행복지수와 배우자 유무
#  -------> p > 0.05
tapply(each$happy, each$배우자유무, shapiro.test)
bartlett.test(each$happy, each$배우자유무)

wedding.lm <- lm(engel ~ 배우자유무, data=each)
anova(wedding.lm)
summary(wedding.lm)

fig <- plot_ly(diamonds, y = ~each$happy, x = ~each$배우자유무, type = "box", name = '배우자 유무')
fig <- fig %>% layout(barmode = 'group',
                      xaxis = list(title = '배우자유무',
                                   ticktext = list('있음(동거)', '있음(비동거)', '없음'), 
                                   tickvals = list(1, 2, 3),
                                   tickmode = "array"),
                      yaxis = list(title = '행복지수'))
fig


################################################################################  
######## 년차별 배우자 유무
#  -------> p < 0.05
ywedding <- read.csv("년차별 배우자유무_J개인.csv", head=T)

tapply(each$배우자유무, each$years, shapiro.test)
bartlett.test(each$배우자유무, each$years)

oneway.test(배우자유무 ~ years, data=each, var.equal=FALSE)
#wedding.lm <- lm(배우자유무 ~ years, data=each)
#anova(wedding.lm)

install.packages("agricolae")
library(agricolae)
model <- aov(배우자유무 ~ years, data=each)
comparison <- LSD.test(model, "years", p.adj="bonferroni", group=T)
comparison
#groupA <- subset(each, years==2)
#groupB <- subset(each, years==6)
#mean(groupA$freetime)
#mean(groupB$freetime)
#mean(engel_hp$freetime)

fig <- plot_ly(diamonds, y = ~ywedding$X1, x = ~ywedding$years, 
               type = "bar", name = '있음(동거)')
fig <- fig %>% add_trace(y = ~ywedding$X2, name = '있음(비동거)')
fig <- fig %>% add_trace(y = ~ywedding$X3, name = '없음')
fig <- fig %>% layout(barmode = 'stack',
                      xaxis = list(title = '년차',
                                   ticktext = list('28 이하', '29~33', '34~39', 
                                                   '39~43', '44~48', '49 이상'), 
                                   tickvals = list(1, 2, 3,4,5,6),
                                   tickmode = "array"),
                      yaxis = list(title = '배우자유무'))
fig

################################################################################
######## 3D : 년차, 배우자 유무별 행복지수
x <- each$years
y <- each$배우자유무
z <- each$engel

fig <- plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers", color=x)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(
                      xaxis = list(title = '년차',
                                   ticktext = list('28 이하', '29~33', '34~39', 
                                                   '39~43', '44~48', '49 이상'), 
                                   tickvals = list(1, 2, 3,4,5,6),
                                   tickmode = "array"),
                      yaxis = list(title = '배우자유무',
                                   ticktext = list('있음(동거)', '있음(비동거)', '없음'), 
                                   tickvals = list(1, 2, 3),
                                   tickmode = "array"),
                      zaxis = list(title = '엥겔지수')))
fig

######## 3D : 년차, 배우자 유무별 여가지수
z2 <- each$freetime
fig <- plot_ly(x=x, y=y, z=z2, type="scatter3d", mode="markers", color=x)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(
  xaxis = list(title = '년차',
               ticktext = list('28 이하', '29~33', '34~39', 
                               '39~43', '44~48', '49 이상'), 
               tickvals = list(1, 2, 3,4,5,6),
               tickmode = "array"),
  yaxis = list(title = '배우자유무',
               ticktext = list('있음(동거)', '있음(비동거)', '없음'), 
               tickvals = list(1, 2, 3),
               tickmode = "array"),
  zaxis = list(title = '여가지수')))
fig

######## 3D : 엥겔, 여가 , 배우자유무
x2 <- each$engel
y2 <- each$freetime
z3 <- each$배우자유무

fig <- plot_ly(x=x2, y=y2, z=z3, type="scatter3d", mode="markers", 
               showscale =FALSE, color=z3)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(
  zaxis = list(title = '배우자유무',
               ticktext = list('있음(동거)', '있음(비동거)', '없음'), 
               tickvals = list(1, 2, 3),
               tickmode = "array"),
  xaxis = list(title = '엥겔지수'),
  yaxis = list(title = '여가지수')))
fig


######## 3D : 엥겔, 여가 , 년차
z4 <- each$years

fig <- plot_ly(x=x2, y=y2, z=z4, type="scatter3d", mode="markers", color=x2)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(
  zaxis = list(title = '년차',
               ticktext = list('28 이하', '29~33', '34~39', 
                               '39~43', '44~48', '49 이상'), 
               tickvals = list(1, 2, 3,4,5,6),
               tickmode = "array"),
  xaxis = list(title = '엥겔지수'),
  yaxis = list(title = '여가지수')))
fig


######## 배우자유무, 년차, 지수평균
yw_mean <- read.csv("배우자유무 J평균(년차별).csv", head=T)

### engel
fig <- plot_ly(yw_mean,x = ~years, type = "bar", y = ~engel_w2, name = '있음(비동거)',
                marker = list(color = 'rgb(240, 154, 110)'))
fig <- fig %>% add_trace(y = ~engel_w1, name = '있음(동거)',
               marker = list(color = 'rgb(135, 153, 236)'))
fig <- fig %>% add_trace(y = ~engel_w3, name = '없음',
                         marker = list(color = 'rgb(5, 238, 236)'))
fig <- fig %>% layout(barmode = 'group',
                      xaxis = list(title = '년차',
                                   ticktext = list('28 이하', '29~33', '34~39', 
                                                   '39~43', '44~48', '49 이상'), 
                                   tickvals = list(1, 2, 3,4,5,6),
                                   tickmode = "array"),
                      yaxis = list(title = '엥겔지수'))
fig

### freetime
fig <- plot_ly(yw_mean,x = ~years, type = "bar", y = ~freetime_w2, name = '있음(비동거)',
               marker = list(color = 'rgb(240, 154, 110)'))
fig <- fig %>% add_trace(y = ~freetime_w1, name = '있음(동거)',
                         marker = list(color = 'rgb(135, 153, 236)'))
fig <- fig %>% add_trace(y = ~freetime_w3, name = '없음',
                         marker = list(color = 'rgb(5, 238, 236)'))
fig <- fig %>% layout(barmode = 'group',
                      xaxis = list(title = '년차',
                                   ticktext = list('28 이하', '29~33', '34~39', 
                                                   '39~43', '44~48', '49 이상'), 
                                   tickvals = list(1, 2, 3,4,5,6),
                                   tickmode = "array"),
                      yaxis = list(title = '여가지수'))
fig

### happy
yw_mean$happy_w2[5] <- 0
yw_mean$happy_w2
fig <- plot_ly(yw_mean,x = ~years, type = "bar", y = ~happy_w2, name = '있음(비동거)',
               marker = list(color = 'rgb(240, 154, 110)'))
fig <- fig %>% add_trace(y = ~happy_w1, name = '있음(동거)',
                         marker = list(color = 'rgb(135, 153, 236)'))
fig <- fig %>% add_trace(y = ~happy_w3, name = '없음',
                         marker = list(color = 'rgb(5, 238, 236)'))
fig <- fig %>% layout(barmode = 'group',
                      xaxis = list(title = '년차',
                                   ticktext = list('28 이하', '29~33', '34~39', 
                                                   '39~43', '44~48', '49 이상'), 
                                   tickvals = list(1, 2, 3,4,5,6),
                                   tickmode = "array"),
                      yaxis = list(title = '행복지수'))
fig

################################################################################  
#####소비 패턴(소비를 모두 더해서)
# 0. 소비지출에 영향을 끼치는 요인
library(psych)
#pairs.panels(each) 
each2 <- read.csv("가계동향조사_J개인_요약.csv", head=T)
model <- lm(소비지출 ~ food+sigar+cloth+life+household+health+traffic+culture
                telecom+edu+lodge+service, data = each2)
summary(model)


#### 년차별 주류,담배 > 0.05
plot_ly(each2, y = ~sigar, x = ~years, type = "box", name = '소비지출')
sigars <- each2 %>%  filter(sigar < 6000)
plot_ly(paying, y = ~소비지출, x = ~years, type = "box", name = '소비지출')
tapply(sigars$sigar, sigars$years, shapiro.test)
bartlett.test(sigars$sigar, sigars$years)

length(sigars$sigar)
sigar.lm <- lm(sigar ~ years, data=sigars)
anova(sigar.lm)
oneway.test(sigar ~ years, data=sigars, var.equal=FALSE)

##### 년차별 복권 > 0.05
plot_ly(each2, y = ~insurance, x = ~years, type = "box", name = '소비지출')
insurance <- each2 %>%  filter(insurance < 150000)
plot_ly(insurance, y = ~insurance, x = ~years, type = "box", name = '소비지출')
tapply(insurance$insurance, insurance$years, shapiro.test)
bartlett.test(insurance$insurance, insurance$years)

oneway.test(insurance ~ years, data=insurance, var.equal=FALSE)

model <- aov(insurance ~ years, data=insurance)
comparison <- LSD.test(model, "years", p.adj="bonferroni", group=T)
comparison

##### 년차별 의류 < 0.05
plot_ly(each2, y = ~cloth, x = ~years, type = "box")
cloth <- each2 %>%  filter(cloth < 500000)
plot_ly(cloth, y = ~cloth, x = ~years, type = "box")
tapply(cloth$cloth, cloth$years, shapiro.test)
bartlett.test(cloth$cloth, cloth$years)

oneway.test(cloth ~ years, data=cloth, var.equal=FALSE)

model <- aov(cloth ~ years, data=cloth)
comparison <- LSD.test(model, "years", p.adj="bonferroni", group=T)
comparison

##### 년차별 교육 < 0.05
plot_ly(each2, y = ~edu, x = ~years, type = "box")
edu <- each2 %>%  filter(edu < 600000)
plot_ly(edu, y = ~edu, x = ~years, type = "box")
tapply(edu$edu, edu$years, shapiro.test)
bartlett.test(edu$edu, edu$years)

oneway.test(edu ~ years, data=edu, var.equal=FALSE)


##### 년차별 오락,문화 > 0.05
plot_ly(each2, y = ~culture, x = ~years, type = "box")
culture <- each2 %>%  filter(culture < 300000)
plot_ly(culture, y = ~culture, x = ~years, type = "box")
tapply(culture$culture, culture$years, shapiro.test)
bartlett.test(culture$culture, culture$years)

oneway.test(culture ~ years, data=culture, var.equal=FALSE)


##### 년차별 주거·수도·광열 < 0.05
plot_ly(each2, y = ~life, x = ~years, type = "box")
life <- each2 %>%  filter(life < 600000)

tapply(life$life, life$years, shapiro.test)
bartlett.test(life$life, life$years)

oneway.test(life ~ years, data=life, var.equal=FALSE)
plot_ly(life, y = ~life, x = ~years, type = "box")


##### 년차별 가정용품 · 가사서비스 > 0.05
plot_ly(each2, y = ~household, x = ~years, type = "box")
household <- each2 %>%  filter(household < 300000)

tapply(household$household, household$years, shapiro.test)
bartlett.test(household$life, household$years)

oneway.test(household ~ years, data=household, var.equal=FALSE)
plot_ly(household, y = ~household, x = ~years, type = "box")


##### 년차별 보건 > 0.05 : 0.051
plot_ly(each2, y = ~health, x = ~years, type = "box")
health <- each2 %>%  filter(health < 400000)

tapply(health$health, health$years, shapiro.test)
bartlett.test(health$life, health$years)

oneway.test(health ~ years, data=health, var.equal=FALSE)
plot_ly(health, y = ~health, x = ~years, type = "box")


##### 년차별 교통 > 0.05
plot_ly(each2, y = ~traffic, x = ~years, type = "box")
traffic <- each2 %>%  filter(traffic < 300000)

tapply(traffic$traffic, traffic$years, shapiro.test)
bartlett.test(traffic$traffic, traffic$years)

oneway.test(traffic ~ years, data=traffic, var.equal=FALSE)
plot_ly(traffic, y = ~traffic, x = ~years, type = "box")


##### 년차별 통신 < 0.05
plot_ly(each2, y = ~telecom, x = ~years, type = "box")
telecom <- each2 %>%  filter(telecom < 200000)

tapply(telecom$telecom, telecom$years, shapiro.test)
bartlett.test(telecom$telecom, telecom$years)

oneway.test(telecom ~ years, data=telecom, var.equal=FALSE)
plot_ly(telecom, y = ~telecom, x = ~years, type = "box")


##### 년차별 식비 < 0.05
plot_ly(each2, y = ~food, x = ~years, type = "box")

tapply(each2$food, each2$years, shapiro.test)
bartlett.test(each2$food, each2$years)

oneway.test(food ~ years, data=each2, var.equal=FALSE)
fig <- plot_ly(each2, y = ~food, x = ~years, type = "box")
fig <- fig %>% layout(xaxis = list(title = '년차'),
                      yaxis = list(title = '식료품.음료 지출'))
fig


##### 년차별 외식.숙박 < 0.05
plot_ly(each2, y = ~lodge, x = ~years, type = "box")

tapply(each2$lodge, each2$years, shapiro.test)
bartlett.test(each2$lodge, each2$years)

oneway.test(lodge ~ years, data=each2, var.equal=FALSE)
fig <- plot_ly(each2, y = ~lodge, x = ~years, type = "box")
fig <- fig %>% layout(xaxis = list(title = '년차'),
                      yaxis = list(title = '외식.숙박 지출'))
fig


##### 년차별 기타 서비스 > 0.05
plot_ly(each2, y = ~service, x = ~years, type = "box")

service <- each2 %>% filter(service < 335000)

tapply(service$service, service$years, shapiro.test)
bartlett.test(service$service, service$years)

service.lm <- lm(service ~ years, data=service)
anova(service.lm)

oneway.test(service ~ years, data=service, var.equal=FALSE)

fig <- plot_ly(service, y = ~service, x = ~years, type = "box")
fig <- fig %>% layout(xaxis = list(title = '년차'),
                      yaxis = list(title = '외식.숙박 지출'))
fig


####### 1. 년차별 소비 패턴 분석
### 소비지출 > 0.05
plot_ly(each, y = ~소비지출, x = ~years, type = "box", name = '소비지출')
paying <- each %>%  filter(소비지출 < 5000000)
plot_ly(paying, y = ~소비지출, x = ~years, type = "box", name = '소비지출')
tapply(paying$소비지출, paying$years, shapiro.test)
bartlett.test(paying$소비지출, paying$years)

oneway.test(소비지출 ~ years, data=paying, var.equal=FALSE)
model <- aov(소비지출 ~ years, data=paying)
comparison <- LSD.test(model, "years", p.adj="bonferroni", group=T)
comparison
#groupA <- subset(each, years==2)
#groupB <- subset(each, years==6)
#mean(groupA$freetime)
#mean(groupB$freetime)
#mean(engel_hp$freetime)

fig <- plot_ly(yw_mean,x = ~years, type = "bar", y = ~happy_w2, name = '있음(비동거)',
               marker = list(color = 'rgb(240, 154, 110)'))
fig <- fig %>% add_trace(y = ~happy_w1, name = '있음(동거)',
                         marker = list(color = 'rgb(135, 153, 236)'))
fig <- fig %>% add_trace(y = ~happy_w3, name = '없음',
                         marker = list(color = 'rgb(5, 238, 236)'))
fig <- fig %>% layout(barmode = 'stack',
                      xaxis = list(title = '년차',
                                   ticktext = list('28 이하', '29~33', '34~39', 
                                                   '39~43', '44~48', '49 이상'), 
                                   tickvals = list(1, 2, 3,4,5,6),
                                   tickmode = "array"),
                      yaxis = list(title = '행복지수'))
fig


################################################################################
# J군 연차별 평균 소비
paying_yjmean <- read.csv("소비패턴 J평균(년차별).csv", head=T)

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "행복지수"
)

fig <- plot_ly(paying_yjmean, x = ~years, y = ~engel, name = '엥겔지수', type = "bar",
               marker = list(color = 'rgb(135, 153, 236)'))
fig <- fig %>% add_trace(y = ~freetime, name = '여가지수',
                         marker = list(color = 'rgb(5, 238, 236)'))
fig <- fig %>% add_trace(paying_yjmean, x = ~years, y = ~happy, name = '행복지수', 
                         type = 'scatter', mode = 'lines', yaxis = "y2",
                         marker = list(color = 'rgb(240, 154, 110)'))
fig <- fig %>% layout(barmode = 'group',
                      xaxis = list(title = '년차',
                                   ticktext = list('28 이하', '29~33', '34~39', 
                                                   '39~43', '44~48', '49 이상'), 
                                   tickvals = list(1, 2, 3,4,5,6),
                                   tickmode = "array"),
                      yaxis = list(title = '엥겔.여가 지수'),
                      yaxis2 = ay)
fig
