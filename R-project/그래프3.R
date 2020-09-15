################################################################################
################################################################################
# 평균의 위와 아래에 따른 분석
# TODO
paying_jmean <- read.csv("소비 J평균.csv", head=T)
engel_tmean <- data.frame(happy=c(91.26),
                          freetime=c(7.54), 
                          engel=c(12.33))
engel.up <- engel.j %>% filter()
engel.up <- subset(engel.j, engel >= engel_tmean$engel, )
engel.dw <- 
  
  ### 3d 그래프  
  x <- each$engel
y <- each$freetime
z <- each$배우자유무

plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers", color=z)