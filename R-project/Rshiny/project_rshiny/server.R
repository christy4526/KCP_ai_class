library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(extrafont)
library(showtext)
library(tidyverse)
library(shinythemes)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(plotly)

P_merge <- read.csv("P_merge.csv", header = TRUE) #시각화할 데이터셋
gu_name <- read.csv('gu_name.csv', header = TRUE)
test5 <- read.csv("test5.csv", header = TRUE)
test1015 <- read.csv("test1015.csv", header = TRUE)
hi1 <- ca(test5)
hi2 <- ca(test1015)
data <- read.csv('money1.csv', header=TRUE)
data2 <- read.csv('years1.csv', header=TRUE)
years <- read.csv('years.csv', header = TRUE)
years_summary <-read.csv('years_summary.csv', header = TRUE)
ycar <- read.csv("년차별 자동차수_J개인.csv", head=TRUE)
home_mean2 <- read.csv("입주형태별 J평균.csv", head=TRUE)
yhome2 <- read.csv("년차별 입주형태_J개인.csv", head=TRUE)
yhome1 <- read.csv("년차별 거처구분_J개인.csv", head=TRUE)
yw_mean <- read.csv("배우자유무 J평균(년차별).csv", head=T)

years2 <- subset(years, 배우자유무 ==1 | 배우자유무 == 3)
years2$연차구분[years2$연차구분==1] <- '1년차 이하'
years2$연차구분[years2$연차구분==2] <- '1~5년차'
years2$연차구분[years2$연차구분==3] <- '6~10년차'
years2$연차구분[years2$연차구분==4] <- '11~15년차'
years2$연차구분[years2$연차구분==5] <- '16~20년차'
years2$연차구분[years2$연차구분==6] <- '21년차 이상'

years2$배우자유무[years2$배우자유무==3] <- 2
years2$배우자유무[years2$배우자유무==1] <- 1
years2$배우자유무[years2$배우자유무==2] <- '2:없음'
years2$배우자유무[years2$배우자유무==1] <- '1:있음'

shinyServer(function (input, output) {
  output$plot <-renderPlot({                           
  if(input$list == "식료품 품목 개수"){
    ggplot() + 
      geom_polygon(data = P_merge, 
                   aes(x = long, 
                       y = lat, 
                       group = group, 
                       fill = grocery),
                   color = "white")  +
      scale_fill_gradient(low = "#FBCF61",
                          high = "#00CC99",
                          space = "Lab",
                          guide = "colourbar") + 
      labs(fill = "구별 식료품 품목 수") +
      theme_void() +
      theme(legend.position = c(.18, .82)) +
      geom_text(data = gu_name,
                aes(x = long,
                    y = lat,
                    label = paste(location, grocery, sep = "\n")))
  }
  else if(input$list == "여가시설 개수"){
    ggplot() + 
      geom_polygon(data = P_merge, 
                   aes(x = long, 
                       y = lat, 
                       group = group, 
                       fill = leisure),
                   color = "white")  +
      scale_fill_gradient(low = "antiquewhite2",
                          high = "cyan3",
                          space = "Lab",
                          guide = "colourbar") + 
      labs(fill = "구별 문화공간 수") +
      theme_void() +
      theme(legend.position = c(.18, .82)) +
      geom_text(data = gu_name,
                aes(x = long,
                    y = lat,
                    label = paste(location, leisure, sep = "\n")))
  }
  else{
    ggplot() + 
      geom_polygon(data = P_merge, 
                   aes(x = long, 
                       y = lat, 
                       group = group, 
                       fill = rank_happy),
                   color = "white")  +
      scale_fill_gradient(low = "Hot Pink 2",
                          high = "#FFDFD3",
                          space = "Lab",
                          guide = "colourbar") + 
      labs(fill = "구별 행복지수 순위") +
      theme_void() +
      theme(legend.position = c(.18, .82)) +
      geom_text(data = gu_name,
                aes(x = long,
                    y = lat,
                    label = paste(location, rank_happy, sep = "\n")))
  }
  }
  ,height = 700, width =850)
  
  output$plot1 <-renderPlot({                           
      plot(hi1, mass = TRUE, contrib = "absolute", map = "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map
    }
    ,height = 700, width =850)
  
  output$plot3 <-renderRglwidget({                           
    rgl.open(useNULL=T)
    bg3d(color = "white")
    plot3d.ca(hi2)
    rglwidget()

  })
  
  output$plot2 <-renderPlotly({                           
    if(input$list_money == "산업별 소득구간"){
      
      top_labels <- c('200만원 미만', '200~300만원', '300~400만원', '400~500만원', '500~600만원','600만원 이상')
      
      fig <- plot_ly(data, x = ~X1, y = ~inderstry, type = 'bar', orientation = 'h',
                     marker = list(color = 'rgba(38, 24, 74, 0.8)',
                                   line = list(color = 'rgb(248, 248, 249)', width = 1)), name = '200만원 미만') 
      fig <- fig %>% add_trace(x = ~X2, marker = list(color = 'rgba(71, 58, 131, 0.8)'), name = '200~300만원') 
      fig <- fig %>% add_trace(x = ~X3, marker = list(color = 'rgba(122, 120, 168, 0.8)'), name = '300~400만원') 
      fig <- fig %>% add_trace(x = ~X4, marker = list(color = 'rgba(164, 163, 204, 0.85)'), name = '400~500만원') 
      fig <- fig %>% add_trace(x = ~X5, marker = list(color = 'rgba(190, 192, 213, 1)'), name = '500~600만원') 
      fig <- fig %>% add_trace(x = ~X6, marker = list(color = 'rgba(214, 108, 232, 1)'), name = '600만원 이상') 
      fig <- fig %>% layout(xaxis = list(title = "",
                                         showgrid = FALSE,
                                         showline = FALSE,
                                         showticklabels = FALSE,
                                         zeroline = FALSE,
                                         domain = c(0.15, 1)),
                            yaxis = list(title = "",
                                         showgrid = FALSE,
                                         showline = FALSE,
                                         showticklabels = FALSE,
                                         zeroline = FALSE),
                            barmode = 'stack',
                            paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
                            margin = list(l = 120, r = 10, t = 140, b = 80),
                            showlegend = TRUE,
                            legend = list(x = 0.55, y = 1.08,
                                          font = list(size = 10),
                                          orientation = 'h', xanchor = "center", traceorder='normal')) 
      # labeling the y-axis
      fig <- fig %>% add_annotations(xref = 'paper', yref = 'inderstry', x = 0.14, y = data$inderstry,
                                     xanchor = 'right',
                                     text = data$inderstry,
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(67, 67, 67)'),
                                     showarrow = FALSE, align = 'right') 
      # labeling the percentages of each bar (x_axis)
      fig <- fig %>% add_annotations(xref = 'x', yref = 'inderstry',
                                     x = data$X1 / 2, y = data$inderstry,
                                     text = paste(data[,"X1"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig <- fig %>% add_annotations(xref = 'x', yref = 'inderstry',
                                     x = data$X1 + data$X2 / 2, y = data$inderstry,
                                     text = paste(data[,"X2"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig <- fig %>% add_annotations(xref = 'x', yref = 'inderstry',
                                     x = data$X1 + data$X2 + data$X3 / 2, y = data$inderstry,
                                     text = paste(data[,"X3"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig <- fig %>% add_annotations(xref = 'x', yref = 'inderstry',
                                     x = data$X1 + data$X2 + data$X3 + data$X4 / 2, y = data$inderstry,
                                     text = paste(data[,"X4"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig <- fig %>% add_annotations(xref = 'x', yref = 'inderstry',
                                     x = data$X1 + data$X2 + data$X3 + data$X4 + data$X5 / 2, y = data$inderstry,
                                     text = paste(data[,"X5"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig <- fig %>% add_annotations(xref = 'x', yref = 'inderstry',
                                     x = data$X1 + data$X2 + data$X3 + data$X4 + data$X5 + data$X6 / 2, y = data$inderstry,
                                     text = paste(data[,"X6"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig
    }
    else if(input$list_money == "연차별 소득구간"){
      
      fig2 <- plot_ly(data2, x = ~X1, y = ~years, type = 'bar', orientation = 'h',
                     marker = list(color = 'rgba(38, 24, 74, 0.8)',
                                   line = list(color = 'rgb(248, 248, 249)', width = 1)), name = '200만원 미만') 
      fig2 <- fig2 %>% add_trace(x = ~X2, marker = list(color = 'rgba(71, 58, 131, 0.8)'), name = '200~300만원') 
      fig2 <- fig2 %>% add_trace(x = ~X3, marker = list(color = 'rgba(122, 120, 168, 0.8)'), name = '300~400만원') 
      fig2 <- fig2 %>% add_trace(x = ~X4, marker = list(color = 'rgba(164, 163, 204, 0.85)'), name = '400~500만원') 
      fig2 <- fig2 %>% add_trace(x = ~X5, marker = list(color = 'rgba(190, 192, 213, 1)'), name = '500~600만원') 
      fig2 <- fig2 %>% add_trace(x = ~X6, marker = list(color = 'rgba(224, 128, 202, 1)'), name = '600만원 이상') 
      fig2 <- fig2 %>% layout(xaxis = list(title = "",
                                         showgrid = FALSE,
                                         showline = FALSE,
                                         showticklabels = FALSE,
                                         zeroline = FALSE,
                                         domain = c(0.15, 1)),
                            yaxis = list(title = "",
                                         showgrid = FALSE,
                                         showline = FALSE,
                                         showticklabels = FALSE,
                                         zeroline = FALSE),
                            barmode = 'stack',
                            paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
                            margin = list(l = 120, r = 10, t = 140, b = 80),
                            showlegend = TRUE,
                            legend = list(x = 0.55, y = 1.08,
                                          font = list(size = 10),
                                          orientation = 'h', xanchor = "center", traceorder='normal')) 
      # labeling the y-axis
      fig2 <- fig2 %>% add_annotations(xref = 'paper', yref = 'years', x = 0.14, y = data2$years,
                                     xanchor = 'right',
                                     text = data2$years,
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(67, 67, 67)'),
                                     showarrow = FALSE, align = 'right', decreasing = TRUE) 
      # labeling the percentages of each bar (x_axis)
      fig2 <- fig2 %>% add_annotations(xref = 'x', yref = 'years',
                                     x = data2$X1 / 2, y = data2$years,
                                     text = paste(data2[,"X1"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig2 <- fig2 %>% add_annotations(xref = 'x', yref = 'years',
                                     x = data2$X1 + data2$X2 / 2, y = data2$years,
                                     text = paste(data2[,"X2"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig2 <- fig2 %>% add_annotations(xref = 'x', yref = 'years',
                                     x = data2$X1 + data2$X2 + data2$X3 / 2, y = data2$years,
                                     text = paste(data2[,"X3"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig2 <- fig2 %>% add_annotations(xref = 'x', yref = 'years',
                                     x = data2$X1 + data2$X2 + data2$X3 + data2$X4 / 2, y = data2$years,
                                     text = paste(data2[,"X4"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig2 <- fig2 %>% add_annotations(xref = 'x', yref = 'years',
                                     x = data2$X1 + data2$X2 + data2$X3 + data2$X4 + data2$X5 / 2, y = data2$years,
                                     text = paste(data2[,"X5"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      fig2 <- fig2 %>% add_annotations(xref = 'x', yref = 'years',
                                     x = data2$X1 + data2$X2 + data2$X3 + data2$X4 + data2$X5 + data2$X6 / 2, y = data2$years,
                                     text = paste(data2[,"X6"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = 'rgb(248, 248, 255)'),
                                     showarrow = FALSE) 
      
      fig2
      
    }
    }
    )
  
  
  output$plot4 <-renderPlotly({    
    if(input$list_years == "연차별_엥겔_여가"){
   
    x <- years2$engel
    y <- years2$freetime
    z <- as.character(years2$연차구분)
    plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers", color=z )
    
    }
    else if(input$list_years == "연차별_배우자유무_여가"){
      
      x <- years2$배우자유무
      y <- years2$freetime
      z <- as.character(years2$연차구분)
      plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers", color=z )
    }
    else{
      
      x <- years2$engel
      y <- years2$happy
      z <- years2$배우자유무
      plot_ly(x=x, y=y, z=z, type="scatter3d", mode="markers", color=z )
      
    }
    })

  output$plot5 <-renderPlotly({    
    if(input$list_all == "입주형태별 복권구매"){
      
      fig3 <- plot_ly(years, y = ~복권, x = ~입주형태, type = 'scatter', 
                     mode = 'markers', size = ~복권, color = ~입주형태, colors = 'Paired')
      fig3 <- fig3 %>% layout(barmode = 'group',xaxis = list(title = '입주형태'),
                            yaxis = list(title = '복권'))
      fig3
      
    }
    else if(input$list_all == "연차별 평균엥겔지수"){
      
      plot_ly(diamonds, y = ~years$engel, x = ~years$years, type = "bar",
              color = ~years$years)
      
    }
    else if(input$list_all == "연차별 평균여가지수"){
      
      plot_ly(diamonds, y = ~years$freetime, x = ~years$years, type = "bar",
              color = ~years$years)
      
    }
    else if(input$list_all == "연차별 비율 그래프"){
      
      ylabel <- c('28세 이하', '29세~33세', '34세~38세', '39세~43세', '44세~48세', '49세 이상')
      ynums <- c(41, 43, 47, 34, 40, 39)
      
      fig4 <- plot_ly(years, labels = ~ylabel, values = ~ynums, type = 'pie',
                      marker = list(color = 'rgba(38, 24, 74, 0.8)',
                                    width = 1))
      fig4
    }
    else if(input$list_all == "입주형태별 평균지수들"){
      
      fig5 <- plot_ly(home_mean2, x = ~htype, y = ~engel, type = 'bar', name = '엥겔지수',
                      marker = list(color = 'rgb(135, 153, 236)'))
      fig5 <- fig5 %>% add_trace(y = ~freetime, name = '여가지수',
                                 marker = list(color = 'rgb(5, 238, 236)'))
      #fig2 <- fig2 %>% add_trace(y = ~happy, name = '행복지수')
      fig5 <- fig5 %>% layout(barmode = 'group',
                              xaxis = list(title = '입주형태',
                                           ticktext = list('자기집', '무상주택', '전세', 
                                                           '영구임대', '보증부 월세', 
                                                           '보증금 없는 월세'), 
                                           tickvals = list(1, 2, 3, 4, 5, 6),
                                           tickmode = "array"),
                              yaxis = list(title = '지수'))
      fig5
      
    }
    else if(input$list_all == "연차별 자동차 수"){
      
      # 자동차 보유댓수
      clabel <- c('0대', '1대', '2대', '3대')

      
      fig6 <- plot_ly(years, x = ~ycar[1], y = ~ycar$X3, type = 'bar', name = clabel[4])
      fig6 <- fig6 %>% add_trace(y = ~ycar$X2, name = clabel[3])
      fig6 <- fig6 %>% add_trace(y = ~ycar$X1, name = clabel[2])
      fig6 <- fig6 %>% add_trace(y = ~ycar$X0, name = clabel[1])
      fig6 <- fig6 %>% layout(xaxis = list(title = '년차', range(1,6)),
                              yaxis = list(title = '자동차 수'),
                              barmode = 'stack')
      fig6
    }
    else if(input$list_all == "연차별 교육소비"){
      
      edu <- years_summary %>%  filter(edu < 600000)
      plot_ly(edu, y = ~edu, x = ~years, type = "box")
    }
    
    else if(input$list_all == "년차 및 배우자별 행복지수"){
      
      yw_mean$happy_w2[5] <- 0
      yw_mean$happy_w2
      fig8 <- plot_ly(yw_mean,x = ~years, type = "bar", y = ~happy_w2, name = '있음(비동거)',
                     marker = list(color = 'rgb(240, 154, 110)'))
      fig8 <- fig8 %>% add_trace(y = ~happy_w1, name = '있음(동거)',
                               marker = list(color = 'rgb(135, 153, 236)'))
      fig8 <- fig8 %>% add_trace(y = ~happy_w3, name = '없음',
                               marker = list(color = 'rgb(5, 238, 236)'))
      fig8 <- fig8 %>% layout(barmode = 'group',
                            xaxis = list(title = '년차',
                                         ticktext = list('28 이하', '29~33', '34~39', 
                                                         '39~43', '44~48', '49 이상'), 
                                         tickvals = list(1, 2, 3,4,5,6),
                                         tickmode = "array"),
                            yaxis = list(title = '행복지수'))
      fig8
      
    }
    
    else{
      
      h2label <- c('자기집', '무상주택', '사택', '전세', '영구임대', 
                   '보증부 월세', '보증금 없는 월세')
      
      fig7 <- plot_ly(years, x = ~yhome2[1], y = ~yhome1$X1, type = 'bar', name = h2label[1])
      fig7 <- fig7 %>% add_trace(y = ~yhome2$X2, name = h2label[2])
      fig7 <- fig7 %>% add_trace(y = ~yhome2$X3, name = h2label[3])
      fig7 <- fig7 %>% add_trace(y = ~yhome2$X4, name = h2label[4])
      fig7 <- fig7 %>% add_trace(y = ~yhome2$X5, name = h2label[5])
      fig7 <- fig7 %>% add_trace(y = ~yhome2$X6, name = h2label[6])
      fig7 <- fig7 %>% add_trace(y = ~yhome2$X7, name = h2label[7])
      fig7 <- fig7 %>% layout(xaxis = list(title = '년차', range(1,6)),
                            yaxis = list(title = '수'),
                            barmode = 'stack')
      fig7
      
      }
  
  })
})
