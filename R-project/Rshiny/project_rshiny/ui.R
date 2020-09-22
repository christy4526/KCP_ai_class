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
library(extrafont)
library(showtext)
library(rsconnect)
library(tidyverse)
library(shinythemes)
library(devtools)
library(ca)
library(rgl)

ui <- fluidPage(theme = shinytheme("lumen"),    #테마 설정
                mainPanel(
                  tabsetPanel(
                    tabPanel("서울에서 나의 행복을 찾아보자",
                             br(),
                             fluidRow(                         #행 구분
                               selectInput(
                                 inputId="list",
                                 label="List:",
                                 choices=c("식료품 품목 개수", "여가시설 개수", "행복지수")
                               )
                             ),
                             br(),
                             plotOutput('plot')),
                    
                    tabPanel("소비지출로 알아본 산업별 이미지(2D)",
                             br(),
                             br(),
                             plotOutput('plot1')),
                    
                    tabPanel("소비지출로 알아본 산업별 이미지(3D)",
                             br(),
                             br(),
                             rglwidgetOutput("plot3",  width = 800, height = 600)),
                    
                    tabPanel("산업별 소득구간",
                             br(),
                             fluidRow(                        
                               selectInput(
                                 inputId="list_money",
                                 label="List:",
                                 choices=c("산업별 소득구간", "연차별 소득구간")
                               )
                             ),
                             br(),
                             plotlyOutput('plot2', height = '700', width = '850')),
                    
                    tabPanel("연차_배우자유무와 지수들의 상관관계(3D)",
                             br(),
                             fluidRow(                        
                               selectInput(
                                 inputId="list_years",
                                 label="List:",
                                 choices=c("연차별_엥겔_여가", "연차별_배우자유무_여가", "배우자유무_엥겔_행복")
                               )
                             ),
                             br(),
                             plotlyOutput('plot4', height = '700', width = '850')),
                    
                    tabPanel("가계지출소득으로 알아본 것들",
                             br(),
                             fluidRow(                        
                               selectInput(
                                 inputId="list_all",
                                 label="List:",
                                 choices=c("입주형태별 복권구매", 
                                           "연차별 평균엥겔지수", 
                                           "연차별 평균여가지수",
                                           "연차별 비율 그래프",
                                           "입주형태별 평균지수들",
                                           "연차별 자동차 수",
                                           "연차별 교육소비",
                                           "년차 및 배우자별 행복지수",
                                           "연차별 입주형태")
                               )
                             ),
                             br(),
                             plotlyOutput('plot5', height = '700', width = '850'))
                    )
                  )
                )

