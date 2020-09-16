data <- read.csv('money1.csv', header=TRUE)

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
                      legend = list(x = 0.5, y = 1.08,
                                    font = list(size = 12),
                                    orientation = 'h', xanchor = "center", 
                                    traceorder='normal')) 
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

