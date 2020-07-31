library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

function(input, output){
  
  newgraph1 <- reactive({
    x = as.numeric(input$abc)
    y = as.numeric(input$def)
    
    table <- as.data.frame(seq(1:101))
    colnames(table) <- 'weight'
    table <- table %>% mutate(weight = (weight - 1)/100)
    corr <- -0.8
    table <- table %>% mutate(stdev = standard(weight, corr, 
                                               newalloc$SD[x], newalloc$SD[y]), 
                              expreturn = expected(weight,
                                                   newalloc$ER[x], newalloc$ER[y]))
    
    table <- table %>% mutate(weight = weight*100, stdev = stdev*100,
                              expreturn = expreturn*100)
    return(table)
  })
  
  newgraph2 <- reactive({
    x = as.numeric(input$abc) + 1 
    y = as.numeric(input$def) + 1
    allocation <- allocation %>% mutate(newstockval = newstockval * 100,
                                        newbondval = newbondval * 100,
                                        newEUROval = newEUROval * 100,
                                        newASIAval = newASIAval * 100,
                                        newIGval = newIGval * 100,
                                        newHYval = newHYval *100)
    colnames(allocation) <- c('year', 1:6)
    newbargraph <- allocation %>% select(year, x, y)
    colnames(newbargraph) <- c('year', 'x', 'y')
    print(allocation)
    return(newbargraph)
  })
  
  output$frontier <- renderPlot(
    newgraph1() %>%
      ggplot(aes(x = stdev, y = expreturn, colour = weight)) + 
      geom_point() +
      labs(title = 'Efficient Frontier', 
           x = 'Standard Deviation',
           y = 'Expected Return') +
      theme_bw()
  )
  
  output$bargraph <- renderPlot(
    newgraph2() %>%
      ggplot() +
      geom_bar(aes(x = year, y = x, fill = 'stocks'),
             stat = 'identity', width = 0.3, position = 'dodge') +
      geom_bar(aes(x = year, y = y, fill = 'bonds'),
            stat = 'identity', width = 0.3, position = position_nudge(x = 0.3)) +
      labs(title = 'Historical Performance',
           y = 'Returns') +
      theme_bw()
  )
  
  output$statistics <- renderTable(newalloc)
  output$allnumbers <- renderTable(table)
}