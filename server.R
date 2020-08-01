library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

function(input, output){
  
  newgraph1 <- reactive({
    x = input$abc
    y = input$def
    
    xlist <- c('US_Stocks', 'US_Bonds', 'Euro_Stocks',
               'Asia_Stocks', 'IG_Bonds', 'HY_Bonds')
    ylist <- c(1:6)
    zlist <- as.data.frame(cbind(xlist, ylist))
    
    list1 <- zlist %>% filter(xlist == x)
    list2 <- zlist %>% filter(xlist == y)
    
    x = as.numeric(list1$ylist)
    y = as.numeric(list2$ylist)
    
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
    x = input$abc
    y = input$def
    allocation <- allocation %>% mutate(newstockval = newstockval * 100,
                                        newbondval = newbondval * 100,
                                        newEUROval = newEUROval * 100,
                                        newASIAval = newASIAval * 100,
                                        newIGval = newIGval * 100,
                                        newHYval = newHYval *100)
    colnames(allocation) <- c('year', 'US_Stocks','US_Bonds', 'Euro_Stocks',
                              'Asia_Stocks', 'IG_Bonds', 'HY_Bonds')
    newbargraph <- allocation %>% select(year, x, y)
    colnames(newbargraph) <- c('year', 'x', 'y')
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
      geom_bar(aes(x = year, y = x, fill = 'Index_1'),
             stat = 'identity', width = 0.3, position = 'dodge') +
      geom_bar(aes(x = year, y = y, fill = 'Index_2'),
            stat = 'identity', width = 0.3, position = position_nudge(x = 0.3)) +
      labs(title = 'Historical Performance',
           y = 'Returns') +
      theme_bw()
  )
  
  output$statistics <- renderTable(newalloc)
  output$allnumbers <- renderTable(table)
}