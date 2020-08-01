library(shiny)

vid1 <- 'https://www.youtube.com/embed/_-Zqz75r9PQ'

fluidPage(
  titlePanel(
    'Efficient Frontier and Asset Allocation'
  ),
  sidebarPanel(
    selectizeInput(inputId = 'abc',
                   label = 'Index_1',
                   choices = c('US_Stocks', 'US_Bonds', 'Euro_Stocks',
                               'Asia_Stocks', 'IG_Bonds', 'HY_Bonds'),
                   selected = 'US_Stocks'),
    selectizeInput(inputId = 'def',
                   label = 'Index_2',
                   choices = c('US_Stocks', 'US_Bonds', 'Euro_Stocks',
                               'Asia_Stocks', 'IG_Bonds', 'HY_Bonds'),
                   selected = 'US_Bonds'),
    h3('Purpose'),
    p('The goal is to determine the appropriate asset allocation in a hypothetical portfolio
      made up of two risky assets. Using index returns provided by Vanguard over a thirty year
      period, we can assess the relationship between expected return and standard deviation 
      for a range of asset classes within equities and fixed income.'), 
    h3('Background'),
    p('In modern portfolio theory, the efficient frontier is an investment portfolio which 
      occupies the efficient parts of the risk-return spectrum. Formally, it is the set of 
      portfolios which satisfy the condition that no other portfolio exists with a higher 
      expected return but with the same standard deviation of return. The efficient frontier 
      was first formulated by Harry Markowitz in 1952.'),
  ),
  mainPanel(
    h3('Portfolio Optimization'),
    plotOutput('frontier', width = '100%'),
    plotOutput('bargraph', width = '100%'),
    h3('The Basics of Investing'),
    tags$iframe(src = vid1 , width = '560', height = '315')
    )
)