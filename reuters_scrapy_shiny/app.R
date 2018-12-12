#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
#library(lubridate)
#library(anytime)
library(shinydashboard)
library(plotly)
library(slickR)
#library(timevis)
#library(googleVis)
#library(DT)
#data for shiny app
eurreuters = read_csv("final_reuters_chart_data.csv")
data_table = read_csv("reuters_plotly_no_post.csv")
eurreuters[eurreuters$Date == "2015-01-15",]['post.y'] = "Hedge funds, speculators face big losses on Swiss franc rally<br>Swiss central bank stuns market with policy U-turn<br>Euro floored as SNB decision puts spotlight on ECB, franc surges<br>Weak oil dictates mutual fund winners and losers in 2014<br>Lagarde said Swiss central bank did not warn IMF<br>Swiss shares post biggest one-day fall since 1989 on franc furore<br>Commodities traders' costs soar with Swiss franc's leap<br>Oil settles down despite promising mid-day rally<br>Swiss franc soars after SNB drops cap on currency<br>Franc, European shares soar after Swiss currency cap lifted<br>Who wants to buy a hotel? Swiss Alps in shock over franc's rise<br>Capital flows to emerging markets to fall in 2015 for second straight year<br>Swiss banks hammered as earnings face hit from currency surge<br>Soaraway Swiss franc threatens downturn and deflation<br>ABB sees limited impact of strong Swiss franc against the euro<br>Pending deals seen unaffected by Swiss franc leap<br>IG forecasts up to 30 million sterling hit from soaring Swiss franc<br>Factbox - Why scrapping the Swiss currency cap matters"

# Define UI for application that draws a histogram
ui <-  navbarPage(
  title = 'An Exploration of FX',
  id = 'nav',
  theme = shinytheme('spacelab'),
  #cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, simplex, slate, spacelab, superhero, united, yeti.
  ###################### CHART APP 4 tabPanel #############################  
  tabPanel('The Fundamentals',
           fluidRow(
             column(12, 
                    plotlyOutput("plot")
             )
           ),
           
           fluidRow(
             column(12, 
                    #h3('Interactive LDA:'),#,
                    h4('Reuters Articles with NLP:'), hr(),
                    DT::dataTableOutput('x11'))), hr()
  ),
  ################################################################ END Chart app 4 tabpanel
  tabPanel('Currency Correlations',
           fluidRow(
             column(12, 
                    h3('The Euro, Swiss Franc, Yen, and Gold from 2007-2018:'),br(),
                    img(src='cc1.png', align = "center"),br(),
                    h3("Notice any correlations in their movement and direction?"),br(),
                    h3("When looking at a large time frame it's hard to tell. Here's a '07 - '18 scatterplot of USDCHF and EURUSD minute bar close prices:"),br(),
                    img(src='cc2.png', align = "center"),br(),
                    h3("It's not a Rorschach test. The instruments don't always walk in tandem, but when they do..."),br(),
                    img(src='cc3.png', align = "center"),br(),
                    h3("This is the same two pair in 2014. It is a part of the '07-'18 Rorschach plot."),br(),
                    h3("Here are the currency correlation values over the years:"),br(),
                    slickROutput("cc", width="80%"),br() #install.packages("slickR")
            )
           )
  ),
  ################################################################ END Chart app 4 tabpanel
  tabPanel('Technical Indicator Correlations',
           fluidRow(
             column(12, 
                    h3('Here are the corrplots of different technical indicators for 9 currency pairs, calculated from minute bar data:'),br(),
                    slickROutput("tic", width="80%"),br(), #install.packages("slickR")
                    h3("This is the data that will be used to predict the pip movement (range) of EURUSD minute bars."),
                    tags$p("Range is the distance between High and Low for each bar.
                           This does not distinguish between Up and Down bars.
                           Through statistical analysis of minute bar movement, whether a bar moves Up/Down is very close to 50/50.
                           Body, TopWick, and BotWick are the measurements of the minute candlestick.
                           Force is Volume*Range. ROC, MOM, ATR, and BB (Bollinger Bands) are different technical indicators.
                           If you are interested in Technical Indicators then look no further than J. Welles Wilder Jr."),
                    h3('And just for fun here are two corrplots of all the indicators:'),br(),
                    img(src='tic1.png', align = "center"),br(),
                    img(src='tic2.png', align = "center"),br()
                    
                    )
           )
  ),
  ################################################################ END Chart app 4 tabpanel
  tabPanel('Minute Bar Range',
           fluidRow(
             column(12, 
                    h3('This is a histogram for the absolute value of EURUSD minute bar pip movement (range) from 2007-2018:'),br(),
                    img(src='mbr1.png', align = "center"),br(),
                    h3("One pip (one unit) for EURUSD is 0.0001. The spikes occur every half pip (0.00005). What's causing this? Algorithms, HFT, limit orders?"),br(),
                    h3("The same occurs for other currency pairs as well:"),br(),
                    img(src='mbr2.png', align = "center"),br(),
                    img(src='mbr3.png', align = "center"),br(),
                    h3("One pip for USDJPY is 0.01. When what I call the real ranges (positive and negative movements differentiated) of these minute bars are plotted:"),br(),
                    slickROutput("mbr", width="80%"),br()
             )
           )
  ),
  ################################################################ END Chart app 4 tabpanel  
  tabPanel('Support and Resistance',
           fluidRow(
             column(12, 
                    h3('Support and resistance lines are not arbitrary, sometimes they are precise to the very pip, but sometimes they represent a price range where bulls and bears tussle. I calculated resistance points from the most common Minute Bar High values within a range, and support values from the most commonly occuring Minute Bar Low price values, through the past decade:'),br(),
                    slickROutput("sr", width="80%"),br(),
                    h3("Support becomes resistance and resistance lines become support lines. Of course, prices go through the same price range over the years so there are more occurences of Highs and Lows being recorded within those ranges. Still, it is something to take into account when seeing prices break and drop or rocket."),br()
             )
           )
  ),
  ################################################################ END Chart app 4 tabpanel 
  tabPanel('Time Series Analysis',
           fluidRow(
             column(12, 
                    h3('For time series analysis I used the daily price data (price close) for 9 currency pairs from 2007-2018. I took the inverse values of EURUSD, XAUUSD, GBPUSD, AUDUSD, and NZDUSD so their prices reflect an amount worth for one USD. So now 1 USD is worth 0.75 EUR, etc.'),br(),
                    img(src='tsa1.png', align = "center"),br(),
                    h3("How do currencies cluster, is it based on their geography or trade policies? Using a hierarchical clustering technique will allow us more insight:"),br(),
                    img(src='tsa2.png', align = "center"),br(),
                    h3("Knowing these relationships can help. If a trader is simultaneously trading both EURUSD and USDSEK (which are grouped within the same cluster), then said trader is not diversifying, they are either amplifying their bet or ultimately hedging their trade to a certain degree."),br(),
                    h3("Using a Markov Chain we can see the probability of tomorrow being an Up, Down, or Mixed day depending on what kind of day it was for the USD today:"),br(),
                    img(src='tsa3.png', align = "center"),br(),
                    h3("And this is as far as I go trying to ascertain the future directional movement of daily prices."),br()
              )
           )
  ),
  ################################################################ END Chart app 4 tabpanel   
  tabPanel('Range Prediction with Machine Learning',
           fluidRow(
             column(12, 
                    h3('Choosing the right variables to use in the machine learning part of my project took multiple tries. Having an AWS EC2 instance to do the computations helped immensely.'),
                    h3('In the end I had 162 variables and almost 12 years of M1 observations which spanned 4,461,334 rows.'),br(),
                    img(src='prml1.png', align = "center"),br(),
                    h3('Imputing Missing Variables: Not all pairs are create equal, XAUUSD for instance only trades for 23 hours a day, while currencies trade 24/5. Luckily, for the hour it does not trade not much else moves either.'),br(),
                    h3('I chose to drop all rows with NA and not remove any outliers. I multiplied the pip range for EURUSD by 10,000 so it read as 2.3 pips instead of 0.00023. For skewed variables I took the log1p transformation of all (skew>0.75) variables.'),br(),
                    h3('All variables were numerical variables. This is what happens after a log1p transformation:'),br(),
                    img(src='prml3.png', align = "center"),br(),
                    img(src='prml2.png', align = "center"),br(),
                    h3('I used Random Forest Regressor to select important variables, but the result was that each of the 162 variables had a positive importance, so I chose the top 50 for later use.'),br(),
                    img(src='prml4.png', align = "center"),br(),
                    h3('XGBoost:'),br(),
                    img(src='prml5.png', align = "center"),br(),
                    h3('I performed a Ridge and Lasso Regression on both the "all variables" set and "50 selected features only" set.'),br(),
                    h3('The Ridge Cross Validation for both sets of data returned to me an optimal alpha that was not penalized, meaning that the best fit was just an Ordinary Least Squares Regression Model!'),br(),
                    img(src='prml6.png', align = "center"),br(),
                    h3('On the other hand, the Lasso Regularization models both returned a Prediction vs True Values plot that was within a narrow range of ~20 pips'),br(),
                    slickROutput("prml", width="80%"),br(),
                    h3('The Lasso Regularization calculations for the "all variables" data set had taken too long to find an optimal alpha. In the end I chose a small alpha manually and the results were still viable.'),br(),
                    h3('I combined all the different models for one final prediction test.'),br(),
                    h3('This is how well Machine Learning algorithms can deduce the minute by minute range of EURUSD:'),br(),
                    img(src='prml9.png', align = "center"),br(),
                    img(src='prml10.png', align = "center"),br(),
                    img(src='prml11.png', align = "center"),br(),
                    img(src='prml12.png', align = "center"),br()
                  )
           )
  ),  
  ################################################################ END Chart app 4 tabpanel
  ##########################info section
  tabPanel('Project Info',
           fluidRow(
             column(12,dashboardPage(dashboardHeader(disable = T),
                                     dashboardSidebar(disable = T),
                                     dashboardBody(br(),
                                                   h2('New York City Data Science Academy, Fall Cohort 2018'),br(),
                                                   h3('Capstone Project: An Exploration of FX'),br(),
                                                   h4('Reuters Articles Scraped From:'),
                                                   tags$div(class = 'header', checked = NA,
                                                            tags$a(href = 'https://uk.reuters.com/news/archive/GCA-ForeignExchange','https://uk.reuters.com/news/archive/GCA-ForeignExchange')),br(),
                                                   h4('2007 - 2018 Tick Data From:'),
                                                   tags$div(class = 'header', checked = NA,
                                                            tags$p("Tickstory (Dukascopy Tick DB)")),br(),br(),
                                                   h4('Created by'), 
                                                   tags$div(class = "header", checked = NA,
                                                            tags$p("Daniel Chen : dchen@taipeifx.com"),br(),
                                                            tags$div(class = "header", checked = NA,
                                                                     tags$a(href= "https://github.com/taipeifx/Capstone-Project-2018","TaipeiFX GitHub")),br(),
                                                            tags$div(class = "header", checked = NA,
                                                                     tags$a(href= "https://nycdatascience.com/blog/author/dchen/","NYCDSA Blog")),
                                                            h4('Thanks!')
                                                   ))
             ))
           ))
  #############################end info section
)

###################### SERVER #############################   
server <- function(input, output) {
  output$plot <- renderPlotly({
    
    hovertext1 <- paste0("Date:<b>", eurreuters$Date, "</b><br>",
                         "News:<b>", eurreuters$post.y, "</b><br>")
    
    plot_ly(data= eurreuters,
                x = ~Date,
                yaxis = "y") %>%
      
      add_lines(data = eurreuters, x = ~Date, y = ~Close, line = list(color = "#00526d", width = 1),
                inherit = F, name = "EURUSD") %>% 
      
      add_lines(data = eurreuters, x = ~Date, y = ~USDJPY, line = list(color = "#F95959", width = 1),
                inherit = F, name = "USDJPY", visible= 'legendonly') %>% 
      
      add_lines(data = eurreuters, x = ~Date, y = ~USDCHF, line = list(color = "green", width = 1),
                inherit = F, name = "USDCHF", visible= 'legendonly') %>% 
      
      add_lines(data = eurreuters, x = ~Date, y = ~GBPUSD, line = list(color = "red", width = 1),
                inherit = F, name = "GBPUSD", visible= 'legendonly') %>% 
      
      add_lines(data = eurreuters, x = ~Date, y = ~XAUUSD, line = list(color = "gold", width = 1),
                inherit = F, name = "XAUUSD", visible= 'legendonly') %>% 
      
      add_lines(data = eurreuters, x = ~Date, y = ~AUDUSD, line = list(color = "blue", width = 1),
                inherit = F, name = "AUDUSD", visible= 'legendonly') %>% 
      
      add_lines(data = eurreuters, x = ~Date, y = ~NZDUSD, line = list(color = "black", width = 1),
                inherit = F, name = "NZDUSD", visible= 'legendonly') %>% 
      
      add_lines(data = eurreuters, x = ~Date, y = ~USDSEK, line = list(color = "orange", width = 1),
                inherit = F, name = "USDSEK", visible= 'legendonly') %>% 
      
      add_lines(data = eurreuters, x = ~Date, y = ~USDCAD, line = list(color = "violet", width = 1),
                inherit = F, name = "USDCAD", visible= 'legendonly') %>% 
      
      add_lines(data = eurreuters, x = ~Date, y = 1, line = list(color = "#00526d", width = 0),
                hoverinfo = text, text = hovertext1, inherit = F, name = "newsline") %>% 
      
      add_bars(data = eurreuters, x = ~Date, y = ~n,
               yaxis = "y2", inherit = F, name = "News Articles", marker = list(color = "#33bbff")) %>% 
      
      
      layout(plot_bgcolor = "rgb(250,250,250)",
             xaxis = list(title = "", domain = c(0,0.95),
                          rangeslider = list(visible = F)),
             yaxis = list(domain = c(0.22, 0.9)),
             yaxis2 = list(domain = c(0, 0.18), side = "left"),
             
             hovermode = 'compare',
             
             annotations = list(x = 0, y = 1, xanchor = "left", yanchor = "top",
                                xref = "paper", yref = "paper",
                                text = paste0("<b>Reuters Articles on a Currency Chart</b>"),
                                font = list(size = 30, family = "serif"),
                                showarrow = FALSE)
             )
    
  })
  #################################################################################################################### interactive LDA
  #slickR photos
  output$cc <- renderSlickR({
    imgs <- list.files("www/cc/", pattern=".png", full.names = TRUE)
    slickR(imgs)
  })
  output$tic <- renderSlickR({
    imgs <- list.files("www/tic/", pattern=".png", full.names = TRUE)
    slickR(imgs)
  })
  output$mbr <- renderSlickR({
    imgs <- list.files("www/mbr/", pattern=".png", full.names = TRUE)
    slickR(imgs)
  })
  output$sr <- renderSlickR({
    imgs <- list.files("www/sr/", pattern=".png", full.names = TRUE)
    slickR(imgs)
  })
  output$prml <- renderSlickR({
    imgs <- list.files("www/prml/", pattern=".png", full.names = TRUE)
    slickR(imgs)
  })
  ### blog post selection table
  options(DT.options = list(pageLength = 5))
  output$x11 = DT::renderDataTable(data_table, server = FALSE, selection = 'single')
  

  #################################################################################################################### end interactive LDA
  ###################### END SERVER #############################     
  }

# Run the application 
shinyApp(ui = ui, server = server)

