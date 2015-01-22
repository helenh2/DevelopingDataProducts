library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Train Journey Stats for Sydney & Around", windowTitle = "Transport NSW"),
        sidebarPanel(
          helpText(div(HTML("<u>Input Options:</u>"))),
          sliderInput("year", "Choose Year(s) of Records:", min=2001, max=2013, value=c(2001,2013)),
          selectInput("line", "Choose the Train Line:", choices = c('Airport' = 'Airport','Bankstown' = 'Bankstown','Blue Mountains' = 'Blue Mountains','Carlingford' = 'Carlingford','CBD' = 'CBD','Central Coast' = 'Central Coast','East Hills' = 'East Hills','Eastern Suburbs'= 'Eastern Suburbs','Hunter' = 'Hunter','Illawarra' = 'Illawarra','Inner West' = 'Inner West','Newcastle' = 'Newcastle','North Shore' = 'North Shore','Northern via Macquarie Park' = 'Northern via Macquarie Park','Northern via Strathfield'= 'Northern via Strathfield','Olympic Park' = 'Olympic Park','South' = 'South','South Coast' = 'South Coast','Southern Highlands' = 'Southern Highlands','Western'= 'Western','Other' = 'Other'), selected = NULL, multiple = TRUE),
          checkboxInput("combine", "display history in combined view", value = FALSE)
        ),
         mainPanel(
           h3('Highest Year from Selected Input'),
           verbatimTextOutput("text1"),
           h3('Highest Month from Selected Input'),
           verbatimTextOutput("text2"),
           h3('Total Journeys from Selected Input'),
           verbatimTextOutput("text3"),
           h3('Historical Graphics'),
           plotOutput("plot")
           )
         ))
