shinyUI(dashboardPage(
  dashboardHeader(title = "Stolen Bikes"),
  dashboardSidebar(
    
    sidebarUserPanel(name = "Wing Yan Sang",
                     image = "http://www.sumo.or.jp/img/sumo_data/rikishi/270x474/20140083.jpg"),
    sidebarMenu(
      menuItem("US Map -Zip Code Level", tabName = "map", icon = icon("map")),
      menuItem("US Map -State Level", tabName = "map2", icon = icon("map")),
      menuItem("Variables Analysis", tabName = "varAnalysis", icon = icon("bar-chart", lib = "font-awesome")),
      menuItem("Word Cloud", tabName = "cloud", icon = icon("cloud", lib = "font-awesome")),
      menuItem("Demographics: Sex", tabName = "demoSex", icon = icon("bar-chart", lib = "font-awesome")),
      menuItem("Demographics: Age", tabName = "demoAge", icon = icon("bar-chart", lib = "font-awesome")),
      menuItem("Demographics: Race", tabName = "demoRace", icon = icon("bar-chart", lib = "font-awesome")))
      ,
    sliderInput("range", "Please Select Year(s):",
                min =2012,
                max = 2017, step = 1,
                value = c(2012, 2017), sep = ""),
      div(style="text-align:center","The dataset covers the period from October 2012 to October 2017")),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                    infoBoxOutput("maxBox"),
                    infoBoxOutput("secBox"),
                    infoBoxOutput("thirdBox")),
              fluidRow(leafletOutput("map", width = 1200, height = 600))
            ),
      
      tabItem(tabName = "map2",
              div(h2("Map of Stolen Bikes on BikeIndex by State"), style = "text-align:center"),
              box(htmlOutput("map2"), width = 1200, height = 600)
            ),
      
     tabItem(tabName = "varAnalysis",
              fluidRow(box(radioButtons("variable", "Please Choose Variable:",
                       choices = c("Manufacturer", "Color", "Season", "Lock_Type",
                                    "How_Stolen"), inline = TRUE))),
              fluidRow(box(plotOutput("varPlot"), width = 1200, height = 600))
              ),
     tabItem(tabName = "cloud",
     fluidPage(
       # Application title
       titlePanel("Word Cloud"),
       
       sidebarLayout(
         # Sidebar with a slider and selection inputs
         sidebarPanel(
           sliderInput("freq",
                       "Minimum Frequency:",
                       min = 1,  max = 50, value = 15),
           sliderInput("max",
                       "Maximum Number of Words:",
                       min = 1,  max = 300,  value = 100)
         ),
         
         # Show Word Cloud
         mainPanel(
           plotOutput("plot")
         )
       )
     )
     ),
     
     
     tabItem(tabName = "demoSex",
             box(plotOutput("demo1Plot"), width = 1000, height = 1000)
             ),
     tabItem(tabName = "demoAge",
             fluidRow(box(radioButtons("county", "Please Choose County:",
                       choices = c("Alameda", "Cook", "D.C.", "King", "Los Angeles", "Multnomah",
                                   "Orleans", "San Diego", "San Francisco","Santa Clara" ), inline = TRUE))),
             fluidRow(box(plotOutput("demo2Plot"), width = 1200, height = 600))
            ),
     tabItem(tabName = "demoRace",
             fluidRow(box(radioButtons("county2", "Please Choose County:",
                        choices = c("Alameda", "Cook", "D.C.", "King", "Los Angeles", "Multnomah",
                                    "Orleans", "San Diego", "San Francisco","Santa Clara" ), inline = TRUE))),
             fluidRow(box(plotOutput("demo3Plot"), width = 1200, height = 600))
            )
     )
     )
    )
)