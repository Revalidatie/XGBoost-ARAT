ui <- fluidPage(
  
  theme = shinytheme("lumen"),

  # Header with logo and title
  titlePanel(
    windowTitle = "XGBoost ARAT Predictions",
    tags$div(
      style = "display: flex; align-items: center;",
      tags$img(src = "logo_head_only.png", alt = "Logo", style = "height: 50px; margin-right: 15px;"),
      tags$h1("XGBoost ARAT Predictions")
    )
  ),
  
  sidebarLayout(
    # Sidebar
    sidebarPanel(width = 3,
      fileInput('patientFile', 
                tags$b('Load patient data'),
                accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.xlsx', '.xls')),
      
      p(tags$b("Examples of patient input files:")),
      downloadLink('downloadData1', 'Patient 1'), 
      p("\n"),
      downloadLink('downloadData2', 'Patient 2'),
      p("\n"),
      downloadLink('downloadData3', 'Patient 3'),
      p("\n"),
      downloadLink('downloadData4', 'Patient 4'),
      p("\n"),
      uiOutput("daysChoose")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("How to use", 
                 includeHTML("www/instructions.html")
        ),
        tabPanel("View prediction",
                 uiOutput("message1"),
                 uiOutput("message2",style = "margin-bottom: 20px;"),
                 plotOutput('plot')
        )
      ),
      style = "padding-bottom: 50px;"
    )
  ),
  hr(),
  includeHTML("www/footer.html")
)
