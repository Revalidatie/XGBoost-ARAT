server <- function(input, output) {
  
  output$downloadData1 <- downloadHandler(
    filename <- function() {
      paste("example_patient1", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("Examples/example_patient1.csv", file)
    },
    contentType = "csv"
  )
  
  output$downloadData2 <- downloadHandler(
    filename <- function() {
      paste("example_patient2", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("Examples/example_patient2.csv", file)
    },
    contentType = "csv"
  )
  
  output$downloadData3 <- downloadHandler(
    filename <- function() {
      paste("example_patient3", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("Examples/example_patient3.csv", file)
    },
    contentType = "csv"
  )
  
  output$downloadData4 <- downloadHandler(
    filename <- function() {
      paste("example_patient4", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("Examples/example_patient4.csv", file)
    },
    contentType = "csv"
  )

  # initialize values
  pat_dat_r <- reactiveVal(NULL)
  cur_obs_r <- reactiveVal(NULL)
  pred_list_r <- reactiveVal(NULL)
  max_T <- 180-14 # max time from which predictions can be made (2 weeks before Tout)
  
  loadPatient <- reactive({
    if (!is.null(input$patientFile)) {
      inFile <- input$patientFile
      file_ext <- tools::file_ext(inFile$name)
      
      if(file_ext %in% c("csv", "txt")){
        pat_dat <- read.csv(inFile$datapath)
      } else if (file_ext %in% c("xlsx", "xls")){
        pat_dat <- read_excel(inFile$datapath)
      } else{
        showNotification("Unsupported file type. Please upload a CSV, TXT, XLSX or XLS file.", type = "error", duration = 12)
        return(NULL)
      }
      
      if (any(is.na(pat_dat))) {
        showNotification("The file contains missing values. Please clean the data and upload again.", type = "error", duration = 12)
        return(NULL)
      }
      
      if (nrow(pat_dat) < 1) {
        showNotification("The file does not contain any rows. Please upload a file with at least one row of data.", type = "error", duration = 12)
        return(NULL)
      }
      
      if (!check_data(pat_dat)) {
        showNotification("The file is not formatted correctly or contains data out of the allowed range. Please review the example patient files and upload again.", type = "error", duration = 12)
        return(NULL)
      }

      pat_dat_r(pat_dat)
      return(pat_dat)
    }
  })
  
  output$daysChoose <- renderUI({
    if (!is.null(input$patientFile)) {
      pat_dat <- loadPatient()
      mindays <- min(pat_dat$Days)
      
      sliderInput("days", tags$b("Select day of model application:"),
                  min = mindays, max = max_T, value = mindays, step = 1,
                  animate = animationOptions(loop = TRUE, interval = 500))
    }
  })
  
  output$plot <- renderPlot({
    pat_dat <- pat_dat_r()
    
    if (!is.null(pat_dat)) {
      applied_at <- ifelse(is.null(input$days), 7, input$days) # avoids an error due to load timing of chooseDays
      
        actual <- pat_dat %>%
          mutate(diff = abs(Days - 180)) %>%
          arrange(diff) %>%
          dplyr::slice(1) %>% 
          pull(ARAT)
        
        pat_filt <- pat_dat %>%
          filter(Days <= max_T) %>% 
          filter(Days <= applied_at)
        
        cur_obs <- pat_filt %>% 
          slice_max(Days, n=1)
        
        cur_obs_r(cur_obs)
      
      # lookup prediction
      pred_list <- lookup(cur_obs)
      pred_list_r(pred_list)
      
      genPlot(pred_list, pat_filt, applied_at, actual)
    }
  }, height = 400, width = 600 )
  
  output$message1 <- renderPrint({
    pat_dat <- pat_dat_r()
    
    if (is.null(pat_dat)) {
      cat("Upload your patient data in the left panel.")
    }
  })
  
  output$message2 <- renderPrint({
    cur_obs <- cur_obs_r()
    pred_list <- pred_list_r()
    
    if (!is.null(cur_obs) && !is.null(pred_list)) {
      ublab <- round(pred_list[[1]]$ub,0)
      lblab <- round(pred_list[[1]]$lb,0)
      
      cat("<h3>Your <span style='color:#440154'>measured</span> and <span style='color:#21918c'>predicted</span> arm/hand function</h3>",
          paste0("Your last measured arm/hand function is <b>", cur_obs$ARAT, "</b>. Based on similar patients, there is
                 an 80% probability that your arm/hand function will fall between <b>", lblab," and ", ublab, "</b> at 6 months."
          )
      )
    } 
  })
  
}