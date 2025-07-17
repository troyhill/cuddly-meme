### works - shows how to create plots and tables based on an uploaded csv file.
### This file implements a three-file upload and checks if a directory is available to auto-load files

# shiny::runGitHub('KaranHub', 'KaranKakouei', ref="main")

# ui.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(bslib)
library(plyr)
library(ggplot2)


ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    fileInput("lab_data", "Browse to CSV file with proficiency test data",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fileInput("location_file", "Browse to CSV file with laboratory locations",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    fileInput("method_file", "Browse to CSV file with method list",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    sidebarMenu(
      # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Preview", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "widgets",
                 h3(htmlOutput('text1'), #output$text1 <- renderText( "Proficiency test data preview")
                 plotOutput("plot_preview"),
                 h3(htmlOutput('text2'), #"Map preview",
                 leafletOutput("map_preview", width = "700px", height = "400px")),
                 h3(htmlOutput('text3'), #"Methods list",
                 tableOutput("table_preview"))
                 )
      )
    )
  ) # The ui side you could create with a loop in renderUI():
)

server <- function(input, output) {
  # https://stackoverflow.com/questions/59055733/generating-multiple-graphs-plots-from-uploaded-files-in-shiny

  global <- reactiveValues()

  # data <- reactive({
  #   inFile <- input$file1
  #   if(!is.null(inFile)){
  #     df1 <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
  #     df1 <- gsub(df1$PT_test_date, pattern = '/| ', replacement = '-')
  #     df1 <- as.Date(df1$PT_test_date, format = '%Y-%m-%d')
  #   }
  # })
  output$text1 <- output$text2 <- output$text3 <- renderText("")

  if(!exists('input$lab_data')) {
    output$text1 <- renderText("Checking if file exists")

    path_tst <- getwd()
    path_components <- c(as.list(strsplit(x = path_tst, split = '/')[[1]][1:4]),
                         'R8 All LSASD - Region 8 Lab - DWLabCert',
                         'Work_Instruction',
                         'test-data')
    path_begin   <- do.call(file.path, as.list(path_components))
    if (dir.exists(path_begin)) {
      file_lst   <- list.files(path_begin, recursive = TRUE, full.names = TRUE)

      locations_path   <- grep(x = file_lst, pattern = 'data_lab_list', value = TRUE)[1]
      dat_loc        <- read.csv(locations_path)
      if(all(c("lat", "long", "Region", "Laboratory.Name", "Laboratory.Location..City..State.") %in% names(dat_loc))) {
        dat_loc      <- dat_loc[grepl(x = dat_loc$Region, pattern = '8'), ]
        dat_loc      <- dat_loc[!is.na(x = dat_loc$lat), ]
        output$text2 <- renderText( "Laboratory locations from input CSV:")
        global$locations <- dat_loc
      } else {
        output$text2 <- renderText(c('WARNING: Lab location dataset should have the following column names:<br>', paste0(c("lat", "long", "Region", "Laboratory.Name", "Laboratory.Location..City..State."), collapse = ', ')))
        global$locations <- NULL
      }

      dat_methods <- read.csv(grep(x = file_lst, pattern = 'data_NPDWS_methods_', value = TRUE)[1])
      if(all(c("method", "common_name", "category") %in% names(dat_methods))) {
        global$method_data <- dat_methods
        output$text3 <- renderText( "NPDWS methods found in CSV:")
      } else {
        output$text3 <- renderText(c('WARNING: Method file should have the following column names:<br>', paste0(c("method", "common_name", "category"), collapse = ', ')))
        global$method_data <- NULL
      }

      dat1 <- read.csv(grep(x = file_lst, pattern = "data_proficiency_test", value = TRUE)[1])
      if(all(c('Laboratory.Name', 'Laboratory.Location..City..State.', 'method', 'PT_result', 'PT_test_date') %in% names(dat1))) {
        date_tmp <- gsub(as.character(dat1$PT_test_date), pattern = '/| ', replacement = '-')
        dat1$PT_test_date <- as.Date(as.character(date_tmp), format = '%m-%d-%Y')
        dat1$PT_test_date[is.na(dat1$PT_test_date)] <- as.Date(date_tmp[is.na(dat1$PT_test_date)], format = '%Y-%m-%d')
        dat1$PT_test_date[is.na(dat1$PT_test_date)] <- as.Date(date_tmp[is.na(dat1$PT_test_date)], format = '%d-%m-%Y')
        global$lab_data <- dat1 # FROM HERE ON USE: global$data
        output$text1 <- renderText( "Proficiency test data preview")
      } else {
        output$text1 <- renderText(c('WARNING: Proficiency test dataset should have the following column names:<br>', paste0(c('Laboratory.Name', 'Laboratory.Location..City..State.', 'method', 'PT_result', 'PT_test_date'), collapse = ', ')))
        global$lab_data <- NA
      }
    }

  }

  observeEvent(input$lab_data, {
    # dat <- read.csv2(input$file1$datapath)
    # DO LOTS OF OPERATIONS ON data
    dat      <- read.csv(input$lab_data$datapath, stringsAsFactors = FALSE)
    if(all(c('Laboratory.Name', 'Laboratory.Location..City..State.', 'method', 'PT_result', 'PT_test_date') %in% names(dat))) {
      date_tmp <- gsub(as.character(dat$PT_test_date), pattern = '/| ', replacement = '-')
      # message(head(date_tmp), '\n')
      # message(head(as.Date(date_tmp[is.na(dat$PT_test_date)], format = '%m-%d-%Y')))
      dat$PT_test_date <- as.Date(as.character(date_tmp), format = '%m-%d-%Y')
      dat$PT_test_date[is.na(dat$PT_test_date)] <- as.Date(date_tmp[is.na(dat$PT_test_date)], format = '%Y-%m-%d')
      dat$PT_test_date[is.na(dat$PT_test_date)] <- as.Date(date_tmp[is.na(dat$PT_test_date)], format = '%d-%m-%Y')
      global$lab_data <- dat # FROM HERE ON USE: global$data
      output$text1 <- renderText( "Proficiency test data preview")
      } else {
          output$text1 <- renderText(c('WARNING: Proficiency test dataset should have the following column names:<br>', paste0(c('Laboratory.Name', 'Laboratory.Location..City..State.', 'method', 'PT_result', 'PT_test_date'), collapse = ', ')))
          global$lab_data <- NA
      # message('Proficiency test dataset should have column names: ', paste0(c('Laboratory.Name', 'Laboratory.Location..City..State.', 'method', 'PT_result', 'PT_test_date'), collapse = ', '))
      }
  })

  observeEvent(input$location_file, {
    # dat <- read.csv2(input$file1$datapath)
    # DO LOTS OF OPERATIONS ON data
    dat      <- read.csv(input$location_file$datapath, stringsAsFactors = FALSE)
    if(all(c("lat", "long", "Region", "Laboratory.Name", "Laboratory.Location..City..State.") %in% names(dat))) {
      dat      <- dat[grepl(x = dat$Region, pattern = '8'), ]
      dat      <- dat[!is.na(x = dat$lat), ]
      output$text2 <- renderText( "Laboratory locations from input CSV:")
      global$locations <- dat
    } else {
      output$text2 <- renderText(c('WARNING: Lab location dataset should have the following column names:<br>', paste0(c("lat", "long", "Region", "Laboratory.Name", "Laboratory.Location..City..State."), collapse = ', ')))
      global$locations <- NULL
    }
  })

  observeEvent(input$method_file, {
    # dat <- read.csv2(input$file1$datapath)
    # DO LOTS OF OPERATIONS ON data
    dat      <- read.csv(input$method_file$datapath, stringsAsFactors = FALSE)
    if(all(c("method", "common_name", "category") %in% names(dat))) {
      global$method_data <- dat
      output$text3 <- renderText("NPDWS methods found in CSV:")
    } else {
      output$text3 <- renderText(c('WARNING: Method file should have the following column names:<br>', paste0(c("method", "common_name", "category"), collapse = ', ')))
      global$method_data <- NULL
    }
  })

  output$plot_preview <- renderPlot({
    req(global$lab_data)
    # plot(table(global$data$PT_result))#, global$data$PT_test_date)
    # print(message(summary(format(global$data$PT_test_date, format = '%m-%d-%Y'))))
    # tmp <- global$data
    # tmp$PT_test_date <- format(tmp$PT_test_date, format = '%m-%d-%Y')
    # ggplot(data = tmp, aes(y = PT_result, x = PT_test_date)) + ggplot2::geom_point()
    ggplot(data = global$lab_data, aes(y = PT_result, x = PT_test_date, col = Laboratory.Name)) + ggplot2::geom_point() + facet_wrap(. ~ method) +
      theme_bw() + theme(legend.position = 'none') +
      labs(y = 'Proficiency test outcome', x = '')
  })

  output$table_preview <- renderTable({
    # req(global$lab_data)
    # head(format(global$data$PT_test_date, format = '%m-%d-%Y')) # displays as date
    # head(global$data$PT_test_date) # displays as numbers
    req(global$method_data)
    paste0(global$method_data$method, collapse = ', ')
    # global$method_data$method
  })


  output$map_preview <- renderLeaflet({
    req(global$locations)
    locations <- global$locations
    marker_colors <- sapply(locations$Laboratory.Name, function(name) {
        # if (most_recent_row == FALSE) {
          return("green")
      #   } else if (most_recent_row == TRUE) {
      #     return("red")
      #   }
      # }
      # return("gray")  # Default color if no tests are available
    })

    # Debugging: Print marker colors and shapes
    # print(marker_colors)
    # print(marker_shapes)

    marker_popups <- sapply(locations$Laboratory.Name, function(name) {
      paste("Lab Name: ", name, "<br><br>")
    })
    # print(marker_popups)


    leaflet(locations) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = locations$long,
        lat = locations$lat,
        color = unname(marker_colors),
        radius = 8,
        popup = unname(marker_popups) # ~Laboratory.Name #
      )
  })

}

shinyApp(ui, server)


