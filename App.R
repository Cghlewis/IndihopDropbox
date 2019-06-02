library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(rdrop2)

#Create a token we can call to access dropbox (the droptoken is from the global r file)
token <- readRDS("droptoken.rds")

#load responses into a response folder on dropbox

outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("Name","Beer", "flavor", "aroma", "appearance","drinkability")

#save data
saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()
  
  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.csv", 
    as.integer(Sys.time()), 
    digest::digest(data)
  )
  
  #Save the file to a temp directory  
  filePath <- file.path(tempdir(), fileName)
  
  write.csv(data, filePath, row.names=FALSE, quote=TRUE)
  
  #upload the file to Dropbox  
  drop_upload(filePath, path=outputDir, dtoken=token)
  
}

#load data
loadData <- function() {
  
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir, dtoken=token)
  files <- filesInfo$path_display
  
  data <- lapply(files, drop_read_csv, stringsAsFactors = FALSE, dtoken=token)
  
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data) 
  
  data
}

#reset form after submission
resetForm <- function(session) {
  # reset values
  updateSelectInput(session,"Name", selected=character(0))
  updateSelectInput(session, "Beer", selected=character(0))
  updateSliderInput(session, "flavor", value = 1)
  updateSliderInput(session, "aroma", value = 1)
  updateSliderInput(session, "appearance", value = 1)
  updateSliderInput(session, "drinkability", value = 1)
}


#ui
ui <- fluidPage(
  
  # App title ----
  titlePanel("IndiHop 2019 Beer Ratings"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("Name",
                  "Your Name",
                  c(" ","Test","Crystal", "Dre", "Ian", "Jacob", "Josh", "JW","Lacy",
                    "Matt", "Rena")
      ),
      selectInput("Beer",
                  "Name of Beer", 
                  c(" ", "Test Beer", "Beer 1","Beer 2","Beer 3", "Beer 4", "Beer 5", 
                    "Beer 6", "Beer 7",
                    "Beer 8", "Beer 9", "Beer 10", "Beer 11", "Beer 12", "Beer 13", 
                    "Beer 14",
                    "Beer 15", "Beer 16", "Beer 17", "Beer 18", "Beer 19", "Beer 20", 
                    "Beer 21",
                    "Beer 22", "Beer 23", "Beer 24", "Beer 25", "Beer 26", "Beer 27", 
                    "Beer 28",
                    "Beer 29", "Beer 30", "Beer 31", "Beer 32", "Beer 33", "Beer 34", 
                    "Beer 35",
                    "Beer 36", "Beer 37", "Beer 38", "Beer 39", "Beer 40", "Beer 41",
                    "Beer 42", "Beer 43"
                  )
      ),
      sliderInput("flavor", "Flavor Rating (Weight 40%)",  min = 1, max = 5, step = 1, value = 1),
      sliderInput("aroma", "Aroma Rating (Weight 10%)",
                  min = 1, max = 5, step = 1, value = 1),
      sliderInput("appearance", "Appearance Rating (Weight 10%)",
                  min = 1, max = 5, step = 1, value = 1),
      sliderInput(
        "drinkability", "Drinkability Rating (Weight 40%)",
        min = 1, max = 5, step = 1, value = 1
      ),
      actionButton("submit", "Submit"),
      actionButton("clear", "Clear Form"),
      downloadButton("downloadData", "Download")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = "flavorPlot"),
      tags$hr(),
      dataTableOutput("responses")
    )
  )
)

#server

server = function(input, output, session) {
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(input)
    resetForm(session)
  })
  
  observeEvent(input$clear, {
    resetForm(session)
  })
  
  
  # Show the previous responses in a reactive table ----
  output$responses <- renderDataTable({
    # update with current response when Submit is clicked
    input$submit
    
    loadData()
  }, options=list(pageLength=5, lengthMenu = c(5, 10, 15, 20, 30, 50)))
  
  
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE, quote= TRUE)
    }
  )
  
  #make plot of output
  #bar chart of weighted score per person averaged across each beer and then only the top 3 displayed 
  #SD for each beer is also displayed (error bars)
  output$flavorPlot <- renderPlot({
    input$submit
    
    data <- loadData()
    
    data%>%
      rowwise%>%
      mutate(Score = (flavor*.4)+(aroma*.1)+(appearance*.1)+(drinkability*.4)) %>%
      group_by(Beer) %>%
      summarize(AvgScore = mean(Score), SD=sd(Score))%>%
      arrange(desc(AvgScore)) %>%
      slice(1:5) %>%
      ggplot(aes(x=reorder(Beer, -AvgScore), y=AvgScore)) +
      geom_bar(stat="identity", fill = "goldenrod2", color="black")+geom_text(aes(label=round(AvgScore,1)),
                                                                              position = position_nudge(y = -8))+
      xlab("Top 5 Beers")+ylab("Average Weighted Score")+geom_errorbar(aes(ymin=AvgScore-SD, ymax=AvgScore+SD), width=.2,
                                                                       position=position_dodge(.9))+
      theme(axis.text=element_text(face="bold"))+theme_classic()+ylim(0,6.5)
    
  })
}

shinyApp(ui, server)