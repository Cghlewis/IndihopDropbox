library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(rdrop2)
library(stringr)

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
                  c(" ", "Rockwell Beer Co. Passing Clouds", "Rockwell Beer Co. Fabricius",
                    "Charleville Brewing Co. Half Wit Wheat", "4 Hands Brewing Co. City Wide", 
                    "Missouri Beer Co. Eastern MO IPA", 
                    "Perennial Artisan Ales Sump Coffee Stout", "Alpha Brewing Co. Raspberry Sour Ale",
                    "Six Mile Bridge Coffee Maple Stout", "Center Ice Brewery Puck O' the Irish",
                    "Earthbound Beer Doing it Extreme IPA", "Cathedral Square Belgian Style White Ale",
                    "Third Wheel Brewing Spandex Specter", "2nd Shift Brewing Hibiscus Wit", 
                    "Wellspent Brewing House Saison",
                    "O'Fallon Brewery St. Louis Red", "Schlafly Raspberry Hefeweizen", 
                    "Alpha Brewing Co. Passionfruit Double IPA", "Cathedral Square Heavenly Honey Razz", "Beer 19", 
                    "Schlafly White Lager", "Charleville Brewing Co. Late Night Karate Kicks"
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
  }, options=list(pageLength=10, lengthMenu = c(10, 20, 30, 50)))
  
  
  
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
                                                                              position = position_nudge(y = -2))+
      xlab("Top 5 Beers")+ylab("Average Weighted Score")+geom_errorbar(aes(ymin=AvgScore-SD, ymax=AvgScore+SD), width=.2,
                                                                       position=position_dodge(.9))+
      theme(axis.text=element_text(face="bold"))+theme_classic()+ylim(0,6.5)+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    
  })
}

shinyApp(ui, server)