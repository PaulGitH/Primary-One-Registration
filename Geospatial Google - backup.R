setwd("D:/SMU/ISSS 616 Mon/Group Project/Geospatial template")

library(shiny)
library(dplyr)
library(googleway)
library(ggmap)
library(ggplot2)
library(shiny)
library(ggrepel)
library(tidyr)

api_key <- "AIzaSyAgnZihgzbWfhxJ3cVYaab57GF78_XYoss"
register_google(key="AIzaSyAgnZihgzbWfhxJ3cVYaab57GF78_XYoss")

# load in Schools data
data <- read.csv("schools.csv")
df <- data.frame(data)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  titlePanel("Primary School Admission Info Deck"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Input: BOX for entering Postal code ----
      numericInput(inputId = "housepc", label = h3("Postal Code:"), 
                   min = 0, max = 999999, step = 1, value = 348196),
      
      # Input: Select Distance from entered Postal code ----
      selectInput(inputId = "Dist_Input",
                  label = h3("Distance:"),
                  choices = c("1km", "2km", "5km", "10km", "Show ALL")),
      
      # Action Button for search
      actionButton("search","Search")
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", google_mapOutput(outputId = "map")
                  ), 
        tabPanel("List of Schools", tableOutput(outputId = "filtered_list")
                 ),
        tabPanel("what is entered", verbatimTextOutput(outputId = "NUM"))
    )
  )
  )
)

# server part write in  
server <- function(input, output) {
  # distance calculation function
  cul_dist <- function(Lng_1,Lat_1, S_Lng, S_Lat){
    Lng_dis = (Lng_1-S_Lng)*111320*cos((S_Lat+Lat_1)/2)
    Lat_dis = (Lat_1-S_Lat)*110574
    distant = sqrt((Lng_dis)^2+(Lat_dis)^2)
    return(distant)
  }
  # put in Action button
  mydata <- eventReactive(input$search, {
    # output the lng and lat of input postal code
    loc_1 <- geocode("input$housepc")
    h_Lng <- loc_1$lon[1]
    h_Lat <- loc_1$lat[1]
    
    df$dist <- cul_dist(h_Lng,h_Lat,df$Lng,df$Lat)
    df$hlat <- h_Lng
    df$hlng <- h_Lat
    
    # df_filter <- data.frame(df)
    filtered <- subset(df,dist <= input$Dist_Input)
    
  })
  # hpc_input <- reactive(input$HousePC)
  # dis_input <- reactive(input$Dist_Input)
  
  # render output HousePC and Distance
  # output$hpc <- renderPrint(input$HousePC)
  # output$dis <- renderPrint(input$Dist_Input)
  
  # call list
  output$filtered_list <- renderTable(mydata())
  
  # call map
  output$map <- renderGoogle_map({
    google_map(key = api_key, data = mydata()) %>%
      add_markers(lat = "Lat", lon = "Lng",mouse_over = "Name")

  })
  
  # call PC
  output$NUM <- renderPrint(input$housepc)
}

# running Shiny APP
shinyApp(ui = ui, server = server)


