library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  textInput("name", "What's your name?"),
  dateInput("birthdate","Your birthday?"),
  checkboxInput("subscribe","Want to subscribe us?", value = FALSE),
  actionButton("submit","Submit")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$greeting <- renderText({
    paste0("Hello ", input$name)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)




#----------------------------------------------------------------------------------
library(shiny)
library(dplyr)
library(googleway)
library(ggmap)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(httr)
library(geosphere)

setwd("D:/SMU/ISSS 616 Mon/Group Project/Geospatial template")
api_key <- "AIzaSyAgnZihgzbWfhxJ3cVYaab57GF78_XYoss"
register_google(key="AIzaSyAgnZihgzbWfhxJ3cVYaab57GF78_XYoss")

data <- read.csv("schoolsinfo.csv")
df <- data.frame(data)

loc1 <- geocode("090109")
h_Lng <- loc1$lon[1]
h_Lat <- loc1$lat[1]

cul_dist <- function(Lng_1,Lat_1, S_Lng, S_Lat){
  # Lng_dis = (Lng_1-S_Lng)*111320*cos((S_Lat+Lat_1)/2)
  # Lat_dis = (Lat_1-S_Lat)*110574
  # distant = sqrt((Lng_dis)^2+(Lat_dis)^2)
  geo_dat <- data.frame(lon = c(Lng_1, S_Lng), lat = c(Lat_1, S_Lat))
  distance <- (distGeo(geo_dat[1, ], geo_dat[2, ]))/1000
  # print(paste("Distance is", distance))
  return(distance)
}

df_filter <- df %>%
  rowwise() %>%
  mutate(distance = cul_dist(Lng, Lat, h_Lng, h_Lat)) %>%
  filter(distance <= 1)

df$dist <- cul_dist(h_Lng,h_Lat,df$Lng,df$Lat)
df_filter <- data.frame(df)

filtered1 <- subset(df_filter, df_filter$dist <= 1)
filtered5 <- subset(df_filter, df_filter$dist <= 5)
filtered30 <- subset(df_filter, df_filter$dist <= 30)

write.csv(df_filter,"D:\\SMU\\ISSS 616 Mon\\Group Project\\Geospatial template\\df_filter.csv", row.names = FALSE)
write.csv(filtered1,"D:\\SMU\\ISSS 616 Mon\\Group Project\\Geospatial template\\filtered1.csv", row.names = FALSE)
write.csv(filtered5,"D:\\SMU\\ISSS 616 Mon\\Group Project\\Geospatial template\\filtered5.csv", row.names = FALSE)
write.csv(filtered30,"D:\\SMU\\ISSS 616 Mon\\Group Project\\Geospatial template\\filtered30.csv", row.names = FALSE)






#----------------------------------------------------------------------------------



# rlang::last_error()
# shiny::runApp(display.mode="showcase")
setwd("D:/SMU/ISSS 616 Mon/Group Project/Geospatial template")

library(shiny)
library(dplyr)
library(googleway)
library(ggmap)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(httr)
library(geosphere)

api_key <- "AIzaSyAgnZihgzbWfhxJ3cVYaab57GF78_XYoss"
register_google(key="AIzaSyAgnZihgzbWfhxJ3cVYaab57GF78_XYoss")


# load in Schools data
data <- read.csv("schoolsinfo.csv")
df <- data.frame(data)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  titlePanel("Primary One Registration Tool"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Input: BOX for entering Postal code ----
      numericInput(inputId = "housepc", label = h3("Postal Code:"), 
                   min = 0, max = 999999, step = 1, value = 348196),
      
      # Input: Select Distance from entered Postal code ----
      numericInput(inputId = "Dist_Input",
                   label = h3("Distance (KM):"),
                   value = 2),
      
      # Action Button for search
      actionButton("search","Search")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", google_mapOutput(outputId = "map"),
                 h6("Note: Class A: most challenging, Class E: easiest")
        ), 
        tabPanel("List of Schools", tableOutput(outputId = "filtered_list")) 
      )
    )
  )
)

# server part write in  
server <- function(input, output) {
  # distance calculation function
  cul_dist <- function(Lng_1,Lat_1, S_Lng, S_Lat){
    geo_dat <- data.frame(lon = c(Lng_1, S_Lng), lat = c(Lat_1, S_Lat))
    distance <- (distGeo(geo_dat[1, ], geo_dat[2, ]))/1000
    return(distance)
  }
  
  #reactive mydata
  mydata <- reactiveValues(filtered = df)
  
  # put in Action button
  observeEvent(input$search, {
    # output the lng and lat of input postal code
    loc_1 <- geocode(toString(input$housepc))
    h_Lng <- loc_1$lon[1]
    h_Lat <- loc_1$lat[1]
    
    mydata$filtered <- df %>%
      rowwise() %>%
      mutate(distance = cul_dist(Lng, Lat, h_Lng, h_Lat)) %>%
      filter(distance <= input$Dist_Input) 
    # %>% 
    #   select(Name,Info,	Lat,	Lng,	Color)
    
    # df$hlat <- h_Lng
    # df$hlng <- h_Lat
    # 
    # df_filter <- data.frame(df)
    # mydata$filtered <- subset(df_filter,dist <= input$Dist_Input)
  })
  # hpc_input <- reactive(input$HousePC)
  # dis_input <- reactive(input$Dist_Input)
  
  # render output HousePC and Distance
  # output$hpc <- renderPrint(input$HousePC)
  # output$dis <- renderPrint(input$Dist_Input)
  
  # call list
  output$filtered_list <- renderTable(mydata$filtered)
  
  # call map
  output$map <- renderGoogle_map({
    google_map(key = api_key, data = mydata$filtered) %>%
      add_markers(lat = "Lat", lon = "Lng", mouse_over = "Info")
  })
}

# running Shiny APP
shinyApp(ui = ui, server = server)

