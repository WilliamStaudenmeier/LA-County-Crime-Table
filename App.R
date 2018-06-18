library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(DT)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)

df = data.frame(readRDS("Crime Data.rds"), stringsAsFactors = F)

df$Date = as.Date(as.character(df$Date.Occurred), "%m/%d/%Y")

df = df %>% arrange(desc(Date))

df$Year = year(df$Date)

df$Crime = df$Crime.Code.Description

df$Area = df$Area.Name

df$Weapon = df$Weapon.Description

data = df%>% 
  group_by(Crime, Year, Weapon, Area) %>% mutate(Count = n()) %>%
  arrange(desc(Year), desc(Count)) %>% 
  dplyr::select(Year, Area, Crime, Weapon, Count) %>% 
  distinct() 


#
ui = fluidPage( theme = shinytheme("simplex"),
                titlePanel("Los Angeles County Crime Table"),
                
                # Create a new Row in the UI for selectInputs
                fluidRow(
                  column(4,
                         selectInput("Year",
                                     "Year:",
                                     c("All",
                                       unique(as.character(data$Year))))
                  ),
                  column(4,
                         selectInput("Crime",
                                     "Crime:",
                                     c("All",
                                       unique(as.character(data$Crime))))
                  ),
                  column(4,
                         selectInput("Area",
                                     "Area:",
                                     c("All",
                                       unique(as.character(data$Area))))
                  )
                ),
                # Create a new row for the table.
                fluidRow(
                  DT::dataTableOutput("table")
                )
)


server = function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- data
    if (input$Year != "All") {
      data <- data[data$Year == input$Year,]
    }
    if (input$Crime != "All") {
      data <- data[data$Crime == input$Crime,]
    }
    if (input$Area != "All") {
      data <- data[data$Area == input$Area,]
    }
    data
  }))
  
}


shinyApp(ui, server)




