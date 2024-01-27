#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(cshapes)
library(ggplot2)
library(sf)
library(ggrepel)

# Load conf data from CSV file
conf <- read.csv("conflict_list.csv")
df <- read.csv("cwed.csv")

cols_map <- c('white', 'pink2', 'lightskyblue2', 'lightskyblue3')

# UI
ui <- fluidPage(
  titlePanel("CWED Map App"),
  
  fluidRow(
    selectInput("selectedID", "Select Conflict ID",
                choices = unique(paste(conf$conflictID, conf$cw_country, conf$startyear))),
    textOutput("selectedInfo")
  ),
  fluidRow(
    plotOutput("mapOutput", height = "600px", width = "100%")
  )
)

# Define server logic
server <- function(input, output) {

  df_int_data <- reactive({
    selected_info <- unlist(strsplit(input$selectedID, " "))
    
    df_int <- df %>%
      filter(conflictID == selected_info[1]) %>%
      select(ccode2, direct) %>%
      rename(cowcode = ccode2) %>%
      mutate(int = 1)
    
    return(df_int)
  })
  
  # Helper function to create the intervention coordinate dataframe
  df_coord_int_data <- reactive({
    selected_info <- unlist(strsplit(input$selectedID, " "))
    
    df_coord_int <- df %>%
      filter(conflictID == selected_info[1]) %>%
      select(caplong_d, caplat_d, caplong_t, caplat_t)
    
    return(df_coord_int)
  })
  
  # Helper function to create the retaliation coordinate dataframe
  df_coord_ret_data <- reactive({
    selected_info <- unlist(strsplit(input$selectedID, " "))
    
    df_coord_ret <- df %>%
      filter(conflictID == selected_info[1] & direct == 1) %>%
      select(caplong_d, caplat_d, centlong_t, centlat_t)
    
    return(df_coord_ret)
  })
  
  # Main reactive function to create the shape data
  shape_data <- reactive({
    selected_info <- unlist(strsplit(input$selectedID, " "))
    selected_year <- as.Date(selected_info[3], format = "%Y")
    
    df_int <- df_int_data()
    df_coord_int <- df_coord_int_data()
    df_coord_ret <- df_coord_ret_data()
    
    shape <- cshp(date = selected_year, useGW = FALSE, dependencies = FALSE) %>%
      mutate(cw = ifelse(country_name == selected_info[2], 1, 0)) %>%
      left_join(., df_int, by = 'cowcode') %>%
      mutate(direct = ifelse(is.na(direct), 0, direct),
             int = ifelse(is.na(int), 0, int)) %>%
      mutate(role = ifelse(direct == 1, 4, 
                           ifelse(direct == 0 & int == 1, 3, 
                                  ifelse(cw == 1, 2, 1))))
    
    labs <- c('Peace', 'Civil war', 'Not targeted INT', 'Targeted INT')
    shape$role <- factor(shape$role, levels = seq_along(labs), labels = labs)
    
    return(shape)
  })  
  # # output selected information
  # output$selectedInfo <- renderText({
  #   selected_info <- unlist(strsplit(input$selectedID, " "))
  #   paste("Selected Information: Conflict ID =", selected_info[1],
  #         ", Country =", selected_info[2], ", Year =", selected_info[3])
  # })


    output$mapOutput <- renderPlot({
      shape <- shape_data()
      df_coord_int <- df_coord_int_data()
      df_coord_ret <- df_coord_ret_data()
      ggplot(data = shape) +
        geom_sf(aes(fill = as.factor(role))) +
        scale_fill_manual(values = cols_map, name = "Civil war expansion", labels = levels(shape$role), drop = FALSE) +
        geom_curve(data = df_coord_int,
                   aes(x = caplong_t, y = caplat_t, xend = caplong_d, yend = caplat_d),
                   curvature = -0.4,
                   arrow = arrow(length = unit(0.1, "cm"), ends = "last", type = "closed"),
                   size = 1,
                   colour = "gray30",
                   alpha = 0.7) +
        geom_curve(data = df_coord_ret,
                   aes(x = caplong_d, y = caplat_d, xend = centlong_t, yend = centlat_t), # update with different coordinates
                   curvature = -0.4,
                   arrow = arrow(length = unit(0.3, "cm"), ends = "last", type = "closed"),
                   size = 1,
                   colour = "firebrick4",
                   alpha = 0.8) +
        geom_label_repel(data = filter(shape, role != "Peace"), 
                         aes(x = caplong, y = caplat, label = country_name), 
                         size = 3, color = "black", box.padding = 0.5, point.padding = 0.5,
                         max.overlaps = 20)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
