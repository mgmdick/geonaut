#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(dplyr)
library(sp)
library(gstat)

#source('Geonaut/seismic_line.R')
#source('Geonaut/generate_resource.R')

# Define UI for application that draws a histogram
header <- dashboardHeader(disable = T)

sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
        
  shinyDashboardThemes(
    theme = "purple_gradient"
  ),
  
  
  
                        column(width = 3, 
                             box(
                                 title = "Energy Balance", width = NULL, solidHeader = TRUE, status = "primary",
                                 
                                 plotOutput("energy_plot", height =150),
                                 height = 220,
                             ),
                             box(
                                 title = "GEONAUT CONTROL PANEL", width = NULL, status = "primary", solidHeader = TRUE,
                                 actionButton("new_quadrant", "Scan New Quadrant"),
                                 actionButton("explore_click", "Deploy Nano-Corers"),
                                 height = 220
                             ),
                             box(
                                 title = "Model Results", width = NULL, height = 220, status = "primary", solidHeader = TRUE
                             )),
                      column(width = 6, 
                             tabBox(
                                 title = "Maps", width = NULL, height = 460, side = "left",
                                 tabPanel("Deploy Scout Drone",  plotOutput("tenement_seismic_plot", click = "seismic_click")),
                                 tabPanel("Deploy Nano-Corers", plotOutput("tenement_drill_plot", click = "drill_click")),
                                 tabPanel("Net Energy Map", plotOutput("idw_z_plot")),
                                 #tabPanel("Projected Thickness", plotOutput("idw_tk_plot")),
                                 tabPanel("Excavation Map", plotOutput("idw_wline_plot"), status = "primary")
                             ),
                             tabBox(
                                 title = "Scanner HUD", width = NULL, height = 220, side = "left",
                                 tabPanel("Drone Scan Profile", plotOutput("seismic_line_plot"))
                             )),
                      column(width = 3, 
                             box(
                                 title = "Equipment", width = NULL, solidHeader = TRUE, status = "primary",
                                 sliderInput("excavator_count", "How many Hyper-Exacavators?", min = 1, max = 10, value = 1),
                                 #sliderInput("weathering_rl", "Starting Depth", min = 0, max = 100, value = 30),
                                 sliderInput("final_depth", "Final Depth", min = 40, max = 300, value = 50, step = 1),
                                 height = 220
                             ),
                             box(
                                 title = "Projected Physicals", width = NULL, status = "primary",solidHeader = TRUE,
                                 plotOutput("progress_plot", height = 150), height = 220
                             ),
                             box(infoBoxOutput("actual"), infoBoxOutput("model_npv"), title = "Results", width = NULL, solidHeader = TRUE, status = "primary"
                               
                             )
                             
                             )
                      
)


dashboardPage(header, sidebar, body)