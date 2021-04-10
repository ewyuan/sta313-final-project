#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

tweaks <- 
    list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 5;    /* Firefox */ 
                                   column-count: 5; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
    ))

provinceControls <- 
    list(tags$div(align = 'left', 
                  class = 'multicol', 
                  checkboxGroupInput(inputId = "province",
                       label = "Select a province",
                       choices = sort(c("Alberta",
                                        "British Columbia",
                                        "Canada",
                                        "Manitoba",
                                        "New Brunswick",
                                        "Newfoundland and Labrador",
                                        "Nova Scotia",
                                        "Ontario",
                                        "Prince Edward Island",
                                        "Quebec",
                                        "Saskatchewan")),
                       selected = "Canada")))


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tweaks,

    # Application title
    titlePanel("The Importance of Having A Primary Healthcare Provider"),

    # Sidebar with a slider input for number of bins
    fluidRow(
        column(width = 4, provinceControls),
        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                plotOutput("provincesPlot")
            )
        )
    ),
    #sidebarLayout(
    #    checkboxGroupInput(inputId = "category",
    #                label = "label",
    #                choices = c("No PHC provider by reasons",
    #                            "No PHC provider by age group",
    #                            "PHC vs. No PHC for Health Improvements",
    #                            "PHC vs. No PHC for Prostate Cancer Screening and Mammogram Tests")),
    #)
))
