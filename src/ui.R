#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("The Importance of Having A Primary Healthcare Provider"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        checkboxGroupInput(inputId = "province",
                    label = "Select a province",
                    choices = sort(c("Canada",
                                "Alberta",
                                "British Columbia",
                                "Saskatchewan",
                                "Manitoba",
                                "Ontario",
                                "Quebec",
                                "Nova Scotia",
                                "Prince Edward Island",
                                "New Brunswick",
                                "Newfoundland and Labrador")),
                    selected = "Canada"),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("provincesPlot")
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
