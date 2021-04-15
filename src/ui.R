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
    verticalLayout(
        tags$head(
            tags$style(
                HTML(
                    "label {
                        font-family: Ubuntu Condensed;
                    }",
                    "h2 {
                        font-family: Ubuntu Condensed;
                    }"
                )
            )
        ),
        # Application title
        titlePanel("The Importance of Having A Primary Healthcare Provider"),
        wellPanel(
            checkboxGroupInput(inputId = "province",
                               label = "Select a province:",
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
                               selected = "Canada")
        ),
        plotOutput("provincesPlot"),
        wellPanel(
            checkboxGroupInput(inputId = "category", 
                               label = "Select a visualization:", 
                               choices = c("No PHC provider by reasons", 
                                           "No PHC provider by age group", 
                                           "PHC vs. No PHC for Health Improvements", 
                                           "PHC vs. No PHC for Prostate Cancer Screening and Mammogram Tests"),
                               selected = "No PHC provider by reasons")
        ),
        conditionalPanel(
            condition = "input.category.includes('No PHC provider by reasons') && input.province.length !== 0",
            plotOutput("reasonsPlot")
        ),
        conditionalPanel(
            condition = "input.category.includes('No PHC provider by age group')  && input.province.length !== 0",
            plotOutput("ageGroupPlot")
        ),
        conditionalPanel(
            condition = "input.category.includes('PHC vs. No PHC for Health Improvements') && input.province.length !== 0",
            plotOutput("healthImprovementsPlot")
        ),
        conditionalPanel(
            condition = "input.category.includes('PHC vs. No PHC for Prostate Cancer Screening and Mammogram Tests') && input.province.length !== 0",
            plotOutput("prostateMammogramPlot")
        )
    )
))
