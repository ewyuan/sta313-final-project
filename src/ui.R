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
        conditionalPanel(
            condition = "input.province.length !== 0",
            tabsetPanel(type = "tabs",
                        tabPanel("No PHC provider by reasons", plotOutput("reasonsPlot")),
                        tabPanel("No PHC provider by age group", plotOutput("ageGroupPlot")),
                        tabPanel("PHC vs. No PHC for Health Improvements", plotOutput("healthImprovementsPlot")),
                        tabPanel("PHC vs. No PHC for Prostate Cancer Screening and Mammogram Tests", plotOutput("prostateMammogramPlot")))
        )
    )
))
