#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(extrafont)

#font_import(pattern="Ubuntu")
#loadfonts(device="win")
#loadfonts(device="postscript")

# Load Plots
source("plots.r")

# Load Data
has_no_phcp_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/has_no_phcp_data.csv") %>% mutate(estimation = ifelse(sex == "Male", estimation*-1, estimation))
no_phcp_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/no_phcp_data.csv") %>% mutate(selected = "0")
health_improvements_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/health_improvements_data.csv")
health_improvements_data$province <- trimws(gsub("[^[:alnum:]]", " ", health_improvements_data$province))

screening_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/screening_data.csv")
screening_data$province <- trimws(gsub("[^[:alnum:]]", " ", screening_data$province))

reasons_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/reasons_data.csv")
reasons_data$province <- trimws(gsub("[^[:alnum:]]", " ", reasons_data$province))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    no_phcp <- reactive({
        mutate(no_phcp_data, 
               selected = ifelse(province %in% input$province, "1", "0"))
    })
    
    reasons <- reactive({
        dplyr::filter(reasons_data, province %in% input$province)
    })
    show_reasons <- reactive({
        ifelse("No PHC provider by reasons" %in% input$category, 1, 0)
    })
    
    has_no_phcp <- reactive({
        dplyr::filter(has_no_phcp_data, province %in% input$province)
    })
    show_no_phcp <- reactive({
        ifelse("No PHC provider by age group" %in% input$category, 1, 0)
    })
    
    health_improvements <- reactive({
        dplyr::filter(health_improvements_data, province %in% input$province)
    })
    show_health_improvements <- reactive({
        ifelse("PHC vs. No PHC for Health Improvements" %in% input$category, 1, 0)
    })
    
    screening <- reactive({
        dplyr::filter(screening_data, province %in% input$province)
    })
    show_screening <- reactive({
        ifelse("PHC vs. No PHC for Prostate Cancer Screening and Mammogram Tests" %in% input$category, 1, 0)
    })
    
    output$provincesPlot <- renderPlot({
        ggplot(no_phcp(), 
                    aes(reorder(province, -percentage, sum), 
                        percentage, 
                        fill=selected)) +
            geom_col() +
            scale_colour_wsj('colors6', '') + 
            scale_fill_wsj('colors6', '') +
            theme_minimal(base_family = "Ubuntu Condensed") +
            theme(legend.position="none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text = element_text(),
                  axis.title = element_text()) +
            xlab("Provinces") +
            ylab("Proportion")
    })
    
    # TODO: Need to finish
    output$reasonsPlot <- renderPlot({
        if (show_reasons()) {
            plot_reasons(reasons())
        }
    })
    
    output$ageGroupPlot <- renderPlot({
        if (show_no_phcp()) {
            plot_age_group(has_no_phcp())
        }
    })
    
    # TODO: Need to finish
    output$healthImprovementsPlot <- renderPlot({
        if (show_health_improvements()) {
            plot_health_improvements(health_improvements())
        }
    })
    
    output$prostateMammogramPlot <- renderPlot({
        if (show_screening()) {
            plot_prostate_mammogram(screening())
        }
    })
})
