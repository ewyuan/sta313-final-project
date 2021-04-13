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
library(gridExtra)

# Load Plots
source("plots.r")

# Load Data
has_no_phcp_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/has_no_phcp_data.csv")
no_phcp_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/no_phcp_data.csv") %>% mutate(selected = "0")
health_improvements_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/health_improvements_data.csv")
health_improvements_data$province <- trimws(gsub("[^[:alnum:]]", " ", health_improvements_data$province))

screening_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/screening_data.csv")
screening_data$province <- trimws(gsub("[^[:alnum:]]", " ", screening_data$province))

reasons_data <- data.frame(Region = c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Canada"), not_need_PHC = c(47.4, 38.4, 23.9,	31.4, 44.6, 46.8, 61.7, 67.8, 65.9,46.8, 47.47), no_PMH_area = c(40.0,38.4, 45.8, 43.9, 34.8, 24.1, 12.7, 12.3, 12.7, 36.5, 30.12), left_retired = c(30.2, 25.7,41.3, 25.3, 23.2, 25.4,24.5, 26.4, 18.2, 19.6, 25.99), other = c(5.8, 12.8, 15.5,15.2, 16.2, 22.0, 19.8, 16.7, 18.9,14.1, 15.7))

names(reasons_data)[1] <- "Province"
names(reasons_data)[2] <- "Did not need PHC or have not tried to find one"
names(reasons_data)[3] <- "No PHC available or taking new patients"
names(reasons_data)[4] <- "Had PHC who left or retired"
names(reasons_data)[5] <- "Other reason"

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    no_phcp <- reactive({
        mutate(no_phcp_data, 
               selected = ifelse(province %in% input$province, "1", "0"))
    })
    
    reasons <- reactive({
        dplyr::filter(reasons_data, Province %in% input$province)
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
            ggtitle("Proportion of Population Without a Primary Health Care Provider") +
            theme(plot.title = element_text(hjust = 0.5)) +
            xlab("Provinces") +
            ylab("Proportion") +
            theme(legend.position="none") +
            scale_fill_manual("legend", values = c("0" = "black", "1" = "blue"))
    })
    
    output$reasonsPlot <- renderPlot({
        if (show_reasons()) {
            n <- length(unique(reasons()$Province))
            if (n != 0) {
                plots <- list()
                for (i in 1:n) {
                    province <- unique(reasons()$Province)[i]
                    df <- reasons() %>%
                        filter(Province == province)
                    plots[[i]] <- plot_reasons(df, province)
                }
                do.call("grid.arrange", c(plots, ncol=n))
            }
        }
    })
    
    output$ageGroupPlot <- renderPlot({
        if (show_no_phcp()) {
            n <- length(unique(has_no_phcp()$province))
            if (n != 0) {
                plots <- list()
                for (i in 1:n) {
                    province <- unique(has_no_phcp()$province)[i]
                    df <- has_no_phcp() %>%
                        filter(province == province)
                    plots[[i]] <- plot_age_group(df, province)
                }
                do.call("grid.arrange", c(plots, ncol=n))
            }
        }
    })
    
    output$healthImprovementsPlot <- renderPlot({
        if (show_health_improvements()) {
            n <- length(unique(health_improvements()$province))
            if (n != 0) {
                plots <- list()
                for (i in 1:n) {
                    province <- unique(has_no_phcp()$province)[i]
                    df <- health_improvements() %>%
                        filter(province == province) %>%
                        select(c("has_phcp", "exercise", "lose_weight", "change_diet", "quit_reduce_smoking", "reduce_stress"))
                    plots[[i]] <- plot_health_improvements(df, province)
                }
                do.call("grid.arrange", c(plots, ncol=n))
            }
        }
    })
    
    output$prostateMammogramPlot <- renderPlot({
        if (show_screening()) {
            n <- length(unique(screening()$province))
            if (n != 0) {
                plots <- list()
                for (i in 1:n) {
                    province <- unique(screening()$province)[i]
                    df <- screening() %>%
                        filter(province == province) %>%
                        select(c("has_phcp", "mammogram_screening", "psa_blood_test"))
                    df1 <- melt(df, id.vars = 'has_phcp')
                    plots[[i]] <- plot_prostate_mammogram(df1, province)
                }
                do.call("grid.arrange", c(plots, ncol=n))
            }
        }
    })
})
