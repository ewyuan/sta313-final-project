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

no_phcp_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/no_phcp_data.csv")
no_phcp_data <- no_phcp_data %>% mutate(selected = "0")
health_improvements_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/health_improvements_data.csv")
screening_data <- read.csv("https://raw.githubusercontent.com/ewyuan/sta313-final-project/master/src/data/screening_data.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    no_phcp <- reactive({
        mutate(no_phcp_data, selected = ifelse(province %in% input$province, "1", "0"))
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
})
