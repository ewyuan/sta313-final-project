library(ggplot2)
library(tidyverse)
library(ggthemes)
library(ggiraphExtra)
library(scales)
library(dplyr)
library(extrafont)

font_import(pattern="Ubuntu")
#loadfonts(device="win")
#loadfonts(device="postscript")

num_rows = 3
num_cols = 4

# Plot No PHCP by Reasons
plot_reasons <- function(df) {    
  df <- df %>% 
    mutate(xmin = if_else(type == "not_need_PHC", -sqrt(proportion), 
                          ifelse(type == "left_retired", -sqrt(proportion), 0)),
           xmax = if_else(type == "other", sqrt(proportion), 
                          ifelse(type == "no_PMH_area", sqrt(proportion), 0)),
           ymin = if_else(type == "not_need_PHC", sqrt(proportion),
                          ifelse(type == "left_retired", -sqrt(proportion), 0)),
           ymax = if_else(type == "other", -sqrt(proportion), 
                          ifelse(type == "no_PMH_area", sqrt(proportion), 0)))
  
  p <- ggplot(df) +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=type)) +
    geom_text(aes(x = (xmin + xmax)/2,
                  y = (ymin + ymax)/2,
                  label = paste(as.integer(proportion * 100), "%", sep="")),
              color = "white",
              size = 4) +
    facet_wrap(~province, ncol = num_cols, nrow = num_rows) +
    scale_fill_wsj('colors6', '', labels = c("Had PHC who left or retired",
                                             "No PHC available or taking new patients",
                                             "Did not need PHC or have not tried to find one", 
                                             "Other reasons")) +
    theme_minimal(base_family = "Ubuntu Condensed") +
    ggtitle("<Reasons Plot Title>") +
    ylab("") +
    xlab("") +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.title = element_blank(),
          legend.position = "top"
    )
  return(p)
}

# Plot No PHCP by Age Group
plot_age_group <- function(df) {
  plot <- ggplot(df, aes(x = age, color = sex)) +   # Fill column
    geom_linerange(data = df[df$sex=="Male",],
                   aes(ymin = -0.3, ymax = -0.3 + estimation), size = 3.5, alpha = 0.8) +
    geom_linerange(data = df[df$sex=="Female",], 
                   aes(ymin = 0.3, ymax = 0.3 + estimation), size = 3.5, alpha = 0.8) +
    coord_flip() +
    geom_label(aes(x = age, y = 0, label = age), 
               inherit.aes = F,
               size = 3.5, label.padding = unit(0.0, "lines"), label.size = 0,
               label.r = unit(0.0, "lines"), 
               alpha = 0.9, 
               color = "#5D646F",
               family = "Ubuntu Condensed") +
    scale_y_continuous(breaks = c(c(-0.5, -0.25, 0) + -0.3, c(0, 0.25, 0.5) + 0.3),
                       labels = c("0.50", "0.25", "0", "0", "0.25", "0.50")) +
    coord_flip() +
    facet_wrap(~province, ncol = num_cols, nrow = num_rows) +
    scale_color_wsj('colors6', '') +
    theme_minimal(base_family = "Ubuntu Condensed") +
    ggtitle("<Age Group Plot Title>") +
    xlab("Age") +
    ylab("Proportion") +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
          axis.text.x = element_text(size = 10, color = "#5D646F"),
          axis.text.y = element_blank(),
          legend.position = "top"
          )
  return(plot)
}

# Plot Health Improvements
plot_health_improvements <- function(df) {
  df <- df %>%
    rename(
      "More Exercise" = exercise,
      "Lost Weight" = lose_weight,
      "Changed Diet" = change_diet,
      "Quit Smoking" = quit_reduce_smoking,
      "Reduced Stress" = reduce_stress,
      "Received Treatment" = receive_medical_treatment
    )
  plot <- ggRadar(df, 
                  rescale = FALSE, # Should we set this to FALSE?
                  aes(group=has_phcp, 
                      facet=province), 
                  interactive = FALSE) +
    guides(fill = FALSE) +
    scale_colour_wsj('colors6', '', labels = c("With PHCP", "Without PHCP")) + 
    ggtitle("<Health Improvements Plot Title>") +
    theme_minimal(base_family = "Ubuntu Condensed") +
    theme(plot.title = element_text(size=16),
          axis.text.y=element_blank(),
          axis.line=element_blank(),
          legend.position = "top",
          legend.title = element_blank())
  return(plot)
}

# Plot Mammogram and Prostate Cancer Screening Rates
plot_prostate_mammogram <- function(df) {
  plot <- df %>% 
    ggplot(aes(x = has_phcp, y=proportion, fill=factor(type))) + 
    geom_bar(position="dodge",stat="identity") +
    coord_flip() +
    labs(x ="With and Without PCHP", y = "Proportion", fill = "Type of Test") +
    scale_colour_wsj('colors6', '') + 
    scale_fill_wsj('colors6', '', labels = c("Mammogram", "Prostate Cancer")) + 
    ggtitle("<Mammogram and Prostate Cancer Screening Plot Title>") +
    theme_minimal(base_family = "Ubuntu Condensed") +
    scale_x_continuous(breaks = c(0, 1),
                       labels = c("Without", "With")) +
    facet_wrap(~province, ncol = num_cols, nrow = num_rows) +
    theme(axis.line=element_blank(),
          legend.position = "top")
  return(plot)
}
