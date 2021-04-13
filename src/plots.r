library(ggplot2)
library(tidyverse)
library(ggthemes)
devtools::install_github("ricardo-bion/ggradar",dependencies=TRUE)
library(ggradar)
library(scales)
library(dplyr)
library(egg)

# Plot No PHCP by Reasons
plot_reasons <- function(df, x, col_quad="gray65", col_text="white") {    
  row <- df %>%
    filter(str_detect(Province, x))
  x1 <- row %>%
    select(`Did not need PHC or have not tried to find one`)
  x2 <- row %>%
    select('No PHC available or taking new patients')
  x3 <- row %>%
    select('Had PHC who left or retired')
  x4 <- row %>%
    select('Other reason')
  nx <- 4
  list = c(as.numeric(x1), as.numeric(x2), as.numeric(x3), as.numeric(x4))
  sqx <- sqrt(list) 
  df <- data.frame(x=c(sqx[1],-sqx[2],-sqx[3],sqx[4])/2, 
                   y=c(sqx[1],sqx[2],-sqx[3],-sqx[4])/2, 
                   size=sqx, label=x)
  mm <- max(df$size)*1.1
  
  p <- ggplot(data=df, aes(x=x, 
                           y=y, 
                           width=size, 
                           height=size, 
                           group=factor(size))) +
    geom_tile(fill=col_quad) +
    geom_text(mapping = aes(label = paste(list, "%")), colour = "white", position = position_stack(vjust = 0.5)) +
    annotate("text", label = "Did not need PHC \nor have not tried to \nfind one", x = sqx[1]-3.5, y = sqx[1]-2, colour = "white") +
    annotate("text", label = 'No PHC \navailable', x = -sqx[2]+2.5, y = sqx[2]-1, colour = "white") +
    annotate("text", label = 'Had PHC who \nleft or retired', x = -sqx[3] + 2.5, y = -sqx[3] +1.5, colour = "white") +
    annotate("text", label = 'Other \nreason', x = sqx[4] - 1.5, y = -sqx[4] + 0.7, size = 3, colour = "white") +
    geom_hline(aes(yintercept=0), size=0.8) +
    geom_vline(aes(xintercept=0), size=0.8) +
    coord_fixed() +
    xlim(c(-mm,mm)) + ylim(c(-mm,mm)) + 
    scale_colour_wsj('colors6', '') + 
    ggtitle(x) +
    theme_wsj(color='gray') +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.line=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title = element_text(size=16, hjust = 0.5))
  p <- egg::set_panel_size(p=p, width=unit(6, "cm"), height=unit(6, "cm"))
  return(p)
}

# Plot No PHCP by Age Group
plot_age_group <- function(df, prov_name) {
  plot <- ggplot(df, aes(x = age, y = estimation, fill = sex)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    coord_flip() +  # Flip axes
    scale_fill_brewer(palette = "Dark2") +  # Color palette
    ggtitle(prov_name) +
    scale_colour_wsj('colors6', '') + 
    theme_wsj(color='gray') +
    theme(plot.title = element_text(size=16, hjust = 0.5))
  plot <- egg::set_panel_size(p=plot, width=unit(6, "cm"), height=unit(6, "cm"))
  return(plot)
}

# Plot Health Improvements
plot_health_improvements <- function(df, prov_name) {
  plot <- ggradar(df, 
                  plot.title = prov_name, 
                  legend.title = "PCHP status", 
                  axis.labels = c("Exercise", "Lose weight", "Changing diet", "Reduce smoking", "Reduce stress")) +
    scale_colour_wsj('colors6', '') + 
    ggtitle(prov_name) +
    theme_wsj(color='gray') +
    theme(plot.title = element_text(size=16, hjust = 0.5))
  pplot <- egg::set_panel_size(p=plot, width=unit(6, "cm"), height=unit(6, "cm"))
  return(plot)
}

# Plot Mammogram and Prostate Cancer Screening Rates
plot_prostate_mammogram <- function(df, prov_name) {
  plot <- df %>% 
    ggplot(aes(x = has_phcp, y = value, fill = variable)) + 
    geom_bar(stat='identity', position='dodge') + 
    coord_flip() + 
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
    labs(x ="With and Without PCHP", y = "% of people who took the test", fill = "Type of Test") + 
    scale_fill_discrete(labels = c("Mammogram", "Prostate")) +
    scale_colour_wsj('colors6', '') + 
    ggtitle(prov_name) +
    theme_wsj(color='gray') +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.line=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title = element_text(size=16, hjust = 0.5))
  plot <- egg::set_panel_size(p=plot, width=unit(6, "cm"), height=unit(6, "cm"))
  return(plot)
}
