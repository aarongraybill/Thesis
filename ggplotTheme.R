library(extrafont)
loadfonts()
# library(fontcm)
# pdf("plot_cm.pdf", family="CM Roman", width=5, height=5)
library(ggplot2)
library(dplyr)
library(purrr)
library(viridis)
library(tidyr)



theme_custom <- function () {
    theme(
      text = element_text(color = "#154F5F",family = 'Trebuchet MS'),
      panel.grid.major = element_line(colour = "#FFFFFF",linetype = 'solid'),
      panel.grid.minor = element_line(colour = "#FFFFFF",linetype = 'solid'),
      panel.border = element_rect(fill = NA, colour = "#154F5F"),
      strip.background.x = element_rect(color = "#154F5F"),
      panel.background = element_rect(fill = '#EBF5F7',colour = "#154F5F"),
      legend.direction = "vertical",
      legend.position = "right",
      legend.title.align = .75,
      axis.ticks = element_line(colour = "#154F5F",lineend = "round"),
      axis.text = element_text(color = "#154F5F"),
      plot.caption = element_text(color='#464646'),
      legend.text = element_text(color='#464646'),
      legend.background = element_rect(fill='#EBF5F7',colour='#154F5F'),
      legend.key = element_rect(fill=NA),
      legend.margin = margin(2,2,2,2),
      plot.title = element_text(colour = '#DD7D17',face = 'bold'),
      plot.subtitle = element_text(color = '#154F5F')
      )
    
}

theme_function <- 
  function () {
    theme(
      text = element_text(color = "#154F5F",family = 'Trebuchet MS'),
      panel.grid.major = element_line(colour = "#FFFFFF",linetype = 'solid'),
      panel.grid.minor = element_line(colour = "#FFFFFF",linetype = 'solid'),
      panel.border = element_rect(fill = NA, colour = "#154F5F"),
      strip.background.x = element_rect(color = "#154F5F"),
      panel.background = element_rect(fill = '#EBF5F7',colour = "#154F5F"),
      legend.direction = "vertical",
      legend.position = "right",
      legend.title.align = .75,
      plot.caption = element_text(color='#464646'),
      legend.text = element_text(color='#464646'),
      legend.background = element_rect(fill='#EBF5F7',colour='#154F5F'),
      legend.key = element_rect(fill=NA),
      legend.margin = margin(2,2,2,2),
      plot.title = element_text(colour = '#DD7D17',face = 'bold'),
      plot.subtitle = element_text(color = '#154F5F'),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
    
  }
