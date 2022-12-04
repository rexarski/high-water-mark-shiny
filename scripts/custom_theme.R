library(tidyverse)

# Customized palette

mypal = c('#00c023', '#559bff')

# Customized ggplot2 theme

mytheme  <- function() {
  theme_minimal() +
    theme(
      legend.position = 'top',
      axis.title.y = element_text(
        face = 'bold',
        vjust = 0.2),
      axis.title.x = element_text(
        face = 'bold',
        hjust = 0.5),
      axis.text.y = element_text(
        angle = 30),
      text = element_text(
        face = 'bold'),
      plot.title = element_text(
        face = 'bold',
        hjust = 0.5),
      plot.subtitle = element_text(
        hjust = 0.5),
      plot.caption = element_text(
        family = 'Roboto'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())
}
