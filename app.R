# setup ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(DT)
library(sf)
library(tmap)
library(tidyverse)

# load data and source script -----------------------------------------

set.seed(2022)

source('scripts/static_renderer.R')

tmap_options(
  basemaps = c(
    'CartoDB.DarkMatter',
    'Stamen.Toner'))

# ui ------------------------------------------------------------------

ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = 'ANLY-585 GIS'),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        'Introduction',
        tabName = 'introduction',
        icon = icon('smile')),
      menuItem(
        'High-water mark',
        tabName = 'high-water-mark',
        icon = icon('ruler')),
      menuItem(
        'Predictions',
        tabName = 'predictions',
        icon = icon('question'),
        startExpanded = TRUE,
        menuSubItem(
          'Duration',
          tabName = 'interactive-map-1',
          icon = icon('clock')),
        menuSubItem(
          'Population density',
          tabName = 'interactive-map-2',
          icon = icon('user')),
        menuSubItem(
          'Elevation',
          tabName = 'interactive-map-3',
          icon = icon('mountain')),
        menuSubItem(
          'Precipitation',
          tabName = 'interactive-map-4',
          icon = icon('tint'))))),
  dashboardBody(
    tags$head(
      tags$link(
        rel = 'stylesheet',
        type = 'text/css',
        href = 'style.css')),
    
    tabItems(
      
      ## 1st tab ---------------------------------------------------------
      
      tabItem(
        tabName = 'introduction',
        tags$br(),
        h2('Fanstastic High-water Marks and Where to Find Them'),
        fluidRow(
          column(
            width = 12,
            tags$figure(
              align = 'center',
              tags$img(
                src = 'https://upload.wikimedia.org/wikipedia/commons/a/aa/Radford_High_Water_Mark_sign_Phillips_WX4SNO.jpg',
                height = 240,
                alt = 'High water mark sign in Bisset Park, VA'),
              tags$figcaption(
                'High water mark sign in Bisset Park, VA, United States')),
            includeMarkdown('data/text/tab1.md')))),
      
      
      ## 2nd tab ----------------------------------------------------------
      tabItem(
        tabName = 'high-water-mark',
        tags$br(),
        fluidRow(
          column(
            width = 12,
            box(
              width = 8,
              plotOutput('fig1')),
            box(
              width = 4,
              includeMarkdown('data/text/tab2.md'),
              height = 6))),
        fluidRow(
          column(
            width = 12,
            box(
              width = 8,
              DTOutput('tbl2'))))),
      
      ## 3rd tab ----------------------------------------------------------
      
      ### 3-1 map ---------------------------------------------------------
      
      
      tabItem(
        tabName = 'interactive-map-1',
        tags$br(),
        h2('High-water mark vs duration'),
        fluidRow(
          column(
            width = 4,
            includeMarkdown('data/text/tab3-map-1.md'),
            hr(),
            selectInput(
              inputId = 'select_type',
              label = h3('Select high-water mark type'),
              choices = list(
                'All' = 'All',
                'Riverine' = 'Riverine',
                'Coastal' = 'Coastal'),
              selected = 'All',
              multiple = FALSE),
            helpText('Note: You can select the either or both high-water ',
                     'marks to display. Any selection will be reflected',
                     ' in the histogram and the map.')),
          column(
            width = 8,
            box(
              width = 10,
              tmapOutput('fig3'),
              title =
                paste0(
                  'High-water mark vs flooded durations ',
                  'after Hurricane Florence (2018)')),
            box(
              width = 10,
              plotOutput('fig4'))))),
      
      
      ### 3-2 map ---------------------------------------------------------
      
      tabItem(
        tabName = 'interactive-map-2',
        tags$br(),
        h2('High-water mark vs population density'),
        fluidRow(
          column(
            width = 4,
            includeMarkdown('data/text/tab3-map-2.md')),
          column(
            width = 8,
            box(
              width = 10,
              tmapOutput('fig5'),
              title = paste0(
                'High-water mark vs population density ',
                'after Hurricane Florence (2018)')),
            box(
              width = 10,
              plotOutput('fig6'))))),
      
      
      ### 3-3map ---------------------------------------------------------
      
      tabItem(
        tabName = 'interactive-map-3',
        tags$br(),
        h2('Height vs elevation'),
        fluidRow(
          column(
            width = 4,
            includeMarkdown('data/text/tab3-map-3.md'),
            hr(),
            helpText('Note: Check to toggle additional elements ',
                     'in the scatterplot.'),
            checkboxInput(
              inputId = 'toggle_overall',
              label = 'Show overall regression line',
              value = TRUE),
            checkboxInput(
              inputId = 'toggle_reference',
              label = 'Show prediction reference',
              value = FALSE)),
          column(
            width = 8,
            box(
              width = 10,
              tmapOutput('fig7'),
              title = 'Height above ground vs elevation'),
            box(
              width = 10,
              plotOutput('fig8'))))),
      
      
      ### 3-4 map --------------------------------------------------------
      
      tabItem(
        tabName = 'interactive-map-4',
        tags$br(),
        h2('Height vs precipitation'),
        fluidRow(
          column(
            width = 4,
            includeMarkdown('data/text/tab3-map-4.md')),
          column(
            width = 8,
            box(
              width = 10,
              tmapOutput('fig9'),
              title = 'High-water mark height vs relative precipitation'),
            box(
              width = 10,
              plotOutput('fig10')))))))
)


# server --------------------------------------------------------------

server <- function(input, output) {
  outputOptions(
    output,
    suspendWhenHidden = FALSE)
  
  
  ## fig1-2 ----------------------------------------------------------
  
  output$fig1 <- renderPlot({
    tmap_mode('plot')
    fig1
  })
  
  output$tbl2 <- renderDT(
    hwm_table,
    options = list(
      pageLength = 5))
  
  
  ## fig3-4 ----------------------------------------------------------
  
  output$fig3 <- renderTmap({
    
    if (input$select_type == 'Riverine') {
      dat_to_plot <-
        dat_to_plot %>%
        filter(hwm_environment == input$select_type)
    } else if (input$select_type == 'Coastal') {
      dat_to_plot <-
        dat_to_plot %>%
        filter(hwm_environment == input$select_type)
    }
    
    tm_shape(
      rasters$duration,
      name = 'Flooded duration') +
      tm_raster(
        title = 'Flooded duration (days)',
        style = 'cont',
        alpha = 0.8,
        palette = 'plasma',
        legend.show = TRUE) +
      tm_shape(
        dat_to_plot %>%
          st_as_sf(),
        name = 'High-water marks') +
      tm_dots(
        title = 'Avg flooded duration<br>within 5km (days)',
        size = 0.05,
        border.alpha = 0,
        col = 'mean_duration_5km',
        pal = 'YlOrRd',
        style = 'cont',
        popup.vars = c(
          'ID' = 'hwm_id',
          'State' = 'stateName',
          'County' = 'countyName',
          'Height above ground (m)' = 'height_above_gnd',
          'Elevation (m)' = 'elev_m',
          'Type' = 'hwm_environment',
          'Average flooded duration (days) within 5km radius' =
            'mean_duration_5km'),
        clustering = TRUE)})
  
  output$fig4 <- renderPlot({
    
    temp_color <- mypal
    
    if (input$select_type == 'Riverine') {
      dat_to_plot <-
        dat_to_plot %>%
        filter(hwm_environment == input$select_type)
      temp_color <- temp_color[2]
    } else if (input$select_type == 'Coastal') {
      dat_to_plot <-
        dat_to_plot %>%
        filter(hwm_environment == input$select_type)
      temp_color <-  temp_color[1]
    }
    
    dat_to_plot %>%
      ggplot(aes(
        x = mean_duration_5km,
        fill = hwm_environment,
        y = after_stat(count))) +
      geom_histogram(
        bins = 20,
        alpha = 0.8,
        position = 'identity') +
      scale_x_continuous(breaks = 0:5) +
      scale_fill_manual(values = temp_color) +
      mytheme() +
      labs(
        title = 'How flooded it is around a high-water mark?',
        caption = 'Data source: USGS',
        x = 'Avg flooded duration within 5km (days)',
        y = 'High-water mark count',
        fill = 'High-water mark type')})
  
  
  ## fig5-6 ----------------------------------------------------------
  
  output$fig5 <- renderTmap({
    tm_shape(rasters$pop_density,
             name = 'Population density') +
      tm_raster(
        title = 'Population density (ppl/km^2)',
        style = 'kmeans',
        alpha = 0.6,
        palette = 'plasma') +
      tm_shape(
        hwm_sfc,
        name = 'High-water mark') +
      tm_dots(
        title = 'High-water mark type',
        border.alpha = 0,
        col = 'hwm_environment',
        style = 'cat',
        pal = mypal,
        size = 0.05,
        popup.vars = c(
          'ID' = 'hwm_id',
          'State' = 'stateName',
          'County' = 'countyName',
          'Height above ground (m)' = 'height_above_gnd',
          'Elevation (m)' = 'elev_m',
          'Type' = 'hwm_environment'),
        alpha = 0.5)})
  
  output$fig6 <- renderPlot({fig6})
  
  ## fig7-8 ----------------------------------------------------------
  
  output$fig7 <- renderTmap({
    tm_shape(
      rasters$hillshade,
      name = 'Hillshade') +
      tm_raster(
        pal = gray.colors(
          n = 10,
          start = 0,
          end = 1),
        style = 'cont',
        alpha = 0.9,
        legend.show = FALSE) +
      tm_shape(
        rasters$elevation,
        name = 'Elevation') +
      tm_raster(
        title = 'Elevation (m)',
        style = 'cont',
        palette = terrain.colors(n = 7),
        alpha = 0.6,
        midpoint = NA) +
      tm_shape(
        name = 'Height above ground',
        hwm_sfc) +
      tm_dots(
        title = 'Height above ground (m)',
        col = 'height_above_gnd',
        border.alpha = 0,
        style = 'pretty',
        size = 'elev_m',
        popup.vars = c(
          'ID' = 'hwm_id',
          'State' = 'stateName',
          'County' = 'countyName',
          'Height above ground (m)' = 'height_above_gnd',
          'Elevation (m)' = 'elev_m',
          'Type' = 'hwm_environment'),
        alpha = 0.3)})
  
  output$fig8 <- renderPlot({
    
    p <- hwm_sfc %>%
      mutate(complete = 'All') %>%
      ggplot(aes(
        x = elev_m,
        y = height_above_gnd,
        color = hwm_environment)) +
      geom_point(
        alpha = 0.5,
        size = 2) +
      geom_smooth(
        formula = 'y ~ x',
        se = FALSE,
        method = lm,
        linewidth = 1.5)
    
    
    if (input$toggle_overall) {
      p <- p +
        geom_smooth(
          formula = 'y ~ x',
          se = FALSE,
          method = lm,
          linewidth = 1.5,
          aes(color = complete))
    } else {
      p <- p +
        scale_color_manual(values = mypal)
    }
    
    if (input$toggle_reference) {
      p <- p +
        geom_abline(
          intercept = 3,
          slope = -1/8,
          size = 2,
          linetype = 'dashed',
          color = 'purple')
    }
    
    p +
      mytheme() +
      labs(
        title = paste0('Heights vs elevations of high-water marks'),
        subtitle = paste0(
          'The red line indicates the regression line',
          'of all data points.'),
        caption = 'Data source: USGS',
        x = 'Elevation (m)',
        y = 'Height above ground (m)',
        color = 'High-water mark type')
  })
  
  
  ## fig9-10 ----------------------------------------------------------
  
  output$fig9 <- renderTmap({
    tm_shape(
      rasters$percent_of_normal_precip,
      name = 'Precipitation') +
      tm_raster(
        title = 'Normal precipitation (%)',
        style = 'cont',
        alpha = 0.8,
        palette = 'Blues',
        legend.show = TRUE) +
      tm_shape(
        hwm_mean_duration_5km,
        name = 'High-water marks') +
      tm_dots(
        title = 'Heigh above ground (ft)',
        size = 0.05,
        border.alpha = 0,
        col = 'height_above_gnd',
        style = 'pretty',
        popup.vars = c(
          'ID' = 'hwm_id',
          'State' = 'stateName',
          'County' = 'countyName',
          'Height above ground (m)' = 'height_above_gnd',
          'Elevation (m)' = 'elev_m',
          'Type' = 'hwm_environment'))})
  
  output$fig10 <- renderPlot({fig10})
}

shinyApp(ui, server)
