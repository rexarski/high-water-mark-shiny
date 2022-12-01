
# setup ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(DT)
library(sf)
library(tmap)
library(tidyverse)

# load data and source script -----------------------------------------

tmap_mode("view")

set.seed(2022)

rasters <-
  terra::aggregate(
    terra::rast("data/rasters.tif"),
    5)

hwm <- read_csv("data/hwm.csv") %>%
  
  # convert feet to meters
  
  mutate(
    height_above_gnd = height_above_gnd * 0.3048,
    elev_m = elev_ft * 0.3048)

rasters$perm_water <-
  rasters$perm_water %>%
  terra::classify(
    cbind(0, NA))

rasters$flooded <-
  rasters$flooded %>%
  terra::classify(
    cbind(0, NA))

rasters$duration <-
  rasters$duration %>%
  terra::classify(
    cbind(0, NA))

tmap_options(
  basemaps = c(
    "CartoDB.DarkMatter",
    "Stamen.Toner"))

samples <-
  rasters$pop_density %>%
  terra::spatSample(
    size = 1000,
    method = "regular",
    as.point = TRUE) %>%
  st_as_sf() %>%
  drop_na()

samples <-
  samples %>%
  mutate(
    pop_density = round(pop_density),
    dist_to_hwm_km = samples %>%
      st_distance(
        # unionized hwm
        hwm %>%
          st_as_sf(
            coords = c(
              "longitude",
              "latitude"),
            crs = 4326) %>%
          st_union()) %>%
      units::set_units("km") %>%
      as.double())

# customized palette

mypal = c("#00c023", "#559bff")

# customized ggplot2 theme

mytheme  <- function() {
  theme_minimal() +
    theme(
      legend.position = 'top',
      axis.title.y = element_text(
        family = 'Roboto',
        face = 'bold',
        vjust = 0.2),
      axis.title.x = element_text(
        family = 'Roboto',
        face = 'bold',
        hjust = 0.5),
      axis.text.y = element_text(
        angle = 30),
      text = element_text(
        family = 'Roboto',
        face = 'bold'),
      plot.title = element_text(
        family = 'Roboto',
        face = 'bold',
        hjust = 0.5),
      plot.subtitle = element_text(
        family = 'Roboto',
        hjust = 0.5),
      plot.caption = element_text(
        family = 'Roboto'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 10, 10))
}

# ui ------------------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "ANLY-585 GIS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Introduction",
        tabName = "introduction",
        icon = icon("smile")),
      menuItem(
        "High-water mark",
        tabName = "high-water-mark",
        icon = icon("ruler")),
      menuItem(
        "Predictions",
        tabName = "predictions",
        icon = icon("question"),
        startExpanded = TRUE,
        menuSubItem(
          "Duration",
          tabName = "interactive-map-1",
          icon = icon("clock")),
        menuSubItem(
          "Population density",
          tabName = "interactive-map-2",
          icon = icon("user")),
        menuSubItem(
          "Elevation",
          tabName = "interactive-map-3",
          icon = icon("mountain")),
        menuSubItem(
          "Precipitation",
          tabName = "interactive-map-4",
          icon = icon("tint"))))),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css")),
    
    tabItems(
      
      ## 1st tab ---------------------------------------------------------
      
      tabItem(
        tabName = "introduction",
        tags$br(),
        h2("Introduction"),
        fluidRow(
          column(
            width = 12,
            includeMarkdown("data/text/tab1.md")))),
      
      
      ## 2nd tab ----------------------------------------------------------
      tabItem(
        tabName = "high-water-mark",
        tags$br(),
        fluidRow(
          column(
            width = 12,
            box(
              plotOutput("fig1")),
            box(
              includeMarkdown("data/text/tab2.md"),
              height = 6))),
        fluidRow(
          column(
            width = 12,
            box(DTOutput("tbl2"),
                width = "100%")))),
      
      ## 3rd tab ----------------------------------------------------------
      
      ### 3-1 map ---------------------------------------------------------
      
      
      tabItem(
        tabName = "interactive-map-1",
        tags$br(),
        h2("High-water mark vs duration"),
        fluidRow(
          column(
            width = 3,
            includeMarkdown("data/text/tab3-map-1.md"),
            hr(),
            selectInput(
              inputId = "select_type",
              label = h3("Select high-water mark type"),
              choices = list(
                "All" = "All",
                "Riverine" = "Riverine",
                "Coastal" = "Coastal"),
              selected = "All",
              multiple = FALSE),
            helpText("Note: Lorem ipsum dolor sit amet, consectetur",
                     "adipiscing elit, sed do eiusmod tempor ut.")),
          column(
            width = 9,
            box(
              tmapOutput("fig3"),
              width = "100%"),
            box(
              plotOutput("fig4"),
              width = "100%")))),
      
      
      ### 3-2 map ---------------------------------------------------------
      
      tabItem(
        tabName = "interactive-map-2",
        tags$br(),
        h2("High-water mark vs population density"),
        fluidRow(
          column(
            width = 3,
            includeMarkdown("data/text/tab3-map-2.md")),
          column(
            width = 9,
            box(
              tmapOutput("fig5"),
              width = "100%"),
            box(
              plotOutput("fig6"),
              width = "100%")))),
      
      
      ### 3-3map ---------------------------------------------------------
      
      tabItem(
        tabName = "interactive-map-3",
        tags$br(),
        h2("Height vs elevation"),
        fluidRow(
          column(
            width = 3,
            includeMarkdown("data/text/tab3-map-3.md"),
            hr(),
            
            checkboxInput(
              inputId = "toggle_overall",
              label = "Show overall regression line",
              value = TRUE),
            checkboxInput(
              inputId = "toggle_reference",
              label = "Show prediction reference",
              value = FALSE),
            
            helpText("Note: Lorem ipsum dolor sit amet, consectetur",
                     "adipiscing elit, sed do eiusmod tempor ut.")),
          column(
            width = 9,
            box(
              tmapOutput("fig7"),
              width = "100%"),
            box(
              plotOutput("fig8"),
              width = "100%")))),
      
      
      ### 3-4 map --------------------------------------------------------
      
      tabItem(
        tabName = "interactive-map-4",
        tags$br(),
        h2("Height vs precipitation"),
        fluidRow(
          column(
            width = 3,
            includeMarkdown("data/text/tab3-map-4.md")),
          column(
            width = 9,
            box(
              tmapOutput("fig9"),
              width = "100%"),
            box(
              plotOutput("fig10"),
              width = "100%"))))))
)


# server --------------------------------------------------------------

server <- function(input, output) {
  outputOptions(
    output,
    suspendWhenHidden = FALSE)
  
  ## reactive -----------------------------------------------------------
  
  hwm_reactive <- reactive({hwm})
  
  hwm_sfc <- reactive({
    hwm_reactive() %>%
      st_as_sf(
        coords = c("longitude", "latitude"),
        crs = 4326)})
  
  hwm_mean_duration_5km <- reactive({
    hwm_sfc() %>%
      mutate(
        mean_duration_5km =
          hwm %>%
          st_as_sf(
            coords = c("longitude", "latitude"),
            crs = 4326) %>%
          st_buffer(5000) %>%
          terra::vect() %>%
          terra::extract(
            rasters$duration %>%
              terra::classify(
                cbind(NA, 0)),
            .,
            mean,
            na.rm = TRUE) %>%
          pull() %>%
          round(2))})
  
  rasters_reactive <- reactive({
    rasters})
  
  rasters_flooded <- reactive({
    rasters_reactive()$flooded})
  
  rasters_duration <- reactive({
    rasters_reactive()$duration})
  
  rasters_elevation <- reactive({
    rasters_reactive()$elevation})
  
  rasters_hillshade <- reactive({
    rasters_reactive()$hillshade})
  
  rasters_pdensity <- reactive({
    rasters_reactive()$pop_density})
  
  rasters_precip <- reactive({
    rasters_reactive()$percent_of_normal_precip})
  
  
  ## fig1-2 ----------------------------------------------------------
  
  output$fig1 <- renderPlot({
    tmap_mode("plot")
    
    tm_style("cobalt") +
      tm_shape(
        rasters_hillshade())+
      tm_raster(
        pal = gray.colors(
          n = 10,
          start = 0,
          end = 1),
        style = "cont",
        alpha = 0.2,
        legend.show = FALSE) +
      tm_shape(
        rasters_flooded(),
        name = "Flooded area") +
      tm_raster(
        style = "cat",
        alpha = 1,
        palette = "Blues",
        legend.show = FALSE) +
      tm_shape(
        hwm_sfc(),
        name = "High-water marks") +
      tm_dots(
        title = "High-water mark type",
        col = "hwm_environment",
        size = 0.05,
        border.alpha = 0,
        style = "cat",
        pal = mypal,
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Height above ground (m)" = "height_above_gnd",
          "Elevation (m)" = "elev_m",
          "Type" = "hwm_environment")) +
      tm_layout(
        title = "High-water mark (NC & SC) after Hurricane Florence")})
  
  output$tbl2 <- renderDT(
    hwm %>%
      group_by(
        stateName,
        hwm_environment) %>%
      summarize(
        `Affected Counties` = n_distinct(countyName),
        `High-water Marks Count` = n(),
        `Avg Height above Ground (m)` = round(
          mean(height_above_gnd), 2),
        `Avg Height Elevation (m)` = round(
          mean(elev_m), 2)) %>%
      arrange(
        desc(`High-water Marks Count`)),
    options = list(
      pageLength = 20))
  
  
  ## fig3-4 ----------------------------------------------------------
  
  tmap_mode("view")
  
  output$fig3 <- renderTmap({
    
    dat_to_plot <- 
      hwm_mean_duration_5km() %>%
      as_tibble()
    
    if (input$select_type == "Riverine") {
      dat_to_plot <-
        dat_to_plot %>%
        filter(hwm_environment == input$select_type)
    } else if (input$select_type == "Coastal") {
      dat_to_plot <-
        dat_to_plot %>%
        filter(hwm_environment == input$select_type)
    }
    
    tm_shape(
      rasters_duration(),
      name = "Flooded duration") +
      tm_raster(
        title = "Flooded duration (days)",
        style = "pretty",
        alpha = 0.8,
        palette = "Reds",
        legend.show = TRUE) +
      tm_shape(
        dat_to_plot %>%
          st_as_sf(),
        name = "High-water marks") +
      tm_dots(
        title = "Avg flooded duration<br>within 5km (days)",
        size = 0.05,
        border.alpha = 0,
        col = "mean_duration_5km",
        pal = "plasma",
        style = "cont",
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Height above ground (m)" = "height_above_gnd",
          "Elevation (m)" = "elev_m",
          "Type" = "hwm_environment",
          "Average flooded duration (days) within 5km radius" =
            "mean_duration_5km")) +
      tm_layout(
        title =
          paste0(
            "High-water mark vs flooded durations<br>",
            "after Hurricane Florence (2018)"))})
  
  output$fig4 <- renderPlot({
    
    dat_to_plot <- 
      hwm_mean_duration_5km() %>%
      as_tibble()
    temp_color <- mypal
    
    if (input$select_type == "Riverine") {
      dat_to_plot <-
        dat_to_plot %>%
        filter(hwm_environment == input$select_type)
      temp_color <- temp_color[2]
    } else if (input$select_type == "Coastal") {
      dat_to_plot <-
        dat_to_plot %>%
        filter(hwm_environment == input$select_type)
      temp_color <-  temp_color[1]
    }
    
    dat_to_plot %>%
      ggplot(aes(
        x = mean_duration_5km,
        fill = hwm_environment,
        y = ..count..)) +
      geom_histogram(
        bins = 20,
        alpha = 0.8,
        position = "identity") +
      scale_x_continuous(breaks = 0:5) +
      scale_fill_manual(values = temp_color) +
      mytheme() +
      labs(
        title = "How flooded it is around a high-water mark?",
        caption = "Data source: USGS",
        x = "Avg flooded duration within 5km (days)",
        y = "High-water mark count",
        fill = "High-water mark type")})
  
  
  ## fig5-6 ----------------------------------------------------------
  
  output$fig5 <- renderTmap({
    tm_shape(rasters_pdensity(),
             name = "Population density") +
      tm_raster(
        title = "Population density (ppl/km^2)",
        style = "kmeans",
        alpha = 0.6,
        palette = "plasma") +
      tm_shape(
        hwm_sfc(),
        name = "High-water mark") +
      tm_dots(
        title = "High-water mark type",
        border.alpha = 0,
        col = "hwm_environment",
        style = "cat",
        pal = mypal,
        size = 0.05,
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Height above ground (m)" = "height_above_gnd",
          "Elevation (m)" = "elev_m",
          "Type" = "hwm_environment"),
        alpha = 0.5) +
      tm_layout(
        title = paste0(
          "High-water mark vs population density<br>",
          "after Hurricane Florence (2018)"))})
  
  output$fig6 <- renderPlot({
    samples %>%
      as_tibble() %>%
      ggplot(aes(
        x = dist_to_hwm_km,
        y = pop_density)) +
      geom_point(
        alpha = 0.5,
        size = 2,
        color = "orange") +
      geom_smooth(
        formula = 'y ~ x',
        method = lm,
        size = 2) +
      mytheme() +
      labs(
        title = "More high-water marks in urban areas than rural areas?",
        subtitle = paste0(
          "Population density of 1,000 rample ",
          "geospatial points vs distances to ",
          "closest high-water marks"),
        caption = "Data source: USGS",
        x = "Distance to closest high-water mark (km)",
        y = "Population denstiy (ppl/km^2)")})
  
  ## fig7-8 ----------------------------------------------------------
  
  output$fig7 <- renderTmap({
    tm_shape(
      rasters_hillshade(),
      name = "Hillshade") +
      tm_raster(
        pal = gray.colors(
          n = 10,
          start = 0,
          end = 1),
        style = "cont",
        alpha = 0.9,
        legend.show = FALSE) +
      tm_shape(
        rasters_elevation(),
        name = "Elevation") +
      tm_raster(
        title = "Elevation (m)",
        style = "cont",
        palette = terrain.colors(n = 7),
        alpha = 0.6,
        midpoint = NA) +
      tm_shape(
        name = "Height above ground",
        hwm_sfc()) +
      tm_dots(
        title = "Height above ground (m)",
        col = "height_above_gnd",
        border.alpha = 0,
        style = "pretty",
        size = "elev_m",
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Height above ground (m)" = "height_above_gnd",
          "Elevation (m)" = "elev_m",
          "Type" = "hwm_environment"),
        alpha = 0.3) +
      tm_layout(
        title =
          "Height above ground vs elevation")})
  
  output$fig8 <- renderPlot({
    
    p <- hwm_sfc() %>%
      mutate(complete = "All") %>%
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
        size = 1.5)
    
    
    if (input$toggle_overall) {
      p <- p +
        geom_smooth(
          formula = 'y ~ x',
          se = FALSE,
          method = lm,
          size = 1.5,
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
          linetype = "dashed",
          color = "purple")
    }
    
    p +
      mytheme() +
      labs(
        title = paste0("Heights vs elevations of high-water marks"),
        subtitle = paste0(
          "The red line indicates the regression line",
          "of all data points."),
        caption = "Data source: USGS",
        x = "Elevation (m)",
        y = "Height above ground (m)",
        color = "High-water mark type")
  })
  
  
  ## fig9-10 ----------------------------------------------------------
  
  output$fig9 <- renderTmap({
    tm_shape(
      rasters_precip(),
      name = "Precipitation") +
      tm_raster(
        title = "Normal precipitation (%)",
        style = "cont",
        alpha = 0.8,
        palette = "Blues",
        legend.show = TRUE) +
      tm_shape(
        hwm_mean_duration_5km(),
        name = "High-water marks") +
      tm_dots(
        title = "Heigh above ground (ft)",
        size = 0.05,
        border.alpha = 0,
        col = "height_above_gnd",
        style = "pretty",
        popup.vars = c(
          "ID" = "hwm_id",
          "State" = "stateName",
          "County" = "countyName",
          "Height above ground (m)" = "height_above_gnd",
          "Elevation (m)" = "elev_m",
          "Type" = "hwm_environment")) +
      tm_layout(
        title =
          paste0("High-water mark height vs relative precipitation"))})
  
  output$fig10 <- renderPlot({
    hwm_sfc() %>%
      mutate(
        precip = hwm_sfc() %>%
          terra::vect() %>%
          terra::extract(
            rasters_precip() %>%
              terra::classify(
                cbind(NA, 0)),
            .) %>%
          pull(percent_of_normal_precip),
        precip = precip / 100) %>%
      ggplot(aes(
        x = precip,
        y = height_above_gnd,
        color = hwm_environment)) +
      geom_point(
        alpha = 0.6) +
      scale_color_manual(values = mypal) +
      geom_smooth(
        formula = 'y ~ x',
        method = lm,
        se = FALSE) +
      scale_x_continuous(labels = scales::percent) +
      mytheme() +
      labs(
        title = "Heights vs Precipitation of high-water marks",
        caption = "Data source: USGS",
        x = "Normal precipitation (%) ",
        y = "Height above ground (m)")})
}

shinyApp(ui, server)
