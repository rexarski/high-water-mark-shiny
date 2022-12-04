library(sf)
library(tmap)
library(tidyverse)

source('scripts/custom_theme.R')


# Preprocessing -------------------------------------------------------

# Load raster file

rasters <-
  terra::aggregate(
    terra::rast('data/rasters.tif'),
    5)

# Load high-water mark metadata

hwm <- read_csv('data/hwm.csv') %>%
  
  # Convert feet to meters
  
  mutate(
    height_above_gnd = height_above_gnd * 0.3048,
    elev_m = elev_ft * 0.3048)

# Reclassify 'not flooded' as NA

rasters$flooded <-
  rasters$flooded %>%
  terra::classify(
    cbind(0, NA))

# Calculate the distance to closest high-water mark of 1000 sample pts 

samples <-
  rasters$pop_density %>%
  terra::spatSample(
    size = 1000,
    method = 'regular',
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
              'longitude',
              'latitude'),
            crs = 4326) %>%
          st_union()) %>%
      units::set_units('km') %>%
      as.double())

# Convert tibble to sfc

hwm_sfc <-
  hwm %>%
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326)

# Calculate the mean duration within a 5km radius of all hwms

hwm_mean_duration_5km <-
  hwm_sfc %>%
  mutate(
    mean_duration_5km =
      hwm %>%
      st_as_sf(
        coords = c('longitude', 'latitude'),
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
      round(2))


# Data preparation for each map/table ---------------------------------

# Prepare summary data for table 2

hwm_table <- hwm %>%
  group_by(
    State = stateName,
    Type = hwm_environment) %>%
  summarize(
    .groups = 'keep',
    `Affected Counties` = n_distinct(countyName),
    `High-water Marks Count` = n(),
    `Avg Height above Ground (m)` = round(
      mean(height_above_gnd), 2),
    `Avg Height Elevation (m)` = round(
      mean(elev_m), 2)) %>%
  arrange(
    desc(`High-water Marks Count`))

# Prepare data for fig 3

dat_to_plot <- 
  hwm_mean_duration_5km %>%
  as_tibble()


# Generate static plots -----------------------------------------------

# fig1

fig1 <-
  tm_style('cobalt') +
  tm_shape(
    rasters$hillshade) +
  tm_raster(
    pal = gray.colors(
      n = 10,
      start = 0,
      end = 1),
    style = 'cont',
    alpha = 0.2,
    legend.show = FALSE) +
  tm_shape(
    rasters$flooded,
    name = 'Flooded area') +
  tm_raster(
    style = 'cat',
    alpha = 1,
    palette = 'Blues',
    legend.show = FALSE) +
  tm_shape(
    hwm_sfc,
    name = 'High-water marks') +
  tm_dots(
    title = 'High-water mark type',
    col = 'hwm_environment',
    size = 0.05,
    border.alpha = 0,
    style = 'cat',
    pal = mypal,
    popup.vars = c(
      'ID' = 'hwm_id',
      'State' = 'stateName',
      'County' = 'countyName',
      'Height above ground (m)' = 'height_above_gnd',
      'Elevation (m)' = 'elev_m',
      'Type' = 'hwm_environment')) +
  tm_layout(
    title = 'High-water mark (NC & SC) after Hurricane Florence')

# fig6

fig6 <-
  samples %>%
  as_tibble() %>%
  ggplot(aes(
    x = dist_to_hwm_km,
    y = pop_density)) +
  geom_point(
    alpha = 0.5,
    size = 2,
    color = 'orange') +
  geom_smooth(
    formula = 'y ~ x',
    method = lm,
    linewidth = 2) +
  mytheme() +
  labs(
    title = 'More high-water marks in urban areas than rural areas?',
    subtitle = paste0(
      'Population density of 1,000 rample ',
      'geospatial points vs distances to ',
      'closest high-water marks'),
    caption = 'Data source: USGS',
    x = 'Distance to closest high-water mark (km)',
    y = 'Population denstiy (ppl/km^2)')

# fig10

fig10 <-
  hwm_sfc %>%
  mutate(
    precip = hwm_sfc %>%
      terra::vect() %>%
      terra::extract(
        rasters$percent_of_normal_precip %>%
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
    title = 'Heights vs Precipitation of high-water marks',
    caption = 'Data source: USGS',
    x = 'Normal precipitation (%) ',
    y = 'Height above ground (m)')