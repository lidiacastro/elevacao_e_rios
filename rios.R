# 1. LIBRARIES
#-------------
install.packages("magick")
install.packages("pacman")
pacman::p_load(
  terra,
  elevatr,
  sf,
  geodata,
  tidyverse,
  rayshader
)

# 2. COUNTRY BORDERS
#-------------------
#
path <- getwd()

regions_sf <- geodata::gadm(
  country = "BRA", #Mudar de acordo com o país
  level = 1, #o nivel define país (0), estados(1), cidades (2)...
  path = path
) |>
  sf::st_as_sf()

plot(sf::st_geometry(regions_sf))
print(regions_sf, n = 20)

region_sf <- subset(
  regions_sf,
  NAME_1 == "Ceará" #aqui muda o nome da coluna e coloca o estado/cidade
)

plot(sf::st_geometry(region_sf))

country_sf <- sf::st_union(regions_sf) #não carregar, pois une todo o país
plot(sf::st_geometry(country_sf))


# 3. DOWNLOAD RIVERS
#-------------------

url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_sa_shp.zip"
destfile <- basename(url)

download.file(
  url = url,
  destfile = destfile,
  mode = "wb"
)

unzip(destfile)

# 4. LOAD RIVERS
#---------------

filename <- list.files(
  path = "HydroRIVERS_v10_sa_shp",   #trocar aqui de acordo com o país
  pattern = ".shp",
  full.names = TRUE
)

country_bbox <- sf::st_bbox(region_sf) #mudei aqui ao qual se refere region ou country

print(country_bbox)

# xmin      ymin      xmax      ymax
#  5.956063 45.817059 10.495112 47.808483 suiça
# -73.989708 -33.746316 -28.847640   5.264878 brasil
# -73.795677  -9.818046 -56.097558   2.229883 Amazonas
# -41.245672  -3.973226 -40.775467  -3.777919 Ubajara
# -41.423518  -7.858182 -37.252659  -2.784323 Ceara

bbox_wkt <- "POLYGON((
    -41.423518 -7.858182,
    -41.423518 2.784323,
    -37.252659 2.784323,
    -37.252659 -7.858182,
    -41.423518 -7.858182
))"

country_rivers <- sf::st_read(
  filename,
  wkt_filter = bbox_wkt
) |>
  sf::st_intersection(
    region_sf #region para cidades/ufs e country para o país
  )

plot(sf::st_geometry(country_rivers))

# 5. RIVER WIDTH
#---------------

sort(
  unique(
    country_rivers$ORD_FLOW
  )
)

crs_country <- "+proj=merc +lat_ts=-2 +lon_0=-43 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

country_river_width <- country_rivers |>
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      width == 4 ~ 14,
      width == 5 ~ 12,
      width == 6 ~ 10,
      width == 7 ~ 6,
      width == 8 ~ 4,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = crs_country)

# 6. DEM
#-------

dem <- elevatr::get_elev_raster(
  locations = region_sf,   #region para cidades/ufs e country para o país
  z = 9, clip = "locations"
)

dem_country <- dem |>
  terra::rast() |>
  terra::project(crs_country)

dem_matrix <- rayshader::raster_to_matrix(
  dem_country
)

# 7. RENDER SCENE
#----------------


dem_matrix |>
  rayshader::height_shade(
    texture = colorRampPalette(
      c(
        "#fcc69f",
        "#c67847"
      )
    )(128)
  ) |>
  rayshader::add_overlay(
    rayshader::generate_line_overlay(
      geometry = country_river_width,
      extent = dem_country,
      heightmap = dem_matrix,
      color = "#387B9C",
      linewidth = country_river_width$width,
      data_column_width = "width"
    ), alphalayer = 1
  ) |>
  rayshader::plot_3d(
    dem_matrix,
    zscale = 20,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(600, 600),
    zoom = .5,
    phi = 89,
    theta = 0
  )


rayshader::render_camera(
  zoom = .75
)

# 8. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(u)

download.file(
  url = u,
  destfile = hdri_file,
  mode = "wb"
)

file_name <- "ceará-3d-elevation-rivers.png"

rayshader::render_highquality(
  filename = file_name,
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity_env = 1,
  interactive = FALSE,
  width = 500,
  height = 500
)

# BONUS: ANNOTATE MAP
#--------------------
install.packages("magick")
install.packages("scales")
library(magick)
library(scales)

# load map
map1 <- magick::image_read("ceará-3d-elevation-rivers.png") #mudar de acordo com o nome do mapa ja salvo na pasta

# COLORS
#----------

cols <- rev(c("#276604", "#ddb746", "#ffd3af", "#ffeadb"))

texture <- colorRampPalette(cols)(256)


# Set text color
title_color <- cols[4]
text_color <- "grey20"

# Title

map2 <- magick::image_annotate(
  map1, "Rios e Relevo",
  font = "Georgia",
  color = alpha("#3B170B", 1),
  size = 70, gravity = "northeast",
  location = "+50+100", weight = 500
)
#subtitle
map3 <- magick::image_annotate(
  map2, "C E A R Á",
  font = "Georgia",
  color = "#3B170B",
  size = 85, gravity = "northeast",
  location = "+50+175", weight = 600
)

# caption 1 - author
map4 <- magick::image_annotate(
  map3, "©2024 Lidia Castro",
  font = "Futura",
  color = alpha("black", 1),  #a função alpha define a transparencia, pode retirar
  size = 25, gravity = "southeast",
  location = "+50+70"
)

# caption 2 - data source

map5 <- magick::image_annotate(
  map4, "Data: World Wildlife Fund, Inc. (2006 - 2013) HydroSHEDS database http://www.hydrosheds.org",
  font = "Futura",
  color = alpha("black", 1),
  size = 20, gravity = "southeast",
  location = "+50+50"
)

magick::image_write(
  map5,
  "ceará-3d-elevation-rivers-annotated6.png"
)