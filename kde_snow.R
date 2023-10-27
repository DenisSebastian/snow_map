
# Librerias ---------------------------------------------------------------


library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(raster)
library(ggspatial) # get basemaps
library(SpatialKDE)
library(viridis)


# Insumos -----------------------------------------------------------------


snow_deaths <- st_read("data/SnowGIS_SHP/Cholera_Deaths.shp")
snow_pumps <- st_read("data/SnowGIS_SHP/Pumps.shp")
# epsg:27700
# mapview(snow_deaths)+ mapview(snow_pumps, col.region = "red")


# Visualización del muertes por Cólera 1854 --------------------------------



colors = c('#f768a1','#dd3497','#7a0177','#49006a')
pal_death<- colorRampPalette(colors)( max(snow_deaths$Count) )

map_snow <-  ggplot(snow_deaths) +
  annotation_map_tile(zoom = 17, "cartolight") +
  geom_sf(aes(color =  Count), 
          alpha=0.5,  size= 2.5,
          show.legend = "point", inherit.aes = FALSE) +
  scale_colour_gradientn(name = "Count Death", 
                         colours = pal_death)+
  geom_sf(data = snow_pumps,  color =  "#1d91c0", 
          alpha=0.8,  size= 4)+
  ggtitle("Muertes por Cólera 1854") +
  # coord_sf(datum = NA) +
  theme_minimal()
map_snow


ggplot() +
  geom_sf(data = snow_deaths,  aes(color =  Count), 
          alpha=0.1,  size= 1)+
  scale_color_continuous()+
  geom_sf(data = snow_pumps,  color =  "blue", 
          alpha=0.8,  size= 1)+
  ggtitle("Muertes por Cólera 1854") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray80"))


# Cálculo de Kernel Density Estimation de Muertes -------------------------



# **Región de Estudio**
  
roi <- st_convex_hull(st_union(snow_deaths))  %>% st_as_sf()
# mapview(roi)+mapview(snow_deaths)

# **Parámetros de KDE**
  
#Definirán Parámemetros de Estudio
cell_size <- 10 # Tamaño de Celda
band_width <- 50

# grilla vacía
raster_death <- roi %>% 
  create_raster(cell_size = cell_size, side_offset = band_width)

# 
# **Cálculo de KDE**
  
kde_raster <- snow_deaths %>% 
  kde(band_width = band_width, kernel = "quartic",
      grid = raster_death, 
      weights = snow_deaths$Count,
      quiet = T)


# **Filtrar valores de baja densidad**
  
  # Primeramente se realizará un histograma para tomar una decición informada sobre el umbral mínimo de densidad.


hist(kde_raster, breaks = 30, 
     col="#f768a1", 
     main="Histograma KDE",
     ylab="Número de Pixeles", xlab="valor KDE")

kde_raster[kde_raster<10] <- NA


mapview(snow_deaths, col.region = "#cb181d", hide = T)+ 
  mapview(snow_pumps, col.region = "#1d91c0")+
  mapview(kde_raster)



### Visualización de Resultados

# Se observa claramente la fuente de agua sobre la mayor densidad de muertes por cólera, pudiendo hacer una relación espacial.

# rastero to df (na omit)
kde_df <- raster::as.data.frame(kde_raster, xy = TRUE) %>%
  na.omit()

map_snow_kde <-  ggplot() +
  geom_tile(data = kde_df ,
            aes(x = x, y = y,
                fill = layer)) +
  scale_fill_gradientn(name = "Density Death",
                       colours = pal_death) +
  geom_sf(
    data = snow_pumps,
    color =  "#1d91c0",
    alpha = 0.8,
    size = 4
  ) +
  ggtitle(label = "Muertes por cólera en Broad Street (1984)",
          subtitle = "Kernel Density Estimation") +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = "gray80"),
    panel.grid.minor = element_line(colour = "gray80")
  )

map_snow_kde
