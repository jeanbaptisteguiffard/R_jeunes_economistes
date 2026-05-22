# ============================================================
# R pour jeunes économistes
# Séance 2 - Données spatiales et cartes avec R
# ============================================================


# ------------------------------------------------------------
# Charger les packages utiles
# ------------------------------------------------------------

library(tidyverse)
library(sf)


# ------------------------------------------------------------
# Importer une couche géographique
# ------------------------------------------------------------

world_map <- st_read("DATA/world-administrative-boundaries/world-administrative-boundaries.shp")


# ------------------------------------------------------------
# Inspecter un objet sf
# ------------------------------------------------------------

glimpse(world_map)

names(world_map)

head(world_map)


# ------------------------------------------------------------
# Afficher rapidement la géométrie
# ------------------------------------------------------------

plot(st_geometry(world_map))


# ------------------------------------------------------------
# Vérifier le système de coordonnées
# ------------------------------------------------------------

st_crs(world_map)


# ------------------------------------------------------------
# Sélectionner des variables
# ------------------------------------------------------------

world_small <- world_map %>%
  select(name, iso3, continent, geometry)


# ------------------------------------------------------------
# Filtrer des objets géographiques
# ------------------------------------------------------------

europe_map <- world_map %>%
  filter(continent == "Europe")

plot(st_geometry(europe_map))


# ------------------------------------------------------------
# Créer de nouvelles variables
# ------------------------------------------------------------

world_map <- world_map %>%
  mutate(
    is_europe = continent == "Europe"
  )


# ------------------------------------------------------------
# Supprimer temporairement la géométrie
# ------------------------------------------------------------

world_table <- world_map %>%
  st_drop_geometry()

world_table %>%
  count(continent)


# ------------------------------------------------------------
# Transformer une projection
# ------------------------------------------------------------

world_map_3857 <- world_map %>%
  st_transform(3857)

france_map_2154 <- world_map %>%
  filter(iso3 == "FRA") %>%
  st_transform(2154)


# ------------------------------------------------------------
# Calculer une surface
# ------------------------------------------------------------

france_map_2154 <- france_map_2154 %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6
  )


# ------------------------------------------------------------
# Calculer un centroïde
# ------------------------------------------------------------

world_centroids <- world_map %>%
  st_centroid()


# ------------------------------------------------------------
# Agréger des polygones
# ------------------------------------------------------------

continents_map <- world_map %>%
  group_by(continent) %>%
  summarise(
    n_countries = n(),
    .groups = "drop"
  )


# ------------------------------------------------------------
# Créer une zone tampon
# ------------------------------------------------------------

france_buffer <- france_map_2154 %>%
  st_buffer(dist = 100000)


# ------------------------------------------------------------
# Sauvegarder une couche spatiale
# ------------------------------------------------------------

st_write(
  europe_map,
  "DATA/europe_map.gpkg",
  delete_dsn = TRUE
)


# ------------------------------------------------------------
# Chaîne simple de traitement
# ------------------------------------------------------------

europe_map <- world_map %>%
  filter(continent == "Europe") %>%
  select(name, iso3, continent, geometry) %>%
  st_transform(3035) %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6
  )


# ------------------------------------------------------------
# Une première carte avec plot()
# ------------------------------------------------------------

plot(st_geometry(world_map))

plot(world_map["continent"])


# ------------------------------------------------------------
# Une carte avec ggplot2
# ------------------------------------------------------------

ggplot(world_map) +
  geom_sf()


# ------------------------------------------------------------
# Colorer une carte par continent
# ------------------------------------------------------------

map_continent <- ggplot(world_map) +
  geom_sf(
    aes(fill = continent),
    color = "grey70",
    size = 0.1
  ) +
  theme_minimal() +
  labs(
    title = "Carte des continents",
    fill = "Continent"
  )

map_continent


# ------------------------------------------------------------
# Améliorer la lisibilité
# ------------------------------------------------------------

map_continent <- ggplot(world_map) +
  geom_sf(
    aes(fill = continent),
    color = "grey70",
    size = 0.1
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Carte des continents",
    fill = "Continent",
    caption = "Source : données administratives mondiales"
  )

map_continent

# ------------------------------------------------------------
# Merger avec d'autres données
# ------------------------------------------------------------

co2_raw <- read_csv("DATA/owid-co2-data.csv")
co2_raw_2019 <- co2_raw %>%
  filter(year==2019)

world_map_2019 <- world_map %>%
  filter(!is.na(iso3)) %>%
  inner_join(
    co2_raw_2019 %>% filter(!is.na(iso_code)),
    by = c("iso3" = "iso_code")
  )

# ------------------------------------------------------------
# Une carte quantitative
# ------------------------------------------------------------

ggplot(world_map_2019) +
  geom_sf(
    aes(fill = population),
    color = "grey80",
    size = 0.05
  ) +
  scale_fill_viridis_c(
    na.value = "grey90"
  ) +
  theme_minimal() +
  labs(title = "Population par pays",
       fill = "Population"  )




# ------------------------------------------------------------
# Gérer les valeurs manquantes
# ------------------------------------------------------------

ggplot(world_map_2019) +
  geom_sf(
    aes(fill = population),
    color = "grey80",
    size = 0.05
  ) +
  scale_fill_viridis_c(
    na.value = "grey90"
  ) +
  theme_minimal() +
  labs(
    title = "Population par pays",
    fill = "Population",
    caption = "Les zones en gris correspondent aux valeurs manquantes"
  )


# ------------------------------------------------------------
# Créer des classes
# ------------------------------------------------------------

world_map_2019 <- world_map_2019 %>%
  mutate(
    pop_class = cut(
      population,
      breaks = c(0, 1e6, 1e7, 5e7, 1e8, Inf),
      labels = c(
        "< 1 million",
        "1-10 millions",
        "10-50 millions",
        "50-100 millions",
        "> 100 millions"
      )
    )
  )


# ------------------------------------------------------------
# Cartographier des classes
# ------------------------------------------------------------

ggplot(world_map_2019) +
  geom_sf(
    aes(fill = pop_class),
    color = "grey80",
    size = 0.05
  ) +
  theme_minimal() +
  labs(
    title = "Population par pays",
    fill = "Classe de population"
  )


# ------------------------------------------------------------
# Une carte avec tmap
# ------------------------------------------------------------

library(tmap)

tm_shape(world_map_2019) +
  tm_polygons("continent")


# ------------------------------------------------------------
# Une carte interactive avec mapview
# ------------------------------------------------------------
#install.packages('mapview')
library(mapview)

mapview(world_map_2019)



# ------------------------------------------------------------
# Importer la base statistique
# ------------------------------------------------------------

co2_raw <- read_csv("data/raw/owid-co2-data.csv")

co2_2019 <- co2_raw %>%
  filter(year == 2019) %>%
  select(
    iso_code, country, year,
    co2_per_capita, population, gdp
  )


# ------------------------------------------------------------
# Vérifier la base à joindre
# ------------------------------------------------------------

glimpse(co2_2019)

co2_2019 %>%
  summarise(
    n_obs = n(),
    n_countries = n_distinct(iso_code),
    missing_co2_pc = sum(is.na(co2_per_capita))
  )

co2_2019 %>%
  filter(is.na(iso_code)) %>%
  select(country)


# ------------------------------------------------------------
# Réaliser la jointure
# ------------------------------------------------------------

world_co2 <- world_map %>%
  left_join(
    co2_2019,
    by = c("iso3" = "iso_code")
  )


# ------------------------------------------------------------
# Vérifier le résultat de la jointure
# ------------------------------------------------------------

world_co2 %>%
  summarise(
    n_polygons = n(),
    matched_co2 = sum(!is.na(co2_per_capita)),
    missing_co2 = sum(is.na(co2_per_capita))
  )

world_co2 %>%
  filter(is.na(co2_per_capita)) %>%
  st_drop_geometry() %>%
  select(name, iso3)


# ------------------------------------------------------------
# Première carte des émissions de CO2
# ------------------------------------------------------------

ggplot(world_co2) +
  geom_sf(
    aes(fill = co2_per_capita),
    color = "grey80",
    size = 0.05
  ) +
  scale_fill_viridis_c(
    na.value = "grey90"
  ) +
  theme_minimal() +
  labs(
    title = "Émissions de CO2 par habitant en 2019",
    fill = "CO2 par habitant",
    caption = "Source : Our World in Data"
  )


# ------------------------------------------------------------
# Rendre la carte plus lisible
# ------------------------------------------------------------

map_co2_2019 <- ggplot(world_co2) +
  geom_sf(
    aes(fill = co2_per_capita),
    color = "grey80",
    size = 0.05
  ) +
  scale_fill_viridis_c(
    na.value = "grey90"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Émissions de CO2 par habitant en 2019",
    fill = "CO2 par habitant",
    caption = "Source : Our World in Data"
  )

map_co2_2019


# ------------------------------------------------------------
# Créer des classes de valeurs
# ------------------------------------------------------------

world_co2 <- world_co2 %>%
  mutate(
    co2_class = cut(
      co2_per_capita,
      breaks = c(0, 1, 3, 5, 10, 20, Inf),
      labels = c(
        "< 1",
        "1-3",
        "3-5",
        "5-10",
        "10-20",
        "> 20"
      )
    )
  )


# ------------------------------------------------------------
# Cartographier les classes
# ------------------------------------------------------------

ggplot(world_co2) +
  geom_sf(
    aes(fill = co2_class),
    color = "grey80",
    size = 0.05
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Émissions de CO2 par habitant en 2019",
    fill = "Tonnes par habitant",
    caption = "Source : Our World in Data"
  )


# ------------------------------------------------------------
# Comparer deux variables cartographiées
# ------------------------------------------------------------

ggplot(world_co2) +
  geom_sf(
    aes(fill = population),
    color = "grey80",
    size = 0.05
  ) +
  scale_fill_viridis_c(
    trans = "log10",
    na.value = "grey90"
  ) +
  theme_minimal() +
  labs(
    title = "Population par pays en 2019",
    fill = "Population",
    caption = "Source : Our World in Data"
  )


# ------------------------------------------------------------
# Sauvegarder la carte
# ------------------------------------------------------------

ggsave(
  filename = "outputs/figures/map_co2_2019.png",
  plot = map_co2_2019,
  width = 10,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------
# Charger une base de points
# ------------------------------------------------------------

power_plants <- st_read("data/raw/Global_Power_Plants/Power_Plants.shp")

glimpse(power_plants)

st_crs(power_plants)

plot(st_geometry(power_plants))


# ------------------------------------------------------------
# Créer des points à partir de coordonnées
# ------------------------------------------------------------

power_plants_sf <- power_plants_df %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )


# ------------------------------------------------------------
# Afficher des points sur une carte
# ------------------------------------------------------------

ggplot() +
  geom_sf(data = world_map, fill = "grey95", color = "grey80") +
  geom_sf(data = power_plants, size = 0.5, alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Localisation des centrales électriques"
  )


# ------------------------------------------------------------
# Filtrer un type de points
# ------------------------------------------------------------

nuclear_plants <- power_plants %>%
  filter(fuel1 == "Nuclear")

ggplot() +
  geom_sf(data = world_map, fill = "grey95", color = "grey80") +
  geom_sf(data = nuclear_plants, size = 0.8, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Centrales nucléaires"
  )


# ------------------------------------------------------------
# Harmoniser les projections
# ------------------------------------------------------------

st_crs(world_map)
st_crs(power_plants)

power_plants <- st_transform(power_plants, st_crs(world_map))


# ------------------------------------------------------------
# Sélectionner une zone d’étude
# ------------------------------------------------------------

europe_map <- world_map %>%
  filter(continent == "Europe")

power_plants_europe <- power_plants %>%
  st_filter(europe_map)


# ------------------------------------------------------------
# Points dans polygones
# ------------------------------------------------------------

plants_with_country <- power_plants %>%
  st_join(world_map)


# ------------------------------------------------------------
# Compter des points dans des polygones
# ------------------------------------------------------------

points_in_country <- st_intersects(world_map, power_plants)

world_map$n_power_plants <- lengths(points_in_country)


# ------------------------------------------------------------
# Cartographier un comptage
# ------------------------------------------------------------

ggplot(world_map) +
  geom_sf(
    aes(fill = n_power_plants),
    color = "grey80",
    size = 0.05
  ) +
  scale_fill_viridis_c(
    na.value = "grey90"
  ) +
  theme_minimal() +
  labs(
    title = "Nombre de centrales électriques par pays",
    fill = "Nombre"
  )


# ------------------------------------------------------------
# Calculer une densité
# ------------------------------------------------------------

world_map <- world_map %>%
  st_transform(3857) %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    density_power_plants = n_power_plants / area_km2
  )


# ------------------------------------------------------------
# Calculer des centroïdes
# ------------------------------------------------------------

world_centroids <- world_map %>%
  st_centroid()


# ------------------------------------------------------------
# Calculer une distance à une infrastructure
# ------------------------------------------------------------

world_centroids <- world_centroids %>%
  st_transform(3857)

nuclear_plants <- nuclear_plants %>%
  st_transform(3857)

dist_to_nuclear <- st_distance(world_centroids, nuclear_plants)


# ------------------------------------------------------------
# Distance minimale
# ------------------------------------------------------------

world_centroids$dist_nearest_nuclear_km <- apply(
  dist_to_nuclear,
  1,
  min
) / 1000


# ------------------------------------------------------------
# Créer une zone tampon autour des points
# ------------------------------------------------------------

nuclear_buffer_50km <- nuclear_plants %>%
  st_buffer(dist = 50000)


# ------------------------------------------------------------
# Identifier les zones exposées
# ------------------------------------------------------------

world_map$near_nuclear_50km <- lengths(
  st_intersects(world_map, nuclear_buffer_50km)
) > 0


# ------------------------------------------------------------
# Cartographier une exposition
# ------------------------------------------------------------

ggplot(world_map) +
  geom_sf(
    aes(fill = near_nuclear_50km),
    color = "grey80",
    size = 0.05
  ) +
  theme_minimal() +
  labs(
    title = "Pays situés à proximité d’une centrale nucléaire",
    fill = "À moins de 50 km"
  )


# ------------------------------------------------------------
# Le package terra
# ------------------------------------------------------------

library(terra)


# ------------------------------------------------------------
# Lire un raster
# ------------------------------------------------------------

temp_raster <- rast("data/raw/temperature.tif")

temp_raster

crs(temp_raster)

plot(temp_raster)


# ------------------------------------------------------------
# Comprendre un raster
# ------------------------------------------------------------

nrow(temp_raster)
ncol(temp_raster)
res(temp_raster)
ext(temp_raster)
crs(temp_raster)


# ------------------------------------------------------------
# Raster à une couche ou plusieurs couches
# ------------------------------------------------------------

names(temp_raster)

temp_may <- temp_raster[[5]]


# ------------------------------------------------------------
# Afficher un raster
# ------------------------------------------------------------

plot(temp_may)

temp_may_df <- as.data.frame(
  temp_may,
  xy = TRUE,
  na.rm = TRUE
)


# ------------------------------------------------------------
# Cartographier un raster avec ggplot2
# ------------------------------------------------------------

ggplot(temp_may_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = temp_may)) +
  coord_equal() +
  theme_minimal() +
  labs(
    title = "Température moyenne en mai",
    x = "Longitude",
    y = "Latitude",
    fill = "Température"
  )


# ------------------------------------------------------------
# Découper un raster
# ------------------------------------------------------------

europe_map <- world_map %>%
  filter(continent == "Europe")

temp_europe <- crop(temp_raster, vect(europe_map))
temp_europe <- mask(temp_europe, vect(europe_map))


# ------------------------------------------------------------
# Harmoniser les projections
# ------------------------------------------------------------

crs(temp_raster)
st_crs(world_map)

temp_raster_proj <- project(
  temp_raster,
  "EPSG:4326"
)


# ------------------------------------------------------------
# Extraire une valeur raster vers des points
# ------------------------------------------------------------

points_temp <- terra::extract(
  temp_raster,
  vect(points_sf)
)

points_sf$temp <- points_temp[, 2]


# ------------------------------------------------------------
# Extraire une moyenne vers des polygones
# ------------------------------------------------------------

library(exactextractr)

world_map$mean_temp <- exact_extract(
  temp_raster,
  world_map,
  "mean"
)


# ------------------------------------------------------------
# Construire une base d’analyse avec un raster
# ------------------------------------------------------------

communes$mean_temp <- exact_extract(
  temp_raster,
  communes,
  "mean"
)

analysis_data <- communes %>%
  st_drop_geometry() %>%
  select(code_commune, mean_temp, population, income)
