# ============================================================
# R pour jeunes économistes
# Séance 1 — Tidyverse pour la recherche empirique
# ============================================================


# ------------------------------------------------------------
# 1. Charger les packages
# ------------------------------------------------------------

library(tidyverse)


# ------------------------------------------------------------
# 2. Importer la base de données
# ------------------------------------------------------------

co2_raw <- read_csv("DATA/owid-co2-data.csv")


# ------------------------------------------------------------
# 3. Inspecter rapidement la base
# ------------------------------------------------------------

glimpse(co2_raw)

dim(co2_raw)

names(co2_raw)

head(co2_raw)


# ------------------------------------------------------------
# 4. Vérifier l’unité d’observation
# ------------------------------------------------------------

co2_raw %>%
  select(country, iso_code, year, co2, co2_per_capita) %>%
  head()


# ------------------------------------------------------------
# 5. Identifier les pays, régions et agrégats
# ------------------------------------------------------------

co2_raw %>%
  filter(is.na(iso_code)) %>%
  distinct(country) %>%
  arrange(country)


# ------------------------------------------------------------
# 6. Explorer les années disponibles
# ------------------------------------------------------------

co2_raw %>%
  summarise(
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE)
  )

co2_raw %>%
  count(year) %>%
  arrange(year)


# ------------------------------------------------------------
# 7. Explorer les variables clés
# ------------------------------------------------------------

co2_raw %>%
  select(country, iso_code, year, population, gdp, co2, co2_per_capita) %>%
  glimpse()


# ------------------------------------------------------------
# 8. Vérifier les valeurs manquantes
# ------------------------------------------------------------

co2_raw %>%
  summarise(
    missing_population = sum(is.na(population)),
    missing_gdp = sum(is.na(gdp)),
    missing_co2 = sum(is.na(co2)),
    missing_co2_pc = sum(is.na(co2_per_capita))
  )


# ------------------------------------------------------------
# 9. Vérifier les doublons pays-année
# ------------------------------------------------------------

co2_raw %>%
  count(country, year) %>%
  filter(n > 1)


# ------------------------------------------------------------
# 10. Premier filtre : garder les observations depuis 1990
# ------------------------------------------------------------

co2_analysis <- co2_raw %>%
  filter(
    year >= 1990,
    !is.na(iso_code)
  )


# ------------------------------------------------------------
# 11. Sélectionner les variables utiles
# ------------------------------------------------------------

co2_analysis <- co2_raw %>%
  filter(
    year >= 1990,
    !is.na(iso_code)
  ) %>%
  select(
    country, iso_code, year,
    population, gdp,
    co2, co2_per_capita
  )


# ------------------------------------------------------------
# 12. Créer de nouvelles variables
# ------------------------------------------------------------

co2_analysis <- co2_analysis %>%
  mutate(
    gdp_per_capita = gdp / population,
    log_gdp_pc = log(gdp_per_capita),
    log_co2_pc = log(co2_per_capita)
  )


# ------------------------------------------------------------
# 13. Vérifier les valeurs manquantes dans la base d’analyse
# ------------------------------------------------------------

co2_analysis %>%
  summarise(
    missing_population = sum(is.na(population)),
    missing_gdp = sum(is.na(gdp)),
    missing_co2_pc = sum(is.na(co2_per_capita))
  )


# ------------------------------------------------------------
# 14. Garder les observations exploitables
# ------------------------------------------------------------

co2_analysis <- co2_analysis %>%
  filter(
    !is.na(population),
    !is.na(gdp),
    !is.na(co2_per_capita)
  )


# ------------------------------------------------------------
# 15. Vérifier la base obtenue
# ------------------------------------------------------------

glimpse(co2_analysis)

co2_analysis %>%
  summarise(
    n_obs = n(),
    n_countries = n_distinct(country),
    first_year = min(year),
    last_year = max(year)
  )


# ------------------------------------------------------------
# 16. Construire la base d’analyse en une seule chaîne
# ------------------------------------------------------------

co2_analysis <- co2_raw %>%
  filter(
    year >= 1990,
    !is.na(iso_code),
    !is.na(population),
    !is.na(gdp),
    !is.na(co2_per_capita),
    population > 0,
    gdp > 0,
    co2_per_capita > 0
  ) %>%
  mutate(
    gdp_per_capita = gdp / population,
    log_gdp_pc = log(gdp_per_capita),
    log_co2_pc = log(co2_per_capita)
  ) %>%
  select(
    country, iso_code, year,
    population, gdp, gdp_per_capita,
    co2, co2_per_capita,
    log_gdp_pc, log_co2_pc
  )


# ------------------------------------------------------------
# 17. Décrire l’échantillon d’analyse
# ------------------------------------------------------------

co2_analysis %>%
  summarise(
    n_obs = n(),
    n_countries = n_distinct(country),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE)
  )


# ------------------------------------------------------------
# 18. Décrire une variable quantitative
# ------------------------------------------------------------

co2_analysis %>%
  summarise(
    mean_co2_pc = mean(co2_per_capita, na.rm = TRUE),
    median_co2_pc = median(co2_per_capita, na.rm = TRUE),
    sd_co2_pc = sd(co2_per_capita, na.rm = TRUE),
    min_co2_pc = min(co2_per_capita, na.rm = TRUE),
    max_co2_pc = max(co2_per_capita, na.rm = TRUE)
  )


# ------------------------------------------------------------
# 19. Comparer moyenne et médiane
# ------------------------------------------------------------

co2_analysis %>%
  summarise(
    mean_co2_pc = mean(co2_per_capita, na.rm = TRUE),
    median_co2_pc = median(co2_per_capita, na.rm = TRUE)
  )


# ------------------------------------------------------------
# 20. Décrire plusieurs variables avec across()
# ------------------------------------------------------------

co2_analysis %>%
  summarise(
    across(
      c(co2_per_capita, gdp_per_capita, population),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE)
      )
    )
  )


# ------------------------------------------------------------
# 21. Produire des statistiques par pays
# ------------------------------------------------------------

country_stats <- co2_analysis %>%
  group_by(country) %>%
  summarise(
    mean_co2_pc = mean(co2_per_capita, na.rm = TRUE),
    mean_gdp_pc = mean(gdp_per_capita, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 22. Classer les pays selon les émissions moyennes
# ------------------------------------------------------------

country_stats %>%
  arrange(desc(mean_co2_pc)) %>%
  select(country, mean_co2_pc, mean_gdp_pc) %>%
  head(10)


# ------------------------------------------------------------
# 23. Produire des statistiques par année
# ------------------------------------------------------------

year_stats <- co2_analysis %>%
  group_by(year) %>%
  summarise(
    mean_co2_pc = mean(co2_per_capita, na.rm = TRUE),
    median_co2_pc = median(co2_per_capita, na.rm = TRUE),
    n_countries = n_distinct(country),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 24. Repérer les valeurs extrêmes
# ------------------------------------------------------------

co2_analysis %>%
  arrange(desc(co2_per_capita)) %>%
  select(country, year, co2_per_capita, gdp_per_capita) %>%
  head(10)


# ------------------------------------------------------------
# 25. Produire une table descriptive simple
# ------------------------------------------------------------

desc_table <- co2_analysis %>%
  summarise(
    Observations = n(),
    Pays = n_distinct(country),
    `CO2 par habitant moyen` = mean(co2_per_capita, na.rm = TRUE),
    `PIB par habitant moyen` = mean(gdp_per_capita, na.rm = TRUE)
  )

desc_table


# ------------------------------------------------------------
# 26. Graphique : logique générale de ggplot
# ------------------------------------------------------------

ggplot(data = co2_analysis, aes(x = co2_per_capita)) +
  geom_histogram() +
  labs(
    title = "Titre du graphique",
    x = "Nom de l’axe horizontal",
    y = "Nom de l’axe vertical"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 27. Graphique : distribution des émissions de CO2 par habitant
# ------------------------------------------------------------

ggplot(co2_analysis, aes(x = co2_per_capita)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution des émissions de CO2 par habitant",
    x = "CO2 par habitant",
    y = "Nombre d'observations"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 28. Graphique : distribution en log
# ------------------------------------------------------------

ggplot(co2_analysis, aes(x = log_co2_pc)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribution du log des émissions de CO2 par habitant",
    x = "log(CO2 par habitant)",
    y = "Nombre d'observations"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 29. Construire une base agrégée par année
# ------------------------------------------------------------

co2_year <- co2_analysis %>%
  group_by(year) %>%
  summarise(
    mean_co2_pc = mean(co2_per_capita, na.rm = TRUE),
    median_co2_pc = median(co2_per_capita, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 30. Graphique : évolution moyenne dans le temps
# ------------------------------------------------------------

ggplot(co2_year, aes(x = year, y = mean_co2_pc)) +
  geom_line() +
  labs(
    title = "Évolution moyenne des émissions de CO2 par habitant",
    x = "Année",
    y = "CO2 par habitant moyen"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 31. Graphique : comparer moyenne et médiane dans le temps
# ------------------------------------------------------------

ggplot(co2_year, aes(x = year)) +
  geom_line(aes(y = mean_co2_pc), linetype = "solid") +
  geom_line(aes(y = median_co2_pc), linetype = "dashed") +
  labs(
    title = "Émissions de CO2 par habitant : moyenne et médiane",
    x = "Année",
    y = "CO2 par habitant"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 32. Construire une base au niveau pays
# ------------------------------------------------------------

country_summary <- co2_analysis %>%
  group_by(country) %>%
  summarise(
    mean_gdp_pc = mean(gdp_per_capita, na.rm = TRUE),
    mean_co2_pc = mean(co2_per_capita, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(mean_gdp_pc > 0, mean_co2_pc > 0)


# ------------------------------------------------------------
# 33. Graphique : PIB par habitant et émissions de CO2
# ------------------------------------------------------------

ggplot(country_summary, aes(x = mean_gdp_pc, y = mean_co2_pc)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "PIB par habitant et émissions de CO2 par habitant",
    subtitle = "Moyennes par pays depuis 1990",
    x = "PIB par habitant, échelle log",
    y = "CO2 par habitant, échelle log"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 34. Première régression descriptive simple
# ------------------------------------------------------------

model_1 <- lm(log_co2_pc ~ log_gdp_pc, data = co2_analysis)

summary(model_1)