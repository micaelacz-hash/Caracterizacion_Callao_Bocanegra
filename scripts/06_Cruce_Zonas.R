# ==============================================================================
# 06_Cruce_Zonas.R
# Caracterizacion Callao - Bocanegra
#
# Objetivo: asignar a cada una de las 215 manzanas de Bocanegra su zona
# (ZONA 1 a ZONA 5) segun ZONAS.shp, y resumir la poblacion por zona.
#
# Por que hace falta: el campo ZONA de la base (1600, 2500, 2600, 2700, 2800) es
# la zona CENSAL del INEI, no las 5 zonas informales de Bocanegra.
#
# Metodo: ZONAS.shp tiene lotes etiquetados con su zona (campo nom_local). Cada
# lote se asigna a la manzana que contiene su centroide; cada manzana queda en
# la zona que cubre mas area de lotes. Ambas capas estan en UTM 18S
# (EPSG:32718), asi que no se reproyecta.
#
# Entradas (fuera del repositorio, en OneDrive; ajustar dir_gis si cambia):
#   - manzanas_bocanegra.gpkg  (215 manzanas, llave LLAVE_MZS)
#   - ZONAS/ZONAS.shp          (lotes con zona en nom_local)
#   - datos/procesados/base_acondicionada_final.csv (213 manzanas con datos)
#
# Salidas (datos/procesados/):
#   - cruce_zona_censal_zona_bocanegra.csv  (una fila por manzana)
#   - resumen_zonas_bocanegra.csv           (poblacion, sexo y % alquiler por zona)
# ==============================================================================

library(sf)
library(tidyverse)

dir_procesados <- file.path("datos", "procesados")
dir_gis <- "C:/Users/Micaela Cusipuma/OneDrive/BOCANEGRA"

archivo_manzanas <- file.path(dir_gis, "manzanas_bocanegra.gpkg")
archivo_zonas    <- file.path(dir_gis, "ZONAS", "ZONAS.shp")

# ------------------------------------------------------------------------------
# 1. Leer capas ---------------------------------------------------------------
# ------------------------------------------------------------------------------
manzanas <- st_read(archivo_manzanas, quiet = TRUE) |>
  select(LLAVE_MZS, ZONA_censal = ZONA, poblacion_total)
lotes <- st_read(archivo_zonas, quiet = TRUE) |>
  select(nom_local)

stopifnot(st_crs(manzanas) == st_crs(lotes))
cat("Manzanas:", nrow(manzanas), "| Lotes en ZONAS.shp:", nrow(lotes), "\n")
print(table(lotes$nom_local, useNA = "ifany"))

# ------------------------------------------------------------------------------
# 2. Asignar cada lote a la manzana que contiene su centroide -----------------
# ------------------------------------------------------------------------------
lotes <- lotes |>
  mutate(
    nom_local = if_else(is.na(nom_local) | nom_local == "", "(vac\u00edo)", nom_local),
    area_lote = as.numeric(st_area(geometry))
  )

centroides <- st_centroid(lotes)

lotes_en_manzana <- st_join(centroides, manzanas["LLAVE_MZS"], join = st_within) |>
  st_drop_geometry()

# Un centroide solo deberia caer en una manzana; si cayera en mas de una, se
# conserva la primera (misma regla que el cruce original).
lotes_en_manzana <- lotes_en_manzana |>
  mutate(id_lote = row_number()) |>
  group_by(id_lote) |>
  slice(1) |>
  ungroup()

sin_manzana <- lotes_en_manzana |> filter(is.na(LLAVE_MZS))
cat("Lotes cuyo centroide cae fuera de las 215 manzanas:", nrow(sin_manzana), "\n")
print(table(sin_manzana$nom_local))

# ------------------------------------------------------------------------------
# 3. Zona dominante por manzana (mayor area de lotes) -------------------------
# ------------------------------------------------------------------------------
votos <- lotes_en_manzana |>
  filter(!is.na(LLAVE_MZS)) |>
  group_by(LLAVE_MZS, nom_local) |>
  summarise(lotes_zona = n(), area = sum(area_lote), .groups = "drop")

resumen_votos <- votos |>
  group_by(LLAVE_MZS) |>
  arrange(desc(area), .by_group = TRUE) |>
  summarise(
    zona_bocanegra     = first(nom_local),
    n_lotes            = sum(lotes_zona),
    pct_area_dominante = round(100 * first(area) / sum(area), 1),
    zonas_en_conflicto = if (n() > 1) {
      o <- order(nom_local)
      paste0(nom_local[o], ":", lotes_zona[o], collapse = "; ")
    } else "",
    .groups = "drop"
  )

cruce <- manzanas |>
  st_drop_geometry() |>
  left_join(resumen_votos, by = "LLAVE_MZS") |>
  mutate(
    zona_bocanegra = replace_na(zona_bocanegra, "SIN_LOTES"),
    n_lotes        = replace_na(n_lotes, 0L)
  ) |>
  select(LLAVE_MZS, ZONA_censal, poblacion_total, zona_bocanegra, n_lotes,
         pct_area_dominante, zonas_en_conflicto)

write.csv(cruce, file.path(dir_procesados, "cruce_zona_censal_zona_bocanegra.csv"),
          row.names = FALSE, na = "", fileEncoding = "UTF-8")

cat("\n== Tabla cruzada: ZONA censal x zona Bocanegra (n de manzanas) ==\n")
print(table(cruce$ZONA_censal, cruce$zona_bocanegra))

cat("\n== Manzanas con lotes de mas de una zona ==\n")
print(cruce |> filter(zonas_en_conflicto != ""))
cat("\n== Manzanas sin lotes ==\n")
print(cruce |> filter(zona_bocanegra == "SIN_LOTES"))

# ------------------------------------------------------------------------------
# 4. Poblacion, sexo y % de alquiler por zona (213 manzanas con datos) --------
# ------------------------------------------------------------------------------
base_limpia <- read.csv(file.path(dir_procesados, "base_acondicionada_final.csv"),
                        stringsAsFactors = FALSE, colClasses = c(LLAVE_MZS = "character"))

por_zona <- base_limpia |>
  select(LLAVE_MZS, sexo_hombre, sexo_mujer, tenencia_alquilada, viviendas_con_tenencia) |>
  left_join(cruce |> select(LLAVE_MZS, zona_bocanegra), by = "LLAVE_MZS") |>
  mutate(zona = str_remove(zona_bocanegra, "^A\\.H BOCANEGRA ")) |>
  group_by(zona) |>
  summarise(
    manzanas   = n(),
    hombres    = sum(sexo_hombre),
    mujeres    = sum(sexo_mujer),
    total      = hombres + mujeres,
    pct_alquiler = round(100 * sum(tenencia_alquilada) / sum(viviendas_con_tenencia), 1),
    .groups = "drop"
  ) |>
  mutate(pct_poblacion = round(100 * total / sum(total), 1)) |>
  select(zona, manzanas, hombres, mujeres, total, pct_poblacion, pct_alquiler)

stopifnot(sum(por_zona$total) == sum(base_limpia$poblacion_total))

write.csv(por_zona, file.path(dir_procesados, "resumen_zonas_bocanegra.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\n== Poblacion por zona de Bocanegra ==\n")
print(por_zona)
cat("TOTAL:", sum(por_zona$manzanas), "manzanas,", sum(por_zona$total), "habitantes\n")
