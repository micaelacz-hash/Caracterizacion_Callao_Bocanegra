# ==============================================================================
# Proyecto: Caracterizacion REDATAM Callao - Bocanegra
# Script: 03_Explorar.R
# Autora: Micaela Cusipum
# Fecha: 10-09-26
#
#
# Objetivo: describir la distribucion de las variables de la base ya
# acondicionada (datos/procesados/base_acondicionada_final.csv), con tablas y
# graficos, para: poblacion (habitantes, % hombres/mujeres), promedio de
# habitaciones, promedio de personas por hogar, hacinamiento, tipo de
# vivienda, y tenencia de la vivienda (% de alquiler).
# ==============================================================================

rm(list = ls())

# ------------------------------------------------------------------------------
# 0. CONFIGURACION Y CARGA DE DATOS
# ------------------------------------------------------------------------------
library(tidyverse)
library(flextable)
library(scales)
library(officer)
library(here)
library(webshot2)   # necesario para exportar las tablas como imagen (paso 6)

# Cargamos la base de datos limpia (ACONDICIONADA, sin NA -- ver Acondicionar.R)
base_limpia <- read_csv(here("datos", "procesados", "base_acondicionada_final.csv"),
                        locale = locale(encoding = "UTF-8"))

cat("Manzanas en la base:", nrow(base_limpia), "\n")
cat("Poblacion total (Bocanegra):", scales::comma(sum(base_limpia$poblacion_total)), "\n")

# ------------------------------------------------------------------------------
# 1. PREPARACION: TABLAS LARGAS POR CATEGORIA -----------------------------------
# ------------------------------------------------------------------------------
# Tipo de vivienda: de columnas anchas (una por categoria) a formato largo
base_tipoviv_larga <- base_limpia %>%
  select(LLAVE_MZS, starts_with("tipoviv_")) %>%
  pivot_longer(cols = starts_with("tipoviv_"), names_to = "categoria", values_to = "viviendas") %>%
  mutate(categoria = case_when(
    categoria == "tipoviv_casa_independiente" ~ "Casa independiente",
    categoria == "tipoviv_departamento_en_edificio" ~ "Departamento en edificio",
    categoria == "tipoviv_vivienda_en_casa_de_vecindad_callej_n_solar_o_corral_n_" ~ "Vivienda en casa de vecindad/callejon/solar",
    categoria == "tipoviv_vivienda_improvisada" ~ "Vivienda improvisada",
    categoria == "tipoviv_viviendas_colectivas" ~ "Vivienda colectiva",
    categoria == "tipoviv_vivienda_en_quinta" ~ "Vivienda en quinta",
    categoria == "tipoviv_local_no_destinado_para_habitaci_n_humana" ~ "Local no destinado para habitacion humana",
    categoria == "tipoviv_otro_tipo_de_vivienda_particular" ~ "Otro tipo de vivienda particular",
    TRUE ~ categoria
  ))

# Tenencia de la vivienda: mismo tratamiento
base_tenencia_larga <- base_limpia %>%
  select(LLAVE_MZS, starts_with("tenencia_")) %>%
  pivot_longer(cols = starts_with("tenencia_"), names_to = "categoria", values_to = "viviendas") %>%
  mutate(categoria = case_when(
    categoria == "tenencia_alquilada" ~ "Alquilada",
    categoria == "tenencia_propia_con_t_tulo_de_propiedad" ~ "Propia, con titulo de propiedad",
    categoria == "tenencia_propia_sin_t_tulo_de_propiedad" ~ "Propia, sin titulo de propiedad",
    categoria == "tenencia_cedida" ~ "Cedida",
    categoria == "tenencia_otra_forma" ~ "Otra forma",
    TRUE ~ categoria
  ))

# ==============================================================================
# 2. TABLAS DESCRIPTIVAS
# ==============================================================================
formato_flextable <- function(tabla, titulo) {
  flextable(tabla) %>%
    add_header_lines(values = titulo) %>%
    add_footer_lines(values = "Fuente: INEI - Censos Nacionales 2017 (REDATAM). A.H. Bocanegra, distrito de Callao.") %>%
    autofit() %>%
    theme_vanilla() %>%
    border_inner_h(part = "body", border = officer::fp_border(width = 0)) %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "body") %>%
    bold(part = "header") %>%
    align(align = "left", part = "footer") %>%
    fontsize(size = 9, part = "footer") %>%
    hline_bottom(part = "body", border = officer::fp_border(width = 1)) %>%
    hline_bottom(part = "footer", border = officer::fp_border(width = 0))
}

# ------------------------------------------------------------------------------
# 2.1 Poblacion segun sexo -------------------------------------------------------
# ------------------------------------------------------------------------------
tabla_sexo <- base_limpia %>%
  summarise(Hombres = sum(sexo_hombre), Mujeres = sum(sexo_mujer)) %>%
  pivot_longer(everything(), names_to = "Sexo", values_to = "Poblacion") %>%
  mutate(Porcentaje = paste0(round(100 * Poblacion / sum(Poblacion), 1), "%"),
         Poblacion = scales::comma(Poblacion)) %>%
  rename(`Total (N)` = Poblacion, `%` = Porcentaje)

ft_sexo <- formato_flextable(tabla_sexo, "Tabla 1. Bocanegra: poblacion segun sexo, Censo 2017")
print(ft_sexo)

# ------------------------------------------------------------------------------
# 2.2 Habitaciones por vivienda (promedio por manzana) ---------------------------
# ------------------------------------------------------------------------------
prom_pond_habitaciones <- weighted.mean(base_limpia$habitaciones_promedio,
                                        w = base_limpia$viviendas_con_dato_habitaciones)
cat("Promedio de habitaciones ponderado (Bocanegra):", round(prom_pond_habitaciones, 2), "\n")

stats_habitaciones <- base_limpia %>%
  summarise(
    `Minimo` = min(habitaciones_promedio, na.rm = TRUE),
    `Percentil 25 (Q1)` = quantile(habitaciones_promedio, 0.25, na.rm = TRUE),
    `Mediana (Q2)` = median(habitaciones_promedio, na.rm = TRUE),
    `Media simple entre manzanas` = mean(habitaciones_promedio, na.rm = TRUE),
    `Promedio ponderado Bocanegra` = prom_pond_habitaciones,
    `Desviacion estandar` = sd(habitaciones_promedio, na.rm = TRUE),
    `Percentil 75 (Q3)` = quantile(habitaciones_promedio, 0.75, na.rm = TRUE),
    `Maximo` = max(habitaciones_promedio, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Estadistico", values_to = "Valor (habitaciones)") %>%
  mutate(`Valor (habitaciones)` = round(`Valor (habitaciones)`, 2))

ft_habitaciones <- formato_flextable(stats_habitaciones,
    "Tabla 2. Bocanegra: numero de habitaciones por vivienda (estadisticos por manzana), Censo 2017") %>%
  add_footer_lines(values = "Nota: estadisticos calculados sobre el promedio de habitaciones de cada manzana (215->213 manzanas). El 'promedio ponderado Bocanegra' pondera por el numero de viviendas con dato en cada manzana.") %>%
  align(align = "justify", part = "footer") %>%
  fontsize(size = 8, part = "footer")
print(ft_habitaciones)

# ------------------------------------------------------------------------------
# 2.3 Personas por hogar (promedio por manzana) -----------------------------------
# ------------------------------------------------------------------------------
prom_pond_pph <- weighted.mean(base_limpia$personas_hogar_promedio,
                               w = base_limpia$hogares_con_dato)
cat("Promedio de personas por hogar ponderado (Bocanegra):", round(prom_pond_pph, 2), "\n")

stats_pph <- base_limpia %>%
  summarise(
    `Minimo` = min(personas_hogar_promedio, na.rm = TRUE),
    `Percentil 25 (Q1)` = quantile(personas_hogar_promedio, 0.25, na.rm = TRUE),
    `Mediana (Q2)` = median(personas_hogar_promedio, na.rm = TRUE),
    `Media simple entre manzanas` = mean(personas_hogar_promedio, na.rm = TRUE),
    `Promedio ponderado Bocanegra` = prom_pond_pph,
    `Desviacion estandar` = sd(personas_hogar_promedio, na.rm = TRUE),
    `Percentil 75 (Q3)` = quantile(personas_hogar_promedio, 0.75, na.rm = TRUE),
    `Maximo` = max(personas_hogar_promedio, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Estadistico", values_to = "Valor (personas)") %>%
  mutate(`Valor (personas)` = round(`Valor (personas)`, 2))

ft_pph <- formato_flextable(stats_pph,
    "Tabla 3. Bocanegra: personas por hogar (estadisticos por manzana), Censo 2017") %>%
  add_footer_lines(values = "Nota: estadisticos calculados sobre el promedio de personas por hogar de cada manzana. El 'promedio ponderado Bocanegra' pondera por el numero de hogares con dato en cada manzana.") %>%
  align(align = "justify", part = "footer") %>%
  fontsize(size = 8, part = "footer")
print(ft_pph)

# ------------------------------------------------------------------------------
# 2.4 Hacinamiento (personas por habitacion, proxy) -------------------------------
# ------------------------------------------------------------------------------
stats_hacinamiento <- base_limpia %>%
  summarise(
    `Minimo` = min(hacinamiento_proxy, na.rm = TRUE),
    `Percentil 25 (Q1)` = quantile(hacinamiento_proxy, 0.25, na.rm = TRUE),
    `Mediana (Q2)` = median(hacinamiento_proxy, na.rm = TRUE),
    `Media` = mean(hacinamiento_proxy, na.rm = TRUE),
    `Desviacion estandar` = sd(hacinamiento_proxy, na.rm = TRUE),
    `Percentil 75 (Q3)` = quantile(hacinamiento_proxy, 0.75, na.rm = TRUE),
    `Maximo` = max(hacinamiento_proxy, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Estadistico", values_to = "Valor (personas/habitacion)") %>%
  mutate(`Valor (personas/habitacion)` = round(`Valor (personas/habitacion)`, 2))

ft_hacinamiento <- formato_flextable(stats_hacinamiento,
    "Tabla 4. Bocanegra: hacinamiento -- personas por habitacion (proxy, por manzana), Censo 2017") %>%
  add_footer_lines(values = "Nota: proxy = promedio de personas por hogar / promedio de habitaciones de la manzana. Incluye TODAS las habitaciones de la vivienda (no solo dormitorios), por lo que puede subestimar el hacinamiento real frente al indicador oficial del INEI (que usa solo dormitorios).") %>%
  align(align = "justify", part = "footer") %>%
  fontsize(size = 8, part = "footer")
print(ft_hacinamiento)

# ------------------------------------------------------------------------------
# 2.5 Tipo de vivienda -------------------------------------------------------------
# ------------------------------------------------------------------------------
tabla_tipoviv <- base_tipoviv_larga %>%
  group_by(categoria) %>%
  summarise(Total = sum(viviendas)) %>%
  mutate(Porcentaje = paste0(round(100 * Total / sum(Total), 1), "%"),
         Total = scales::comma(Total)) %>%
  arrange(desc(parse_number(Total))) %>%
  rename(`Tipo de vivienda` = categoria, `Total (N)` = Total, `%` = Porcentaje)

ft_tipoviv <- formato_flextable(tabla_tipoviv, "Tabla 5. Bocanegra: viviendas segun tipo, Censo 2017")
print(ft_tipoviv)

# ------------------------------------------------------------------------------
# 2.6 Tenencia de la vivienda (incluye % de alquiler) -------------------------------
# ------------------------------------------------------------------------------
tabla_tenencia <- base_tenencia_larga %>%
  group_by(categoria) %>%
  summarise(Total = sum(viviendas)) %>%
  mutate(Porcentaje = paste0(round(100 * Total / sum(Total), 1), "%"),
         Total = scales::comma(Total)) %>%
  arrange(desc(parse_number(Total))) %>%
  rename(`Tenencia de la vivienda` = categoria, `Total (N)` = Total, `%` = Porcentaje)

ft_tenencia <- formato_flextable(tabla_tenencia, "Tabla 6. Bocanegra: viviendas segun tenencia, Censo 2017")
print(ft_tenencia)

pct_alquiler_bocanegra <- round(100 * sum(base_limpia$tenencia_alquilada) / sum(base_limpia$viviendas_con_tenencia), 1)
cat("% de viviendas en alquiler (Bocanegra):", pct_alquiler_bocanegra, "%\n")

# ==============================================================================
# 3. GRAFICOS
# ==============================================================================
tema_graficos <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    plot.caption = element_text(hjust = 0, size = 8, color = "grey50")
  )

fuente_caption <- "Fuente: INEI - Censos Nacionales 2017 (REDATAM). A.H. Bocanegra, distrito de Callao."

# 3.1 Barras: poblacion segun sexo
plot_sexo <- ggplot(tabla_sexo %>% mutate(pob_num = parse_number(`Total (N)`)),
                    aes(x = Sexo, y = pob_num, fill = Sexo)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = paste0(scales::comma(pob_num), " (", `%`, ")")), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Hombres" = "#2E5B88", "Mujeres" = "#D6604D")) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Grafico 1. Poblacion de Bocanegra segun sexo",
       x = "", y = "Poblacion", caption = fuente_caption) +
  tema_graficos + theme(legend.position = "none")
print(plot_sexo)

# 3.2 Histograma: habitaciones promedio por manzana
plot_habitaciones <- ggplot(base_limpia, aes(x = habitaciones_promedio)) +
  geom_histogram(fill = "#4A7C59", color = "white", binwidth = 1) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Grafico 2. Distribucion del numero de habitaciones promedio por manzana",
       x = "Habitaciones promedio (por manzana)", y = "Cantidad de manzanas",
       caption = fuente_caption) +
  tema_graficos
print(plot_habitaciones)

# 3.3 Histograma: personas por hogar promedio por manzana
plot_pph <- ggplot(base_limpia, aes(x = personas_hogar_promedio)) +
  geom_histogram(fill = "#2E5B88", color = "white", binwidth = 1) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Grafico 3. Distribucion de personas por hogar promedio por manzana",
       x = "Personas por hogar promedio (por manzana)", y = "Cantidad de manzanas",
       caption = fuente_caption) +
  tema_graficos
print(plot_pph)

# 3.4 Histograma: hacinamiento (personas por habitacion) por manzana
plot_hacinamiento <- ggplot(base_limpia, aes(x = hacinamiento_proxy)) +
  geom_histogram(fill = "#D73027", color = "white", binwidth = 0.25, alpha = 0.85) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Grafico 4. Distribucion del hacinamiento (personas por habitacion) por manzana",
       x = "Personas por habitacion (proxy)", y = "Cantidad de manzanas",
       caption = fuente_caption) +
  tema_graficos
print(plot_hacinamiento)

# 3.5 Barras horizontal: tipo de vivienda
plot_tipoviv <- ggplot(tabla_tipoviv %>% mutate(pct_num = parse_number(`%`)),
                       aes(x = reorder(`Tipo de vivienda`, pct_num), y = pct_num)) +
  geom_col(fill = "#2E5B88", alpha = 0.85) +
  coord_flip() +
  labs(title = "Grafico 5. Bocanegra: viviendas segun tipo",
       x = "", y = "% de viviendas", caption = fuente_caption) +
  tema_graficos
print(plot_tipoviv)

# 3.6 Barras horizontal: tenencia de la vivienda (resaltando alquiler)
plot_tenencia <- ggplot(tabla_tenencia %>% mutate(pct_num = parse_number(`%`),
                                                  es_alquiler = `Tenencia de la vivienda` == "Alquilada"),
                        aes(x = reorder(`Tenencia de la vivienda`, pct_num), y = pct_num, fill = es_alquiler)) +
  geom_col(alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#D73027", "FALSE" = "#8C96A8")) +
  labs(title = "Grafico 6. Viviendas segun tenencia (alquiler resaltado)",
       x = "", y = "% de viviendas", caption = fuente_caption) +
  tema_graficos + theme(legend.position = "none")
print(plot_tenencia)

# ==============================================================================
# 4. EXPLORACION BIVARIADA (bonus): % alquiler vs. hacinamiento por manzana
# ==============================================================================
# ¿Las manzanas con mas alquiler tienden a tener mas o menos hacinamiento?
correlacion_alquiler_hacin <- cor(base_limpia$pct_alquiler, base_limpia$hacinamiento_proxy,
                                  use = "complete.obs")
cat("Correlacion (% alquiler vs. hacinamiento, por manzana):", round(correlacion_alquiler_hacin, 2), "\n")

plot_alquiler_hacinamiento <- ggplot(base_limpia, aes(x = pct_alquiler, y = hacinamiento_proxy)) +
  geom_point(color = "#2E5B88", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#D73027", linewidth = 0.8) +
  labs(title = "Grafico 7. % de alquiler vs. hacinamiento, por manzana",
       subtitle = paste0("Correlacion = ", round(correlacion_alquiler_hacin, 2)),
       x = "% de viviendas en alquiler", y = "Personas por habitacion (proxy)",
       caption = fuente_caption) +
  tema_graficos
print(plot_alquiler_hacinamiento)

# ==============================================================================
# 5. EXPORTACION MASIVA (Imagenes para el informe descriptivo)
# ==============================================================================
ruta_salida <- "outputs/outputs_exploracion_inicial"

if (!dir.exists(ruta_salida)) {
  dir.create(ruta_salida, recursive = TRUE)
}

save_as_image(ft_sexo,         path = paste0(ruta_salida, "/Tabla1_Sexo.png"))
save_as_image(ft_habitaciones, path = paste0(ruta_salida, "/Tabla2_Habitaciones.png"))
save_as_image(ft_pph,          path = paste0(ruta_salida, "/Tabla3_PersonasPorHogar.png"))
save_as_image(ft_hacinamiento, path = paste0(ruta_salida, "/Tabla4_Hacinamiento.png"))
save_as_image(ft_tipoviv,      path = paste0(ruta_salida, "/Tabla5_TipoVivienda.png"))
save_as_image(ft_tenencia,     path = paste0(ruta_salida, "/Tabla6_Tenencia.png"))

ggsave(paste0(ruta_salida, "/Grafico1_Sexo.jpg"),                  plot = plot_sexo,                 width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico2_Habitaciones.jpg"),          plot = plot_habitaciones,          width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico3_PersonasPorHogar.jpg"),      plot = plot_pph,                   width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico4_Hacinamiento.jpg"),          plot = plot_hacinamiento,          width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico5_TipoVivienda.jpg"),          plot = plot_tipoviv,               width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico6_Tenencia.jpg"),              plot = plot_tenencia,              width = 8, height = 5, bg = "white")
ggsave(paste0(ruta_salida, "/Grafico7_AlquilerHacinamiento.jpg"),  plot = plot_alquiler_hacinamiento, width = 8, height = 5, bg = "white")

cat("\nListo. Tablas y graficos exportados a:", ruta_salida, "\n")
