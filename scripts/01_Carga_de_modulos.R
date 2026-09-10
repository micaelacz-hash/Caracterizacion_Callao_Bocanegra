# ==============================================================================
# 01_importar_datos.R
# Caracterizacion Callao - Bocanegra
#
# Objetivo: leer los exports de REDATAM (uno por variable, formato "Frecuencia"
# a nivel de manzana), aplanarlos y unirlos en una sola tabla por manzana lista
# para usar en QGIS (join por el codigo de manzana) o para analisis en R.
#
# Estructura esperada de cada archivo crudo (hoja "Output"):
#   AREA # <codigo>          <codigo>, dpto,prov,distrito,Centro Poblado: X,Mza: NNN
#   (fila en blanco)
#   V:/H:/P: <variable>      Casos   %   Acumulado %
#   <categoria 1>            <casos> <%> <% acum>
#   <categoria 2>            ...
#   Total                    <casos totales>
#   (fila en blanco)
#   No Aplica :              <casos no aplica>      [opcional, no siempre aparece]
#
# A veces un bloque viene como "Tabla vacia" (la manzana no tiene casos para
# esa variable) en vez del cuadro de categorias.
# ==============================================================================

library(readxl)

# ------------------------------------------------------------------------------
# 1. Rutas -------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Se asume que este script se corre desde la raiz del proyecto (RStudio Project
# abierto normalmente ya deja el working directory ahi). Si no, descomenta y
# ajusta la linea de abajo:
# setwd("C:/Caracterizacion_Callao_Bocanegra")

dir_crudos     <- file.path("datos", "crudos")
dir_procesados <- file.path("datos", "procesados")

if (!dir.exists(dir_procesados)) dir.create(dir_procesados, recursive = TRUE)

# Cada archivo crudo con un nombre corto para usarlo despues como columna.
archivos <- c(
  sexo               = "sexo_callao.xlsx",
  tipo_vivienda      = "tipo_vivienda_callao.xlsx",
  condicion_ocupacion = "condicion_ocupacion_vivienda_callao.xlsx",
  tenencia           = "tenencia_vivienda_callao.xlsx",
  habitaciones       = "nro_habitaciones_callao.xlsx",
  personas_hogar     = "personas_por_hogar_callao.xlsx"
)

# ------------------------------------------------------------------------------
# 2. Funcion generica: aplana un export de "Frecuencia" de REDATAM -----------
# ------------------------------------------------------------------------------
# Devuelve un data.frame largo (tidy): una fila por manzana x categoria, con
# columnas: area_id, manzana_label, mza, variable, categoria, casos, pct
leer_frecuencia_redatam <- function(ruta, etiqueta_variable) {

  hoja <- read_excel(ruta, sheet = "Output", col_names = FALSE, .name_repair = "minimal")
  # Trabajamos con una matriz de texto para no pelear con tipos de columna
  m <- as.matrix(hoja)
  n <- nrow(m)

  area_pat <- "^AREA #\\s*([0-9]+)$"

  resultados <- vector("list", length = 3000)  # reserva de espacio, se recorta al final
  k <- 0
  i <- 1

  while (i <= n) {

    celda_b <- m[i, 2]

    if (!is.na(celda_b) && grepl(area_pat, trimws(celda_b))) {

      # OJO: el codigo que aparece despues de "AREA # " (celda_b) NO es el
      # mismo codigo de manzana que usa la cartografia (le sobran 2 ceros al
      # final). El codigo que SI coincide con el shapefile de manzanas
      # (campo LLAVE_MZS) es el que va antes de la primera coma en la
      # descripcion de la manzana (celda C). Verificado contra el shapefile
      # real: ~97.5% de match exacto (el resto son manzanas "8888", que son
      # el codigo generico de viviendas dispersas sin manzana asignada y por
      # lo tanto no tienen poligono propio).
      manzana_label <- m[i, 3]
      area_id       <- trimws(sub(",.*", "", manzana_label))
      mza           <- sub(".*Mza:\\s*", "", manzana_label)

      idx <- i + 2  # fila en blanco en i+1, contenido en i+2
      if (idx > n) { i <- i + 1; next }

      celda_idx <- m[idx, 2]

      # Caso "Tabla vacia": no hay categorias para esta manzana/variable
      if (!is.na(celda_idx) && trimws(celda_idx) == "Tabla vac\u00eda") {
        resultados[[k <- k + 1]] <- data.frame(
          area_id = area_id, manzana_label = manzana_label, mza = mza,
          variable = etiqueta_variable, categoria = NA_character_,
          casos = 0, pct = NA_real_, stringsAsFactors = FALSE
        )
        j <- idx + 1
        # saltar posible "No Aplica" (no lo usamos en el largo, ya se puede
        # agregar aparte si se necesita)
        if (j <= n && is.na(m[j, 2])) {
          if ((j + 1) <= n && !is.na(m[j + 1, 2]) &&
              startsWith(trimws(m[j + 1, 2]), "No Aplica")) {
            j <- j + 2
          }
        }
        i <- j
        next
      }

      # Caso normal: fila de cabecera con "Casos" en la 3ra columna
      if (is.na(m[idx, 3]) || trimws(m[idx, 3]) != "Casos") {
        warning(sprintf("Estructura inesperada en '%s' cerca de la fila %d (area %s); se omite este bloque.",
                         basename(ruta), idx, area_id))
        i <- i + 1
        next
      }

      j <- idx + 1
      while (j <= n) {
        rb <- m[j, 2]
        if (is.na(rb)) { j <- j + 1; break }
        if (trimws(rb) == "Total") { j <- j + 1; break }

        resultados[[k <- k + 1]] <- data.frame(
          area_id = area_id, manzana_label = manzana_label, mza = mza,
          variable = etiqueta_variable, categoria = trimws(rb),
          casos = as.numeric(m[j, 3]), pct = as.numeric(m[j, 4]),
          stringsAsFactors = FALSE
        )
        j <- j + 1
      }

      i <- j

    } else {
      i <- i + 1
    }
  }

  do.call(rbind, resultados[seq_len(k)])
}

# ------------------------------------------------------------------------------
# 3. Leer los 6 archivos y consolidar en una tabla larga ----------------------
# ------------------------------------------------------------------------------
cat("Leyendo archivos crudos de REDATAM...\n")

datos_largos <- do.call(rbind, lapply(names(archivos), function(nombre) {
  ruta <- file.path(dir_crudos, archivos[[nombre]])
  cat(" -", archivos[[nombre]], "\n")
  leer_frecuencia_redatam(ruta, nombre)
}))

cat("Total de filas en formato largo:", nrow(datos_largos), "\n")
cat("Manzanas distintas encontradas:", length(unique(datos_largos$area_id)), "\n")

write.csv(datos_largos, file.path(dir_procesados, "datos_largos_manzanas.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# 4. Tabla ancha por manzana (una fila por manzana, lista para QGIS) ---------
# ------------------------------------------------------------------------------
# Tabla base de manzanas (llave + etiqueta), sin duplicados
manzanas <- unique(datos_largos[, c("area_id", "manzana_label", "mza")])

# Funcion auxiliar: de datos_largos, para una variable dada, arma columnas
# anchas (una por categoria) con los "casos"
ensanchar <- function(var_nombre, prefijo) {
  sub_df <- datos_largos[datos_largos$variable == var_nombre & !is.na(datos_largos$categoria), ]
  if (nrow(sub_df) == 0) return(data.frame(area_id = character(0)))
  anchos <- reshape(
    sub_df[, c("area_id", "categoria", "casos")],
    idvar = "area_id", timevar = "categoria", direction = "wide"
  )
  names(anchos) <- gsub("^casos\\.", paste0(prefijo, "_"), names(anchos))
  # nombres de columna mas cortos y sin caracteres raros
  names(anchos) <- tolower(gsub("[^A-Za-z0-9_]+", "_", names(anchos)))
  anchos[is.na(anchos)] <- 0
  anchos
}

sexo_w        <- ensanchar("sexo", "sexo")
tipo_viv_w    <- ensanchar("tipo_vivienda", "tipoviv")
ocup_w        <- ensanchar("condicion_ocupacion", "ocup")
tenencia_w    <- ensanchar("tenencia", "tenencia")

# Promedios ponderados para habitaciones y personas por hogar (categorias
# numericas -> se usan como valor central de cada categoria)
promedio_ponderado <- function(var_nombre) {
  sub_df <- datos_largos[datos_largos$variable == var_nombre & !is.na(datos_largos$categoria), ]
  sub_df$valor_num <- as.numeric(gsub("[^0-9.]", "", sub_df$categoria))
  agregada <- aggregate(
    cbind(suma_ponderada = valor_num * casos, total_casos = casos) ~ area_id,
    data = sub_df, FUN = sum
  )
  agregada$promedio <- round(agregada$suma_ponderada / agregada$total_casos, 0)
  agregada[, c("area_id", "promedio", "total_casos")]
}

habit_prom <- promedio_ponderado("habitaciones")
names(habit_prom) <- c("area_id", "habitaciones_promedio", "viviendas_con_dato_habitaciones")

pph_prom <- promedio_ponderado("personas_hogar")
names(pph_prom) <- c("area_id", "personas_hogar_promedio", "hogares_con_dato")

# ------------------------------------------------------------------------------
# 5. Unir todo por area_id -----------------------------------------------------
# ------------------------------------------------------------------------------
resumen <- manzanas
for (tabla in list(sexo_w, tipo_viv_w, ocup_w, tenencia_w, habit_prom, pph_prom)) {
  resumen <- merge(resumen, tabla, by = "area_id", all.x = TRUE)
}

# Poblacion total (suma de categorias de sexo) y % hombres
cols_sexo <- grep("^sexo_", names(resumen), value = TRUE)
resumen$poblacion_total <- rowSums(resumen[, cols_sexo, drop = FALSE], na.rm = TRUE)
if ("sexo_hombre" %in% names(resumen)) {
  resumen$pct_hombres <- round(100 * resumen$sexo_hombre / resumen$poblacion_total, 1)
}

# % de viviendas en alquiler (dato que pediste originalmente)
if ("tenencia_alquilada" %in% names(resumen)) {
  cols_tenencia <- grep("^tenencia_", names(resumen), value = TRUE)
  resumen$viviendas_con_tenencia <- rowSums(resumen[, cols_tenencia, drop = FALSE], na.rm = TRUE)
  resumen$pct_alquiler <- round(100 * resumen$tenencia_alquilada / resumen$viviendas_con_tenencia, 1)
}

# Indicador de hacinamiento (proxy socioeconomico: personas por habitacion)
if (all(c("personas_hogar_promedio", "habitaciones_promedio") %in% names(resumen))) {
  resumen$hacinamiento_proxy <- round(resumen$personas_hogar_promedio / resumen$habitaciones_promedio, 2)
}

write.csv(resumen, file.path(dir_procesados, "resumen_manzanas_callao.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cat("\nListo.\n")
cat("- Tabla larga (tidy):", file.path(dir_procesados, "datos_largos_manzanas.csv"), "\n")
cat("- Tabla ancha por manzana (para QGIS):", file.path(dir_procesados, "resumen_manzanas_callao.csv"), "\n")
cat("Manzanas en la tabla ancha:", nrow(resumen), "\n")
cat("Columnas:", paste(names(resumen), collapse = ", "), "\n")
