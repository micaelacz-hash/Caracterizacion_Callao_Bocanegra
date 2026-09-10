# ==============================================================================
# Acondicionar.R
# Caracterizacion Callao - Bocanegra
#
# Objetivo: acondicionar la tabla consolidada de manzanas de Bocanegra
# (datos/procesados/resumen_manzanas_bocanegra.csv) antes de usarla en el
# analisis final. Pasos:
#   1. Cargar los datos
#   2. Diagnosticar valores NA (por columna y por manzana)
#   3. Guardar un reporte de NA en outputs/
#   4. Decidir el tratamiento: se eliminan las manzanas sin ningun dato de
#      REDATAM (no se pudieron unir en el cruce espacial)
#   5. Guardar la base limpia final: datos/procesados/base_acondicionada_final.csv
#
# Se corre desde la raiz del proyecto (al abrir el .Rproj el working directory
# ya queda ahi). Si no:
# setwd("C:/Caracterizacion_Callao_Bocanegra")
# ==============================================================================

dir_procesados <- file.path("datos", "procesados")
dir_outputs    <- file.path("outputs")

if (!dir.exists(dir_outputs)) dir.create(dir_outputs, recursive = TRUE)

archivo_entrada <- file.path(dir_procesados, "resumen_manzanas_bocanegra.csv")

# ------------------------------------------------------------------------------
# PASO 1: Cargar los datos
# ------------------------------------------------------------------------------
cat("==============================================================\n")
cat(" Acondicionar.R\n")
cat("==============================================================\n\n")

cat("PASO 1: Cargando datos...\n")

if (!file.exists(archivo_entrada)) {
  stop("No se encontro ", archivo_entrada,
       ". Corre primero scripts/01_importar_datos.R y el cruce espacial con zonas.")
}

datos <- read.csv(archivo_entrada, encoding = "UTF-8", stringsAsFactors = FALSE)

cat("  Archivo leido:", archivo_entrada, "\n")
cat("  Filas (manzanas):", nrow(datos), " | Columnas:", ncol(datos), "\n\n")

# ------------------------------------------------------------------------------
# PASO 2: Diagnostico de valores NA
# ------------------------------------------------------------------------------
cat("PASO 2: Diagnosticando valores NA...\n")

n_na   <- sapply(datos, function(col) sum(is.na(col)))
pct_na <- round(100 * n_na / nrow(datos), 1)

reporte_na <- data.frame(
  variable  = names(datos),
  n_na      = as.integer(n_na),
  pct_na    = pct_na,
  row.names = NULL
)
reporte_na <- reporte_na[order(-reporte_na$n_na), ]
con_na <- reporte_na[reporte_na$n_na > 0, ]

cat("  Columnas con al menos 1 NA:", nrow(con_na), "de", ncol(datos), "\n")

filas_con_na    <- sum(!complete.cases(datos))
filas_completas <- nrow(datos) - filas_con_na

cat("  Manzanas completas (sin ningun NA):", filas_completas,
    sprintf("(%.1f%%)", 100 * filas_completas / nrow(datos)), "\n")
cat("  Manzanas con al menos 1 NA:        ", filas_con_na,
    sprintf("(%.1f%%)", 100 * filas_con_na / nrow(datos)), "\n")

col_llave <- intersect(c("LLAVE_MZS", "area_id", "mza"), names(datos))
llave     <- if (length(col_llave) > 0) col_llave[1] else NA

manzanas_con_na <- character(0)
if (!is.na(llave) && filas_con_na > 0) {
  manzanas_con_na <- datos[[llave]][!complete.cases(datos)]
  cat("  Manzanas (", llave, ") con NA: ", paste(manzanas_con_na, collapse = ", "), "\n", sep = "")
}
cat("\n")

# ------------------------------------------------------------------------------
# PASO 3: Guardar el reporte de NA
# ------------------------------------------------------------------------------
cat("PASO 3: Guardando reporte de NA...\n")

archivo_reporte_csv <- file.path(dir_outputs, "reporte_NA_bocanegra.csv")
write.csv(reporte_na, archivo_reporte_csv, row.names = FALSE, fileEncoding = "UTF-8")

archivo_reporte_txt <- file.path(dir_outputs, "reporte_NA_bocanegra.txt")
sink(archivo_reporte_txt)
cat("Reporte de valores NA - resumen_manzanas_bocanegra.csv\n")
cat("Generado:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("Filas:", nrow(datos), " | Columnas:", ncol(datos), "\n\n")
cat("Columnas con al menos 1 NA:\n")
if (nrow(con_na) == 0) {
  cat("  Ninguna.\n")
} else {
  print(con_na, row.names = FALSE)
}
cat("\nManzanas completas:", filas_completas,
    sprintf("(%.1f%%)", 100 * filas_completas / nrow(datos)), "\n")
cat("Manzanas con al menos 1 NA:", filas_con_na,
    sprintf("(%.1f%%)", 100 * filas_con_na / nrow(datos)), "\n")
if (!is.na(llave) && filas_con_na > 0) {
  cat("\nManzanas (", llave, ") con NA:\n", sep = "")
  print(manzanas_con_na)
}
sink()

cat("  Guardado:", archivo_reporte_csv, "\n")
cat("  Guardado:", archivo_reporte_txt, "\n\n")

# ------------------------------------------------------------------------------
# PASO 4: Decision de tratamiento -> eliminar manzanas sin dato
# ------------------------------------------------------------------------------
cat("PASO 4: Tratamiento de los NA...\n")
cat("  Decision: las manzanas con NA no tienen NINGUN dato de REDATAM (no se\n")
cat("  pudieron unir en el cruce espacial con las zonas), por lo que no se\n")
cat("  pueden imputar. Se eliminan de la base para el analisis estadistico.\n")
cat("  Nota: para el mapa en QGIS estas manzanas se pueden seguir mostrando\n")
cat("  con la categoria 'Sin dato', ya que su geometria si existe.\n\n")

if (filas_con_na > 0) {
  cat("  Manzanas eliminadas (", llave, "):\n", sep = "")
  print(manzanas_con_na)
} else {
  cat("  No hay manzanas que eliminar (0 filas con NA).\n")
}

base_acondicionada_final <- datos[complete.cases(datos), ]

cat("\n  Filas antes de limpiar: ", nrow(datos), "\n")
cat("  Filas despues de limpiar:", nrow(base_acondicionada_final), "\n")
cat("  Filas eliminadas:        ", nrow(datos) - nrow(base_acondicionada_final), "\n\n")

# ------------------------------------------------------------------------------
# PASO 5: Guardar la base final limpia
# ------------------------------------------------------------------------------
cat("PASO 5: Guardando la base acondicionada final...\n")

archivo_base_final <- file.path(dir_procesados, "base_acondicionada_final.csv")
write.csv(base_acondicionada_final, archivo_base_final, row.names = FALSE, fileEncoding = "UTF-8")

cat("  Guardado:", archivo_base_final, "\n")
cat("  Filas:", nrow(base_acondicionada_final), " | Columnas:", ncol(base_acondicionada_final), "\n")
stopifnot(sum(is.na(base_acondicionada_final)) == 0)
cat("  Verificacion: 0 valores NA en la base final.\n\n")

cat("==============================================================\n")
cat(" Listo. 'base_acondicionada_final' quedo cargada en el Environment\n")
cat(" y guardada en:", archivo_base_final, "\n")
cat("==============================================================\n")
