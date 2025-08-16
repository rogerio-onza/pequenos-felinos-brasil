# =====================================================================
# Filtra pontos (Longitude/Latitude/Species/Fonte) por shapefile de range
# - Salva: <nome>_out_of_range.csv (fora/coords inválidas)
# - Sobrescreve o CSV original com apenas os pontos "dentro"
# Requisitos: sf, dplyr, readr, tools
# =====================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(tools)
})

# ======== CONFIGURAÇÕES ========================================================
csv_paths <- c(
  "Trigrinus/Leopardus_tigrinus_consolidated.csv")

shp_path  <- "Trigrinus/tigrinus_brazil_iucn_range.shp"
out_dir   <- "Trigrinus/"
overwrite_original <- TRUE       # <- sobrescreve o CSV com os pontos DENTRO
boundary_policy    <- "intersects"  # "intersects" (conta borda como dentro) ou "within"
# Se seus números usam vírgula decimal, ative:
use_decimal_comma  <- FALSE
# ==============================================================================

# Utilitário: força numérico e aceita vírgula decimal se necessário
to_num <- function(x, comma = FALSE) {
  x <- as.character(x)
  if (comma) x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Função principal: processa 1 CSV
filter_points_by_range <- function(csv_path, range_union, out_dir,
                                   overwrite = TRUE, boundary = c("intersects","within"),
                                   decimal_comma = FALSE) {
  boundary <- match.arg(boundary)
  
  # Lê CSV
  df <- readr::read_csv(csv_path, show_col_types = FALSE, guess_max = 200000)
  
  # Checa colunas obrigatórias
  required_cols <- c("Longitude","Latitude","Species","Fonte")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop("CSV não contém as colunas esperadas: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  
  # Valida coordenadas
  lon <- to_num(df$Longitude, comma = decimal_comma)
  lat <- to_num(df$Latitude,  comma = decimal_comma)
  valid_coord <- is.finite(lon) & is.finite(lat) & abs(lon) <= 180 & abs(lat) <= 90
  
  invalid_df <- df[!valid_coord, , drop = FALSE] %>%
    mutate(.filter_flag = "invalid_coords")
  
  df_valid <- df[valid_coord, , drop = FALSE]
  
  # Se nada válido, só exporta o "fora"
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  base <- tools::file_path_sans_ext(basename(csv_path))
  path_out <- file.path(out_dir, paste0(base, "_out_of_range.csv"))
  
  if (nrow(df_valid) == 0) {
    readr::write_csv(invalid_df, path_out)
    message(sprintf("Arquivo: %s — 0 válidos; %d inválidos -> %s",
                    basename(csv_path), nrow(invalid_df), path_out))
    return(tibble(file = basename(csv_path), n_total = nrow(df),
                  n_valid = 0, n_inside = 0, n_out = nrow(invalid_df),
                  overwritten = FALSE))
  }
  
  # Cria sf de pontos (WGS84) e projeta para o CRS do range
  pts <- sf::st_as_sf(
    df_valid,
    coords = c("Longitude","Latitude"),
    crs = 4326,
    remove = FALSE
  ) |> sf::st_transform(sf::st_crs(range_union))
  
  # Teste espacial
  inside <- if (boundary == "intersects") {
    lengths(sf::st_intersects(pts, range_union)) > 0
  } else {
    as.logical(sf::st_within(pts, range_union, sparse = FALSE)[,1])
  }
  
  inside_df  <- sf::st_drop_geometry(pts[inside, ])
  outside_df <- sf::st_drop_geometry(pts[!inside, ]) %>%
    mutate(.filter_flag = "outside_range")
  
  outside_all <- bind_rows(outside_df, invalid_df)
  
  # Caminhos de saída
  path_in  <- if (overwrite) csv_path else file.path(out_dir, paste0(base, "_in_range.csv"))
  path_out <- file.path(out_dir, paste0(base, "_out_of_range.csv"))
  
  # Grava dentro (sobrescreve original se overwrite = TRUE) e fora
  readr::write_csv(inside_df,  path_in)
  readr::write_csv(outside_all, path_out)
  
  message(sprintf(
    "Arquivo: %s — total=%d; dentro=%d; fora/invalid=%d  -> dentro: %s | fora: %s",
    basename(csv_path), nrow(df), nrow(inside_df), nrow(outside_all),
    ifelse(overwrite, "SOBRESCREVEU ORIGINAL", path_in), path_out
  ))
  
  tibble(
    file        = basename(csv_path),
    n_total     = nrow(df),
    n_valid     = nrow(df_valid),
    n_inside    = nrow(inside_df),
    n_out       = nrow(outside_all),
    overwritten = overwrite
  )
}

# ======== Lê e prepara o shapefile de range ===================================
if (!file.exists(shp_path)) stop("Shapefile não encontrado: ", shp_path, call. = FALSE)
range_raw <- sf::st_read(shp_path, quiet = TRUE)
if (is.na(sf::st_crs(range_raw))) {
  stop("O shapefile não possui CRS definido. Defina o CRS correto (ex.: EPSG:4326) e tente novamente.")
}
range_valid <- sf::st_make_valid(range_raw)
range_union <- sf::st_union(range_valid)

# ======== Roda para todos os CSVs =============================================
if (length(csv_paths) == 0) stop("Nenhum CSV informado em 'csv_paths'.")

resumos <- lapply(csv_paths, function(p) {
  if (!file.exists(p)) stop("CSV não encontrado: ", p, call. = FALSE)
  filter_points_by_range(
    csv_path       = p,
    range_union    = range_union,
    out_dir        = out_dir,
    overwrite      = overwrite_original,
    boundary       = boundary_policy,
    decimal_comma  = use_decimal_comma
  )
}) %>% bind_rows()

print(resumos)
# =====================================================================
# Fim
# =====================================================================
