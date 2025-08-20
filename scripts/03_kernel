# ===========================================================
# KDE (RAW) POR ESPÉCIE — GeoTIFF p/ QGIS
# - CRS métrico: EPSG:5880
# - Grade global fixa: 1 km por célula (ajuste em res_alvo_m)
# - Saída por espécie:
#     KDE_<sp>_intensity_obs_per_km2_RAW_grid<NN>km.tif
# - Requisitos: sf, dplyr, readr, KernSmooth, raster, rnaturalearth (se baixar Brasil)
# ===========================================================

# --------- PACOTES ---------
# install.packages(c("sf","dplyr","readr","KernSmooth","raster","rnaturalearth"))
library(sf)
library(dplyr)
library(readr)
library(KernSmooth)
library(raster)

# --------- ENTRADAS (ajuste caminhos se quiser ler de arquivo) ---------
# Se você JÁ tem objetos dados_sf (POINT WGS84) e brasil_limite no ambiente, eles serão usados.
# Caso contrário, descomente e ajuste os caminhos abaixo.
caminho_planilha <- "todas_especies.csv"   # CSV com Longitude, Latitude, Species, Fonte
caminho_brasil   <- "brasil.shp"           # shapefile do Brasil (opcional)

# Leitura opcional do CSV -> sf (se dados_sf não existe)
if (!exists("dados_sf", inherits = FALSE)) {
  if (!exists("caminho_planilha", inherits = FALSE)) {
    stop("Defina 'dados_sf' no ambiente OU informe 'caminho_planilha' para leitura do CSV.")
  }
  cat("Lendo planilha...\n")
  dados <- readr::read_csv(caminho_planilha, show_col_types = FALSE)
  stopifnot(all(c("Longitude","Latitude","Species") %in% names(dados)))
  dados <- dados %>% filter(!is.na(Longitude), !is.na(Latitude), !is.na(Species))
  dados_sf <- st_as_sf(dados, coords = c("Longitude","Latitude"), crs = 4326) # WGS84
}

# Leitura opcional do Brasil -> sf (se brasil_limite não existe)
if (!exists("brasil_limite", inherits = FALSE)) {
  if (exists("caminho_brasil", inherits = FALSE) && file.exists(caminho_brasil)) {
    cat("Lendo shapefile do Brasil...\n")
    brasil_limite <- st_read(caminho_brasil, quiet = TRUE)
  } else {
    cat("Baixando Brasil via rnaturalearth...\n")
    brasil_limite <- rnaturalearth::ne_countries(country = "brazil", returnclass = "sf", scale = "large")
  }
}

# --------- 1) PROJEÇÃO MÉTRICA ---------
crs_m    <- 5880
dados_m  <- st_transform(dados_sf, crs_m)
brasil_m <- st_transform(brasil_limite, crs_m)

# --------- 2) BBOX GLOBAL + GRADE (5 KM) ---------
bbox <- st_bbox(brasil_m)

# Grade alvo: 5 km por célula (ajuste aqui se quiser outra resolução)
res_alvo_m <- 1e3  # 5 km

nx <- ceiling((bbox[3] - bbox[1]) / res_alvo_m) + 1
ny <- ceiling((bbox[4] - bbox[2]) / res_alvo_m) + 1
nx <- pmin(pmax(nx, 2), 2500)
ny <- pmin(pmax(ny, 2), 2500)
gridsize <- c(nx, ny)

# Passo real (pode variar alguns metros por arredondamento)
dx <- (bbox[3] - bbox[1]) / (nx - 1)
dy <- (bbox[4] - bbox[2]) / (ny - 1)
grid_km <- round(mean(c(dx, dy))/1000)  # usado nos nomes dos arquivos
cat(sprintf("Grade global: %dx%d (~%.2f km × %.2f km por célula)\n", nx, ny, dx/1000, dy/1000))

range.x <- list(c(bbox[1], bbox[3]), c(bbox[2], bbox[4]))

# --------- 3) BANDWIDTH GLOBAL (comparável entre espécies) ---------
coords_all <- st_coordinates(dados_m)
bw_x_g <- tryCatch(KernSmooth::dpik(coords_all[,1]), error = function(e) stats::bw.nrd0(coords_all[,1]))
bw_y_g <- tryCatch(KernSmooth::dpik(coords_all[,2]), error = function(e) stats::bw.nrd0(coords_all[,2]))
# mínimos de segurança (8–10 km) se dpik falhar ou vier muito pequeno
bw_x_g <- if (!is.finite(bw_x_g) || bw_x_g <= 0) 10000 else bw_x_g
bw_y_g <- if (!is.finite(bw_y_g) || bw_y_g <= 0) 10000 else bw_y_g
cat(sprintf("Bandwidth global (m): bw_x=%.0f, bw_y=%.0f (~%.1f, %.1f km)\n", bw_x_g, bw_y_g, bw_x_g/1000, bw_y_g/1000))

# --------- 4) MÁSCARA E SAÍDAS ---------
br_sp <- as(st_make_valid(st_union(brasil_m)), "Spatial")  # máscara Spatial*
outdir <- "Kernel_density"
dir.create(outdir, showWarnings = FALSE)

# helper: cria raster a partir de XYZ e aplica CRS/máscara
toRaster <- function(df, colname, crs_wkt, mask_sp = NULL) {
  r <- raster::rasterFromXYZ(df[, c("x","y", colname)])
  raster::crs(r) <- crs_wkt
  if (!is.null(mask_sp)) r <- raster::mask(r, mask_sp)
  r
}

# --------- 5) LOOP POR ESPÉCIE (somente KDE RAW) ---------
species_vec <- sort(unique(dados_m$Species))
cat(sprintf("Espécies detectadas: %d\n", length(species_vec)))

for (sp in species_vec) {
  pts_sp <- dplyr::filter(dados_m, Species == sp)
  n_sp   <- nrow(pts_sp)
  if (n_sp < 1) next
  
  cat(sprintf("\n== %s | n = %d ==\n", sp, n_sp))
  
  # ----- KDE -> intensidade obs/km² (RAW) -----
  cm <- as.matrix(st_coordinates(pts_sp)[,1:2, drop = FALSE])
  cm <- cm[stats::complete.cases(cm), , drop = FALSE]
  if (nrow(cm) < 1) next
  
  # usa bw global p/ comparabilidade; assegura mínimos
  bw_x <- max(bw_x_g, 10000)
  bw_y <- max(bw_y_g, 10000)
  
  dens <- KernSmooth::bkde2D(
    x         = cm,
    bandwidth = c(bw_x, bw_y),
    gridsize  = c(nx, ny),
    range.x   = range.x
  )
  
  dens_df <- expand.grid(x = dens$x1, y = dens$x2)
  dens_df$z          <- as.vector(dens$fhat)    # densidade 1/m²
  dens_df$lambda_km2 <- n_sp * dens_df$z * 1e6  # obs esperadas por km²
  
  # ----- salvar apenas RAW -----
  sp_tag   <- gsub("[^A-Za-z0-9]+", "_", sp)
  f_kde_raw <- file.path(outdir, sprintf("KDE_%s_intensity_obs_per_km2_RAW_grid%dkm.tif", sp_tag, grid_km))
  
  r_lkm2_raw <- toRaster(dens_df, "lambda_km2", st_crs(dados_m)$wkt, br_sp)
  raster::writeRaster(r_lkm2_raw, f_kde_raw,
                      format = "GTiff", options = c("COMPRESS=LZW"),
                      datatype = "FLT4S", NAflag = -9999, overwrite = TRUE)
  cat(sprintf(" -> KDE RAW (obs/km²) salvo: %s\n", f_kde_raw))
}

cat("\nFeito! Apenas os arquivos KDE_*_intensity_obs_per_km2_RAW_gridNNkm.tif foram gerados.\n")
