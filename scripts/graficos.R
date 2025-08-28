# ============================================================
# SUMÁRIO + GRÁFICOS (UCs x Fonte x Espécie) — versão "Felinos do Pampa"
# Requisitos: sf, dplyr, readr, tidyr, forcats, ggplot2, stringr, scales, ggtext, showtext
# Saídas: PNGs e CSVs em ./SAIDAS_RELATORIO
# ============================================================

# -------------------- 0) PACOTES --------------------
# install.packages(c("sf","dplyr","readr","tidyr","forcats","ggplot2","stringr","scales","ggtext","showtext"))
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(ggplot2)
library(stringr)
library(scales)
library(ggtext)
library(showtext)

options(scipen = 999)

# -------------------- 1) PARÂMETROS --------------------
CSV_PATH    <- "todas_especies.csv"         # <-- ajuste para o seu arquivo
UC_SHP_PATH <- "wdpa_brazil.shp"    # <-- caminho do .shp de UCs
OUTDIR      <- "SAIDAS_RELATORIO"
dir.create(OUTDIR, showWarnings = FALSE)

# Paletas (rótulos exibidos nos gráficos)
pal_fonte <- c(
  "Felinos do Pampa" = "#4d4d4d",
  "SALVE"            = "#6a51a3",
  "GBIF"             = "#e24a33"
)
col_inout <- c("Dentro de UCs" = "#228B22", "Fora de UCs" = "#D95D39")

# Projeção métrica para o Brasil (SIRGAS 2000 / Brazil Polyconic)
crs_m <- 5880

# -------------------- 2) FONTE ROBOTO + TEMA GLOBAL (tamanhos maiores) --------------------
try({
  sysfonts::font_add_google("Roboto", "roboto")
  showtext_auto()
}, silent = TRUE)

theme_set(theme_minimal(base_size = 14, base_family = "roboto"))
theme_update(
  axis.title.x = element_text(size = 16, margin = margin(t = 8)),
  axis.title.y = element_text(size = 16, margin = margin(r = 8)),
  axis.text.x  = ggtext::element_markdown(size = 14),
  axis.text.y  = ggtext::element_markdown(size = 14),
  legend.title = element_text(size = 13),
  legend.text  = element_text(size = 13),
  plot.margin  = margin(10, 12, 10, 12)
)

italicize <- function(x) paste0("<i>", x, "</i>")

# -------------------- 3) LEITURA E LIMPEZA --------------------
stopifnot(file.exists(CSV_PATH), file.exists(UC_SHP_PATH))

dados_raw <- read_csv(CSV_PATH, show_col_types = FALSE)
req_cols <- c("Longitude","Latitude","Species","Fonte")
if (!all(req_cols %in% names(dados_raw))) {
  stop("O CSV precisa conter as colunas: Longitude, Latitude, Species e Fonte.")
}

# >>> Substitui FP por Felinos do Pampa logo após ler o CSV <<<
dados <- dados_raw %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude  = as.numeric(Latitude),
    Species   = as.character(Species),
    Fonte     = str_trim(as.character(Fonte)),
    # recode direto na importação
    Fonte     = dplyr::recode(Fonte,
                              "FP"                 = "Felinos do Pampa",
                              "Felino dos Pampas"  = "Felinos do Pampa",
                              "Felinos dos Pampas" = "Felinos do Pampa",
                              .default = Fonte)
  ) %>%
  filter(!is.na(Longitude), !is.na(Latitude), !is.na(Species), !is.na(Fonte)) %>%
  # mantém apenas as fontes finais válidas
  filter(Fonte %in% c("Felinos do Pampa","SALVE","GBIF")) %>%
  mutate(
    Fonte = factor(Fonte, levels = c("Felinos do Pampa","SALVE","GBIF"))
  )

# Ordem alfabética das espécies (consistente em tudo)
species_levels <- sort(unique(dados$Species))
dados <- dados %>% mutate(Species = factor(Species, levels = species_levels))

# -------------------- 4) SF: PONTOS E UCs --------------------
pts_ll <- st_as_sf(dados, coords = c("Longitude","Latitude"), crs = 4326, remove = FALSE)

ucs_ll <- st_read(UC_SHP_PATH, quiet = TRUE)
if (is.na(st_crs(ucs_ll))) st_crs(ucs_ll) <- 4326

pts_m <- st_transform(pts_ll, crs_m)
ucs_m <- st_transform(ucs_ll, crs_m)

# Dentro/Fora
inside_flag <- lengths(st_intersects(pts_m, ucs_m)) > 0

dados_uc <- pts_m %>%
  st_drop_geometry() %>%
  mutate(
    UC_status = factor(ifelse(inside_flag, "Dentro de UCs", "Fora de UCs"),
                       levels = c("Dentro de UCs","Fora de UCs")),
    Species = factor(Species, levels = species_levels)             # garante ordem
  )

# -------------------- 5) TABELAS-SUMÁRIO --------------------
cont_sp_fonte <- dados_uc %>%
  count(Species, Fonte, name = "N") %>%
  arrange(Species, Fonte)

sumario_sp_fonte_uc <- dados_uc %>%
  group_by(Species, Fonte) %>%
  summarise(
    N_total   = n(),
    N_inside  = sum(UC_status == "Dentro de UCs"),
    N_outside = sum(UC_status == "Fora de UCs"),
    .groups   = "drop"
  ) %>%
  mutate(
    pct_inside  = round(100 * N_inside / N_total, 1),
    pct_outside = round(100 * N_outside / N_total, 1)
  ) %>%
  arrange(Species, Fonte)

sumario_por_fonte_uc <- dados_uc %>%
  group_by(Fonte) %>%
  summarise(
    N_total   = n(),
    N_inside  = sum(UC_status == "Dentro de UCs"),
    N_outside = sum(UC_status == "Fora de UCs"),
    pct_inside  = round(100 * N_inside  / N_total, 1),
    pct_outside = round(100 * N_outside / N_total, 1),
    .groups = "drop"
  )

sumario_total_uc <- dados_uc %>%
  summarise(
    N_total   = n(),
    N_inside  = sum(UC_status == "Dentro de UCs"),
    N_outside = sum(UC_status == "Fora de UCs")
  ) %>%
  mutate(
    pct_inside  = round(100 * N_inside  / N_total, 1),
    pct_outside = round(100 * N_outside / N_total, 1)
  )

write_csv(sumario_sp_fonte_uc,  file.path(OUTDIR, "sumario_por_especie_e_fonte_com_UC.csv"))
write_csv(sumario_por_fonte_uc, file.path(OUTDIR, "sumario_por_fonte_com_UC.csv"))
write_csv(sumario_total_uc,     file.path(OUTDIR, "sumario_total_UC.csv"))
write_csv(cont_sp_fonte,        file.path(OUTDIR, "contagem_por_especie_e_fonte.csv"))

# -------------------- 6) GRÁFICOS PRINCIPAIS --------------------
# 6.1) Registros por espécie e fonte (empilhado) — ordem A→Z (A no topo)
g1 <- ggplot(cont_sp_fonte,
             aes(x = Species, y = N, fill = Fonte)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = rev(species_levels), labels = italicize) +
  scale_fill_manual(values = pal_fonte, drop = FALSE, na.translate = FALSE) +
  labs(title = NULL, x = "Espécie", y = "Número de registros", fill = "Fonte")

ggsave(file.path(OUTDIR, "01_registros_por_especie_e_fonte.png"),
       g1, width = 10, height = 7, dpi = 300, bg = "white")

# 6.2) Dentro vs fora por fonte — sem números nas barras
cont_fonte_inout <- dados_uc %>%
  count(Fonte, UC_status, name = "N") %>%
  tidyr::complete(
    Fonte = factor(c("Felinos do Pampa","SALVE","GBIF"),
                   levels = c("Felinos do Pampa","SALVE","GBIF")),
    UC_status = factor(c("Dentro de UCs","Fora de UCs"),
                       levels = c("Dentro de UCs","Fora de UCs")),
    fill = list(N = 0)
  )

g2 <- ggplot(cont_fonte_inout,
             aes(x = Fonte, y = N, fill = UC_status)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = col_inout, drop = FALSE) +
  labs(title = NULL, x = "Fonte", y = "Número de registros", fill = NULL)


ggsave(file.path(OUTDIR, "02_dentro_vs_fora_por_fonte.png"),
       g2, width = 9, height = 6, dpi = 300, bg = "white")

# 6.2b) Total geral dentro vs fora
cont_total_inout <- dados_uc %>% count(UC_status, name = "N")

g2b <- ggplot(cont_total_inout, aes(x = UC_status, y = N, fill = UC_status)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = col_inout, drop = FALSE, na.translate = FALSE) +
  labs(title = NULL, x = NULL, y = "Número de registros")

ggsave(file.path(OUTDIR, "02b_total_dentro_vs_fora.png"),
       g2b, width = 7, height = 5, dpi = 300, bg = "white")

# -------------------- 7) GRÁFICOS EXTRAS --------------------
# 7.1) % dentro/fora por espécie (barras 100%) — ordem A→Z (A no topo)
cont_sp_inout <- dados_uc %>%
  count(Species, UC_status, name = "N") %>%
  group_by(Species) %>%
  mutate(pct = N / sum(N)) %>%
  ungroup()

g3 <- ggplot(cont_sp_inout,
             aes(x = Species, y = pct, fill = UC_status)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_x_discrete(limits = rev(species_levels), labels = italicize) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = col_inout, drop = FALSE, na.translate = FALSE) +
  labs(title = NULL, x = "Espécie", y = "Porcentagem de registros (%)", fill = NULL)

ggsave(file.path(OUTDIR, "03_percentual_dentro_fora_por_especie.png"),
       g3, width = 10, height = 7, dpi = 300, bg = "white")

# 7.2) Heatmap espécie × fonte (contagem) — A no topo
g4 <- ggplot(cont_sp_fonte,
             aes(x = Fonte, y = Species, fill = N)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "grey95", high = "black", na.value = "grey90") +
  scale_y_discrete(limits = rev(species_levels), labels = italicize) +
  labs(title = NULL, x = "Fonte", y = "Espécie", fill = "Número de registros")

ggsave(file.path(OUTDIR, "04_heatmap_especie_x_fonte.png"),
       g4, width = 7.5, height = 8.5, dpi = 300, bg = "white")

# -------------------- 8) TOP 10 UCs COM MAIS REGISTROS --------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

pick_uc_name_field <- function(nms) {
  prefs_exact <- c("nome","name","nm_uc","no_uc","nm_unidade","ds_nome","uc_name","ucname","desig","unit_name")
  for (p in prefs_exact) {
    idx <- which(tolower(nms) == p)
    if (length(idx)) return(nms[idx])
  }
  cand <- grep("nome|name|unid|uc", tolower(nms), value = TRUE)
  if (length(cand)) return(cand[1])
  setdiff(nms, c(attr(ucs_m, "sf_column") %||% "geometry"))[1]
}

# Alias manual p/ unificar UCs equivalentes
uc_alias_map <- c(
  "Iguaçu National Park"                       = "Parque Nacional do Iguaçu",
  "Parque Nacional Do Iguaçu"                  = "Parque Nacional do Iguaçu",
  "Discovery Coast Atlantic Forest Reserves"   = "Reservas de Mata Atlântica da Costa do Descobrimento"
  # adicione outras equivalências aqui se necessário
)

normalize_articles_pt <- function(x_title) {
  out <- str_squish(x_title)
  out <- str_to_title(out)
  for (w in c("Do","Da","De","Dos","Das")) {
    out <- str_replace_all(out, paste0("\\b", w, "\\b"), tolower(w))
  }
  out
}

normalize_uc_name <- function(x) {
  x0 <- str_squish(as.character(x))
  from_lc <- str_to_lower(names(uc_alias_map))
  to_val  <- unname(uc_alias_map)
  idx     <- match(str_to_lower(x0), from_lc)
  x1      <- ifelse(!is.na(idx), to_val[idx], x0)
  normalize_articles_pt(x1)
}

uc_name_col <- pick_uc_name_field(names(ucs_m))

# Une pontos às UCs e obtém nome normalizado
pts_in_uc <- st_join(pts_m, ucs_m, join = st_within, left = FALSE)

if (!uc_name_col %in% names(pts_in_uc)) {
  uc_name_col <- setdiff(names(pts_in_uc),
                         c("Longitude","Latitude","Species","Fonte", attr(pts_in_uc, "sf_column") %||% "geometry"))[1]
}

top_uc_counts <- pts_in_uc %>%
  st_drop_geometry() %>%
  transmute(UC_raw = as.character(.data[[uc_name_col]]),
            UC = normalize_uc_name(UC_raw)) %>%
  filter(!is.na(UC), UC != "") %>%
  count(UC, name = "N") %>%
  arrange(desc(N))

write_csv(top_uc_counts, file.path(OUTDIR, "contagem_registros_por_UC_normalizada.csv"))

top10_uc <- top_uc_counts %>%
  slice_max(N, n = 10, with_ties = FALSE) %>%
  mutate(UC = fct_reorder(UC, N))

# Barras com viridis (N maior = cor mais "quente" ~ amarelo)
g5 <- ggplot(top10_uc, aes(x = UC, y = N, fill = N)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_viridis_c(
    name = NULL,                 # remove o título da legenda
    option = "plasma",
    direction = 1,
    guide = guide_colorbar(      # legenda horizontal
      barwidth  = grid::unit(8, "cm"),
      barheight = grid::unit(0.5, "cm")
    )
  ) +
  labs(title = NULL, x = "Unidade de Conservação", y = "Número de registros", fill = NULL) +
  theme(
    legend.position = "rigth",
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.box.margin = margin(t = 6)
  )

ggsave(file.path(OUTDIR, "05_top10_UCs_mais_registros.png"),
       g5, width = 9, height = 6, dpi = 300, bg = "white")

# -------------------- 9) MOSTRA TABELAS NO CONSOLE --------------------
cat("\n===== SUMÁRIO POR ESPÉCIE & FONTE (com UC) =====\n")
print(sumario_sp_fonte_uc)

cat("\n===== SUMÁRIO POR FONTE (com UC) =====\n")
print(sumario_por_fonte_uc)

cat("\n===== SUMÁRIO TOTAL (com UC) =====\n")
print(sumario_total_uc)

cat(paste0("\nArquivos salvos em: ", normalizePath(OUTDIR), "\n"))
# ============================================================
