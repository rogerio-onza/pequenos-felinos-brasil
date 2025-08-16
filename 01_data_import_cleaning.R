# Script de Importação e Limpeza de Dados - Felinos Pequenos
# Autor: Rogério Nunes Oliveira
# Data: 10/08/2025

# Carregar pacotes necessários
library(rgbif)
library(dplyr)
library(readxl)
library(readr)
library(sf)
library(sp)
library(tibble)

# Definir diretório de trabalho (ajustar conforme necessário)
# setwd("seu/diretorio/aqui")

# =====================================
# 1. FUNÇÃO PARA BUSCAR DADOS NO GBIF
# =====================================

fetch_gbif <- function(sp) {
  message("Buscando GBIF: ", sp)
  res <- tryCatch(
    occ_search(
      scientificName = sp,
      country        = "BR",
      hasCoordinate  = TRUE,
      limit          = 10000
    ),
    error = function(e) {
      warning("Erro na busca GBIF para ", sp, ": ", e$message)
      return(NULL)
    }
  )
  if (is.null(res) || is.null(res$data)) {
    return(tibble())
  }
  occ <- res$data
  if (!is.data.frame(occ) || nrow(occ) == 0) {
    return(tibble())
  }
  occ %>%
    transmute(
      Longitude = as.numeric(decimalLongitude),
      Latitude  = as.numeric(decimalLatitude),
      Species   = sp,
      Fonte     = "GBIF"
    )
}

# ================================================
# 2. FUNÇÃO PARA SEPARAR L. COLOCOLA EM BRACCATUS E MUNOAI
# ================================================

separate_colocola <- function(colocola_data, braccatus_shp_path, munoai_shp_path) {
  if (nrow(colocola_data) == 0) {
    return(list(braccatus = tibble(), munoai = tibble()))
  }
  
  # Carregar shapefiles
  braccatus_range <- st_read(braccatus_shp_path, quiet = TRUE)
  munoai_range <- st_read(munoai_shp_path, quiet = TRUE)
  
  # Converter pontos em objeto sf
  points_sf <- st_as_sf(colocola_data, 
                        coords = c("Longitude", "Latitude"), 
                        crs = 4326)
  
  # Verificar interseções
  in_braccatus <- st_intersects(points_sf, braccatus_range, sparse = FALSE)[,1]
  in_munoai <- st_intersects(points_sf, munoai_range, sparse = FALSE)[,1]
  
  # Separar os dados
  braccatus_data <- colocola_data[in_braccatus, ] %>%
    mutate(Species = "Leopardus braccatus")
  
  munoai_data <- colocola_data[in_munoai, ] %>%
    mutate(Species = "Leopardus munoai")
  
  # Pontos que não caíram em nenhum range ficam como colocola
  neither <- !in_braccatus & !in_munoai
  colocola_remaining <- colocola_data[neither, ] %>%
    mutate(Species = "Leopardus colocola")
  
  message("Separação L. colocola:")
  message("  - L. braccatus: ", nrow(braccatus_data), " pontos")
  message("  - L. munoai: ", nrow(munoai_data), " pontos")
  message("  - L. colocola (restante): ", nrow(colocola_remaining), " pontos")
  
  return(list(
    braccatus = braccatus_data,
    munoai = munoai_data,
    colocola = colocola_remaining
  ))
}

# =====================================
# 3. BAIXAR DADOS DO GBIF
# =====================================

message("=== INICIANDO DOWNLOAD DOS DADOS DO GBIF ===")

# Espécies para buscar diretamente
especies_diretas <- c(
  "Leopardus guttulus",
  "Leopardus geoffroyi", 
  "Leopardus wiedii",
  "Leopardus tigrinus",
  "Herpailurus_yagouaroundi"
)

# Baixar dados das espécies diretas
gbif_data <- map_dfr(especies_diretas, fetch_gbif)

# Baixar L. colocola para separar depois
colocola_data <- fetch_gbif("Leopardus colocola")

message("=== DADOS GBIF BAIXADOS ===")
message("Total de registros diretos: ", nrow(gbif_data))
message("Registros L. colocola para separar: ", nrow(colocola_data))

# =====================================
# 4. SEPARAR L. COLOCOLA (AJUSTAR CAMINHOS DOS SHAPEFILES)
# =====================================

# AJUSTAR ESTES CAMINHOS PARA SEUS SHAPEFILES:
braccatus_shp_path <- "caminho/para/braccatus_range.shp"
munoai_shp_path <- "caminho/para/munoai_range.shp"

# Verificar se os shapefiles existem
if (file.exists(braccatus_shp_path) && file.exists(munoai_shp_path)) {
  message("=== SEPARANDO L. COLOCOLA ===")
  colocola_separated <- separate_colocola(colocola_data, braccatus_shp_path, munoai_shp_path)
  
  # Adicionar os dados separados ao conjunto principal
  gbif_data <- bind_rows(
    gbif_data,
    colocola_separated$braccatus,
    colocola_separated$munoai,
    colocola_separated$colocola
  )
} else {
  warning("Shapefiles não encontrados. Mantendo L. colocola sem separação.")
  gbif_data <- bind_rows(gbif_data, colocola_data)
}

# =====================================
# 5. IMPORTAR PLANILHAS LOCAIS
# =====================================

message("=== IMPORTANDO PLANILHAS LOCAIS ===")

# Definir nomes das espécies e arquivos para as 7 primeiras planilhas
especies_salve <- c(
  "Leopardus guttulus",
  "Leopardus geoffroyi", 
  "Leopardus wiedii",
  "Leopardus braccatus",
  "Leopardus munoai",
  "Leopardus tigrinus",
  "Herpailurus yagouaroundi"
)

# Nomes dos arquivos das 7 primeiras planilhas (SEUS ARQUIVOS ATUALIZADOS)
arquivos_salve <- c(
  "Guttulus/guttulus_brazil.csv",
  "Geoffroyi/geoffroyi.csv",
  "Maracajá/maracaja.csv", 
  "Palheiro/braccatus_brazil.csv",
  "Palheiro Pampeano/munoai_brazil.csv",
  "Trigrinus/tigrinus_brazil.csv",
  "Jaguarundi/jaguarundi_brazil.csv"
)

# Importar as 7 planilhas SALVE
salve_data <- tibble()

for (i in 1:length(arquivos_salve)) {
  arquivo <- arquivos_salve[i]
  especie <- especies_salve[i]
  
  if (file.exists(arquivo)) {
    message("Importando: ", arquivo)
    
    # Detectar tipo de arquivo e importar
    if (grepl("\\.xlsx?$", arquivo)) {
      dados_temp <- read_excel(arquivo)
    } else if (grepl("\\.csv$", arquivo)) {
      dados_temp <- read_csv(arquivo, show_col_types = FALSE)
    } else {
      warning("Formato de arquivo não reconhecido: ", arquivo)
      next
    }
    
    # Padronizar colunas e adicionar espécie
    dados_temp <- dados_temp %>%
      select(ID, Longitude, Latitude) %>%
      mutate(
        Longitude = as.numeric(Longitude),
        Latitude = as.numeric(Latitude),
        Species = especie,
        Fonte = "SALVE"
      ) %>%
      select(Longitude, Latitude, Species, Fonte)  # Remover ID para consistência
    
    salve_data <- bind_rows(salve_data, dados_temp)
    
  } else {
    warning("Arquivo não encontrado: ", arquivo)
  }
}

# Importar a 8ª planilha (FP) - AJUSTAR NOME DO ARQUIVO
arquivo_fp <- "planilha_fp.xlsx"  # Ajustar nome

fp_data <- tibble()

if (file.exists(arquivo_fp)) {
  message("Importando planilha FP: ", arquivo_fp)
  
  if (grepl("\\.xlsx?$", arquivo_fp)) {
    fp_data <- read_excel(arquivo_fp)
  } else if (grepl("\\.csv$", arquivo_fp)) {
    fp_data <- read_csv(arquivo_fp, show_col_types = FALSE)
  }
  
  # Padronizar colunas
  fp_data <- fp_data %>%
    select(Longitude, Latitude, Especie) %>%
    mutate(
      Longitude = as.numeric(Longitude),
      Latitude = as.numeric(Latitude),
      Species = Especie,
      Fonte = "FP"
    ) %>%
    select(Longitude, Latitude, Species, Fonte)
} else {
  warning("Arquivo FP não encontrado: ", arquivo_fp)
}

message("=== DADOS LOCAIS IMPORTADOS ===")
message("Registros SALVE: ", nrow(salve_data))
message("Registros FP: ", nrow(fp_data))

# =====================================
# 6. CONSOLIDAR E LIMPAR DADOS
# =====================================

message("=== CONSOLIDANDO DADOS ===")

# Combinar todos os dados
todos_dados <- bind_rows(gbif_data, salve_data, fp_data)

# Remover registros com coordenadas faltantes
todos_dados <- todos_dados %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

message("Total de registros antes da limpeza: ", nrow(todos_dados))

# =====================================
# 7. REMOVER DUPLICATAS E CRIAR PLANILHAS POR ESPÉCIE
# =====================================

message("=== REMOVENDO DUPLICATAS E CRIANDO PLANILHAS POR ESPÉCIE ===")

# Função para remover duplicatas espaciais (pontos muito próximos)
remove_duplicates <- function(data, tolerance = 0.001) {
  if (nrow(data) <= 1) return(data)
  
  # Criar uma chave de localização aproximada
  data <- data %>%
    mutate(
      lon_round = round(Longitude / tolerance) * tolerance,
      lat_round = round(Latitude / tolerance) * tolerance,
      location_key = paste(lon_round, lat_round, sep = "_")
    )
  
  # Manter apenas um registro por localização, priorizando por fonte
  data_clean <- data %>%
    arrange(match(Fonte, c("SALVE", "FP", "GBIF"))) %>%  # Prioridade: SALVE > FP > GBIF
    distinct(location_key, .keep_all = TRUE) %>%
    select(-lon_round, -lat_round, -location_key)
  
  return(data_clean)
}

# Obter lista de espécies únicas
especies_unicas <- unique(todos_dados$Species)
especies_unicas <- especies_unicas[!is.na(especies_unicas)]

message("Espécies encontradas: ", paste(especies_unicas, collapse = ", "))

# Criar planilha para cada espécie
planilhas_especies <- list()

for (sp in especies_unicas) {
  message("Processando: ", sp)
  
  dados_sp <- todos_dados %>%
    filter(Species == sp) %>%
    remove_duplicates()
  
  planilhas_especies[[sp]] <- dados_sp
  
  # Salvar planilha individual
  nome_arquivo <- paste0(gsub("[^A-Za-z0-9]", "_", sp), "_consolidated.csv")
  write_csv(dados_sp, nome_arquivo)
  
  message("  - ", nrow(dados_sp), " registros finais")
  message("  - Salvo como: ", nome_arquivo)
}

# =====================================
# 8. SALVAR PLANILHA GERAL CONSOLIDADA
# =====================================

# Combinar todas as espécies limpas
dados_finais <- bind_rows(planilhas_especies)

write_csv(dados_finais, "todas_especies_consolidated.csv")

message("=== PROCESSAMENTO CONCLUÍDO ===")
message("Planilha geral salva como: todas_especies_consolidated.csv")
message("Planilhas individuais salvas com sufixo '_consolidated.csv'")

# =====================================
# 9. RESUMO DOS DADOS
# =====================================

resumo <- dados_finais %>%
  group_by(Species, Fonte) %>%
  summarise(n_registros = n(), .groups = "drop") %>%
  arrange(Species, Fonte)

print("RESUMO DOS DADOS FINAIS:")
print(resumo)

# Salvar resumo
write_csv(resumo, "resumo_dados_consolidated.csv")