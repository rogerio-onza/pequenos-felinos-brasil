# Base de Dados de Felinos Pequenos do Brasil 🐱

## 📋Descrição

Este repositório contém scripts em R para processamento, limpeza e filtragem de dados de ocorrência de felinos pequenos nativos do Brasil. O projeto integra dados de múltiplas fontes (GBIF, SALVE, FP) e realiza validação geográfica usando shapefiles de distribuição das espécies.

## 🎯 Objetivos
 - Consolidar dados de ocorrência de felinos pequenos de diferentes bases de dados
 - Realizar limpeza e padronização dos registros
 - Separar taxonomicamente *Leopardus colocola* em *L. braccatus* e *L. munoai* baseado em distribuição geográfica
 - Filtrar registros fora dos ranges de distribuição conhecidos
 - Gerar datasets limpos e padronizados para análises posteriores

## 🐾 Espécies
*Leopardus guttulus* (Gato-do-mato-pequeno)

*Leopardus geoffroyi* (Gato-do-mato-grande)

*Leopardus wiedii* (Maracajá)

*Leopardus tigrinus* 

*Leopardus braccatus* (Gato-palheiro)

*Leopardus munoai* (Gato-palheiro-pampeano)

*Herpailurus yagouaroundi* (Jaguarundi)

# 📁 Estrutura do Repositório
├── scripts/

│   ├── 01_data_import_cleaning.R     # Script principal de importação e limpeza

│   └── 02_range_filtering.R          # Script de filtragem por range geográfico

├── data/

│   ├── raw/                          # Dados brutos (planilhas originais)

│   ├── processed/                    # Dados processados

│   └── shapefiles/                   # Shapefiles dos ranges das espécies

├── output/

│   ├── consolidated/                 # Datasets consolidados

│   └── filtered/                     # Datasets filtrados por range

├── docs/

│   └── data_dictionary.md            # Dicionário dos dados

├── README.md

└── .gitignore

# 🔧 Dependências
```
install.packages(c(
  "rgbif",      # Acesso aos dados do GBIF
  "dplyr",      # Manipulação de dados
  "readxl",     # Leitura de arquivos Excel
  "readr",      # Leitura/escrita de CSV
  "sf",         # Análise espacial
  "sp",         # Classes espaciais
  "tibble",     # Data frames modernos
  "tools"       # Utilitários gerais))
```

# 🚀 Como Usar

**1. Script de Importação e Limpeza (01_data_import_cleaning.R)**

Este script:

- Busca dados no GBIF para as espécies alvo
- Importa planilhas locais (SALVE e FP)
- Separa *L. colocola* em *L. braccatus* e *L. munoai* usando shapefiles
- Remove duplicatas espaciais
- Gera datasets consolidados por espécie

```
# Ajustar caminhos dos shapefiles para separação de L. colocola
braccatus_shp_path <- "data/shapefiles/braccatus_range.shp"
munoai_shp_path <- "data/shapefiles/munoai_range.shp"

# Ajustar caminhos das planilhas locais
arquivos_salve <- c(
  "data/raw/Guttulus/guttulus_brazil.csv",
  "data/raw/Geoffroyi/geoffroyi.csv",
  # ... outros arquivos
)
```

**2. Script de Filtragem por Range (02_range_filtering.R)**

Este script:

- Valida coordenadas geográficas
- Filtra pontos fora dos ranges de distribuição
- Salva registros válidos e inválidos separadamente
- Pode sobrescrever o arquivo original com dados limpos

```
# Definir arquivos CSV a serem processados
csv_paths <- c("output/consolidated/Leopardus_tigrinus_consolidated.csv")

# Caminho do shapefile de range da espécie
shp_path <- "data/shapefiles/tigrinus_brazil_iucn_range.shp"

# Configurações de processamento
overwrite_original <- TRUE
boundary_policy <- "intersects"
```

# 📊 Dados de Entrada

#### Fontes de Dados

**1. GBIF (Global Biodiversity Information Facility)**

 - Registros de ocorrência com coordenadas
 - Filtrados para território brasileiro
 - Limite de 10.000 registros por espécie


**2. SALVE (Sistema de Avaliação do Risco de Extinção)**

 - Dados locais em formato CSV/Excel
 - Colunas: ID, Longitude, Latitude


**3. FP (Felinos do Pampa)**

 - Dados complementares
 - Colunas: Longitude, Latitude, Especie

#### Formato dos Dados de Saída

Todas as planilhas de saída seguem o padrão:

 - **Longitude**: Coordenada X em graus decimais

 - **Latitude**: Coordenada Y em graus decimais

 - **Species**: Nome científico da espécie

 - **Fonte**: Origem do dado (GBIF, SALVE, FP)

# 📈 Resultados

O processamento gera:

 - **Arquivos por espécie**: <especie>_consolidated.csv
 - **Arquivo geral**: todas_especies_consolidated.csv
 - **Resumo estatístico**: resumo_dados_consolidated.csv
 - **Registros fora do range**: <especie>_out_of_range.csv

# ⚙️ Configurações Avançadas
Remoção de Duplicatas

Tolerância espacial padrão: 0.001° (~111m)
Prioridade de fontes: SALVE > FP > GBIF

# Validação Geográfica

Coordenadas válidas: -180° ≤ Longitude ≤ 180°, -90° ≤ Latitude ≤ 90°
Política de fronteira: intersects (padrão) ou within
Suporte a vírgula decimal (configurável)

# 🤝 Contribuições
Este é um projeto de pesquisa acadêmica. Para sugestões ou correções:

- Faça um fork do repositório
- Crie uma branch para sua modificação
- Commit suas mudanças
- Abra um Pull Request

# 📝 Licença
Este projeto está sob a licença MIT. Veja o arquivo LICENSE para mais detalhes.

# 👤 Autor
Rogério Nunes Oliveira

Projeto desenvolvido como parte de bolsa de pesquisa
Data de criação: Agosto/2025

📚 Citação

Se você usar este código em sua pesquisa, por favor cite:

```
@software{oliveira2025felinos,
  author = {Oliveira, Rogério Nunes},
  title = {Processamento de Dados de Felinos Pequenos do Brasil},
  year = {2025},
  url = {https://github.com/seu-usuario/felinos-pequenos-brasil}
}
```

# 🔍 Palavras-chave
felinos neotropicais, biogeografia, dados de ocorrência, GBIF, conservação, Leopardus, distribuição geográfica, Brasil
