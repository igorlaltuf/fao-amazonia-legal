# Infraestrutura energ�tica

### 1 - Gera��o hidrel�trica e PCHs
rm(list = ls()) # limpar as vari�veis carregadas
source("Rscripts/00_bibliotecas.R")
source("Rscripts/00_variaveis_globais.R")
source("Rscripts/00_funcoes_globais.R")
setwd("F:/Meu reposit�rio/fao-amazonia-legal/")

# Dados de gera��o de energia na Amaz�nia Legal
infra.energetica <- read_excel(path = "Input/BD SIGA_01072021.xlsx", skip = 1)

colnames(infra.energetica) <- c(
  "empreendimento", "ceg", "uf", "fonte", "fase", "origem", "tipo", "tipo_atuacao", "combustivel_final",
  "entrada_operacao", "potencia_outorgada_kw", "potencia_fisica_kw", "garantia_fisica_kw",
  "geracao_qualificada", "lat_gms", "long_gms", "inicio_vigencia", "fim_vigencia",
  "proprietario_reg_exploracao", "cod_e_desc_sub_bacia", "muni"
)

infra.energetica.bacias <- infra.energetica %>%
  dplyr::filter(uf %in% uf.amz.legal &
    fonte %in% c("PCH", "UHE") & # filtra apenas PCHs e UHEs
    fase == "Opera��o") # apenas exclui as que est�o em obras ou ainda s�o um projeto
  
  #filter(!is.na(cod_e_desc_sub_bacia)) # retorna as linhas que n�o t�m NA O ERRO EST� AQUI!!!!!!!!!!!!!



# dividir colunas com dois munic�pios em tr�s colunas
infra.energetica.bacias$muni1 <- str_split_fixed(infra.energetica.bacias$muni, ",", 3)[, 1]
infra.energetica.bacias$muni2 <- str_split_fixed(infra.energetica.bacias$muni, ",", 3)[, 2]
infra.energetica.bacias$muni3 <- str_split_fixed(infra.energetica.bacias$muni, ",", 3)[, 3]

# pontuar munic�pio e colocar numera��o do IBGE
# reunir os munic�pios das colunas 1, 2 e 3
muni3 <- as.data.frame(infra.energetica.bacias$muni3, stringsAsFactors = FALSE)
muni2 <- as.data.frame(infra.energetica.bacias$muni2, stringsAsFactors = FALSE)
infra.energetica.bacias <- as.data.frame(infra.energetica.bacias$muni1, stringsAsFactors = FALSE)
colnames(muni2) <- "infra.energetica.bacias$muni1"
colnames(muni3) <- "infra.energetica.bacias$muni1"
infra.energetica.bacias <- rbind(infra.energetica.bacias, muni2)
infra.energetica.bacias <- rbind(infra.energetica.bacias, muni3)
colnames(infra.energetica.bacias) <- "muni"
infra.energetica.bacias$muni <- str_trim(infra.energetica.bacias$muni, side = "left") # remove o espa�o antes dos nomes das vari�veis
infra.energetica.bacias <- unique(infra.energetica.bacias)
infra.energetica.bacias[1] <- substr(infra.energetica.bacias$muni, 1, nchar(infra.energetica.bacias$muni) - 6)

# tirar os �ltimos caracteres das cidades tbm em cidades.amazonia legal antes de fazer o left join
padronizar <- cidades.amazonia.legal.nome
padronizar$muni <- substr(padronizar$muni, 1, nchar(padronizar$muni) - 6)
infra.bacias.amzl <- left_join(infra.energetica.bacias, padronizar)

infra.bacias.amzl <- left_join(infra.bacias.amzl, cidades.amazonia.legal.nome, by = "cod_muni") %>%
  select(2, 3) %>%
  drop_na() %>%
  mutate(existe_UHE_PCH = 1)

colnames(infra.bacias.amzl) <- c("cod_muni", "muni", "existe_UHE_PCH")


### 2 - Gera��o Termel�tricas
infra.energetica.termicas <- infra.energetica %>%
  dplyr::filter(uf %in% uf.amz.legal &
    fonte %in% c("UTE") &
    fase == "Opera��o")


# dividir colunas com dois munic�pios em duas colunas, agrupar em uma e descobrir o c�digo do IBGE
infra.energetica.termicas$muni1 <- str_split_fixed(infra.energetica.termicas$muni, ",", 3)[, 1]
infra.energetica.termicas$muni2 <- str_split_fixed(infra.energetica.termicas$muni, ",", 3)[, 2]
muni2 <- as.data.frame(infra.energetica.termicas$muni2, stringsAsFactors = FALSE)
infra.energetica.termicas <- as.data.frame(infra.energetica.termicas$muni1, stringsAsFactors = FALSE)
colnames(muni2) <- "infra.energetica.termicas$muni1"
infra.energetica.termicas <- rbind(infra.energetica.termicas, muni2)
colnames(infra.energetica.termicas) <- "muni"
infra.energetica.termicas$muni <- str_trim(infra.energetica.termicas$muni, side = "left") # remove o espa�o antes dos nomes das vari�veis

infra.energetica.termicas <- unique(infra.energetica.termicas)
nome.cidades <- data.frame("muni" = c("Alta Floresta D'Oeste (RO)", "Alvorada D'Oeste (RO)", "Espig�o D'Oeste (RO)", "Machadinho D'Oeste (RO)", "Pindar�-Mirim (MA)"))
infra.energetica.termicas <- rbind(infra.energetica.termicas, nome.cidades)

infra.energetica.termicas[1] <- substr(infra.energetica.termicas$muni, 1, nchar(infra.energetica.termicas$muni) - 5)

padronizar <- cidades.amazonia.legal.nome
padronizar$muni <- substr(padronizar$muni, 1, nchar(padronizar$muni) - 5)
infra.energetica.termicas <- left_join(infra.energetica.termicas, padronizar)

infra.energetica.termicas <- left_join(infra.energetica.termicas, cidades.amazonia.legal.nome, by = "muni")

infra.energetica.termicas <- left_join(infra.energetica.termicas, cidades.amazonia.legal.nome, by = c("cod_muni.x" = "cod_muni")) %>%
  select(2, 4) %>%
  drop_na() %>%
  mutate(existe_UTE = 1)

colnames(infra.energetica.termicas) <- c("cod_muni", "muni", "existe_UTE")

# remove o Rio Branco que classificou errado
infra.energetica.termicas <- infra.energetica.termicas[!(infra.energetica.termicas$muni == "Rio Branco (MT)"), ]

# Exportar dados de gera��o de energia
geracao.amzl <- left_join(cidades.amazonia.legal.nome, infra.bacias.amzl, by = c("cod_muni", "muni"))
geracao.amzl <- left_join(geracao.amzl, infra.energetica.termicas, by = c("cod_muni", "muni"))
geracao.amzl[is.na(geracao.amzl)] <- 0

write.csv(geracao.amzl, file = "Outputs/01_tabelas/01_geracao_energia_amzl.csv", row.names = F)
