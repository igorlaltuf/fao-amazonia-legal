# Dados sobre Financiamentos do BNDES
rm(list = ls()) # limpar as variáveis carregadas
source("Rscripts/00_bibliotecas.R")
source("Rscripts/00_variaveis_globais.R")
source("Rscripts/00_funcoes_globais.R")
setwd("F:/Meu repositório/fao-amazonia-legal/")

# 1 - FINANCIAMENTOS PARA GERAÇÃO DE ENERGIA ELÉTRICA
# Grandes projetos de geração de energia financiados pelo BNDES na Amazônia Legal
fin.bndes.original <- read_excel("Input/bndes_financiamentos.xlsx")
which(is.na(fin.bndes.original$valor_contratado_corrente)) # retorna a linha dos NAs se existirem

fin.bndes.deflac <- fin.bndes.original %>%
  mutate(fin.bndes.original, desembolso_deflac = deflate(valor_desembolsado_corrente, as.Date(ymd(fin.bndes.original$data_contrato)), "04/2021", "igpdi")) %>%
  dplyr::filter(desembolso_deflac >= 40000000 &
    porte_cliente == "GRANDE" &
    natureza_cliente == "PRIVADA") # filtra os grandes investimentos (acima de R$ 40 milhões de 2021)

bndes.energia <- fin.bndes.deflac %>%
  filter(str_detect(nome_sub_setor_cnae, "^GERACAO DE ENERGIA ELETRICA - HIDRELETRICA|^GERACAO DE ENERGIA ELETRICA - PCH|^GERACAO DE ENERGIA ELETRICA - TERMICA"))

# Filtrar de acordo com o nome do município
bndes.energia.cidades <- bndes.energia %>%
  select(5, 6, 23) %>%
  unique() %>%
  dplyr::filter(cod_muni %in% cidades.amazonia.legal)

# Criar um DF com nomes em caixa alta e sem Ç
amz.legal.uppercase <- mutate_all(cidades.amazonia.legal.nome, .funs = toupper)
amz.legal.uppercase$muni <- str_sub(amz.legal.uppercase$muni, 1, nchar(amz.legal.uppercase$muni) - 5)
nomes.amz.legal <- as.vector(amz.legal.uppercase$muni)

# padronizar a descrição para ficar igual o campo dos municípios
bndes.energia$descricao <- stri_trans_general(bndes.energia$descricao, "Latin-ASCII") # remove Ç

# Filtrar de acordo com a descrição do projeto (quando o projeto for classificado como 'SEM MUNICÍPIO')
energia.descricao <- bndes.energia %>%
  dplyr::filter(cod_muni %in% c("9999999", "0000000") & # codigo 'SEM MUNICIPIO' ou 'DIVERSOS'
    uf %in% uf.amz.legal) %>% # Filtra UFs da Amazônia Legal, para o caso de existirem duas cidades com o mesmo nome
  filter(grepl(paste(nomes.amz.legal, collapse = "|"), descricao)) # filtra cidades da AMZ LEGAL

# criar um vetor com todos os nomes em um elemento e separar os elementos por |
w <- paste(nomes.amz.legal, collapse = "|")

# colocar o nome do município na coluna sobre o município
energia.descricao$muni <- str_extract(energia.descricao$descricao, w)

# Juntar essa tabela com a tabela das cidades.
energia.descricao <- energia.descricao %>%
  select(5, 6, 23) %>%
  unique()

bndes.energia.cidades <- rbind(bndes.energia.cidades, energia.descricao) %>%
  mutate(energia_bndes = 1) %>%
  select(1, 2, 4) %>%
  unique()

# Valor agrupado por município
valor.bndes.energia <- bndes.energia %>%
  select(5, 6, 28) %>%
  group_by(muni, cod_muni) %>%
  summarise(desembolsos_energia_deflac = sum(desembolso_deflac)) %>%
  arrange(desc(desembolsos_energia_deflac))


# 2 - FINANCIAMENTOS PARA MINERAÇÃO
bndes.mineracao <- fin.bndes.deflac %>%
  filter(str_detect(cod_sub_setor_cnae, "^B07")) # Filtrar categoria CNAE 2.0 pela divisão B07: Extração de minerais metálicos

# Por município
bndes.mineracao.cidades <- bndes.mineracao %>%
  select(5, 6, 23) %>%
  unique() %>%
  dplyr::filter(cod_muni %in% cidades.amazonia.legal)

# Verificar os projetos "SEM MUNICÍPIO"
mineracao.descricao <- bndes.mineracao %>%
  dplyr::filter(cod_muni %in% c("9999999", "0000000") & # codigo 'SEM MUNICIPIO' ou 'DIVERSOS'
    uf %in% uf.amz.legal) %>% # Filtra UFs da Amazônia Legal, para o caso de existirem duas cidades com o mesmo nome
  filter(grepl(paste(nomes.amz.legal, collapse = "|"), descricao)) # filtra cidades da AMZ LEGAL

# colocar o nome do município na coluna sobre o município
mineracao.descricao$muni <- str_extract(mineracao.descricao$descricao, w)

# Juntar essa tabela com a tabela das cidades.
mineracao.descricao <- mineracao.descricao %>%
  select(5, 6, 23) %>%
  unique()

bndes.mineracao.cidades <- rbind(bndes.mineracao.cidades, mineracao.descricao) %>%
  mutate(mineracao_bndes = 1)

# 3 - FINANCIAMENTOS PARA PETRÓLEO (não foram financiados grandes projetos na Amazonia Legal)
bndes.petroleo <- fin.bndes.deflac %>%
  dplyr::filter(nome_sub_setor_cnae %in% "EXTRACAO DE PETROLEO E GAS NATURAL")

# Nenhum dos grandes projetos de petróleo foi na Amazônia Legal


# Reunir dados
grandes.projetos.bndes <- full_join(bndes.energia.cidades, bndes.mineracao.cidades)
grandes.projetos.bndes[grandes.projetos.bndes$muni == "PEDRA BRANCA DO AMAPARI" & grandes.projetos.bndes$cod_muni == "0000000", "cod_muni"] <- "2310506"
grandes.projetos.bndes[grandes.projetos.bndes$muni == "ALTAMIRA" & grandes.projetos.bndes$cod_muni == "0000000", "cod_muni"] <- "1500602"
grandes.projetos.bndes[grandes.projetos.bndes$muni == "SINOP" & grandes.projetos.bndes$cod_muni == "0000000", "cod_muni"] <- "5107909"
grandes.projetos.bndes[grandes.projetos.bndes$muni == "TERRA SANTA" & grandes.projetos.bndes$cod_muni == "0000000", "cod_muni"] <- "1507979"
grandes.projetos.bndes[grandes.projetos.bndes$muni == "JURUENA" & grandes.projetos.bndes$cod_muni == "0000000", "cod_muni"] <- "5105176"
grandes.projetos.bndes[grandes.projetos.bndes$muni == "SAPEZAL" & grandes.projetos.bndes$cod_muni == "0000000", "cod_muni"] <- "5107875"
grandes.projetos.bndes[grandes.projetos.bndes$muni == "PARECIS" & grandes.projetos.bndes$cod_muni == "0000000", "cod_muni"] <- "1101450"

grandes.projetos.bndes <- grandes.projetos.bndes %>%
  select(1, 2, 3, 5) %>%
  unique()

grandes.projetos.bndes[is.na(grandes.projetos.bndes)] <- 0

# exportar tabela
write.csv(grandes.projetos.bndes, file = "Outputs/01_tabelas/01_financ_bndes.csv", row.names = F)


# Análises extras:
# Frigoríficos
bndes.frigo <- fin.bndes.deflac %>%
  filter(str_detect(cod_sub_setor_cnae, "^C101"))
select(4, 5, 6, 23) %>%
  unique()

# Encontrados: MT (Várzea Grande (class SEM MUNICÍPIO) e Lucas do Rio Verde)

# Papel e Celulose
bndes.celulose <- fin.bndes.deflac %>%
  filter(str_detect(cod_sub_setor_cnae, "^C17")) %>%
  select(3, 4, 5, 6, 23) %>%
  unique() %>%
  dplyr::filter(uf %in% uf.amz.legal)

# Encontrados: Almeirim (PA) e Imperatriz (MA)
# nenhum classificado como sem município
