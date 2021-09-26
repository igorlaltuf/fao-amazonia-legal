# Dados de empregos formais da RAIS na Amazônia Legal
# Foram usados os dados de vínculos ativos para todos os setores, com exceção da agropecuária, considerando que
# o período de safras não bate com a data de verificação dos vínculos ativos (31/12).

rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# 1 - ENERGIA ElÉTRICA - vínculos ativos em 31-12-2019 por município e relacionados a geração de energia elétrica
cnae.cidades <- read_csv('Outputs/00_shapes_e_dados/00_rais_ativos_2019.csv',
                         col_types = list(
                           id_municipio = 'n',
                           ano = 'n',
                           cnae_2 = 'c',
                           vinculos_ativos = 'n'))

which(is.na(cnae.cidades$vinculos_ativos)) # retorna a linha dos NAs caso existam
cnae.cidades$id_municipio <- as.numeric(cnae.cidades$id_municipio)

# 35115 Geração de energia elétrica (acima de 200 vinculos ativos)
cnae.energia <- cnae.cidades %>% 
                select(id_municipio,cnae_2,vinculos_ativos) %>% 
                filter(str_detect(cnae_2, "^35115")) %>%  # filtrar empregos relacionados à Geração de energia elétrica
                group_by(id_municipio) %>% 
                summarise(empregos_energia = sum(vinculos_ativos, na.rm = TRUE))

cnae.energia$empregos_energia <- as.numeric(cnae.energia$empregos_energia)

cnae.energia <- left_join(cidades.amazonia.legal.nome,cnae.energia, by = c('cod_muni'='id_municipio')) %>% 
                dplyr::filter(empregos_energia > 0) %>% 
                arrange(desc(empregos_energia))


# Classifica empregos de energia elétrica
cnae.energia <- classificar.variavel(cnae.energia,'empregos_energia','class_empregos_energia')

cnae.energia.inter <- cnae.energia %>%
                dplyr::filter(cod_muni %in% cidades.intermediadoras)

# x <- cnae.energia %>% 
#   group_by(class_empregos_energia) %>%
#   mutate(N_category = n()) %>%
#   count(N_category)


# 2 - MINERAÇÃO - Vínculos ativos em 31-12-2019 por município e relacionados à mineração
# Códigos CNAE iniciados por B07: extração de minerais metálicos
cnae.mineracao <- cnae.cidades %>% 
  select(id_municipio, cnae_2, vinculos_ativos) %>%   
  filter(str_detect(cnae_2, "^07")) %>%   # filtrar coluna caracteres iniciados em B07
  group_by(id_municipio) %>% 
  summarise(empregos_mineracao = sum(vinculos_ativos, na.rm = TRUE))

cnae.mineracao$empregos_mineracao <- as.numeric(cnae.mineracao$empregos_mineracao)

cnae.mineracao <- left_join(cidades.amazonia.legal.nome,cnae.mineracao, by = c('cod_muni'='id_municipio')) %>% 
  dplyr::filter(empregos_mineracao > 0) %>% 
  arrange(desc(empregos_mineracao))

# Classifica empregos de mineração
cnae.mineracao <- classificar.variavel(cnae.mineracao,'empregos_mineracao','class_empregos_mineracao')

cnae.mineracao.inter <- cnae.mineracao %>%
  dplyr::filter(cod_muni %in% cidades.intermediadoras)

x <- cnae.mineracao %>% 
  group_by(class_empregos_mineracao) %>%
  mutate(N_category = n()) %>%
  count(N_category)


# 3 - AGROPECUÁRIA - Número total de vínculos (não são vínculos ativos)
cnae.cidades <- read_csv('Outputs/00_shapes_e_dados/00_rais_vinculos_2019.csv',
                         col_types = list(
                           id_municipio = 'n',
                           ano = 'n',
                           cnae_2 = 'c',
                           vinculos = 'n'))

which(is.na(cnae.cidades$vinculos)) # retorna a linha dos NAs caso existam
cnae.cidades$id_municipio <- as.numeric(cnae.cidades$id_municipio)

# Grupos da CNAE 2.0:
# 01.1 Produção de lavouras temporárias
# 01.5 Pecuária
# 01.6 Atividades de apoio à agricultura e à pecuária; atividades de pós-colheita

cnae.agro <- cnae.cidades %>% 
             select(id_municipio,cnae_2,vinculos) %>% 
             filter(str_detect(cnae_2, "^011|^015|^016")) %>%  
             group_by(id_municipio) %>% 
             summarise(empregos_agro = sum(vinculos, na.rm = TRUE))

cnae.agro$empregos_agro <- as.numeric(cnae.agro$empregos_agro)

cnae.agro <- left_join(cidades.amazonia.legal.nome,cnae.agro, by = c('cod_muni'='id_municipio')) %>%  
             dplyr::filter(empregos_agro>0 &
             cod_muni %in% cidades.amazonia.legal) %>% 
             arrange(desc(empregos_agro))

# Classifica empregos do agro 
cnae.agro <- classificar.variavel(cnae.agro,'empregos_agro','class_empregos_agro')

cnae.agro.inter <- cnae.agro %>%
                   dplyr::filter(cod_muni %in% cidades.intermediadoras)

x <- cnae.agro %>% 
  group_by(class_empregos_agro) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# 5 - Reunir os dataframes
empregos.rais <- full_join(cnae.energia,cnae.mineracao)
empregos.rais <- full_join(empregos.rais,cnae.agro)
empregos.rais <- full_join(empregos.rais,cidades.amazonia.legal.nome)


# Salvar csv com dados da RAIS para os setores analisados e suas respectivas classificações
write.csv(empregos.rais,file='Outputs/01_tabelas/01_empregos_rais.csv', row.names = F)

# filtrar por cidades intermediadoras
rais.cidades.intermed <- empregos.rais %>% 
                         dplyr::filter(cod_muni %in% cidades.intermediadoras) 
