# Dados de empregos formais da RAIS na Amazônia Legal
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar credenciais do data lake base dos dados
load_dot_env()
credencial <- Sys.getenv('CREDENCIAL_BASE_DOS_DADOS')
basedosdados::set_billing_id(credencial)   

# 1 - ENERGIA ElÉTRICA ##############################

# Total de vínculos ativos da RAIS em 31-12-2019 por município de acordo com a CNAE 2.0
query <- "SELECT id_municipio, ano, cnae_2, SUM(numero_vinculos) as vinculos_ativos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE ano = 2019 AND vinculo_ativo_3112 = 1
          GROUP BY id_municipio, ano, cnae_2"

cnae.cidades <- read_sql(query)
which(is.na(cnae.cidades$vinculos_ativos)) # retorna a linha dos NAs caso existam
cnae.cidades$id_municipio <- as.numeric(cnae.cidades$id_municipio)

# Empregos no setor de energia elétrica
# Total de vínculos ativos em 31-12-2019 por município e relacionados a geração, transmissão e distribuição de energia
# códigos CNAE:
# 351 Geração, transmissão e distribuição de energia elétrica
# Classes:	 	
# 35115 Geração de energia elétrica
# 35123 Transmissão de energia elétrica
# 35131 Comércio atacadista de energia elétrica
# 35140 Distribuição de energia elétrica

cnae.energia <- cnae.cidades %>% 
                select(id_municipio,cnae_2,vinculos_ativos) %>% 
                filter(str_detect(cnae_2, "^351")) %>%  # filtrar coluna caracteres iniciados em 351
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


x <- cnae.energia %>% 
  group_by(class_empregos_energia) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# 2 - MINERAÇÃO #################################
# Vínculos ativos em 31-12-2019 por município e relacionados à mineração
# códigos CNAE
# 07 EXTRAÇÃO DE MINERAIS METÁLICOS
# 08 EXTRAÇÃO DE MINERAIS NÃO-METÁLICOS
# 09 ATIVIDADES DE APOIO À EXTRAÇÃO DE MINERAIS

cnae.mineracao <- cnae.cidades %>% 
                  select(id_municipio,cnae_2,vinculos_ativos) %>% 
                  filter(str_detect(cnae_2, "^07|^08|^09")) %>% 
                  group_by(id_municipio) %>% 
                  summarise(empregos_mineracao = sum(vinculos_ativos, na.rm = TRUE))

cnae.mineracao$empregos_mineracao <- as.numeric(cnae.mineracao$empregos_mineracao)

cnae.mineracao <- left_join(cidades.amazonia.legal.nome,cnae.mineracao, by = c('cod_muni'='id_municipio')) %>% 
                  dplyr::filter(empregos_mineracao > 0 &
                  cod_muni %in% cidades.amazonia.legal) %>% 
                  arrange(desc(empregos_mineracao))

# Classifica empregos de mineração
cnae.mineracao <- classificar.variavel(cnae.mineracao,'empregos_mineracao','class_empregos_mineracao')

cnae.mineracao.inter <- cnae.mineracao %>%
                        dplyr::filter(cod_muni %in% cidades.intermediadoras)


x <- cnae.mineracao %>% 
  group_by(class_empregos_mineracao) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# 3 - PETRÓLEO #################################
# código CNAE
# 06 Extração de petróleo e gás natural
cnae.petroleo <- cnae.cidades %>% 
                 select(id_municipio,cnae_2,vinculos_ativos) %>% 
                 filter(str_detect(cnae_2, "^06")) %>%  
                 group_by(id_municipio) %>% 
                 summarise(empregos_petroleo = sum(vinculos_ativos, na.rm = TRUE))

cnae.petroleo$empregos_petroleo <- as.numeric(cnae.petroleo$empregos_petroleo)

cnae.petroleo <- left_join(cidades.amazonia.legal.nome,cnae.petroleo, by = c('cod_muni'='id_municipio')) %>%  
                 dplyr::filter(empregos_petroleo>0 &
                 cod_muni %in% cidades.amazonia.legal) %>% 
                 arrange(desc(empregos_petroleo))

# Classifica empregos de petróleo 
cnae.petroleo <- classificar.variavel(cnae.petroleo,'empregos_petroleo','class_empregos_petroleo')

x <- cnae.petroleo %>% 
  group_by(class_empregos_petroleo) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# 4 - AGRO ######################################
cnae.agro <- cnae.cidades %>% 
             select(id_municipio,cnae_2,vinculos_ativos) %>% 
             filter(str_detect(cnae_2, "^01|^02|^03")) %>%  
             group_by(id_municipio) %>% 
             summarise(empregos_agro = sum(vinculos_ativos, na.rm = TRUE))

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
empregos.rais <- full_join(cnae.energia,cnae.petroleo)
empregos.rais <- full_join(empregos.rais,cnae.mineracao)
empregos.rais <- full_join(empregos.rais,cnae.agro)
empregos.rais <- full_join(empregos.rais,cidades.amazonia.legal.nome)

# Salvar csv com dados da RAIS para os setores analisados e suas respectivas classificações
write.csv(empregos.rais,file='Outputs/01_tabelas/01_empregos_rais.csv',na = '0')

# filtrar por cidades intermediadoras
rais.cidades.intermed <- empregos.rais %>% 
                         dplyr::filter(cod_muni %in% cidades.intermediadoras) 
