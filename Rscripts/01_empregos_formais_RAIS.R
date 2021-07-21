# Dados de empregos formais da RAIS
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar credenciais do data lake base dos dados
load_dot_env()
credencial <- Sys.getenv('CREDENCIAL_BASE_DOS_DADOS')
basedosdados::set_billing_id(credencial)   

# 1 - ENERGIA ##############################

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

cnae.energia <- left_join(cidades.brasil.nome,cnae.energia,by=c("cod_muni" = "id_municipio")) %>% 
                dplyr::filter(empregos_energia>0)


#####################
# classificar cnae.energia
#####################


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

cnae.mineracao <- left_join(cidades.brasil.nome,cnae.mineracao,by=c("cod_muni" = "id_municipio")) %>% 
                  dplyr::filter(empregos_mineracao>0)

#####################
# classificar cnae.mineracao em alto, muito alto etc? 
#####################


# 3 - PETRÓLEO #################################
# código CNAE
# 06 Extração de petróleo e gás natural
cnae.petroleo <- cnae.cidades %>% 
                 select(id_municipio,cnae_2,vinculos_ativos) %>% 
                 filter(str_detect(cnae_2, "^06")) %>%  
                 group_by(id_municipio) %>% 
                 summarise(empregos_petroleo = sum(vinculos_ativos, na.rm = TRUE))

cnae.petroleo <- left_join(cidades.brasil.nome,cnae.petroleo,by=c("cod_muni" = "id_municipio")) %>% 
  dplyr::filter(empregos_petroleo>0)

#####################
# classificar cnae.petroleo
#####################


# 4 - AGRO ######################################
cnae.agro <- cnae.cidades %>% 
             select(id_municipio,cnae_2,vinculos_ativos) %>% 
             filter(str_detect(cnae_2, "^01|^02|^03")) %>%  
             group_by(id_municipio) %>% 
             summarise(empregos_agro = sum(vinculos_ativos, na.rm = TRUE))

cnae.agro <- left_join(cidades.brasil.nome,cnae.agro,by=c("cod_muni" = "id_municipio")) %>% 
  dplyr::filter(empregos_agro>0)
            
#####################
# classificar cnae.petroleo
#####################      

# 5 - Reunir os dataframes
empregos.rais <- full_join(cnae.energia,cnae.petroleo)
empregos.rais <- full_join(empregos.rais,cnae.mineracao)
empregos.rais <- full_join(empregos.rais,cnae.agro)

# filtrar por cidades da amazônia legal e as intermediadoras
rais.amz.legal <- empregos.rais %>% 
                  dplyr::filter(cod_muni %in% cidades.amazonia.legal) 

rais.cidades.intermed <- empregos.rais %>% 
                         dplyr::filter(cod_muni %in% cidades.intermediadoras) 