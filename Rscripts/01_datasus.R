# Datasus - mortalidade por município
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar credenciais do data lake base dos dados
load_dot_env()
credencial <- Sys.getenv('CREDENCIAL_BASE_DOS_DADOS')
basedosdados::set_billing_id(credencial)   

# Fazer a query
query <- "SELECT * FROM `basedosdados.br_ms_sim.municipio_causa`
          WHERE ano = 2019"
datasus <- read_sql(query)

# J68, J70, J96
datasus.amzl <- datasus %>% 
                dplyr::filter(id_municipio %in% cidades.amazonia.legal) %>% 
                filter(str_detect(causa_basica, '^J68|^J70|^J96')) %>% # ^N14 é de mercúrio
                select('id_municipio','sigla_uf','numero_obitos') %>% 
                group_by(id_municipio,sigla_uf) %>% 
                summarise(numero_de_obitos_2019 = sum(numero_obitos, na.rm = TRUE))

datasus.amzl$id_municipio <- as.numeric(datasus.amzl$id_municipio)
                           
datasus.amzl <- left_join(datasus.amzl,cidades.amazonia.legal.nome, by = c('id_municipio'='cod_muni')) 


query <- "SELECT * FROM `basedosdados.br_ms_sim.dicionario`"
df <- read_sql(query)
