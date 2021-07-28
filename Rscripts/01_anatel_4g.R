rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# read_excel(path = 'Input/')

query <- "SELECT ano, sigla_uf, id_municipio, empresa, sinal, produto, SUM(acessos) as acessos FROM `basedosdados.br_anatel_telefonia_movel.municipio`
          WHERE ano = 2019
          GROUP BY ano, sigla_uf, id_municipio, empresa, sinal, produto"

internet <- read_sql(query)

unique(internet$produto) #Ver as categorias

internet.amzl.2019 <- internet %>% 
                      dplyr::filter(sinal %in% '4G' &
                                    produto %in% c('DADOS','VOZ+DADOS') &
                                    id_municipio %in% cidades.amazonia.legal) %>% 
                      select(3,7) %>% 
                      group_by(id_municipio) %>% 
                      summarise(acessos_2019 = sum(acessos, na.rm = TRUE))

internet.amzl.2019$acessos_2019 <- as.numeric(internet.amzl.2019$acessos_2019)

internet.amzl.2019 <- classificar.variavel(internet.amzl.2019,'acessos_2019','class_acessos_2019')

# Calcular a cada 100 mil habitantes pelas estimativas de 2019


x <- internet.amzl.2019 %>% 
     group_by(class_acessos_2019) %>%
     mutate(N_category = n()) %>%
     count(N_category)
