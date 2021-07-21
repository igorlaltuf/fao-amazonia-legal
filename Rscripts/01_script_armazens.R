# Dados sobre armazens e portos
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

armazens <- read_excel(path = './Input/armazens.xlsx') 

armazens <- armazens %>% 
            mutate(existe_armazem = ifelse(armazens$armazens_e_silos_2_sem_2020 >0,1,0)) %>% 
            mutate(existe_armazem2 = ifelse(armazens$armazens_e_silos_2_sem_2020 >0,'sim','não'))

armazens.amz.legal <- armazens %>% 
                      dplyr::filter(cod_muni %in% cidades.amazonia.legal)

armazens.intermed <- armazens %>% 
                     dplyr::filter(cod_muni %in% cidades.intermediadoras)


# Incluir dados sobre portos que escoam Soja, Milho e Arroz


# Incluir dados sobre terminais ferroviários que escoam Soja, Milho e Arroz, minério de ferro etc


