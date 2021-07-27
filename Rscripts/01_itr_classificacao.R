# ITR - classificação
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

itr.2017 <- read_excel(path = './Input/itr_ipea.xlsx')

itr.2017$cota_parte_itr <- as.numeric(itr.2017$cota_parte_itr)
itr.2017[is.na(itr.2017)] <- 0

itr.2017 <- itr.2017 %>% 
            dplyr::filter(cota_parte_itr > 0 &
                          cod_muni %in% cidades.amazonia.legal) 
            
itr.2017 <- classificar.variavel(itr.2017,'cota_parte_itr','class_cota_parte_itr') 
           
x <- itr.2017 %>% 
  group_by(class_cota_parte_itr) %>%
  mutate(N_category = n()) %>%
  count(N_category)

write.csv(itr.2017,file = 'Outputs/01_tabelas/01_itr_cota_parte.csv')
