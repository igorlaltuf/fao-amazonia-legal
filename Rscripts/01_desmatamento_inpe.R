# Desmatamento -INPE
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

desmatamento.2015 <- read.delim('Input/dados_desmatamento_inpe/DesmatamentoMunicipios2015.txt', header = T, sep = ',', dec = '.')
desmatamento.2016 <- read.delim('Input/dados_desmatamento_inpe/DesmatamentoMunicipios2016.txt', header = T, sep = ',', dec = '.')
desmatamento.2017 <- read.delim('Input/dados_desmatamento_inpe/DesmatamentoMunicipios2017.txt', header = T, sep = ',', dec = '.')
desmatamento.2018 <- read.delim('Input/dados_desmatamento_inpe/DesmatamentoMunicipios2018.txt', header = T, sep = ',', dec = '.')
desmatamento.2019 <- read.delim('Input/dados_desmatamento_inpe/DesmatamentoMunicipios2019.txt', header = T, sep = ',', dec = '.')
desmatamento.2020 <- read.delim('Input/dados_desmatamento_inpe/DesmatamentoMunicipios2020.txt', header = T, sep = ',', dec = '.')

desmatamento.2015 <- desmatamento.2015 %>% 
                     select('CodIbge','Desmatado2015')
desmatamento.2016 <- desmatamento.2016 %>% 
                     select('CodIbge','Desmatado2016')
desmatamento.2017 <- desmatamento.2017 %>% 
                     select('CodIbge','Desmatado2017')
desmatamento.2018 <- desmatamento.2018 %>% 
                     select('CodIbge','Desmatado2018')
desmatamento.2019 <- desmatamento.2019 %>% 
                     select('CodIbge','Desmatado2019')
desmatamento.2020 <- desmatamento.2020 %>% 
                     select('CodIbge','Desmatado2020')

desmatamento <- left_join(desmatamento.2015,desmatamento.2016)
desmatamento <- left_join(desmatamento,desmatamento.2017)
desmatamento <- left_join(desmatamento,desmatamento.2018)
desmatamento <- left_join(desmatamento,desmatamento.2019)
desmatamento <- left_join(desmatamento,desmatamento.2020)

desmatamento <- left_join(desmatamento,cidades.brasil.nome,by=c('CodIbge'='cod_muni')) %>% 
                select(1,8,2,3,4,5,6,7) 
colnames(desmatamento) <- c('cod_muni','muni','desm_2015','desm_2016','desm_2017','desm_2018','desm_2019','desm_2020')

desmatamento <- desmatamento %>% 
                mutate(desmatamento_total = desm_2015+desm_2016+desm_2017+desm_2018+desm_2019+desm_2020) %>% 
                dplyr::filter(desmatamento_total > 0 & cod_muni %in% cidades.amazonia.legal)

desmatamento <- classificar.variavel(desmatamento,'desmatamento_total','class_desmatamento')

desmatamento.interm <- desmatamento %>% 
                       dplyr::filter(cod_muni %in% cidades.intermediadoras)

# Exportar
write.csv(desmatamento, file='Outputs/01_tabelas/01_desmatamento.csv')