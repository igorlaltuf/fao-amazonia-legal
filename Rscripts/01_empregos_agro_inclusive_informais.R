# Dados de empregos formais e informais (Censo Agro 2017)
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')
empregos.agro.censo <- read_excel(path = './Input/empregos agro censo 2017.xlsx')

# População estimada de 2017
empregos.agro.censo <- empregos.agro.censo %>% 
                       mutate(perc_familiar_sobre_agro = pessoal_ocupado_agro_familiar/pessoal_ocupado_agro) %>% 
                       dplyr::filter(cod_muni %in% cidades.amazonia.legal)

empregos.agro.censo[5] <- round(empregos.agro.censo['perc_familiar_sobre_agro'],2)

# Classificar empregos agro censo
empregos.agro.censo <- classificar.variavel(empregos.agro.censo,'perc_familiar_sobre_agro','class_familiar_sobre_agro')

# Exportar
write.csv(empregos.agro.censo,file='Outputs/01_tabelas/01_emprego_agro_censo.csv',na = '0')

# Checar quantidades por faixa
x <- empregos.agro.censo %>% 
  group_by(class_familiar_sobre_agro) %>%
  mutate(N_category = n()) %>%
  count(N_category)

cidades.intermed <- empregos.agro.censo %>% 
                    dplyr::filter(cod_muni %in% cidades.intermediadoras) %>% 
                    arrange(desc(perc_familiar_sobre_agro)) 