
# EM ANDAMENTO!

library(readxl)
library(tidyverse)
library(gt)

rm(list=ls()) # limpar as variáveis carregadas
setwd('F:/Meu repositório/classificacao-agro/')

gado <- read_excel(path = 'Input/tabela3939-cabeca-de-gado.xlsx') %>% 
        dplyr::filter(qtd_cabecas_de_gado_2019>0)

gado$qtd_cabecas_de_gado_2019[is.na(gado$qtd_cabecas_de_gado_2019)] <- 0

area.produtiva <- read_excel(path = 'Input/tabela6771-area-estab-agro.xlsx')

tabela.pecuaria <- left_join(gado,area.produtiva,by='cod_muni') %>% 
                   select(-4)

# criar coluna com quantidade de cabeça de gado (pesquisa da pecuária municipal 2019) por hectare da agropecuária (Censo 2017)


cod.cidades <- c('1100023','1100122','1100205','1200401','1200203','1302405','1302504','1301902','1304203',
                 '1304062','1303403','1400472','1400100','1502400','1500602','1501808','1504208','1506138',
                 '1506807','1600303','1600501','1702109','1709500','1721000','2101202','2109908','2109106',
                 '2105302','2103000','5102504','5101803','5107602','5103403','5107909')

tabela.pecuaria <- tabela.pecuaria %>% 
                   mutate(gado_por_hectare = qtd_cabecas_de_gado_2019/area_estab_agro_hecta) 



tabela.pecuaria.intermed <- tabela.pecuaria %>% 
                            dplyr::filter(cod_muni %in% cod.cidades)

# Analisar

