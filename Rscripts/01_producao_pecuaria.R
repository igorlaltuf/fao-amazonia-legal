# Produção pecuária
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

gado <- read_excel(path = 'Input/tabela3939-cabeca-de-gado.xlsx') %>% 
        dplyr::filter(qtd_cabecas_de_gado_2019 > 0 &
                      cod_muni %in% cidades.amazonia.legal)

gado <- classificar.variavel(gado,'qtd_cabecas_de_gado_2019','class_gado_2019')

# teste para ver quantos itens existem em cada categoria
x <- gado %>% 
  group_by(class_gado_2019) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# Exportar arquivo
write.csv(gado,file='Outputs/01_tabelas/01_prod_gado.csv', na = '0', row.names = F)

gado.intermed <- gado %>% 
         dplyr::filter(cod_muni %in% cidades.intermediadoras)