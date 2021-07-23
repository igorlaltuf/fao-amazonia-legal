# Dados sobre valor adicionado bruto - 2017
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar dados
indicadores <- read_excel('./Input/criterios-ipea-nota-tecnica.xlsx') %>% 
               dplyr::filter(cod %in% cidades.amazonia.legal) %>% 
               select(-'gini_faixas',-'faixas_pib_per_capita') # remove a classifição a nível nacional

# Classificar PIB per capita e gini entre as cidades da Amazônia Legal
indicadores <- classificar.variavel(indicadores,'pib_per_capita_corrente_2017','class_pib_per_capita_2017')
indicadores <- classificar.variavel(indicadores,'ibge_gini_2010','class_ibge_gini_2010')


vab.cidades <- function(atividade.economica) {
  vab.maior <- indicadores %>% 
               dplyr::filter(ativ_maior_vab_2017 %in% c(atividade.economica))
  vab.seg.maior <- indicadores %>% 
                   dplyr::filter(ativ_segundo_maior_vab_2017 %in% c(atividade.economica))
  vab.terc.maior <- indicadores %>% 
                    dplyr::filter(ativ_terceiro_maior_vab_2017 %in% c(atividade.economica))
  criterio.vab <- rbind(vab.maior,vab.seg.maior,vab.terc.maior) %>% 
                  unique() 
              }

mun.agro <- vab.cidades(c('Agricultura',
                          'inclusive apoio à agricultura e a pós colheita',
                          'Pecuária, inclusive apoio à pecuária'))

mun.extrativistas <- vab.cidades('Indústrias extrativas')

mun.energia <- vab.cidades('Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação')

# Exportar
write.csv(mun.agro,file='Outputs/01_tabelas/01_vab_agropecuaria_2017.csv',sep = ';',na = '0')
write.csv(mun.extrativistas,file='Outputs/01_tabelas/01_vab_extrativista_2017.csv',sep = ';',na = '0')
write.csv(mun.energia,file='Outputs/01_tabelas/01_vab_energia_2017.csv',sep = ';',na = '0')

# Analise do PIB per capita e Gini
# Junta dataframes
# vab.agrominerge <- rbind(mun.extrativistas,mun.agro)
# vab.agrominerge <- rbind(vab.agrominerge,mun.energia)
# 
# # filtrar cidades que tiveram tanto PIB per capita quanto índice de gini nas categorias alta ou muito alta.
# vab.pib.gini <- vab.agrominerge %>% 
#   dplyr::filter(class_pib_per_capita_2017 %in% c('Alto','Muito Alto') &
#                 class_ibge_gini_2010 %in% c('Alto','Muito Alto'))
# 
# # Ver cidades intermediadoras
# vab.agrominerge.intermediadoras <- vab.agrominerge %>% 
#                              dplyr::filter(cod %in% cidades.intermediadoras)