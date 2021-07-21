# Dados sobre valor adicionado bruto - 2017
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

indicadores <- read_excel('./Input/criterios-ipea-nota-tecnica.xlsx') 

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

mun.extrativistas <- vab.cidades('Indústrias extrativas')

mun.agro <- vab.cidades(c('Agricultura',
                          'inclusive apoio à agricultura e a pós colheita',
                          'Pecuária, inclusive apoio à pecuária'))

mun.energia <- vab.cidades('Eletricidade e gás, água, esgoto, atividades de gestão de resíduos e descontaminação')

# Junta dataframes
vab.agrominerge <- rbind(mun.extrativistas,mun.agro)
vab.agrominerge <- rbind(vab.agrominerge,mun.energia)


vab.agrominerge.amz.legal <- vab.agrominerge %>% 
                             dplyr::filter(cod %in% cidades.amazonia.legal)

vab.agrominerge.intermediadoras <- vab.agrominerge %>% 
                             dplyr::filter(cod %in% cidades.intermediadoras)


# se eu incluir o critério 'médio alto', fica muito abrangente.
indicadores.pibgini <- vab.agrominerge.amz.legal %>% 
                       dplyr::filter(faixas_pib_per_capita %in% c('Alto','Muito Alto') &
                                      gini_faixas %in% c('Alto','Muito Alto'))




# reclassificar todas as cidades apenas de acordo com a amazonia legal!!!!!!





