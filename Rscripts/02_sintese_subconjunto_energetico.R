# Subconjunto energético
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar tabelas output
empregos.energia <- read.csv('Outputs/01_tabelas/01_empregos_rais.csv')
royalties.energia <- read.csv('Outputs/01_tabelas/01_royalties_mineracao_energia.csv')
bndes <- read.csv('Outputs/01_tabelas/01_financ_bndes.csv') 
vab.energia <- read.csv('Outputs/01_tabelas/01_vab_energia_2017.csv')
infra.energia <- read.csv('Outputs/01_tabelas/01_geracao_energia_amzl.csv')
desmatamento <- read.csv('Outputs/01_tabelas/01_desmatamento_bacias_energia.csv')

# Empregos Energia
empregos.rais.energia <- empregos.energia %>% 
                         select(2:5) %>% 
                         mutate(empregos_energia_rais = ifelse(empregos.energia$class_empregos_energia %in% c('Alto','Muito Alto'),1,0))

# Royalties
royalties.energia <- royalties.energia %>% 
                     select(2:5) %>% 
                     mutate(royalties_energia_cfh = ifelse(royalties.energia$class_royalties_cfh %in% c('Alto','Muito Alto'),1,0))

# BNDES
bndes.energia <- bndes %>% 
                 select('cod_muni','energia_bndes')

# Infra energia elétrica - Térmicas e hidrelétrica
infra.energia


# Desmatamento bacias hidrográficas
desmatamento <- desmatamento %>% 
  select('cod_muni','class_hidro') %>% 
  mutate(desmatamento_bacias = ifelse(desmatamento$class_hidro %in% c('Alto','Muito Alto'), 1,0))



# Juntar tabela
sintese.energia <- left_join(empregos.rais.energia,royalties.energia, by=c('cod_muni','muni'))
sintese.energia <- left_join(sintese.energia,bndes.energia, by='cod_muni')
sintese.energia <- left_join(sintese.energia,infra.energia, by=c('cod_muni','muni'))
sintese.energia <- left_join(sintese.energia,desmatamento, by='cod_muni')

sintese.energia <- sintese.energia %>% 
                   select('cod_muni','muni','empregos_energia_rais','royalties_energia_cfh','energia_bndes','existe_UTE','existe_UHE_PCH','desmatamento_bacias')

# 
# # VAB Energia elétrica
# vab.energia <- vab.energia %>% 
#                select(2) %>% 
#                mutate(vab_energia_eletrica = 1)
          
sintese.energia[is.na(sintese.energia)] <- 0 # transformar N.A em zero
sintese.energia <- sintese.energia %>% 
                   mutate(total_energia = empregos_energia_rais + royalties_energia_cfh + energia_bndes + existe_UTE + existe_UHE_PCH + desmatamento_bacias)



# Exportar tabela
write.csv(sintese.energia,file='Outputs/02_tabelas/02_subconjunto_energia.csv', na = '0')



# Coari gás financiamento (não teve), empregos (ver municípios) e royalties (incluir municípios com valor acima de 1 milhão)
# Explicar que o critério do petróleo será diferente. Ver VAB petróleo


# Incluir as infraestruturas de energia elétrica (já tenho os dados. Ver financiamentos do BNDES para ver usinas pós 2015 e contrapor os dados (belo monte não consta)
# emprego em construção civil na época da obra
# Ver área de influencia dos empreendimentos UHE