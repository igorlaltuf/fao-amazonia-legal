# Subconjunto energético
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')



# Carregar tabelas output
empregos.rais.energia <- read_csv('Outputs/01_tabelas/01_empregos_rais.csv', locale = locale(encoding = "latin1"))
royalties.energia <- read_csv('Outputs/01_tabelas/01_royalties_mineracao_energia.csv', locale = locale(encoding = "latin1"))
bndes <- read_csv('Outputs/01_tabelas/01_financ_bndes.csv', locale = locale(encoding = "latin1")) 
vab.energia <- read_csv('Outputs/01_tabelas/01_vab_energia_2017.csv', locale = locale(encoding = "latin1"))
infra.energia <- read_csv('Outputs/01_tabelas/01_geracao_energia_amzl.csv', locale = locale(encoding = "latin1"))
desmatamento <- read_csv('Outputs/01_tabelas/01_desmatamento_bacias_energia.csv', locale = locale(encoding = "latin1"))

# Empregos Energia
empregos.rais.energia <- empregos.rais.energia %>% 
                         select('cod_muni','muni','class_empregos_energia') %>% 
                         mutate(pont_empregos_energia = ifelse(empregos.rais.energia$class_empregos_energia %in% 'Muito Alto', 1, 0))

# Royalties
royalties.energia <- royalties.energia %>% 
                     select('cod_muni','muni','class_royalties_cfh') %>%
                     mutate(royalties_cfh = ifelse(royalties.energia$class_royalties_cfh %in% 'Muito Alto', 1, 0))

# BNDES
bndes.energia <- bndes %>% 
                 select('cod_muni','energia_bndes')

# Infra energia elétrica - Térmicas e hidrelétrica
# pronto

# Desmatamento bacias hidrográficas
desmatamento <- desmatamento %>% 
  select('cod_muni','class_hidro') %>% 
  mutate(desmatamento_bacias = ifelse(desmatamento$class_hidro %in% c('Alto','Muito Alto'), 1,0))


# O ERRO ESTÁ NESSE SCRIPT A PARTIR DAQUI!!!!
# Tucuruí tem hidrelétrica, mas não está mostrando. PQ????

# Juntar tabela
sintese.energia <- full_join(royalties.energia,empregos.rais.energia, by = c('muni', 'cod_muni'))
sintese.energia <- left_join(sintese.energia, bndes.energia, by = 'cod_muni')
sintese.energia <- left_join(sintese.energia, infra.energia, by = 'cod_muni')
sintese.energia <- left_join(sintese.energia, desmatamento, by = 'cod_muni')

sintese.energia <- sintese.energia %>% 
                   select('cod_muni','muni.x','pont_empregos_energia','royalties_cfh','energia_bndes','existe_UTE','existe_UHE_PCH','desmatamento_bacias') %>% 
                   rename(muni = muni.x)

colnames(sintese.energia)[5] <- 'pont_financ_bndes'
colnames(sintese.energia)[8] <- 'pont_desm_bacia'

sintese.energia[is.na(sintese.energia)] <- 0 # transformar N.A em zero
sintese.energia <- sintese.energia %>% 
                   mutate(total_energia = pont_empregos_energia + royalties_cfh + pont_financ_bndes + existe_UTE + existe_UHE_PCH + pont_desm_bacia)

# Exportar tabela
write.csv(sintese.energia, file='Outputs/02_tabelas/02_subconjunto_energia.csv', row.names = F, fileEncoding = 'Latin1')

x <- sintese.energia %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras) %>% 
  arrange(desc(total_energia))
