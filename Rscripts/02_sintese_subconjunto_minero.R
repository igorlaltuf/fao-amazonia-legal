# Subconjunto mineral
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# Carregar tabelas output
empregos.mineral <- read.csv('Outputs/01_tabelas/01_empregos_rais.csv')
royalties.cfem <- read.csv('Outputs/01_tabelas/01_royalties_mineracao_energia.csv')
bndes <- read.csv('Outputs/01_tabelas/01_financ_bndes.csv') 
infra.mineracao <- read.csv('Outputs/01_tabelas/01_infra_logistica.csv')
desmatamento.mineracao <- read.csv('Outputs/01_tabelas/01_desmatamento_mineracao.csv')
mineracao.ilegal <- read.csv('Outputs/01_tabelas/01_minerac_ilegal.csv')

# Reunir as tabelas
royalties.cfem <- royalties.cfem %>% 
                  select('cod_muni','muni','class_royalties_cfem_mineracao') %>% 
                  mutate(pont_cfem = ifelse(royalties.cfem$class_royalties_cfem_mineracao %in% c('Muito Alto'), 1, 0)) 

bndes <- bndes %>% 
         select('cod_muni','mineracao_bndes')

empregos.mineral <- empregos.mineral %>% 
  select('cod_muni','muni','class_empregos_mineracao') %>% 
  mutate(emprego_mineracao = ifelse(empregos.mineral$class_empregos_mineracao %in% c('Alto','Muito Alto'), 1, 0))

desmatamento.mineracao <- desmatamento.mineracao %>% 
                          mutate(desmat_minerac = ifelse(desmatamento.mineracao$class_desmat_min %in% c('Muito Alto'), 1, 0)) %>% 
                          select('cod_muni','desmat_minerac') %>% 
                          arrange(desc(desmat_minerac))

# Infra log�stica minero incluir (pronto)
tabela.sintese.mineral <- left_join(empregos.mineral, royalties.cfem, by = c('cod_muni','muni'))
tabela.sintese.mineral <- left_join(tabela.sintese.mineral, bndes, by = c('cod_muni'))
tabela.sintese.mineral <- left_join(tabela.sintese.mineral, infra.mineracao, by = c('cod_muni','muni'))
tabela.sintese.mineral <- left_join(tabela.sintese.mineral, desmatamento.mineracao, by = 'cod_muni')
tabela.sintese.mineral <- left_join(tabela.sintese.mineral, mineracao.ilegal, by = c('cod_muni'='code_muni','muni'))

tabela.sintese.mineral[is.na(tabela.sintese.mineral)] <- 0 # transformar N.A em zero

tabela.sintese.mineral <- tabela.sintese.mineral %>% 
                          select('cod_muni','muni','emprego_mineracao','pont_cfem','mineracao_bndes','infra_mineral','desmat_minerac','minerac_ilegal') %>% 
                          mutate(total_mineral = emprego_mineracao + pont_cfem + mineracao_bndes + infra_mineral + desmat_minerac + minerac_ilegal) %>% 
                          arrange(desc(total_mineral))
  
# Exportar os arquivos
write.csv(tabela.sintese.mineral, file = 'Outputs/02_tabelas/02_subconjunto_mineral.csv', row.names = F)

# Filtrar as intermediadoras
tabela.mineral.inter <- tabela.sintese.mineral %>% 
                        dplyr::filter(cod_muni %in% cidades.intermediadoras &
                                      total_mineral > 0)