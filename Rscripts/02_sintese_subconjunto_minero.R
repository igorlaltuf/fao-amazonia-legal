# Subconjunto mineral
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar tabelas output
# vab.min <- read.csv('Outputs/01_tabelas/01_vab_extrativista_2017.csv')
empregos.mineral <- read.csv('Outputs/01_tabelas/01_empregos_rais.csv')
royalties <- read.csv('Outputs/01_tabelas/01_royalties_mineracao_energia.csv')
bndes <- read.csv('Outputs/01_tabelas/01_financ_bndes.csv') 
infra.mineracao <- read.csv('Outputs/01_tabelas/01_infra_logistica.csv')
desmatamento.mineracao <- read.csv('Outputs/01_tabelas/01_desmatamento_mineracao.csv')
mineracao.ilegal <- read.csv('Outputs/01_tabelas/01_minerac_ilegal.csv')

# Reunir as tabelas
royalties.cfem <- royalties %>% 
                  select('cod_muni','class_royalties_cfem_mineracao') %>% 
                  mutate(royalties_cfem = ifelse(royalties$class_royalties_cfem_mineracao %in% c('Alto','Muito Alto'), 1,0))

bndes <- bndes %>% 
         select('cod_muni','mineracao_bndes')

empregos.mineral <- empregos.mineral %>% 
  select('cod_muni','muni','class_empregos_mineracao') %>% 
  mutate(emprego_mineracao = ifelse(empregos.mineral$class_empregos_mineracao %in% c('Alto','Muito Alto'), 1,0))

desmatamento.mineracao <- desmatamento.mineracao %>% 
                          mutate(desmatamento_minerac = ifelse(desmatamento.mineracao$class_desmatamento_minerac %in% c('Alto','Muito Alto'), 1,0))


# vab.min <- vab.min %>% 
#            select(2) %>% 
#            mutate(vab_extrativista = 1)

# Infra logística minero incluir (pronto)
tabela.sintese.mineral <- left_join(empregos.mineral, royalties.cfem, by = c('cod_muni'))
tabela.sintese.mineral <- left_join(tabela.sintese.mineral, bndes, by = c('cod_muni'))
tabela.sintese.mineral <- left_join(tabela.sintese.mineral, infra.mineracao, by = c('cod_muni','muni'))
tabela.sintese.mineral <- left_join(tabela.sintese.mineral, desmatamento.mineracao, by = c('cod_muni','muni'))
tabela.sintese.mineral <- left_join(tabela.sintese.mineral, mineracao.ilegal, by = c('cod_muni'='code_muni','muni'))

tabela.sintese.mineral[is.na(tabela.sintese.mineral)] <- 0 # transformar N.A em zero

tabela.sintese.mineral <- tabela.sintese.mineral %>% 
                          select('cod_muni','muni','emprego_mineracao','royalties_cfem','mineracao_bndes','infra_mineral','desmatamento_minerac','minerac_ilegal') %>% 
                          mutate(total_mineral = emprego_mineracao + royalties_cfem + mineracao_bndes + infra_mineral + desmatamento_minerac + minerac_ilegal) %>% 
                          arrange(desc(total_mineral))
  
# Exportar os arquivos
write.csv(tabela.sintese.mineral,file='Outputs/02_tabelas/02_subconjunto_mineral.csv', na = '0')

# Filtrar as intermediadoras
tabela.mineral.inter <- tabela.sintese.mineral %>% 
                        dplyr::filter(cod_muni %in% cidades.intermediadoras &
                                      total_mineral > 0)

# Tabela
tabela.mineral.inter <- gt(tabela.mineral.inter) %>%
  cols_label(
    muni = 'Cidade',
    emprego_mineracao = 'Empregos formais vinculados a extração de minerais metálicos',
    royalties_cfem = 'Royalties recebidos via CFEM',
    vab_extrativista = 'VAB extrativista entre os três maiores',
    total_mineral = 'Pontuação total',
  ) %>% 
  tab_header(
    title = 'Pontuação das cidades intermediadoras da Amazônia Legal segundo o subconjunto da mineração',
  ) %>%
  cols_hide(
    columns = c(cod_muni)
  ) %>% 
  fmt_markdown(
    columns = c(muni)
  ) %>% 
  fmt_number(
    columns = c(emprego_mineracao,royalties_cfem, vab_extrativista, total_mineral),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Dados fornecidos pelo IPEA (2017) e RAIS 2019.')


tabela.mineral.inter

gtsave(tabela.mineral.inter, 'Outputs/02_tabelas/02_subconjunto_mineral.png') 


# incluir desembolsos do BNDES (acabar aqueles SEM MUNICÍPIO)
