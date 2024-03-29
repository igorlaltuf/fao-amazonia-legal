# Subconjunto da agropecu�ria
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# Carregar tabelas output
prod.agro <- read.csv(file = "Outputs/01_tabelas/01_producao_agro.csv") 
gado <- read.csv(file = 'Outputs/01_tabelas/01_prod_gado.csv')
emprego.rais <- read.csv(file = "Outputs/01_tabelas/01_empregos_rais.csv") 
estabelecimentos <- read.csv(file = "Outputs/01_tabelas/01_medios_e_grandes_estab_agropec.csv") 
itr.cota.parte <- read.csv(file = "Outputs/01_tabelas/01_itr_cota_parte.csv")
desmatamento <- read.csv(file = 'Outputs/01_tabelas/01_desmatamento_agro.csv')
infraestrutura <- read.csv(file = 'Outputs/01_tabelas/01_infra_logistica.csv')

# Pontua��o para categorias "alto" ou "muito alto"
# Empregos formais
emprego.rais <- emprego.rais %>%   
                select('cod_muni','muni','qtd_empregos_agro','class_empregos_agro') %>% 
                mutate(pont_emprego_agro = ifelse(emprego.rais$class_empregos_agro %in% c('Alto','Muito Alto'), 1, 0)) %>% 
                arrange(desc(qtd_empregos_agro))

# Quantidade de m�dios e grandes estabelecimentos agropecu�rios
estabelecimentos <- estabelecimentos %>% 
                    select('cod_muni','muni','medios_e_grandes_estab','class_medios_e_grandes_estab') %>% 
                    mutate(pont_concentr_terra = ifelse(estabelecimentos$class_medios_e_grandes_estab %in% c('Alto','Muito Alto'), 1, 0)) %>% 
                    arrange(desc(medios_e_grandes_estab))

# Produ��o de Soja, milho e Arroz por m�dios e grandes produtores soja ou quantidade de cabe�as de gado
gado <- gado %>% 
  mutate(pont_gado = ifelse(gado$class_gado_2019 %in% c('Alto','Muito Alto'), 1, 0)) %>% 
  arrange(desc(qtd_cabecas_de_gado_2019))

prod.agro <- prod.agro %>% 
             select('cod_muni','muni','valor_producao','class_valor_producao')%>% 
             mutate(pont_prod_agro = ifelse(prod.agro$class_valor_producao %in% c('Alto','Muito Alto'), 1, 0)) %>% 
             arrange(desc(valor_producao))

graos_gado <- left_join(gado,prod.agro,by=c('cod_muni','muni')) %>% 
              mutate(pont_graos_gado = ifelse(pont_gado|pont_prod_agro == 1, 1, 0))

# ITR Cota Parte
itr.cota.parte <- itr.cota.parte %>% 
                  select('cod_muni','cota_parte_itr') %>% 
                  mutate(pont_itr = 1)

# Desmatamento INPE
desmatamento <- desmatamento %>% 
                select('cod_muni','class_desmat','total_desm','class_desmat') %>% 
                mutate(pont_desmat = ifelse(desmatamento$class_desmat %in% c('Alto','Muito Alto'), 1, 0))
               
# Infra log�stica agro incluir - portos, ferrovias e armaz�ns
infraestrutura <- infraestrutura %>% 
  select('cod_muni','muni','infra_agro')
colnames(infraestrutura)[3] <- 'pont_infra_agro'

# Tabela s�ntese
tabela.sintese.agro <- left_join(emprego.rais, estabelecimentos, by = c('cod_muni','muni'))
tabela.sintese.agro <- left_join(tabela.sintese.agro, graos_gado, by = c('cod_muni','muni'))
tabela.sintese.agro <- left_join(tabela.sintese.agro, desmatamento, by = c('cod_muni'))
tabela.sintese.agro <- left_join(tabela.sintese.agro, infraestrutura, by = c('cod_muni','muni'))
tabela.sintese.agro <- left_join(tabela.sintese.agro, itr.cota.parte, by = c('cod_muni'))

# Tabela com Pontos
tabela.agro.pontos <- tabela.sintese.agro %>%     
                      select('cod_muni','muni','pont_emprego_agro','pont_concentr_terra','pont_graos_gado','pont_itr','pont_desmat','pont_infra_agro')
                        
tabela.agro.pontos[is.na(tabela.agro.pontos)] <- 0 # transformar N.A em zero

tabela.agro.pontos <- tabela.agro.pontos %>% 
                      mutate(total_agro = pont_emprego_agro + pont_concentr_terra + pont_graos_gado + pont_itr + pont_desmat + pont_infra_agro) %>% 
                      arrange(desc(total_agro))

# Exportar tabela de pontos
write.csv(tabela.agro.pontos, file='Outputs/02_tabelas/02_subconjunto_agro_pontos.csv', row.names = F)

# Tabela com valores
tabela.agro.valores <- tabela.sintese.agro %>%     
  select('cod_muni','muni','qtd_empregos_agro','medios_e_grandes_estab','qtd_cabecas_de_gado_2019','valor_producao','cota_parte_itr','total_desm')

colnames(tabela.agro.valores)[3] <- 'qtd_empregos_agro_2019'
colnames(tabela.agro.valores)[4] <- 'qtd_medios_grandes_estab_agro_2017'
colnames(tabela.agro.valores)[6] <- 'valor_prod_agro_2017'
# colnames(tabela.agro.valores)[7] <- 'valor_cota_ITR_ano?'
colnames(tabela.agro.valores)[8] <- 'desmatamento_2005_2020'

tabela.agro.valores[is.na(tabela.agro.valores)] <- 0 # transformar N.A em zero

# exportar tabela com valores
write.csv(tabela.agro.valores, file='Outputs/02_tabelas/02_subconjunto_agro_valores.csv', row.names = F)


# tabela intermediadoras que tiveram pelo menos 1 ponto
z <- tabela.agro.pontos %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras,
                total_agro > 0) %>% 
  arrange(desc(total_agro))


tabela.agro <- gt(z) %>%
  cols_label(
    muni = 'Munic�pio',
    pont_emprego_agro = 'Empregos Formais',
    pont_concentr_terra = 'Concentra��o de terra',
    pont_graos_gado = 'Gr�os e gado',
    pont_itr = 'ITR',
    pont_desmat = 'Desmatamento',
    pont_infra_agro = 'Infraestrutura',
    total_agro = 'Pontua��o total'
    
  ) %>% 
  tab_header(
    title = 'Pontua��o da agropecu�ria nas cidades intermediadoras da Amaz�nia Legal'
  ) %>% 
  cols_hide(
    columns = c(cod_muni)
  ) %>% 
  fmt_markdown(
    columns = c(muni)
  ) %>% 
  fmt_number(
    columns = c(pont_emprego_agro, pont_concentr_terra, pont_graos_gado, pont_itr, pont_desmat),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  )

tabela.agro
gtsave(tabela.agro, 'Outputs/03_mapas/Agropecu�ria/03_tabela_agro_inter.png')
