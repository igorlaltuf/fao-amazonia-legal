# Subconjunto da agropecuária
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar tabelas output
emprego.familiar <- read.csv(file = "Outputs/01_tabelas/01_emprego_agro_censo.csv") 
emprego.rais <- read.csv(file = "Outputs/01_tabelas/01_empregos_rais.csv") 
estabelecimentos <- read.csv(file = "Outputs/01_tabelas/01_medios_e_grandes_estab_agropec.csv") 
prod.agro <- read.csv(file = "Outputs/01_tabelas/01_producao_agro.csv") 
itr.cota.parte <- read.csv(file = "Outputs/01_tabelas/01_itr_cota_parte.csv")
datasus <- read.csv(file = "Outputs/01_tabelas/01_datasus_agro.csv")
gado <- read.csv(file = 'Outputs/01_tabelas/01_prod_gado.csv')
desmatamento <- read.csv(file = 'Outputs/01_tabelas/01_desmatamento_agro.csv')
infraestrutura <- read.csv(file = 'Outputs/01_tabelas/01_infra_logistica.csv')
# Pontuação para categorias "alto" ou "muito alto"

# 
# emprego.familiar <- emprego.familiar %>% 
#                     select(2,3,6,7) %>% 
#                     mutate(emprego_familiar = ifelse(emprego.familiar$class_familiar_sobre_agro %in% c('Baixo','Muito Baixo'), 1,0))

# Empregos formais
emprego.rais <- emprego.rais %>% 
                select(2,3,10,11) %>% 
                mutate(emprego_rais = ifelse(emprego.rais$class_empregos_agro %in% c('Alto','Muito Alto'), 1,0))

# Quantidade de médios e grandes estabelecimentos agropecuários
estabelecimentos <- estabelecimentos %>% 
                    select(2,3,8,9)%>% 
                    mutate(estabelecimentos = ifelse(estabelecimentos$class_medios_e_grandes_estab %in% c('Alto','Muito Alto'), 1,0))

# soja ou gado
gado <- gado %>% 
  select(2,4,5) %>% 
  mutate(prod_gado = ifelse(gado$class_gado_2019 %in% c('Alto','Muito Alto'), 1,0))

prod.agro <- prod.agro %>% 
             select(2,3,7,8)%>% 
             mutate(producao_agro = ifelse(prod.agro$class_valor_producao %in% c('Alto','Muito Alto'), 1,0))

graos_gado <- left_join(gado,prod.agro,by='cod_muni') %>% 
              select(1,5,4,8) %>% 
              mutate(graos_gado = ifelse(prod_gado|producao_agro == 1, 1, 0))


# ITR cota parte
itr.cota.parte <- itr.cota.parte %>% 
                  select(2,5) %>% 
                  mutate(cota_parte_itr = ifelse(itr.cota.parte$class_cota_parte_itr %in% c('Alto','Muito Alto'), 1,0))

# incidência de câncer (atualizar para demais anos)
datasus <- datasus %>% 
           select(2,7,8) %>% 
           mutate(datasus = ifelse(datasus$class_cada_100_mil %in% c('Alto','Muito Alto'), 1,0))

# Desmatamento INPE
desmatamento <- desmatamento %>% 
                select('cod_muni','class_desmat') %>% 
                mutate(desmatamento = ifelse(desmatamento$class_desmat %in% c('Alto','Muito Alto'), 1,0))
               
# Infra logística agro incluir (pronto) - portos, ferrovias e armazéns
infraestrutura <- infraestrutura %>% 
                  select(2:4)
                  


# Tabela síntese
tabela.sintese.agro <- left_join(emprego.rais,estabelecimentos, by=c('cod_muni','muni'))
tabela.sintese.agro <- left_join(tabela.sintese.agro,graos_gado, by=c('cod_muni','muni'))
tabela.sintese.agro <- left_join(tabela.sintese.agro, datasus, by=c('cod_muni'))
tabela.sintese.agro <- left_join(tabela.sintese.agro,itr.cota.parte, by=c('cod_muni'))
tabela.sintese.agro <- left_join(tabela.sintese.agro,desmatamento, by=c('cod_muni'))
tabela.sintese.agro <- left_join(tabela.sintese.agro,infraestrutura, by=c('cod_muni','muni'))

tabela.sintese.agro <- tabela.sintese.agro %>%     
                       select('cod_muni','muni','emprego_rais','estabelecimentos','graos_gado','cota_parte_itr','datasus','desmatamento','infra_agro')
                        
tabela.sintese.agro[is.na(tabela.sintese.agro)] <- 0 # transformar N.A em zero

tabela.sintese.agro <- tabela.sintese.agro %>% 
                       mutate(total_agro = emprego_rais + estabelecimentos + graos_gado + cota_parte_itr + datasus + desmatamento + infra_agro)

# Exportar tabela
write.csv(tabela.sintese.agro,file='Outputs/02_tabelas/02_subconjunto_agro.csv', na = '0')

# Cidades intermediadoras
tabela.sintese.agro.intermed <- tabela.sintese.agro %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras &
                total_agro > 0) %>% 
                arrange(desc(total_agro))


#tabela
tabela.sintese.agro.intermed <- gt(tabela.sintese.agro.intermed) %>%
  cols_label(
    muni = 'Cidade',
    emprego_familiar = 'Proporção de pessoal ocupado com vínculo familiar na agropecuária',
    emprego_rais = 'Emprego formal na agropecuária',
    estabelecimentos = 'Quantitativo de médios e grandes estabelecimentos agro',
    producao_agro = 'Valor da produção de soja, milho e arroz',
    cota_parte_itr = 'Cota-parte do ITR',
    total_agro = 'Pontuação total'
  ) %>% 
  tab_header(
    title = 'Pontuação das cidades intermediadoras da Amazônia Legal segundo o subconjunto da agropecuária',
  ) %>%
  cols_hide(
    columns = c(cod_muni)
  ) %>% 
  fmt_markdown(
    columns = c(muni)
  ) %>% 
  fmt_number(
    columns = c(emprego_familiar,emprego_rais, estabelecimentos, producao_agro,cota_parte_itr),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Censo Agropecuário 2017, RAIS 2019 e dados fornecidos pelo IPEA (2017).')


tabela.sintese.agro.intermed

gtsave(tabela.sintese.agro.intermed, 'Outputs/02_tabelas/02_subconjunto_agro.png') 




Incluir datasus, desmatamento e boi na tabela final.
# Feitos:
# RAIS
# Censo agro
# Concentração de terra
# valor da produção
# cota-parte ITR

# Em andamento:
# produção pecuária
# infraestruturas existentes ou previstas de suporte ao agro
# desmatamento
# incidência de doenças respiratórias


