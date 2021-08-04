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
desmatamento <- read.csv(file = 'Outputs/01_tabelas/01_desmatamento.csv')

# Pontuação para categorias "alto" ou "muito alto"
emprego.familiar <- emprego.familiar %>% 
                    select(2,3,6,7) %>% 
                    mutate(emprego_familiar = ifelse(emprego.familiar$class_familiar_sobre_agro %in% c('Baixo','Muito Baixo'), 1,0))

emprego.rais <- emprego.rais %>% 
                select(2,3,10,11) %>% 
                mutate(emprego_rais = ifelse(emprego.rais$class_empregos_agro %in% c('Alto','Muito Alto'), 1,0))

estabelecimentos <- estabelecimentos %>% 
                    select(2,3,8,9)%>% 
                    mutate(estabelecimentos = ifelse(estabelecimentos$class_medios_e_grandes_estab %in% c('Alto','Muito Alto'), 1,0))

prod.agro <- prod.agro %>% 
             select(2,3,7,8)%>% 
             mutate(producao_agro = ifelse(prod.agro$class_valor_producao %in% c('Alto','Muito Alto'), 1,0))

itr.cota.parte <- itr.cota.parte %>% 
                  select(2,5) %>% 
                  mutate(cota_parte_itr = ifelse(itr.cota.parte$class_cota_parte_itr %in% c('Alto','Muito Alto'), 1,0))

datasus <- datasus %>% 
           select(2,7,8) %>% 
           mutate(datasus = ifelse(datasus$class_cada_100_mil %in% c('Alto','Muito Alto'), 1,0))

gado <- gado %>% 
        select(2,4,5) %>% 
        mutate(prod_gado = ifelse(gado$class_gado_2019 %in% c('Alto','Muito Alto'), 1,0))

desmatamento <- desmatamento %>% 
                select(2,10,11) %>% 
                mutate(desmatamento = ifelse(desmatamento$class_desmatamento %in% c('Alto','Muito Alto'), 1,0))
                
# Tabela síntese
tabela.sintese.agro <- left_join(emprego.familiar,emprego.rais)
tabela.sintese.agro <- left_join(tabela.sintese.agro,estabelecimentos)
tabela.sintese.agro <- left_join(tabela.sintese.agro,prod.agro)
tabela.sintese.agro <- left_join(tabela.sintese.agro, datasus)
tabela.sintese.agro <- left_join(tabela.sintese.agro,gado)
tabela.sintese.agro <- left_join(tabela.sintese.agro,desmatamento)
tabela.sintese.agro <- left_join(tabela.sintese.agro,itr.cota.parte) %>%     
                       select('cod_muni','muni','emprego_familiar','emprego_rais','estabelecimentos','producao_agro','cota_parte_itr','datasus','prod_gado','desmatamento')
                        

tabela.sintese.agro[is.na(tabela.sintese.agro)] <- 0 # transformar N.A em zero

tabela.sintese.agro <- tabela.sintese.agro %>% 
                       mutate(total_agro = emprego_familiar + emprego_rais + estabelecimentos + producao_agro + cota_parte_itr + datasus + prod_gado + desmatamento)

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


