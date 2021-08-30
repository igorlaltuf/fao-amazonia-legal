# Classificação das intermediadoras segundo os indicadores
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# importar dados
mineracao <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_mineral.csv")
energia <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_energia.csv") 
agro <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_agro.csv") 


mineracao <- mineracao %>% 
    dplyr::filter(cod_muni %in% cidades.intermediadoras &
                  total_mineral >= 5)


agro <- agro %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras)

energia <- energia %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras)

# comparar com todas as cidades do subconjunto agromineral energético na planilha

# Somar dados da intermediadoras

# fazer mapa da regic













####### CALCULAR!!!!


st_coordinates(coord.cidades) # retorna as coordenadas do objeto sf
geom_point(data = coord.cidades,aes(geometry = geom), stat = "sf_coordinates") # retornar as coordenadas de objeto sf



# incluir os shapes de portos, ferrovias e rodovias

# correlation matriz heatmap (fazer com dados)
# ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()



# Fazer tabelão das intermediadoras!!!


# Cidades intermediadoras
tabela.sintese.agro.intermed <- tabela.sintese.agro %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras &
                  total_agro > 0) %>% 
  arrange(desc(total_agro))

#tabela
# Incluir desmatamento e boi na tabela final.

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





