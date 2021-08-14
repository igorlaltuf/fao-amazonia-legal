# COVID AMZL
# valores batem com os valores do site para o dia 12-08-2021 (inclusive óbitos a cada 100 mil hab)

rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

covid <- read.csv('Input/caso_full.csv',fileEncoding = 'UTF-8')

covid.amzl <- covid %>% 
  dplyr::select(city_ibge_code,city,date,last_available_deaths,estimated_population) %>% 
  dplyr::filter(city_ibge_code %in% cidades.amazonia.legal &
                date == '2021-08-12') %>% 
  mutate(obitos_100_mil_ha = (last_available_deaths/estimated_population)*100000)

covid.amzl <- classificar.variavel(covid.amzl,'obitos_100_mil_ha','class_obit_100_mil_ha')


intermed <- covid.amzl %>% 
            dplyr::filter(city_ibge_code %in% cidades.intermediadoras) %>% 
            arrange(desc(obitos_100_mil_ha))
  
  x <- covid.amzl %>% 
  group_by(class_obit_100_mil_ha) %>%
  mutate(N_category = n()) %>%
  count(N_category)
  

  
  
  
tabela.covid <- gt(intermed) %>%
    cols_label(
      city = 'Município',
      last_available_deaths = 'Quantidade de óbitos',
      obitos_100_mil_ha = 'Quantidade de óbitos a cada 100 mil habitantes',
      class_obit_100_mil_ha = 'Classificação'
    ) %>% 
    cols_hide(
      columns = c('city_ibge_code','date','estimated_population')
    ) %>% 
    tab_header(
      title = 'Mortalidade do COVID-19 na Amazônia Legal',
      subtitle = 'Entre 17/03/2020 e 12/08/2021'
    ) %>%
    fmt_markdown(
      columns = c(city,class_obit_100_mil_ha)
    ) %>% 
    fmt_number(
      columns = c(last_available_deaths,obitos_100_mil_ha),
      decimals = 0,
      sep_mark = '.',
      dec_mark = ','
    ) %>% 
    cols_align(
      align = 'center'
    ) %>% 
    tab_source_note('Fonte: Elaborado por Igor Laltuf com base nos dados disponíveis no dia 13 de agosto de 2021 de COVID-19 (AJ et al.): agregador desenvolvido por Álvaro Justen e colaboradores (https://brasil.io/dataset/covid19/boletim).')
  
tabela.covid

gtsave(tabela.covid, 'Outputs/Covid_intermediadoras.png')


# mapa AMZL
shape.muni.amzl <- geobr::read_municipality() %>% 
  dplyr::filter(code_muni %in% cidades.amazonia.legal)


shape.muni.amzl <- left_join(covid.amzl,shape.muni.amzl, by = c('city_ibge_code'='code_muni'))

ggplot(shape.muni.amzl)+
  geom_sf(aes(fill=class_obit_100_mil_ha, geometry = geom))+
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')
  
# fazer mapa para amzl

Filtrar mapas de muni da amzl pelas cidades da amzl
E destacar as intermediadoras



