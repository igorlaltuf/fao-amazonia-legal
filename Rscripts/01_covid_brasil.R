# COVID Brasil
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

covid <- read.csv('Input/caso_full.csv',fileEncoding = 'UTF-8')

covid <- covid %>% 
  dplyr::select(city_ibge_code,city,place_type,state,date,last_available_deaths,estimated_population) %>% 
  dplyr::filter(place_type == 'city' &
                  date == '2021-08-12') %>% 
  mutate(obitos_100_mil_ha = (last_available_deaths/estimated_population)*100000) %>% 
  na.omit()

covid <- classificar.variavel(covid,'obitos_100_mil_ha','class_obit_100_mil_ha')

intermed <- covid %>% 
  dplyr::filter(city_ibge_code %in% cidades.intermediadoras) %>% 
  arrange(desc(obitos_100_mil_ha))

x <- covid %>% 
  group_by(class_obit_100_mil_ha) %>%
  mutate(N_category = n()) %>%
  count(N_category)

mean(covid$obitos_100_mil_ha)

# Tabela das cidades intermediadoras e classificação nacional
tabela.covid <- gt(intermed) %>%
  cols_label(
    city = 'Município',
    last_available_deaths = 'Quantidade de óbitos',
    obitos_100_mil_ha = 'Quantidade de óbitos a cada 100 mil habitantes',
    class_obit_100_mil_ha = 'Classificação'
  ) %>% 
  cols_hide(
    columns = c('city_ibge_code','date','estimated_population','place_type','state')
  ) %>% 
  tab_header(
    title = 'Mortalidade por Covid-19 nas cidades intermediárias da Amazônia Legal',
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

gtsave(tabela.covid, 'Outputs/03_mapas/Saúde/Covid_intermediadoras_classif_brasil.png')

write.csv(covid,'Outputs/03_mapas/Saúde/covid_classificacao_brasil.csv')

mean(covid$obitos_100_mil_ha)

# mapa Brasil por município
shape.muni <- st_read('Outputs/00_shapes_e_dados/shape.muni.shp')

covid.shape <- left_join(covid, shape.muni, by = c('city_ibge_code' = 'cd_mn'))

# transforma character em factors
covid.shape$class_obit_100_mil_ha <- as.factor(covid.shape$class_obit_100_mil_ha)

# define a ordem dos factors (em 6 levels)
covid.shape$class_obit_100_mil_ha <- factor(covid.shape$class_obit_100_mil_ha, levels = c('Muito Alto','Alto','Médio Alto','Médio Baixo','Baixo','Muito Baixo'))

# coord dos pontos
coord.cidades <- st_read('Outputs/00_shapes_e_dados/coord.cidades.shp')

# Brasil
# óbitos a cada 100 mil hab
ggplot(covid.shape)+
  geom_sf(aes(fill=class_obit_100_mil_ha, geometry = geometry), colour = NA)+
  scale_fill_manual(values = rev(brewer.pal(6,"BuPu")))+
  labs(fill= 'Classificação dos óbitos a \n cada 100 mil habitantes', y=NULL, x=NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_classic()+ # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')

ggsave('Outputs/03_mapas/Saúde/covid_obitos_brasil.png', width = 9, height = 6)