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

gtsave(tabela.covid, 'Outputs/Covid_intermediadoras.png')

write.csv(covid,'Outputs/covid_classificacao.csv')

mean(covid$obitos_100_mil_ha)

# mapa Brasil por município
shape.muni <- geobr::read_municipality() 
shape.muni <- left_join(covid,shape.muni, by = c('city_ibge_code'='code_muni'))

# transforma character em factors
shape.muni$class_obit_100_mil_ha <- as.factor(shape.muni$class_obit_100_mil_ha)

# define a ordem dos factors (em 6 levels)
shape.muni$class_obit_100_mil_ha <- factor(shape.muni$class_obit_100_mil_ha, levels = c('Muito Alto','Alto','Médio Alto','Médio Baixo','Baixo','Muito Baixo'))

# coord dos pontos
coord.cidades <- read_municipal_seat(year = 2010,showProgress = T) %>% 
  dplyr::filter(code_muni %in% cidades.intermediadoras)

# recorte da amzl com classificação do Brasil # REMOVER PARA VER O MAPA DO BRASIL INTEIRO
shape.muni <- shape.muni %>% 
              dplyr::filter(city_ibge_code %in% cidades.amazonia.legal)


# óbitos a cada 100 mil hab
ggplot(shape.muni)+
  geom_sf(aes(fill=class_obit_100_mil_ha, geometry = geom), colour = NA)+
  scale_fill_manual(values = rev(brewer.pal(6,"BuPu")))+
  geom_point(data = coord.cidades, aes(geometry = geom), stat = "sf_coordinates")+
  geom_sf_text(data = coord.cidades, aes(label = name_muni), colour='grey10',vjust=1.3, size = 1.8) +
  labs(fill= 'Classificação dos óbitos a cada 100 mil habitantes') + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_classic()+ # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')


ggsave('Outputs/covid_nacional.png', width = 9, height = 6)


# estabelecimentos de saúde
cnes <- read_health_facilities()
cnes <- cnes %>% 
  dplyr::filter(abbrev_state %in% uf.amz.legal)


ggplot(shape.muni)+
  geom_sf(aes(fill=class_obit_100_mil_ha, geometry = geom), colour = NA)+
  scale_fill_manual(values = rev(brewer.pal(6,"BuPu")))+
  geom_point(data = cnes, aes(geometry = geom), stat = "sf_coordinates", size = .05)+
  labs(fill= 'Classificação dos óbitos a cada 100 mil habitantes') + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_classic()+ # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')











# óbitos (desenvolver mais)
ggplot(shape.muni)+
  geom_sf(aes(fill=last_available_deaths/1000, geometry = geom), colour = NA)+
  scale_fill_gradientn(colours = brewer.pal(10,"YlOrRd"))+
  # scale_fill_manual(values = rev(brewer.pal(6,"BuPu")))+
  labs(fill= 'Classificação dos óbitos a cada 100 mil habitantes') + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_classic()+ # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')






