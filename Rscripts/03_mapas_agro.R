# Mapas agro
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

agro <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_agro.csv") 

# mapa AMZL por município
shape.muni <- geobr::read_municipality() %>% 
              dplyr::filter(code_muni %in% cidades.amazonia.legal)

agro <- left_join(agro,shape.muni, by = c('cod_muni'='code_muni')) %>% 
        select(2:11,15)


# coordenadas das cidades intermediadoras
coord.cidades <- read_municipal_seat(year = 2010,showProgress = T) %>% 
                 dplyr::filter(code_muni %in% cidades.intermediadoras)


# emprego_rais
# estabelecimentos
# graos_gado
# cota_parte_itr
# datasus
# desmatamento
# infra_agro
# total_agro


# incluir os shapes de portos, ferrovias e rodovias


ggplot(agro)+
  geom_sf(aes(fill = total_agro, geometry = geom), colour = NA)+
  stat_sf_coordinates(data = coord.cidades) +
  geom_point(data = coord.cidades, aes(geometry = geom), stat = "sf_coordinates", size = 1)+
  geom_sf_text(data = coord.cidades, aes(label = name_muni), colour='grey10',vjust=1.3, size = 2.7) +
  scale_fill_gradientn(colors = brewer.pal(7,"BuPu"))+
  labs(fill= 'Classificação da agropecuária na AMZL') + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')



st_coordinates(coord.cidades) # retorna as coordenadas do objeto sf
geom_point(data = coord.cidades,aes(geometry = geom), stat = "sf_coordinates") # retornar as coordenadas de objeto sf

# correlation matriz heatmap (fazer com dados)
# ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()

