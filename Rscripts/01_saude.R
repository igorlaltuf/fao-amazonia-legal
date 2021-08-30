# Dados de Saúde
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')


# estabelecimentos de saúde 
# baixar dados de internações pelo SIH e cruzar com geobr
# https://github.com/rfsaldanha/microdatasus

Cruzar dados abaixo com tipo de estabelecimento de saúde do datasus para sber onde estão os hospitais, upas etc


# corrigir no script de shape (o filtro lá resultou em zero objetos no shape)
cnes.amzl <- st_read('Outputs/00_shapes_e_dados/shape.cnes.amzl.shp')

shape.estad.amzl <- st_read('Outputs/00_shapes_e_dados/shape.estad.amzl.shp')

ggplot(shape.estad.amzl) +
  geom_sf(aes(geometry = geometry)) +
  #geom_sf(data = shape.hidrovia, aes(col = 'Hidrovias navegáveis'), size = 0.5, show.legend = 'line') +
  geom_point(data = cnes.amzl, aes(geometry = geometry, col = 'CNES'), stat = "sf_coordinates", size = .05, show.legend = 'point') +
  scale_colour_discrete("") + # muda o título da legenda (remove o color)
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')
















# 
# 
# ggplot(shape.muni)+
#   geom_sf(aes(fill=class_obit_100_mil_ha, geometry = geom), colour = NA)+
#   scale_fill_manual(values = rev(brewer.pal(6,"BuPu")))+
#   geom_point(data = cnes, aes(geometry = geom), stat = "sf_coordinates", size = .05)+
#   labs(fill= 'Classificação dos óbitos a cada 100 mil habitantes') + #Muda o nome da legenda com o fill.
#   coord_sf(crs = 4674) +
#   annotation_scale(location = 'br')+
#   annotation_north_arrow(location='tl', 
#                          style = north_arrow_fancy_orienteering())+
#   theme_classic()+ # retira o grid e coloca o fundo branco
#   theme(legend.position = 'bottom')

