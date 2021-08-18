# Mapas energia
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

energia <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_energia.csv") 
linhas.de.transmissao <- read_sf('Input/linhasdetransmissão/Linha_de_Transimssão_ONS.shp') %>% 
  dplyr::filter(Situação %in% 'Existente')

# mapa AMZL por município
shape.muni <- geobr::read_municipality() %>% 
  dplyr::filter(code_muni %in% cidades.amazonia.legal)

energia <- left_join(energia,shape.muni, by = c('cod_muni'='code_muni')) %>% 
  select(2:10,14)

# coordenadas das cidades intermediadoras
coord.cidades <- read_municipal_seat(year = 2010,showProgress = T) %>% 
  dplyr::filter(code_muni %in% cidades.intermediadoras)

# descobrir o crs de cada shape
st_crs(linhas.de.transmissao)
st_crs(shape.muni)
# transformar o crs do shape das linhas de transmissão no mesmo da amzl
linhas.de.transmissao <- st_transform(linhas.de.transmissao, crs = 4674)

# remover as linhas de transmissão fora da amzl
linhas.de.transmissao <- st_intersection(linhas.de.transmissao,shape.muni)



# empregos_energia_rais
# royalties_energia_cfh
# energia_bndes
# existe_UTE
# existe_UHE_PCH
# desmatamento_bacias
# total_energia

ggplot(energia)+
  geom_sf(aes(fill = total_energia, geometry = geom), colour = NA) +
  scale_fill_gradientn(colors = brewer.pal(6,"BuGn")) +
  geom_sf(data = linhas.de.transmissao) +
  geom_point(data = coord.cidades, aes(geometry = geom), stat = "sf_coordinates",size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = name_muni), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill = 'Classificação do setor de geração de energia na AMZL') + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br') +
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')


