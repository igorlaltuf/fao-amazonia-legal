# Mapas mineração
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

mineracao <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_mineral.csv")
hidrovias <- read_sf('Input/shapes logística/hidrovias/Hidrovias.shp') %>% 
             dplyr::filter(cla_icacao %in% c('Navegável', 'Navegação sazonal'))
ferrovias <- read_sf('Input/shapes logística/ferrovias/Ferrovias.shp') %>% 
             dplyr::filter(NOM_FERROV %in% c('FNS-TN','EFC','RMN') &
                           TIP_SITUAC %in% 'Em Operação')

# mapa AMZL por município
shape.muni <- geobr::read_municipality() %>% 
  dplyr::filter(code_muni %in% cidades.amazonia.legal)

mineracao <- left_join(mineracao,shape.muni, by = c('cod_muni'='code_muni')) %>% 
  select('cod_muni','muni','emprego_mineracao','royalties_cfem','mineracao_bndes','infra_mineral',
         'desmatamento_minerac','minerac_ilegal','total_mineral','geom')

# coordenadas das cidades intermediadoras
coord.cidades <- read_municipal_seat(year = 2010,showProgress = T) %>% 
  dplyr::filter(code_muni %in% cidades.intermediadoras)
   
ferrovias <- st_intersection(ferrovias,shape.muni)
hidrovias <- st_intersection(hidrovias,shape.muni)

# Não está aparecendo o pedaço da ferrovia que passa por Rondonópolis

# 1 - Mapa do somatório dos indicadores
ggplot(mineracao)+
  geom_sf(aes(fill = total_mineral, geometry = geom), colour = NA) +
  scale_fill_gradientn(colors = brewer.pal(6,"OrRd")) +
  geom_sf(data = ferrovias, colour='blue') +
  geom_sf(data = hidrovias, colour='red') +
  geom_point(data = coord.cidades, aes(geometry = geom), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = name_muni), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= 'Indicadores acumulados de mineração na AMZL', x = 'Lng', y = 'Lat') + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')


# salvar mapa


# 2 - Mapas dos indicadores separados
x <- c('emprego_mineracao','royalties_cfem','mineracao_bndes',
       'infra_mineral','desmatamento_minerac','minerac_ilegal')
y <- c('Empregos formais na mineração','Royalties CFEM','Grandes desembolsos BNDES',
       'Infraestrutura de apoio', 'Desmatamento','Mineração Ilegal')

# transformar de numeric para factor
mineracao$emprego_mineracao <- as.factor(mineracao$emprego_mineracao)
mineracao$royalties_cfem <- as.factor(mineracao$royalties_cfem )
mineracao$mineracao_bndes <- as.factor(mineracao$mineracao_bndes)
mineracao$infra_mineral <- as.factor(mineracao$infra_mineral)
mineracao$desmatamento_minerac <- as.factor(mineracao$desmatamento_minerac)
mineracao$minerac_ilegal <- as.factor(mineracao$minerac_ilegal)

i <- 1

while(i<=length(x)){
  mapa <- ggplot(mineracao)+
    geom_sf(aes(fill=!!as.name(x[i]), geometry = geom), colour = NA) +
    scale_fill_manual(breaks = c('0','1'),
                      values=c('#fee8c8','#e34a33'),
                      label = c('demais valores','Alto/Muito alto')) +
    geom_sf(data = ferrovias, colour='blue') +
    geom_sf(data = hidrovias, colour='red') +
    geom_point(data = coord.cidades, aes(geometry = geom), stat = "sf_coordinates", size = .5) +
    labs(fill = y[i], x = 'Lng', y = 'Lat') + #Muda o nome da legenda com o fill.
    coord_sf(crs = 4674) +
    annotation_scale(location = 'br')+
    annotation_north_arrow(location = 'tl', 
                           style = north_arrow_fancy_orienteering()) +
    theme_classic() + # retira o grid e coloca o fundo branco
    theme(legend.position = 'bottom')
  
  assign(paste("mapa", i, sep="."),mapa)
  
  i <- i + 1
}

# Fazer os mapas
mapa.1 + mapa.2 
mapa.3 + mapa.4
mapa.5 
mapa.6

# salvar mapas