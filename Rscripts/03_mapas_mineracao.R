# Mapas mineração
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# importar dados
mineracao <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_mineral.csv")

# importar shape
hidrovias <- read_sf('Input/shapes logística/hidrovias/Hidrovias.shp') %>% 
             dplyr::filter(cla_icacao %in% c('Navegável', 'Navegação sazonal'))
ferrovias <- read_sf('Input/shapes logística/ferrovias/Ferrovias.shp') %>% 
             dplyr::filter(NOM_FERROV %in% c('FNS-TN','EFC','RMN') &
                           TIP_SITUAC %in% 'Em Operação')

# mapa AMZL por município
shape.muni <- st_read('Outputs/00_shapes_e_dados/shape.muni.amzl.shp')


mineracao <- left_join(mineracao,shape.muni, by = c('cod_muni'='cd_mn')) %>%   
  select('cod_muni','muni','emprego_mineracao','royalties_cfem','mineracao_bndes','infra_mineral',
         'desmatamento_minerac','minerac_ilegal','total_mineral','geometry')

# coordenadas das cidades intermediadoras
coord.cidades <- st_read('Outputs/00_shapes_e_dados/coord.cidades.shp')

sf_use_s2(FALSE)
ferrovias <- st_intersection(ferrovias, shape.muni)
hidrovias <- st_intersection(hidrovias, shape.muni)

# Não está aparecendo o pedaço da ferrovia que passa por Rondonópolis

# 1 - Mapa do somatório dos indicadores
ggplot(mineracao)+
  geom_sf(aes(fill = total_mineral, geometry = geometry), colour = NA) +
  scale_fill_gradientn(colors = brewer.pal(6,"OrRd")) +
  geom_sf(data = ferrovias, aes(col = 'Ferrovias'), size = 0.5, show.legend = 'line') +
  geom_sf(data = hidrovias, aes(col = 'Hidrovias navegáveis'), size = 0.5, show.legend = 'line') +
  scale_colour_discrete("") + # muda o título da legenda
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= 'Classificação de mineração na\nAmazônia Legal', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 9))

# salvar mapa
ggsave('Outputs/03_mapas/Mineração/03_sintese_mineracao.png', scale = 1.1)

# 2 - Mapas dos indicadores separados
x <- c('emprego_mineracao','royalties_cfem','mineracao_bndes',
       'infra_mineral','desmatamento_minerac','minerac_ilegal')
y <- c('Empregos formais na\nmineração','Royalties CFEM','Grandes desembolsos\nBNDES',
       'Infraestrutura de apoio', 'Desmatamento','Mineração ilegal')

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
    geom_sf(aes(fill=!!as.name(x[i]), geometry = geometry), colour = NA) +
    scale_fill_manual(breaks = c('0','1'),
                      values=c('#fee8c8','#e34a33'),
                      label = c('demais faixas','Alto/Muito alto')) +
    #geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = .5) +
    #geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2) +
    labs(fill = y[i], x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
    coord_sf(crs = 4674) +
    annotation_scale(location = 'br')+
    annotation_north_arrow(location = 'tl', 
                           style = north_arrow_fancy_orienteering()) +
    theme_classic() + # retira o grid e coloca o fundo branco
    theme(legend.position = 'bottom')
  
  assign(paste("mapa", i, sep="."), mapa)
  
  i <- i + 1
}

mapa.4 <- mapa.4 + scale_fill_manual(breaks = c('0','1'),
                           values=c('#fee8c8','#e34a33'),
                           label = c('não existe','existe'))

# Fazer os mapas
(mapa.1 | mapa.2)/
(mapa.3 | mapa.5)/
(mapa.4 | mapa.6)
ggsave('Outputs/03_mapas/Mineração/indicadores_juntos_mineral.png', scale = 1.75)



mapa.1
ggsave('Outputs/03_mapas/Mineração/indicadores_mineral_1.png', scale = 1.3)

mapa.2
ggsave('Outputs/03_mapas/Mineração/indicadores_mineral_2.png', scale = 1.3)

mapa.3
ggsave('Outputs/03_mapas/Mineração/indicadores_mineral_3.png', scale = 1.3)

mapa.4
ggsave('Outputs/03_mapas/Mineração/indicadores_mineral_4.png', scale = 1.3)

mapa.5
ggsave('Outputs/03_mapas/Mineração/indicadores_mineral_5.png', scale = 1.3)

mapa.6
ggsave('Outputs/03_mapas/Mineração/indicadores_mineral_6.png', scale = 1.3)