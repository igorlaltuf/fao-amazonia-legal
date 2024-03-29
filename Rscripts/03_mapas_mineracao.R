# Mapas minera��o
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# importar dados
mineracao <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_mineral.csv")

# importar shape
hidrovias <- read_sf('Input/shapes log�stica/hidrovias/Hidrovias.shp') %>% 
             dplyr::filter(cla_icacao %in% c('Naveg�vel', 'Navega��o sazonal'))
ferrovias <- read_sf('Input/shapes log�stica/ferrovias/Ferrovias.shp') %>% 
             dplyr::filter(NOM_FERROV %in% c('FNS-TN','EFC','RMN') &
                           TIP_SITUAC %in% 'Em Opera��o')

# mapa AMZL por munic�pio
shape.muni <- st_read('Outputs/00_shapes_e_dados/shape.muni.amzl.shp')


mineracao <- left_join(mineracao,shape.muni, by = c('cod_muni'='cd_mn')) %>%   
  select('cod_muni','muni','emprego_mineracao','pont_cfem','mineracao_bndes','infra_mineral',
         'desmat_minerac','minerac_ilegal','total_mineral','geometry')

# coordenadas das cidades intermediadoras
coord.cidades <- st_read('Outputs/00_shapes_e_dados/coord.cidades.shp')

sf_use_s2(FALSE)
ferrovias <- st_intersection(ferrovias, shape.muni)
hidrovias <- st_intersection(hidrovias, shape.muni)

# N�o est� aparecendo o peda�o da ferrovia que passa por Rondon�polis

# 1 - Mapa do somat�rio dos indicadores
ggplot(mineracao)+
  geom_sf(aes(fill = total_mineral, geometry = geometry), colour = NA) +
  scale_fill_gradientn(colors = brewer.pal(6,"OrRd")) +
  geom_sf(data = ferrovias, aes(col = 'Ferrovias'), size = 0.7, show.legend = 'line') +
  geom_sf(data = hidrovias, aes(col = 'Hidrovias naveg�veis'), size = 0.7, show.legend = 'line') +
  scale_color_manual(values = c('Ferrovias' = '#636363', 'Hidrovias naveg�veis' = '#43a2ca'), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid", "solid")))) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = .8) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10', vjust=1.25, size = 2.4) +
  labs(fill= 'Classifica��o de minera��o na\nAmaz�nia Legal', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 9))

# salvar mapa
ggsave('Outputs/03_mapas/Minera��o/03_sintese_mineracao.png', scale = 1.4)

# 2 - Mapas dos indicadores separados
x <- c('emprego_mineracao','royalties_cfem','mineracao_bndes',
       'infra_mineral','desmatamento_minerac','minerac_ilegal')
y <- c('Empregos formais na\nminera��o','Royalties CFEM','Grandes desembolsos\nBNDES',
       'Infraestrutura de apoio', 'Desmatamento','Minera��o ilegal')

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
                           label = c('n�o existe','existe'))

# Fazer os mapas
(mapa.1 | mapa.2)/
(mapa.3 | mapa.5)/
(mapa.4 | mapa.6)
ggsave('Outputs/03_mapas/Minera��o/indicadores_juntos_mineral.png', scale = 1.75)



mapa.1
ggsave('Outputs/03_mapas/Minera��o/indicadores_mineral_1.png', scale = 1.3)

mapa.2
ggsave('Outputs/03_mapas/Minera��o/indicadores_mineral_2.png', scale = 1.3)

mapa.3
ggsave('Outputs/03_mapas/Minera��o/indicadores_mineral_3.png', scale = 1.3)

mapa.4
ggsave('Outputs/03_mapas/Minera��o/indicadores_mineral_4.png', scale = 1.3)

mapa.5
ggsave('Outputs/03_mapas/Minera��o/indicadores_mineral_5.png', scale = 1.3)

mapa.6
ggsave('Outputs/03_mapas/Minera��o/indicadores_mineral_6.png', scale = 1.3)

x <- read.csv('Outputs/02_tabelas/02_subconjunto_mineral.csv')
