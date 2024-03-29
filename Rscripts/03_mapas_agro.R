# Mapas agro
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# importar dados
agro <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_agro_pontos.csv") 

# importar shapes
hidrovias <- read_sf('Input/shapes log�stica/hidrovias/Hidrovias.shp') %>% 
  dplyr::filter(cla_icacao %in% c('Naveg�vel', 'Navega��o sazonal'))
ferrovias <- read_sf('Input/shapes log�stica/ferrovias/Ferrovias.shp') %>% 
  dplyr::filter(NOM_FERROV %in% c('FNS-TN','EFC','RMN') &
                  TIP_SITUAC %in% 'Em Opera��o')

# mapa AMZL por munic�pio
shape.muni <- st_read('Outputs/00_shapes_e_dados/shape.muni.amzl.shp')
  
agro <- left_join(agro, shape.muni, by = c('cod_muni' = 'cd_mn')) %>%   
        select(1:9,13)

# coordenadas das cidades intermediadoras
coord.cidades <- st_read('Outputs/00_shapes_e_dados/coord.cidades.shp')

sf_use_s2(FALSE) # usar sempre antes do st_intersection ap�s a vers�o 1.0 do sf
ferrovias <- st_intersection(ferrovias, shape.muni)
hidrovias <- st_intersection(hidrovias, shape.muni)

# 1 - Mapa do somat�rio dos indicadores
ggplot(agro)+
  geom_sf(aes(fill = total_agro, geometry = geometry), colour = NA)+
  scale_fill_gradientn(colors = brewer.pal(6,"YlOrBr"))+
  geom_sf(data = ferrovias, aes(col = 'Ferrovias'), size = 0.7, show.legend = 'line') +
  geom_sf(data = hidrovias, aes(col = 'Hidrovias naveg�veis'), size = 0.7, show.legend = 'line') +
  scale_color_manual(values = c('Ferrovias' = '#636363', 'Hidrovias naveg�veis' = '#43a2ca'), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid", "solid")))) +
  stat_sf_coordinates(data = coord.cidades) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = .8)+
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10', vjust = 1.25, size = 2.6) +
  labs(fill = 'Classifica��o da agropecu�ria\nna Amaz�nia Legal', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering())+
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 9))

ggsave('Outputs/03_mapas/Agropecu�ria/03_sintese_agro.png', scale = 1.4)



# 2 - Mapas dos indicadores separados (n�o foram usados no relat�rio final)
x <- c('pont_emprego_agro','pont_concentr_terra','pont_graos_gado',
       'pont_itr','pont_desmat','pont_infra_agro')

y <- c('Empregos formais\nda agropecu�ria','M�dios e grandes\nestabelecimentos agropecu�rios',
       'Valor da produ��o de\ngr�os/cabe�as de gado','Cota parte ITR',
       'Desmatamento','Infraestrutura log�stica')

# transformar de numeric para factor
agro$pont_emprego_agro <- as.factor(agro$pont_emprego_agro)
agro$pont_concentr_terra <- as.factor(agro$pont_concentr_terra )
agro$pont_graos_gado <- as.factor(agro$pont_graos_gado)
agro$pont_itr <- as.factor(agro$pont_itr)
agro$pont_desmat <- as.factor(agro$pont_desmat)
agro$pont_infra_agro <- as.factor(agro$pont_infra_agro)

i <- 1
while(i<=length(x)){
  mapa <- ggplot(agro)+
    geom_sf(aes(fill=!!as.name(x[i]), geometry = geometry), colour = NA) +
    scale_fill_manual(breaks = c('0','1'),
                      values=c('#e0ecf4','#8856a7'),
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
  
  assign(paste("mapa", i, sep="."),mapa)
  
  i <- i + 1
}

# Fazer os mapas
mapa.1
ggsave('Outputs/03_mapas/Agropecu�ria/03_agro_indicadores_1.png')

mapa.2
ggsave('Outputs/03_mapas/Agropecu�ria/03_agro_indicadores_2.png')

mapa.3
ggsave('Outputs/03_mapas/Agropecu�ria/03_agro_indicadores_3.png')

mapa.4
ggsave('Outputs/03_mapas/Agropecu�ria/03_agro_indicadores_4.png')

mapa.5
ggsave('Outputs/03_mapas/Agropecu�ria/03_agro_indicadores_5.png')

mapa.6 <- mapa.6 + scale_fill_manual(breaks = c('0','1'),
                           values=c('#e0ecf4','#8856a7'),
                           label = c('n�o existe','existe'))

ggsave('Outputs/03_mapas/Agropecu�ria/03_agro_indicadores_6.png')

# indicadores juntos
(mapa.1 | mapa.2)/
(mapa.3 | mapa.5)/
(mapa.4 | mapa.6)
ggsave('Outputs/03_mapas/Agropecu�ria/indicadores_juntos_agro.png', scale = 1.75)
