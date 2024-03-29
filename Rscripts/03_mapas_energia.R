# Mapas energia
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

energia <- read.csv(file = "Outputs/02_tabelas/02_subconjunto_energia.csv") 
linhas.de.transmissao <- read_sf('Input/linhasdetransmiss�o/Linha_de_Transimss�o_ONS.shp') %>% 
  dplyr::filter(Situa��o %in% 'Existente')

# mapa AMZL por munic�pio
shape.muni <- st_read('Outputs/00_shapes_e_dados/shape.muni.amzl.shp')
  
energia <- left_join(energia,shape.muni, by = c('cod_muni'='cd_mn')) %>% 
  select(1:9,13)

# coordenadas das cidades intermediadoras
coord.cidades <- st_read('Outputs/00_shapes_e_dados/coord.cidades.shp')
  
# descobrir o crs de cada shape
st_crs(linhas.de.transmissao)
st_crs(shape.muni)

# transformar o crs do shape das linhas de transmiss�o no mesmo da amzl
linhas.de.transmissao <- st_transform(linhas.de.transmissao, crs = 4674)

# remover as linhas de transmiss�o fora da amzl
sf_use_s2(FALSE)
linhas.de.transmissao <- st_intersection(linhas.de.transmissao,shape.muni)
# ap�s a atualiza��o do sf 1.0, precuso usar sf_use_sf2(FALSE)


# 1 - Mapa do somat�rio dos indicadores
ggplot(energia)+
  geom_sf(aes(fill = total_energia, geometry = geometry), colour = NA) +
  scale_fill_gradientn(colors = brewer.pal(6,"BuGn")) +
  geom_sf(data = linhas.de.transmissao, aes(col = 'Linhas de Transmiss�o'), size = 0.7, show.legend = 'line') +
  stat_sf_coordinates(data = coord.cidades) +
  scale_color_manual(values = c('Linhas de Transmiss�o' = '#f03b20'), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid")))) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates",size = .6) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.25, size = 2.2) +
  labs(fill = 'Classifica��o da gera��o de\nenergia el�trica na Amaz�nia Legal', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br') +
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 9))

ggsave('Outputs/03_mapas/Energia/03_sintese_energia.png', scale = 1.4)

# 2 - Mapas dos indicadores separados
x <- c('pont_empregos_energia','pont_royalties_energia_cfh','pont_financ_bndes',
       'existe_UTE','existe_UHE_PCH','pont_desm_bacia')

y <- c('Empregos no setor de gera��o\nde energia el�trica','Royalties de energia\nel�trica - CFH',
       'Grandes projetos apoiados\npelo BNDES','Exist�ncia de UTE', 'Exist�ncia de PCH ou UHE',
       'Desmatamento em regi�o de\nbacia hidrogr�fica')

# transformar de numeric para factor
energia$pont_empregos_energia <- as.factor(energia$pont_empregos_energia)
energia$pont_royalties_energia_cfh <- as.factor(energia$pont_royalties_energia_cfh )
energia$pont_financ_bndes <- as.factor(energia$pont_financ_bndes)
energia$existe_UTE <- as.factor(energia$existe_UTE)
energia$existe_UHE_PCH <- as.factor(energia$existe_UHE_PCH)
energia$pont_desm_bacia <- as.factor(energia$pont_desm_bacia)

i <- 1
while(i<=length(x)){
  mapa <- ggplot(energia)+
    geom_sf(aes(fill=!!as.name(x[i]), geometry = geometry), colour = NA) +
    scale_fill_manual(breaks = c('0','1'),
                      values=c('#e5f5f9','#2ca25f'),
                      label = c('demais faixas','Alto/Muito alto')) +
    # geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = .5) +
    # geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2) +
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


mapa.3 <- mapa.3 + scale_fill_manual(breaks = c('0','1'),
                                     values=c('#e5f5f9','#2ca25f'),
                                     label = c('n�o existe','existe'))

mapa.4 <- mapa.4 + scale_fill_manual(breaks = c('0','1'),
                                     values=c('#e5f5f9','#2ca25f'),
                                     label = c('n�o existe','existe'))

mapa.5 <- mapa.5 + scale_fill_manual(breaks = c('0','1'),
                                     values=c('#e5f5f9','#2ca25f'),
                                     label = c('n�o existe','existe'))

# Fazer os mapas
mapa.1 
ggsave('Outputs/03_mapas/Energia/03_energia_indicadores_1.png', scale = 1.3)

mapa.2
ggsave('Outputs/03_mapas/Energia/03_energia_indicadores_2.png', scale = 1.3)

mapa.3 
ggsave('Outputs/03_mapas/Energia/03_energia_indicadores_3.png', scale = 1.3)

mapa.4
ggsave('Outputs/03_mapas/Energia/03_energia_indicadores_4.png', scale = 1.3)

mapa.5  
ggsave('Outputs/03_mapas/Energia/03_energia_indicadores_5.png', scale = 1.3)

mapa.6
ggsave('Outputs/03_mapas/Energia/03_energia_indicadores_6.png', scale = 1.3)


# indicadores juntos
(mapa.1 | mapa.2)/
(mapa.3 | mapa.5)/
(mapa.4 | mapa.6)
ggsave('Outputs/03_mapas/Energia/indicadores_juntos_energia.png', scale = 1.75)
