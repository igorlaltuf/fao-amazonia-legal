# Frigoríficos
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Empregos formais - Vínculos ativos em 2019 - RAIS
cnae.cidades <- read_csv('Outputs/00_shapes_e_dados/00_rais_ativos_2019.csv',
                         col_types = list(
                           id_municipio = 'n',
                           ano = 'n',
                           cnae_2 = 'c',
                           vinculos_ativos = 'n'))

which(is.na(cnae.cidades$vinculos_ativos)) # retorna a linha dos NAs caso existam
cnae.cidades$id_municipio <- as.numeric(cnae.cidades$id_municipio)

# Empregos relacionados ao abate e a fabricação de produtos de carne
cnae.frigo <- cnae.cidades %>% 
  select(id_municipio,cnae_2,vinculos_ativos) %>% 
  filter(str_detect(cnae_2, "^101")) %>%  
  group_by(id_municipio) %>% 
  summarise(empregos_frigo = sum(vinculos_ativos, na.rm = TRUE))

cnae.frigo$empregos_frigo <- as.numeric(cnae.frigo$empregos_frigo)

cnae.frigo <- left_join(cidades.amazonia.legal.nome,cnae.frigo, by = c('cod_muni'='id_municipio')) %>%  
  dplyr::filter(empregos_frigo>0 &
                  cod_muni %in% cidades.amazonia.legal) %>% 
  arrange(desc(empregos_frigo))

# Classifica empregos de petróleo 
cnae.frigo <- classificar.variavel(cnae.frigo,'empregos_frigo','class_empregos_frigo')

x <- cnae.frigo %>% 
  group_by(class_empregos_frigo) %>%
  mutate(N_category = n()) %>%
  count(N_category)


# Mapa
cnae.frigo$class_empregos_frigo <- as.factor(cnae.frigo$class_empregos_frigo)
coord.cidades <- st_read('Outputs/00_shapes_e_dados/coord.cidades.shp')
shape.muni.amzl <- st_read('Outputs/00_shapes_e_dados/shape.muni.amzl.shp')

cnae.frigo <- left_join(shape.muni.amzl, cnae.frigo, by = c('cd_mn'='cod_muni')) %>% 
  select(1,5:8)

st_geometry(cnae.frigo)


########### CONTINUAR DAQUI!
ggplot(cnae.frigo)+
  geom_sf(aes(fill = class_empregos_frigo), geometry = geometry, colour = NA) +
  scale_fill_manual(breaks = c('0','1'),
                    values=c('#e5f5f9','#2ca25f'),
                    label = c('demais faixas','Alto/Muito alto')) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = .5) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2) +
  labs(fill = y[i], x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')