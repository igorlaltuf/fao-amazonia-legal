# Minera��o Ilegal 
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

y <- read_sf('Input/MineriaIlegal2020/MineriaIlegal_pol.shp') # pontos de minera��o ilegal
x <- read_sf('Input/MineriaIlegal2020/MineriaIlegal_ln.shp') # minera��o em rios
z <- read_sf('Input/MineriaIlegal2020/MineriaIlegal_pt.shp') # �reas de minera��o ilegal

shape.muni <- geobr::read_municipality() %>% 
  dplyr::filter(code_muni %in% cidades.amazonia.legal)

# todos t�m o mesmo CRS
st_crs(x)
st_crs(y)
st_crs(z)
st_crs(shape.muni)

# 1 - pontos de minera��o 
sf_use_s2(F)
df1 <- st_intersection(shape.muni,st_buffer(y, 0))

df.pontos <- df1 %>% 
  dplyr::filter(pa�s == 'Brasil')

# 2 - rios com minera��o 
df2 <- st_intersection(shape.muni, x)

df.rios <- df2 %>% 
  dplyr::filter(pa�s == 'Brasil')


# 3 - �reas com minera��o
df3 <- st_intersection(shape.muni,z) 

df.areas <- df3 %>% 
  dplyr::filter(pa�s == 'Brasil')


# juntar os c�digos de munic�pios que cont�m minera��o ilegal nas tr�s categorias acima
df.pontos <- df.pontos %>% 
  st_drop_geometry() %>% 
  select(1) %>% 
  unique()
  
df.rios <- df.rios %>% 
  st_drop_geometry() %>% 
  select(1) %>% 
  unique()

df.areas <- df.areas%>% 
  st_drop_geometry() %>% 
  select(1) %>% 
  unique()


mineracao.ilegal <- df.pontos %>% 
  rbind(df.rios) %>% 
  rbind(df.areas) %>% 
  unique() %>% 
  left_join(cidades.amazonia.legal.nome, by=c('code_muni'='cod_muni')) %>% 
  mutate(minerac_ilegal = 1)

# exportar tabela
write.csv(mineracao.ilegal,'Outputs/01_tabelas/01_minerac_ilegal.csv', row.names = F)

# exportar shapefile da minera��o ilegal
shape.mineracao.ilegal <- left_join(mineracao.ilegal,shape.muni,by=c('code_muni')) %>% 
  select(-'name_muni')

st_write(shape.mineracao.ilegal, "Outputs/03_mapas/Shapefile minera��o ilegal/shape_minerac_ilegal.shp")
