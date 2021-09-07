# Mineração Ilegal 
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

y <- read_sf('Input/MineriaIlegal2020/MineriaIlegal_pol.shp') # pontos de mineração ilegal
x <- read_sf('Input/MineriaIlegal2020/MineriaIlegal_ln.shp') # mineração em rios
z <- read_sf('Input/MineriaIlegal2020/MineriaIlegal_pt.shp') # áreas de mineração ilegal

shape.muni <- geobr::read_municipality() %>% 
  dplyr::filter(code_muni %in% cidades.amazonia.legal)

# todos têm o mesmo CRS
st_crs(x)
st_crs(y)
st_crs(z)
st_crs(shape.muni)

# 1 - pontos de mineração 
sf_use_s2(F)
df1 <- st_intersection(shape.muni,st_buffer(y, 0))

df.pontos <- df1 %>% 
  dplyr::filter(país == 'Brasil')

# 2 - rios com mineração 
df2 <- st_intersection(shape.muni, x)

df.rios <- df2 %>% 
  dplyr::filter(país == 'Brasil')


# 3 - áreas com mineração
df3 <- st_intersection(shape.muni,z) 

df.areas <- df3 %>% 
  dplyr::filter(país == 'Brasil')


# juntar os códigos de municípios que contém mineração ilegal nas três categorias acima
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

# exportar shapefile da mineração ilegal
shape.mineracao.ilegal <- left_join(mineracao.ilegal,shape.muni,by=c('code_muni')) %>% 
  select(-'name_muni')

st_write(shape.mineracao.ilegal, "Outputs/03_mapas/Shapefile mineração ilegal/shape_minerac_ilegal.shp")
