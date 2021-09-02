# Shapes
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Municípios Brasil
shape.muni <- geobr::read_municipality() 
colnames(shape.muni) <- c('cd_mn','mn','cd_uf','uf','geom')
st_geometry(shape.muni) <- "geom"
st_write(shape.muni, 'Outputs/00_shapes_e_dados/shape.muni.shp',append = F)

# Municípios AMZL
shape.muni.amzl <- shape.muni %>% 
  dplyr::filter(cd_mn %in% cidades.amazonia.legal)
st_write(shape.muni.amzl,'Outputs/00_shapes_e_dados/shape.muni.amzl.shp',append = F)

# Estados Brasil
shape.estad <- geobr::read_state()
colnames(shape.estad) <- c('cd_uf','uf','nm_uf','cd_reg','nm_reg','geom')
st_geometry(shape.estad) <- "geom"
st_write(shape.estad, 'Outputs/00_shapes_e_dados/shape.estad.shp', append = F)

# Estados AMZL
shape.estad.amzl <- shape.estad %>% 
  dplyr::filter(uf %in% uf.amz.legal)
st_write(shape.estad.amzl, 'Outputs/00_shapes_e_dados/shape.estad.amzl.shp', append = F)

# Cidades intermediadoras - Pontos 
coord.cidades <- read_municipal_seat(year = 2010, showProgress = T) %>% 
  dplyr::filter(code_muni %in% cidades.intermediadoras)
colnames(coord.cidades) <- c('cd_mn','mn','cd_uf','uf','cd_reg','nm_reg','ano','geom')
st_geometry(coord.cidades) <- "geom"
st_write(coord.cidades, 'Outputs/00_shapes_e_dados/coord.cidades.shp', append = F)

# Escolas Brasil - INEP 2020 Censo Escolar
ponto.escolas <- read_schools(year = 2020, showProgress = TRUE) %>%   
  select(1:3,10,19)
colnames(ponto.escolas) <- c('uf','mn','id_esc','tipo','geom')
st_geometry(ponto.escolas) <- "geom"
st_write(ponto.escolas, 'Outputs/00_shapes_e_dados/shape.escol.brasil.shp', append = F)

# Escolas AMZL - INEP 2020 Censo Escolar
ponto.escolas.amzl <- ponto.escolas %>% 
  dplyr::filter(uf %in% uf.amz.legal)
st_write(ponto.escolas.amzl, 'Outputs/00_shapes_e_dados/shape.escol.amzl.shp', append = F)

# Estabelecimentos de Saúde Brasil - CNES
pontos.cnes <- read_health_facilities(showProgress = T) %>% 
  select(1:4,8)
colnames(pontos.cnes) <- c('cd_cnes','cd_mn','cd_uf','uf','geom')
st_geometry(pontos.cnes) <- "geom"
st_write(pontos.cnes, 'Outputs/00_shapes_e_dados/shape.cnes.brasil.shp', append = F)

# Estabelecimentos de Saúde AMZL - CNES
pontos.cnes.amzl <- pontos.cnes %>%
  dplyr::filter(uf %in% uf.amz.legal)
st_write(pontos.cnes.amzl, 'Outputs/00_shapes_e_dados/shape.cnes.amzl.shp', append = F)