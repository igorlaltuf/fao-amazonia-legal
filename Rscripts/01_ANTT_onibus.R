rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Bases de 22-08-2019 e 01-08-2021.
# Totalmunic = nº total de municípios em que há parada na linha.
# Secoesmunic = nº de municípios em que há seções (vendas de bilhetes) na linha.

shape.estad.amzl <- st_read('Outputs/00_shapes_e_dados/shape.estad.amzl.shp') 


# substitui por zero quando não havia informações do prefixo naquele ano
dados <- read_excel('Input/shapes ANTT rodovias/dados/viagensPorAnoPorPrefixo.xlsx', sheet=4)

shape.itinerario.2019 <- st_read('Input/shapes ANTT rodovias/20190822/20190822itinerario.shp')
shape.itinerario.2019 <- left_join(shape.itinerario.2019,dados)

# acima de 3637 viagens ao ano (aprox 10 ao dia)
viagens1 <- shape.itinerario.2019 %>% 
  dplyr::filter(dados_2019 > 3637)

ggplot(shape.estad.amzl) +
  geom_sf(aes(geometry = geometry)) +
  geom_sf(data = viagens1, aes(color = dados_2019))


# abaixo de 3637 e 363 menos de 10 e até 1 vez ao dia
viagens2 <- shape.itinerario.2019 %>% 
  dplyr::filter(dados_2019 > 363 & 
                dados_2019 <3637)

ggplot(shape.estad.amzl) +
  geom_sf(aes(geometry = geometry)) +
  geom_sf(data = viagens2, aes(color = dados_2019))

# abaixo de 363 não tem todos os dias 
viagens3 <- shape.itinerario.2019 %>% 
  dplyr::filter(dados_2019 < 363)




ggplot(shape.estad.amzl) +
  geom_sf(aes(geometry = geometry)) +
  geom_sf(data = viagens3, aes(color = dados_2019))

x <- read_amazon()
y <- read_state()
amzl <- st_intersection(x, y)
z <- st_intersection(amzl, viagens3)

plot(z) # shape de rotas na amzl continuar daqui!!!!








# mapa pontos com as paradas
dados <- read_excel('Input/shapes ANTT rodovias/dados/viagensPorAnoPorPrefixo.xlsx', sheet=4)

shape.paradas.2019 <- st_read('Input/shapes ANTT rodovias/20190822/20190822paradas.shp')
shape.paradas.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801paradas.shp')

dados <- left_join(dados,shape.paradas.2019)

x <- read_amazon()
y <- read_state()
amzl <- st_intersection(x,y)



z <- left_join(amzl, dados)
z <- st_intersection(amzl,viagens3)



shape.parada.2019 <- left_join(z,dados)





# salvar mapa
ggsave('Outputs/03_mapas/Mineração', scale = 2)







shape.itinerario.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801itinerario.shp')

shape.paradas.2019 <- st_read('Input/shapes ANTT rodovias/20190822/20190822paradas.shp')
shape.paradas.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801paradas.shp')

shape.secoes.2019 <- st_read('Input/shapes ANTT rodovias/20190822/20190822secoes.shp')
shape.secoes.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801secoes.shp')

plot(shape.itinerario.2019$geometry)
plot(shape.itinerario.2021$geometry)

plot(shape.paradas.2019$geometry)
plot(shape.paradas.2021$geometry)

plot(shape.secoes.2019$geometry, type = 'l')
plot(shape.secoes.2021$geometry, type = 'l')



shape.itinerario.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801itinerario.shp')
left_join(shape.itinerario.2021,dados,by='id')
# ver linhas para as cidades intermediadoras!

