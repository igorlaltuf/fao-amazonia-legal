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

intermediadoras <- "CRUZEIRO DO SUL(AC)|RIO BRANCO(AC)CRUZEIRO DO SUL(AC)|RIO BRANCO(AC)|ITACOATIARA(AM)|LABREA(AM)|MANACAPURU(AM)|
                    PARINTINS(AM)|TABATINGA(AM)|TEFE(AM)|MACAPA(AP)|OIAPOQUE(AP)|BACABAL(MA)|
                    CAXIAS(MA)|IMPERATRIZ(MA)|PRESIDENTE DUTRA(MA)|SANTA INES(MA)|BARRA DO GARCAS(MT)|
                    CACERES(MT)|CUIABA(MT)|RONDONOPOLIS(MT)|SINOP(MT)|ALTAMIRA(PA)|BREVES(PA)|
                    CASTANHAL(PA)|MARABA(PA)|REDENCAO(PA)|SANTAREM(PA)|ARIQUEMES(RO)|JI-PARANA(RO)|
                    PORTO VELHO(RO)|BOA VISTA(RR)|RORAINOPOLIS(RR)|ARAGUAINA(TO)|GURUPI(TO)|PALMAS(TO)"

# intermed
intermed <- shape.itinerario.2019 %>%
  dplyr::filter(str_detect(descricao, intermediadoras))





intermed <- shape.itinerario.2019 %>% 
  filter(stringr::str_detect(descricao,'RIO BRANCO(AC)'))
          
intermed <- shape.itinerario.2019 %>% 
  dplyr::filter(descricao %in% 'RIO BRANCO(AC)')



filter(grepl('MCO|BWI', Dest))



intermed <- shape.itinerario.2019 %>%
  filter(str_detect(descricao, 'GOIANIA(GO) - PALMAS(TO)'))
  
  dplyr::filter(descricao %in% '^GOIANIA(GO)')


  #filter(str_detect(descricao, regex("GOIANIA(GO) - PALMAS(TO)", ignore_case = TRUE)))
  

intermed <- shape.itinerario.2019 %>%
  filter(grepl('GOIANIA(GO) - PALMAS(TO)', descricao))
  



  

  filter(grepl('GOIANIA(GO) - PALMAS(TO)', descricao))

  
  filter(grepl('GOIANIA(GO)', descricao))
                  
                 


  dplyr::filter(descricao %in% 'intermediadoras')


  
  
  
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




# ANTAQ
# https://www.sopesp.com.br/2019/05/27/instalacoes-portuarias-publicas-de-pequeno-porte-ip4/
# hidrovias
shape.hidrovia <- read_sf('Input/shapes logística/hidrovias/Hidrovias.shp')  
  dplyr::filter(cla_icacao %in% c('Navegável', 'Navegação sazonal'))
shape.hidrovia <- st_intersection(shape.estad.amzl,shape.hidrovia)

# portos
portos.ip4 <- st_read('Input/ANTAQ dados/dados_Passageiros/SHP_Instalaçõs portuaria-IP4/Instalaçõs portuaria-IP4.shp') 
portos.ip4 <- st_intersection(shape.estad.amzl,portos.ip4)
unique(portos.ip4$situacao)
portos.ip4$situacao <- as.factor(portos.ip4$situacao)

# linhas de travessia
linhas.travessias <- st_read('Input/ANTAQ dados/dados_Passageiros/SHP-LINHAS DE TRAVESSIAS/LinhasTravessia.shp', options = "ENCODING=windows-1252") %>% 
  mutate(est_origem = as.character(est_origem)) %>% 
  dplyr::filter(est_estino %in% uf.amz.legal)  
  

# passageiros???
passageiros <- st_read('Input/ANTAQ dados/dados_Passageiros/SHP_VEN_2018-PASSAGEIROS/VEN_PASSAGEIROS.shp')

# Portos 
ggplot() +
  geom_sf(data = shape.estad.amzl, aes(geometry = geometry)) +
  geom_sf(data = shape.hidrovia, aes(col = 'Hidrovias'), size = 0.6, show.legend = 'line') +
  geom_sf(data = portos.ip4, aes(geometry = geometry, col = situacao, color = situacao), stat = "sf_coordinates", size = 1.5, show.legend = 'point') +
  scale_color_manual(values = c('Em obras' = '#feb24c','Planejado' = '#e34a33','Operando' = '#2ca25f', "Hidrovias" = "#2b8cbe"),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("blank","blank","blank","solid"),
                                                              shape=c(16,16,16,NA)))) +
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size= 10))

ggsave('Outputs/03_mapas/Outros/03_portos_ip4_antaq.png')

#geom_sf(data = passageiros, aes(geometry = geometry)) +








