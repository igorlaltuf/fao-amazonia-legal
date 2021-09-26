rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Bases de 22-08-2019 e 01-08-2021.
# Totalmunic = nº total de municípios em que há parada na linha.
# Secoesmunic = nº de municípios em que há seções (vendas de bilhetes) na linha.
shape.br <- read_country()
shape.amzl <- read_amazon()
estad.amzl <- st_read('Outputs/00_shapes_e_dados/shape.estad.amzl.shp')
sf_use_s2(FALSE)



# substitui por zero quando não havia informações do prefixo naquele ano
dados <- read_excel('Input/shapes ANTT rodovias/dados/viagensPorAnoPorPrefixo.xlsx', sheet=4)

shape.itinerario.2019 <- st_read('Input/shapes ANTT rodovias/20190822/20190822itinerario.shp')
shape.itinerario.2019 <- left_join(shape.itinerario.2019, dados)

shape.itinerario.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801itinerario.shp')
shape.itinerario.2021 <- left_join(shape.itinerario.2021, dados)


intermediadoras <- c('CRUZEIRO DO SUL\\(AC\\)|RIO BRANCO\\(AC\\)|ITACOATIARA\\(AM\\)|LABREA\\(AM\\)|MANACAPURU\\(AM\\)|
                    PARINTINS\\(AM\\)|TABATINGA\\(AM\\)|TEFE\\(AM\\)|MACAPA\\(AP\\)|OIAPOQUE\\(AP\\)|BACABAL\\(MA\\)|
                    CAXIAS\\(MA\\)|IMPERATRIZ\\(MA\\)|PRESIDENTE DUTRA\\(MA\\)|SANTA INES\\(MA\\)|BARRA DO GARCAS\\(MT\\)|
                    CACERES\\(MT\\)|CUIABA\\(MT\\)|RONDONOPOLIS\\(MT\\)|SINOP\\(MT\\)|ALTAMIRA\\(PA\\)|BREVES\\(PA\\)|
                    CASTANHAL\\(PA\\)|MARABA\\(PA\\)|REDENCAO\\(PA\\)|SANTAREM\\(PA\\)|ARIQUEMES\\(RO\\)|JI-PARANA\\(RO\\)|
                    PORTO VELHO\\(RO\\)|BOA VISTA\\(RR\\)|RORAINOPOLIS\\(RR\\)|ARAGUAINA\\(TO\\)|GURUPI\\(TO\\)|PALMAS\\(TO\\)')


shape.itinerario.2019 <- st_intersection(shape.amzl, shape.itinerario.2019) %>% na.omit()
shape.itinerario.2021 <- st_intersection(shape.amzl, shape.itinerario.2021) %>% na.omit()

# 276 itinerários nas cidades intermediadoras (filtrar até a segunda linha abaixo)
# os valores absolutos são referentes às viagens de vda e volta por ano (99 linhas registraram queda no número de viagens e 40 foram extintas)
# 2019
intermed <- shape.itinerario.2019 %>%
  dplyr::filter(str_detect(descricao, intermediadoras)) %>% 
  mutate(var_covid = (dados_2021/dados_2019)-1,
         cat_covid = cut(var_covid, breaks = c(-Inf, -0.6, -0.3, 0), labels = c('-100% até -60%','-60% até -30%','-30% até 0%'))) %>% 
  dplyr::filter(var_covid < 0)  # filtrar as linhas em que houve queda de saídas

extintas <- intermed %>% 
  dplyr::filter(dados_2021 == 0)


# queda de saídas em % de 2019 a 2021
cidades.ponto <- st_read("Input/REGIC2018_cidades_ponto/REGIC2018_Cidades_ponto.shp", options = "ENCODING = UTF-8") %>% 
  dplyr::filter(cod_cidade %in% cidades.intermediadoras)

# itinerários que registraram queda
grafico <- ggplot() + 
  geom_sf(data = estad.amzl, aes(geometry = geometry)) +
  geom_sf(data = intermed, aes(geometry = geom, color = cat_covid), size = .6, show.legend = 'line') +
  geom_sf(data = cidades.ponto, aes(geometry = geometry), size = 1, shape = 16) +
  scale_color_manual(values = c('-100% até -60%' = 'red', '-60% até -30%' = 'orange', '-30% até 0%' = 'green'), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid", "solid", "solid")))) +
  coord_sf(crs = 4674) +
  annotation_scale(location='br') +
  annotation_north_arrow(location='tl',
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom')


grafico +
  geom_sf_text(data = cidades.ponto, aes(label = label), colour='grey10', vjust = 1.3, size = 2.2) 

ggsave('Outputs/03_mapas/Outros/rodo_queda_covid.png')


# itinerários que não registraram queda (125 no total)
intermed <- shape.itinerario.2019 %>%
  dplyr::filter(str_detect(descricao, intermediadoras)) %>% 
  mutate(var_covid = (dados_2021/dados_2019)-1) %>% 
  dplyr::filter(var_covid == 0)

grafico <- ggplot() + 
  geom_sf(data = estad.amzl, aes(geometry = geometry)) +
  geom_sf(data = intermed, aes(geometry = geom), size = .6, show.legend = 'line') +
  geom_sf(data = cidades.ponto, aes(geometry = geometry), size = 1, shape = 16) +
  coord_sf(crs = 4674) +
  annotation_scale(location='br') +
  annotation_north_arrow(location='tl',
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom')

grafico +
  geom_sf_text(data = cidades.ponto, aes(label = label), colour='grey10', vjust = 1.3, size = 2.2) 

ggsave('Outputs/03_mapas/Outros/rodo_estavel_covid.png')

# itinerários que aumentaram (11 no total)
# 2019
intermed <- shape.itinerario.2019 %>%
  dplyr::filter(str_detect(descricao, intermediadoras)) %>% 
  mutate(var_covid = (dados_2021/dados_2019)-1,
         cat_covid = cut(var_covid, breaks = c(0, 0.3, 0.6, Inf), labels = c('0% até 30%', '30% até 60%','60% até 100%'))) %>% 
  dplyr::filter(var_covid > 0)  # filtrar as linhas em que houve queda de saídas

grafico <- ggplot() + 
  geom_sf(data = estad.amzl, aes(geometry = geometry)) +
  geom_sf(data = intermed, aes(geometry = geom, color = cat_covid), size = .6, show.legend = 'line') +
  geom_sf(data = cidades.ponto, aes(geometry = geometry), size = 1, shape = 16) +
  scale_color_manual(values = c('0% até 30%' = 'red', '30% até 60%' = 'orange', '60% até 100%' = 'green'), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid", "solid", "solid")))) +
  coord_sf(crs = 4674) +
  annotation_scale(location='br') +
  annotation_north_arrow(location='tl',
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom')

grafico +
  geom_sf_text(data = cidades.ponto, aes(label = label), colour='grey10', vjust = 1.3, size = 2.2) 

ggsave('Outputs/03_mapas/Outros/rodo_aument_covid.png')


# novos itinerários
intermed <- shape.itinerario.2021 %>%
  dplyr::filter(str_detect(descricao, intermediadoras)) %>% 
  dplyr::filter(dados_2019 == 0 & dados_2021 > 0) %>% 
  mutate(cat_dados_2021 = cut(dados_2021, breaks = c(0, 1000, 2000, Inf), labels = c('até 1 mil', '1 mil - 2 mil','acima de 2 mil'))) 

grafico <- ggplot() + 
  geom_sf(data = estad.amzl, aes(geometry = geometry)) +
  geom_sf(data = intermed, aes(geometry = geom, color = cat_dados_2021), size = .6, show.legend = 'line') +
  geom_sf(data = cidades.ponto, aes(geometry = geometry), size = 1, shape = 16) +
  scale_color_manual(values = c('até 1 mil' = 'red', '1 mil - 2 mil' = 'orange', 'acima de 2 mil' = 'green'), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid", "solid", "solid")))) +
  coord_sf(crs = 4674) +
  annotation_scale(location='br') +
  annotation_north_arrow(location='tl',
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom')

grafico +
  geom_sf_text(data = cidades.ponto, aes(label = label), colour='grey10', vjust = 1.3, size = 2.2) 

ggsave('Outputs/03_mapas/Outros/rodo_novas_covid.png')


queda.viagens.nac <- (sum(shape.itinerario.2019$dados_2019)/sum(shape.itinerario.2021$dados_2021))-1 # 13,07% no Brasil


x <- shape.itinerario.2019 %>% 
  dplyr::filter(str_detect(descricao, intermediadoras))
y <- shape.itinerario.2021 %>% 
  dplyr::filter(str_detect(descricao, intermediadoras))

queda.intermed <- (sum(x$dados_2019)/sum(y$dados_2021))-1 # 20,4% de queda nas intermediadoras




# 363 viagens em uma direção ou 726 ida e volta = 1 viagem pro dia (aparentemente eles consideram o ano com 363 dias)
# linhas diárias 2019
intermed1 <- intermed %>% 
  dplyr::filter(dados_2019 >= 726)

# linhas diárias 2021
intermed <- shape.itinerario.2021 %>%
  dplyr::filter(str_detect(descricao, intermediadoras)) %>% 
  dplyr::filter(dados_2021 >= 726)


  












# queda COVID-19





# 
# 
# 
# 
# ggplot(shape.itinerario.2019) +
#   geom_sf(aes(geometry = geom)) +
#   geom_sf(data = intermed, aes(color = cat_covid))+
#   theme_classic()
# 
# 
# 
# 
# 
# 
# 
# 
# intermed <- shape.itinerario.2019 %>%
#   dplyr::filter(str_detect(descricao, intermediadoras))%>% 
#   mutate(categ_2019 = cut(dados_2019, breaks=c(0,500,1000,1500), labels = c('0-500','500-1000','1000-1500')))
# 
# 
# rotas.2019 <- ggplot(shape.amzl) +
#   geom_sf(aes(geometry = geometry)) +
#   geom_sf(data = intermed, aes(color = categ_2019))+
#   theme_classic()
#   
# 
# 
# # 2021
# shape.itinerario.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801itinerario.shp')
# shape.itinerario.2021 <- left_join(shape.itinerario.2021, dados)
# 
# intermed <- shape.itinerario.2021 %>%
#   dplyr::filter(str_detect(descricao, intermediadoras))%>% 
#   mutate(categ_2021 = cut(dados_2021, breaks=c(0,500,1000,1500), labels = c('0-500','500-1000','1000-1500')))
# 
# rotas.2021 <-ggplot(shape.br) +
#   geom_sf(aes(geometry = geom)) +
#   geom_sf(data = intermed, aes(color = categ_2021))+
#   theme_classic()
# 
# 
# rotas.2019 | rotas.2021
# 
# 
# 

# 
# 
# # Rotas com pelo menos 2 viagens por dia em 2019
# diaria <- intermed %>% 
#   dplyr::filter(dados_2019 >= 727) 
#   
# ggplot(shape.br) +
#   geom_sf(aes(geometry = geom)) +
#   geom_sf(data = diaria, aes(color = dados_2019))+
#   theme_classic()
# 
# # Rotas com pelo menos 2 viagens por dia em 2021
# diaria <- intermed %>% 
#   dplyr::filter(dados_2021 >= 727) %>% 
#   mutate()
# 
# ggplot(shape.br) +
#   geom_sf(aes(geometry = geom)) +
#   geom_sf(data = diaria, aes(color = dados_2021))+
#   theme_classic()
# 
# 
# 
# 
# 
# 
# 
# # rodovias e hidrovias 2016
# # usar para mostrar ligações mais enxutas
# 
# 
# 
# 
# 
# 
# 
# 
#   
# # acima de 3637 viagens ao ano (aprox 10 ao dia)
# viagens1 <- shape.itinerario.2019 %>% 
#   dplyr::filter(dados_2019 > 3637)
# 
# ggplot(estad.amzl) +
#   geom_sf(aes(geometry = geometry)) +
#   geom_sf(data = viagens1, aes(color = dados_2019))
# 
# 
# # abaixo de 3637 e 363 menos de 10 e até 1 vez ao dia
# viagens2 <- shape.itinerario.2019 %>% 
#   dplyr::filter(dados_2019 > 363 & 
#                 dados_2019 <3637)
# 
# ggplot(estad.amzl) +
#   geom_sf(aes(geometry = geometry)) +
#   geom_sf(data = viagens2, aes(color = dados_2019))
# 
# # abaixo de 363 não tem todos os dias 
# viagens3 <- shape.itinerario.2019 %>% 
#   dplyr::filter(dados_2019 < 363)
# 
# 
# 
# 
# ggplot(estad.amzl) +
#   geom_sf(aes(geometry = geometry)) +
#   geom_sf(data = viagens3, aes(color = dados_2019))
# 
# x <- read_amazon()
# y <- read_state()
# amzl <- st_intersection(x, y)
# z <- st_intersection(amzl, viagens3)
# 
# plot(z) # shape de rotas na amzl continuar daqui!!!!
# 
# 
# 
# 
# 
# 
# 
# 
# # mapa pontos com as paradas
# dados <- read_excel('Input/shapes ANTT rodovias/dados/viagensPorAnoPorPrefixo.xlsx', sheet=4)
# 
# shape.paradas.2019 <- st_read('Input/shapes ANTT rodovias/20190822/20190822paradas.shp')
# shape.paradas.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801paradas.shp')
# 
# dados <- left_join(dados,shape.paradas.2019)
# 
# x <- read_amazon()
# y <- read_state()
# amzl <- st_intersection(x,y)
# 
# 
# 
# z <- left_join(amzl, dados)
# z <- st_intersection(amzl,viagens3)
# 
# 
# 
# shape.parada.2019 <- left_join(z,dados)
# 
# 
# 
# 
# 
# # salvar mapa
# ggsave('Outputs/03_mapas/Mineração', scale = 2)
# 
# 
# 
# 
# 
# 
# 
# shape.itinerario.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801itinerario.shp')
# 
# shape.paradas.2019 <- st_read('Input/shapes ANTT rodovias/20190822/20190822paradas.shp')
# shape.paradas.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801paradas.shp')
# 
# shape.secoes.2019 <- st_read('Input/shapes ANTT rodovias/20190822/20190822secoes.shp')
# shape.secoes.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801secoes.shp')
# 
# plot(shape.itinerario.2019$geometry)
# plot(shape.itinerario.2021$geometry)
# 
# plot(shape.paradas.2019$geometry)
# plot(shape.paradas.2021$geometry)
# 
# plot(shape.secoes.2019$geometry, type = 'l')
# plot(shape.secoes.2021$geometry, type = 'l')
# 
# 
# 
# shape.itinerario.2021 <- st_read('Input/shapes ANTT rodovias/20210801/20210801itinerario.shp')
# left_join(shape.itinerario.2021,dados,by='id')
# # ver linhas para as cidades intermediadoras!
# 
# 
# 
# 
# # ANTAQ
# # https://www.sopesp.com.br/2019/05/27/instalacoes-portuarias-publicas-de-pequeno-porte-ip4/
# # hidrovias
shape.hidrovia <- read_sf('Input/shapes logística/hidrovias/Hidrovias.shp') %>% 
  dplyr::filter(cla_icacao %in% c('Navegável', 'Navegação sazonal'))
shape.hidrovia <- st_intersection(estad.amzl, shape.hidrovia)


# portos
portos.ip4 <- st_read('Input/ANTAQ dados/dados_Passageiros/SHP_Instalaçõs portuaria-IP4/Instalaçõs portuaria-IP4.shp')
portos.ip4 <- st_intersection(estad.amzl, portos.ip4)
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
  geom_sf(data = estad.amzl, aes(geometry = geometry), fill = NA) +
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





