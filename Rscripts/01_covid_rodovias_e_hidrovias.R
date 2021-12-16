# Rodovias e hidrovias 2016 IBGE
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

options(scipen=999)
sf_use_s2(F) # permite que o sf fucione como na versão anterior

# Importar shapefiles da REGIC
ligacoes.manaus <- st_read("Input/REGIC2018_ligacoes_cidades/REGIC2018_Ligacoes_entre_Cidades.shp",
                           options = "ENCODING=UTF-8") %>% 
                   dplyr::filter(cod_dest %in% 1302603,
                                 quest_4 > 0)

ligac.manaus <- as.character(ligacoes.manaus$cod_ori)

######## ligações de saúde de alta complexidade
# Importar dados do IBGE
shape <- st_read("Input/rodovias e hidrovias 2016 IBGE/LRH2016_00_Base_Completa.shp", options = "ENCODING=WINDOWS-1252") # Este encoding ENCODING=WINDOWS-1252 foi a forma dele conseguir ler o os acentos do shapefile.

# Baixar e organizar coordenadas das cidades brasileiras
coordMunicipal <- read_municipal_seat(year = 2010, showProgress = T) # baixar todas as coordenadas das cidades brasileiras
coordMunicipal$lat <- as.numeric(st_coordinates(coordMunicipal$geom)[,2])
coordMunicipal$lng <- as.numeric(st_coordinates(coordMunicipal$geom)[,1])
coordMunicipal <- as.data.frame(coordMunicipal)

# Estabelecer parâmetros (tempo e custo)
tempo <- Inf # limite de tempo (máx 9000 min) padrão 120 min
custo <- Inf # limite de custo (máx R$ 950) padrão 35 reais
rodo <- 1
hidro <- 1

# var05 é quantidade de saídas semanais via hidrovias
# var06 é quantidade de saídas semanais via rodovias
# filtrar separadamente acima de 100 saídas semanais

# Filtrar as 34 cidades intermediadoras como sendo a origem e quanto ao tempo
manaus <- '1302603' # destino manaus

# Hidrovias 2016
origem_intermed_lines <- shape %>% 
  dplyr::filter(codmundv_a %in% manaus & var04 <= tempo & var03 <= custo & var05 > hidro) # & var06 > rodo) 
# Os pares de ligação nessa pesquisa não tem "direção", então a posição do par de Municípios A-B não significa que B seja destino necessariamente,
# B é nesse caso tanto origem quanto destino. Por isso eu também filtro procurando as cidades intermediadoras na variável codmundv_b
origem_intermed_lines2 <- shape %>% 
  dplyr::filter(codmundv_b %in% manaus & var04 <= tempo & var03 <= custo & var05 > hidro) # & var06 > rodo)

origem_intermed_lines <- rbind(origem_intermed_lines,origem_intermed_lines2) %>%   
  dplyr::filter(codmundv_a %in% ligac.manaus)


# Rodovias
origem_intermed_lines <- shape %>% 
  dplyr::filter(codmundv_a %in% manaus & var04 <= tempo & var03 <= custo & var06 > rodo) # & var06 > rodo) 
# Os pares de ligação nessa pesquisa não tem "direção", então a posição do par de Municípios A-B não significa que B seja destino necessariamente,
# B é nesse caso tanto origem quanto destino. Por isso eu também filtro procurando as cidades intermediadoras na variável codmundv_b
origem_intermed_lines2 <- shape %>% 
  dplyr::filter(codmundv_b %in% manaus & var04 <= tempo & var03 <= custo & var06 > rodo) # & var06 > rodo)

origem_intermed_lines <- rbind(origem_intermed_lines,origem_intermed_lines2) %>%   
  dplyr::filter(codmundv_a %in% ligac.manaus)






# CONTINUAR DAQUI. Ver se tem algum município que esteja em ambos hidrovias e REGIC
    # E agora junto as duas bases
 



  




# Mapa estático das hidrovias
sf_use_s2(FALSE) 
mapa <- read_amazon()
hidrovias.navegaveis <- read_sf('Input/shapes logística/hidrovias/Hidrovias.shp') %>% 
  dplyr::filter(cla_icacao %in% c('Navegável', 'Navegação sazonal'))

x <- origem_intermed_lines %>% 
  mutate(class_var05 = cut(var05, breaks=c(0,40,80,120,Inf), labels = c('0-40 saídas','40-80 saídas','80-120 saídas','120 ou mais saídas')))

cidades <- unique(append(x$codmundv_a, x$codmundv_b)) # cidades que tem ligações hidro ou rodoviárias
cidades <- paste(cidades, collapse = '|')

cidades.ponto <- st_read("Input/REGIC2018_cidades_ponto/REGIC2018_Cidades_ponto.shp", options = "ENCODING=UTF-8") %>% 
  dplyr::filter(str_detect(cod_cidade, cidades)) 
cidades.ponto <- cidades.ponto %>%  
  mutate(lat = unlist(map(cidades.ponto$geometry, 1)),
         lng = unlist(map(cidades.ponto$geometry, 2)))

# mapa base
grafico <- ggplot() +
  geom_sf(data = mapa, aes(geometry = geom)) +
  geom_sf(data = hidrovias.navegaveis, aes(geometry = geometry, color = 'Hidrovias'), size = 1, show.legend = 'line') +
  geom_sf(data = x, aes(geometry = geometry, color = class_var05), size = 0.5, show.legend = 'line') +
  geom_sf(data = cidades.ponto, aes(geometry = geometry), size = 1, shape = 16) +
  scale_color_manual(values = c('Hidrovias' = '#2c7fb8',"0-40 saídas" = "#ffffb2", "40-80 saídas" = "#fecc5c", '80-120 saídas' = '#fd8d3c', '120 ou mais saídas' = '#e31a1c'), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid", "solid", "solid","solid", "solid")))) +
  coord_sf(crs = 4674) +
  annotation_scale(location='br') +
  annotation_north_arrow(location='tl',
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() +
  labs(x = NULL, y = NULL)

# Mapas das rotas próximas com frequência das saídas semanais
# Manacapuru
grafico +
  coord_sf(xlim = c(-62,-59.5), ylim = c(-4.5,-2.5), expand = FALSE) +
  geom_text(
    data = cidades.ponto,
    label = cidades.ponto$label, 
    nudge_x = 0.05, nudge_y = 0.05, 
    aes(x = lat, y = lng),
    check_overlap = T,
    size = 2.8, 
    fontface = "bold")
ggsave('Outputs/03_mapas/Outros/frequencia_saídas_hidro_manacapuru.png')

# Parintins e Itacoatiara
grafico +
  coord_sf(xlim = c(-59.5,-55.65), ylim = c(-4,-1.5), expand = FALSE) +
  geom_text(
    data = cidades.ponto,
    label = cidades.ponto$label, 
    nudge_x = 0.05, nudge_y = 0.05, 
    aes(x = lat, y = lng),
    check_overlap = T,
    size = 2.8, 
    fontface = "bold")
ggsave('Outputs/03_mapas/Outros/frequencia_saídas_hidro_parintins e itacoat.png')

# Santarém
grafico +
  coord_sf(xlim = c(-55.6,-53), ylim = c(-4,-1), expand = FALSE) +
  geom_text(
    data = cidades.ponto,
    label = cidades.ponto$label, 
    nudge_x = 0.05, nudge_y = 0.05, 
    aes(x = lat, y = lng),
    check_overlap = T,
    size = 2.8, 
    fontface = "bold")

ggsave('Outputs/03_mapas/Outros/frequencia_saídas_hidro_santarem.png')

# Breves
grafico +
  coord_sf(xlim = c(-51,-49), ylim = c(-2,-1), expand = FALSE) +
  geom_text(
    data = cidades.ponto,
    label = cidades.ponto$label, 
    nudge_x = 0.05, nudge_y = 0.05, 
    aes(x = lat, y = lng),
    check_overlap = T,
    size = 2.8, 
    fontface = "bold")

ggsave('Outputs/03_mapas/Outros/frequencia_saídas_hidro_breves.png')


# Rodovias 2016 
origem_intermed_lines <- shape %>% 
  dplyr::filter(codmundv_a %in% cid_inter & var04 <= tempo & var03 <= custo & var06 > rodo) 
origem_intermed_lines2 <- shape %>% 
  dplyr::filter(codmundv_b %in% cid_inter & var04 <= tempo & var03 <= custo & var06 > rodo)
origem_intermed_lines <- rbind(origem_intermed_lines,origem_intermed_lines2) # E agora junto as duas bases

# Mapa estático das rodovias
x <- origem_intermed_lines %>% 
  mutate(class_var06 = cut(var06, breaks=c(0,400,Inf), labels = c('até 400 saídas','acima de 400 saídas')))
cidades <- unique(append(x$codmundv_a, x$codmundv_b)) # cidades que tem ligações hidro ou rodoviárias
cidades <- paste(cidades, collapse = '|')

cidades.ponto <- st_read("Input/REGIC2018_cidades_ponto/REGIC2018_Cidades_ponto.shp", options = "ENCODING=UTF-8") %>% 
  dplyr::filter(str_detect(cod_cidade, cidades))  

cidades.ponto <- cidades.ponto %>% 
  mutate(lat = unlist(map(cidades.ponto$geometry, 1)),
         lng = unlist(map(cidades.ponto$geometry, 2)))

# mapa base
shape.muni.uf <- st_read('Outputs/00_shapes_e_dados/shape.estad.amzl.shp')

grafico <- ggplot() +
  geom_sf(data = shape.muni.uf, aes(geometry = geometry), colour = NA) +
  geom_sf(data = x, aes(geometry = geometry, color = class_var06), size = .6, show.legend = 'line') +
  geom_sf(data = cidades.ponto, aes(geometry = geometry), size = .7, shape = 16) +
  scale_color_manual(values = c('até 400 saídas' = '#2ca25f',"acima de 400 saídas" = "#f03b20"), name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("solid","solid")))) +
  coord_sf(crs = 4674) +
  annotation_scale(location='br') +
  annotation_north_arrow(location='tl',
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom')

y <- cidades.ponto %>% 
  dplyr::filter(cod_cidade %in% cid_inter)

# gráfico geral
grafico +
  geom_sf_text(data = y, aes(label = label), colour='grey10', vjust = 1.3, size = 2.2) 

ggsave('Outputs/03_mapas/Outros/frequencia_saídas_rodo_amzl.png')



# 
# 
# 
# # Rio Branco
# grafico +
#   coord_sf(xlim = c(-69,-67), ylim = c(-10.5,-8.5), expand = FALSE) +
#   geom_text(
#     data = cidades.ponto,
#     label = cidades.ponto$label, 
#     nudge_x = 0.05, nudge_y = 0.05, 
#     aes(x = lat, y = lng),
#     check_overlap = T,
#     size = 2.8, 
#     fontface = "bold")
# 
# ggsave('Outputs/03_mapas/Outros/frequencia_saídas_rodo_rio_branco.png')
