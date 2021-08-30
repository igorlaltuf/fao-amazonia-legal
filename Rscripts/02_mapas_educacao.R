# Mapas educa��o
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# importar dados
tabela <- read.csv('Outputs/01_tabelas/01_escolas_publicas_AMZL_2009_2020.csv')
coord.cidades <- st_read('Outputs/00_shapes_e_dados/coord.cidades.shp')


# matr�culas AMZL (quanto maior melhor)
# dados 2019 
dados.2019 <- tabela %>% 
  dplyr::filter(ano %in% 2019) %>% 
  classificar.variavel('matr_100_mil','class_matr_100_mil') %>% 
  classificar.variavel('escol_100_mil','class_escol_100_mil') %>% 
  mutate(matr_100 = ifelse(class_matr_100_mil == 'Alto' | class_matr_100_mil == 'Muito Alto', 1,0)) %>%
  mutate(escol_100 = ifelse(class_escol_100_mil == 'Alto' | class_escol_100_mil == 'Muito Alto', 1,0)) 

shape.muni <- st_read('Outputs/00_shapes_e_dados/shape.muni.amzl.shp')

dados.2019 <- left_join(dados.2019, shape.muni, by = c('cod_muni'='cd_mn')) 
dados.2019$matr_100 <- as.factor(dados.2019$matr_100)
dados.2019$escol_100 <- as.factor(dados.2019$escol_100)


# Mapa de Matr�culas 2019
matr.2019 <- ggplot(dados.2019)+
  geom_sf(aes(fill = matr_100, geometry = geometry), colour = NA) +
  scale_fill_manual(breaks = c('0','1'),
                    values=c('#fee8c8','#e34a33'),
                    label = c('demais faixas','Alto/Muito alto')) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= 'Matr�culas a cada 100 mil \n habitantes em 2019', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')

# Mapa de Escolas 2019
esc.2019 <- ggplot(dados.2019)+
  geom_sf(aes(fill = escol_100, geometry = geometry), colour = NA) +
  scale_fill_manual(breaks = c('0','1'),
                    values=c('#fee8c8','#e34a33'),
                    label = c('demais faixas','Alto/Muito alto')) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= 'Escolas a cada 100 mil \n habitantes em 2019', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')


# Dados 2009
dados.2009 <- tabela %>% 
  dplyr::filter(ano %in% 2009) %>% 
  classificar.variavel('matr_100_mil','class_matr_100_mil') %>% 
  classificar.variavel('escol_100_mil','class_escol_100_mil') %>% 
  mutate(matr_100 = ifelse(class_matr_100_mil == 'Alto' | class_matr_100_mil == 'Muito Alto', 1,0)) %>%
  mutate(escol_100 = ifelse(class_escol_100_mil == 'Alto' | class_escol_100_mil == 'Muito Alto', 1,0)) 

dados.2009 <- left_join(dados.2009, shape.muni, by = c('cod_muni'='cd_mn')) 

dados.2009$matr_100 <- as.factor(dados.2009$matr_100)
dados.2009$escol_100 <- as.factor(dados.2009$escol_100)


# Mapa de matr�culas 2009
matr.2009 <- ggplot(dados.2009) +
  geom_sf(aes(fill = matr_100, geometry = geometry), colour = NA) +
  scale_fill_manual(breaks = c('0','1'),
                    values=c('#fee8c8','#e34a33'),
                    label = c('demais faixas','Alto/Muito alto')) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= 'Matr�culas a cada 100 mil \n habitantes em 2009', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')

# Mapa de escolas 2009
esc.2009 <- ggplot(dados.2009)+
  geom_sf(aes(fill = escol_100, geometry = geometry), colour = NA) +
  scale_fill_manual(breaks = c('0','1'),
                    values=c('#fee8c8','#e34a33'),
                    label = c('demais faixas','Alto/Muito alto')) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= 'Escolas a cada 100 mil \n habitantes em 2009', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')

# reunir mapas com patchwork
(esc.2009|esc.2019)/
(matr.2009|matr.2019)

ggsave('Outputs/03_mapas/Educa��o/03_educacao.png', scale = 3)


# Mapa de pontos com as escolas p�blicas da AMZL 2020
pontos.escolas <- st_read('Outputs/00_shapes_e_dados/shape.escol.amzl.shp') %>% 
  dplyr::filter(tipo %in% c('Estadual','Municipal','Federal')) %>% # filtrar as p�blicas!
  mutate_if(is.factor,as.character) # muda as colunas do tipo factor para caractere
unique(pontos.escolas$tipo)
shape.estad.amzl <- st_read('Outputs/00_shapes_e_dados/shape.estad.amzl.shp')
shape.hidrovia <- read_sf('Input/shapes log�stica/hidrovias/Hidrovias.shp') %>% 
  dplyr::filter(cla_icacao %in% c('Naveg�vel', 'Navega��o sazonal'))
shape.hidrovia <- st_intersection(shape.estad.amzl,shape.hidrovia)



#### CONTINUAR DAQUI!!!! MUDAR LEGENDA DE LINHA PARA PONTOS
# COLOCAR R COLOR BREWER PARA MELHORAR CORES
ggplot(shape.estad.amzl) +
  geom_sf(aes(geometry = geometry)) +
  geom_sf(data = shape.hidrovia, aes(col = 'Hidrovias naveg�veis'), size = 0.5, show.legend = 'line') +
  geom_point(data = pontos.escolas, aes(geometry = geometry, col = 'Escolas'), stat = "sf_coordinates", size = .05, show.legend = 'point') +
  scale_colour_discrete("") + # muda o t�tulo da legenda (remove o color)
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom')
  

ggsave('Outputs/03_mapas/Educa��o/03_escolas_2020_amzl.png', scale = 3)





# Tabela das intermediadoras 
tab.2009 <- dados.2009 %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras) %>% 
  select(sigla_uf,muni,matr_100_mil,escol_100_mil)

tab.2019 <- dados.2019 %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras) %>% 
  select(muni,matr_100_mil,escol_100_mil)

tabela.2009.2019 <- left_join(tab.2009, tab.2019, by = 'muni') %>% 
  mutate(variacao_matr = (matr_100_mil.y / matr_100_mil.x) - 1) %>% 
  mutate(variacao_escol = (escol_100_mil.y / escol_100_mil.x) - 1) %>% 
  select(1,2,3,5,7,4,6,8) 
  
colnames(tabela.2009.2019) <- c('uf_sigla','muni','matr_100_mil_2009','matr_100_mil_2019','var_matr','escol_100_mil_2009','escol_100_mil_2019','var_escol')


tabela.educacao <- gt(tabela.2009.2019) %>%
  cols_label(
    uf_sigla = 'UF',
    muni = 'Munic�pio',
    matr_100_mil_2009 = 'Matr�culas a cada 100 mil \n habitantes em 2009',
    matr_100_mil_2019 = 'Matr�culas a cada 100 mil \n habitantes em 2019',
    var_matr = 'Varia��o - Matr�culas',
    escol_100_mil_2009 = 'Escolas a cada 100 mil \n habitantes em 2009',
    escol_100_mil_2019 = 'Escolas a cada 100 mil \n habitantes em 2019',
    var_escol = 'Varia��o - Escolas'
  ) %>% 
  tab_header(
    title = 'Quantidade de matr�culas e escolas a cada 100 mil habitantes \n nas cidades intermediadoras da Amaz�nia Legal',
    subtitle = '2009 e 2019'
  ) %>%
  fmt_markdown(
    columns = c(uf_sigla,muni)
  ) %>% 
  fmt_number(
    columns = c(matr_100_mil_2009,matr_100_mil_2019,escol_100_mil_2009,escol_100_mil_2019),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  fmt_percent(
    columns = c(var_matr,var_escol),
    decimals = 1,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Dados do Censo Escolar (INEP) via Carabetta, Jo�o; Dahis, Ricardo; Israel, Fred; Scovino, Fernanda (2020) \n Base dos Dados: Reposit�rio de Dados Abertos em https://basedosdados.org.')


tabela.educacao

gtsave(tabela.educacao, 'Outputs/03_mapas/Educa��o/03_tabela_educacao.png')

# Gr�ficos Altamira (exemplo)
cidade <- tabela %>% 
  dplyr::filter(cod_muni == 1500602) # altamira

a <- ggplot(cidade, aes(x = ano, y = qtd_escolas)) +
  geom_line(color = 'blue', group = 1) +
  scale_y_continuous(name = 'Quantidade de Escolas P�blicas', n.breaks = 6) +
  scale_x_continuous(name = 'Ano', n.breaks = 7) +
  theme_classic() 

b <- ggplot(cidade, aes(x = ano, y = qtd_matriculas)) +
  geom_line(color = 'blue', group = 1) +
  scale_y_continuous(name = 'Quantidade de Matr�culas \n em escolas p�blicas', n.breaks = 6) +
  scale_x_continuous(name = 'Ano', n.breaks = 7) +
  theme_classic() 

c <- ggplot(cidade, aes(x = ano, y = matr_100_mil)) +
  geom_line(color = 'blue', group = 1) +
  scale_y_continuous(name = 'Matr�culas em escolas p�blicas \n a cada 100 mil habitantes', n.breaks = 6) +
  scale_x_continuous(name = 'Ano', n.breaks = 7) +
  theme_classic() 

d <- ggplot(cidade, aes(x = ano, y = escol_100_mil)) +
  geom_line(color = 'blue', group = 1) +
  scale_y_continuous(name = 'Escolas p�blicas \n a cada 100 mil habitantes', n.breaks = 6) +
  scale_x_continuous(name = 'Ano', n.breaks = 7) +
  theme_classic() 

(a|b)/
(c|d)