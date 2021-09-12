# Mapas educação
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# importar dados
tabela <- read.csv('Outputs/01_tabelas/01_escolas_publicas_AMZL_2009_2020.csv')
coord.cidades <- st_read('Outputs/00_shapes_e_dados/coord.cidades.shp')

# matrículas AMZL (quanto maior melhor)
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


# Mapa de Matrículas 2019
matr.2019 <- ggplot(dados.2019)+
  geom_sf(aes(fill = matr_100, geometry = geometry), colour = NA) +
  scale_fill_manual(breaks = c('0','1'),
                    values=c('#fee8c8','#e34a33'),
                    label = c('demais faixas','Alto/Muito alto')) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= '', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  ggtitle('Matrículas a cada 100 mil habitantes em 2019')+
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))

# Mapa de Escolas 2019
esc.2019 <- ggplot(dados.2019)+
  geom_sf(aes(fill = escol_100, geometry = geometry), colour = NA) +
  scale_fill_manual(breaks = c('0','1'),
                    values=c('#fee8c8','#e34a33'),
                    label = c('demais faixas','Alto/Muito alto')) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= '', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  ggtitle('Escolas a cada 100 mil habitantes em 2019')+
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))


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


# Mapa de matrículas 2009
matr.2009 <- ggplot(dados.2009) +
  geom_sf(aes(fill = matr_100, geometry = geometry), colour = NA) +
  scale_fill_manual(breaks = c('0','1'),
                    values=c('#fee8c8','#e34a33'),
                    label = c('demais faixas','Alto/Muito alto')) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= '', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  ggtitle('Matrículas a cada 100 mil habitantes em 2009')+
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))


# Mapa de escolas 2009
esc.2009 <- ggplot(dados.2009) +
  geom_sf(aes(fill = escol_100, geometry = geometry), colour = NA) +
  scale_fill_manual(breaks = c('0','1'),
                    values=c('#fee8c8','#e34a33'),
                    label = c('demais faixas','Alto/Muito alto')) +
  geom_point(data = coord.cidades, aes(geometry = geometry), stat = "sf_coordinates", size = 1) +
  geom_sf_text(data = coord.cidades, aes(label = mn), colour='grey10',vjust=1.3, size = 2.7) +
  labs(fill= '', x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  ggtitle('Escolas a cada 100 mil habitantes em 2009')+
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))
  
  
# reunir mapas com patchwork
(esc.2009|esc.2019)/
(matr.2009|matr.2019)

ggsave('Outputs/03_mapas/Educação/03_educacao.png', scale = 3)


# Mapa de pontos com as escolas públicas da AMZL 2020
pontos.escolas <- st_read('Outputs/00_shapes_e_dados/shape.escol.amzl.shp') %>% 
  dplyr::filter(tipo %in% c('Estadual','Municipal','Federal')) %>% # filtrar as públicas!
  mutate_if(is.factor,as.character) # muda as colunas do tipo factor para caractere
unique(pontos.escolas$tipo)
shape.estad.amzl <- st_read('Outputs/00_shapes_e_dados/shape.estad.amzl.shp')
shape.hidrovia <- read_sf('Input/shapes logística/hidrovias/Hidrovias.shp') %>% 
  dplyr::filter(cla_icacao %in% c('Navegável', 'Navegação sazonal'))
sf_use_s2(FALSE)
shape.hidrovia <- st_intersection(shape.estad.amzl,shape.hidrovia)



# IMPORTANTE: avisar que faltam as coordenadas de mais da metade das escolas!!!
# Não usar no produto final.
# Escolas em 2009 
base.2009 <- read.csv('Outputs/00_shapes_e_dados/00_base_escolas_2009_2020.csv') %>% 
  dplyr::filter(ano == 2009)
pontos.2009 <- left_join(base.2009, pontos.escolas, by = c('id_escola' = 'id_esc'))   


pontos.esc.2009 <- ggplot() +
  geom_sf(data = shape.estad.amzl, aes(geometry = geometry)) +
  geom_sf(data = shape.hidrovia, aes(col = 'Hidrovias navegáveis'), size = 0.5, show.legend = 'line') +
  geom_sf(data = pontos.2009, aes(geometry = geometry, col = 'Escolas'), stat = "sf_coordinates", size = .2, show.legend = 'point') +
  scale_color_manual(values = c("Escolas" = "#99d8c9", "Hidrovias navegáveis" = "#2b8cbe"),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("blank", "solid"),
                                                              shape=c(16, NA)))) +
  ggtitle('Escolas em 2009') +
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))
  


# Escolas em 2019 
base.2019 <- read.csv('Outputs/00_shapes_e_dados/00_base_escolas_2009_2020.csv') %>% 
  dplyr::filter(ano == 2019)
pontos.2019 <- left_join(base.2019, pontos.escolas, by = c('id_escola' = 'id_esc'))   


pontos.esc.2019 <- ggplot() +
  geom_sf(data = shape.estad.amzl, aes(geometry = geometry)) +
  geom_sf(data = shape.hidrovia, aes(col = 'Hidrovias navegáveis'), size = 0.5, show.legend = 'line') +
  geom_sf(data = pontos.2019, aes(geometry = geometry, col = 'Escolas'), stat = "sf_coordinates", size = .2, show.legend = 'point') +
  scale_color_manual(values = c("Escolas" = "#99d8c9", "Hidrovias navegáveis" = "#2b8cbe"),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("blank", "solid"),
                                                              shape=c(16, NA)))) +
  ggtitle('Escolas em 2019') +
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))


pontos.esc.2009|pontos.esc.2019

ggsave('Outputs/03_mapas/Educação/03_escolas_pontos_amzl_não_usar.png', scale = 2)


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
    muni = 'Município',
    matr_100_mil_2009 = 'Matrículas em escolas públicas a cada 100 mil \n habitantes em 2009',
    matr_100_mil_2019 = 'Matrículas em escolas públicas a cada 100 mil \n habitantes em 2019',
    var_matr = 'Variação - Matrículas',
    escol_100_mil_2009 = 'Escolas públicas a cada 100 mil \n habitantes em 2009',
    escol_100_mil_2019 = 'Escolas públicas a cada 100 mil \n habitantes em 2019',
    var_escol = 'Variação - Escolas'
  ) %>% 
  tab_header(
    title = 'Quantidade de matrículas em escolas públicas e quantidade de escolas públicas a cada 100 mil habitantes \n nas cidades intermediadoras da Amazônia Legal',
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
  tab_source_note('Fonte: Dados do Censo Escolar (INEP) via Carabetta, João; Dahis, Ricardo; Israel, Fred; Scovino, Fernanda (2020) \n Base dos Dados: Repositório de Dados Abertos em https://basedosdados.org.')


tabela.educacao

gtsave(tabela.educacao, 'Outputs/03_mapas/Educação/03_tabela_educacao.png')


# Gráficos Altamira (exemplo)
cidade <- tabela %>% 
  dplyr::filter(cod_muni == 1500602) # altamira

a <- ggplot(cidade, aes(x = ano, y = qtd_escolas)) +
  geom_line(color = 'blue', group = 1) +
  scale_y_continuous(name = 'Quantidade de Escolas Públicas', n.breaks = 6) +
  scale_x_continuous(name = 'Ano', n.breaks = 7) +
  theme_classic() 

b <- ggplot(cidade, aes(x = ano, y = qtd_matriculas)) +
  geom_line(color = 'blue', group = 1) +
  scale_y_continuous(name = 'Quantidade de Matrículas \n em escolas públicas', n.breaks = 6) +
  scale_x_continuous(name = 'Ano', n.breaks = 7) +
  theme_classic() 

c <- ggplot(cidade, aes(x = ano, y = matr_100_mil)) +
  geom_line(color = 'blue', group = 1) +
  scale_y_continuous(name = 'Matrículas em escolas públicas \n a cada 100 mil habitantes', n.breaks = 6) +
  scale_x_continuous(name = 'Ano', n.breaks = 7) +
  theme_classic() 

d <- ggplot(cidade, aes(x = ano, y = escol_100_mil)) +
  geom_line(color = 'blue', group = 1) +
  scale_y_continuous(name = 'Escolas públicas \n a cada 100 mil habitantes', n.breaks = 6) +
  scale_x_continuous(name = 'Ano', n.breaks = 7) +
  theme_classic() 

(a|b)/
(c|d)

ggsave('Outputs/03_mapas/Educação/03_grafico_altamira.png')




# tabela por faixas - Matrículas 2009 e 2019
matr.tab.2009 <- dados.2009 %>% 
  select('cod_muni','muni','matr_100_mil','class_matr_100_mil')
matr.tab.2019 <- dados.2019 %>% 
  select('cod_muni','muni','matr_100_mil','class_matr_100_mil')
matr.2009.2019 <- left_join(matr.tab.2009, matr.tab.2019, by = c('cod_muni','muni')) %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras) %>% 
  arrange(desc(matr_100_mil.x))


tabela.educacao <- gt(matr.2009.2019) %>%
  cols_label(
    muni = 'Município',
    matr_100_mil.x = 'Matrículas em escolas públicas a cada 100 mil \n habitantes em 2009',
    class_matr_100_mil.x = 'Classificação das matrículas \n em 2009',
    matr_100_mil.y = 'Matrículas em escolas públicas a cada 100 mil \n habitantes em 2019',
    class_matr_100_mil.y = 'Classificação das matrículas \n em 2019'
  ) %>% 
  tab_header(
    title = 'Quantidade de matrículas em escolas públicas a cada 100 mil habitantes \n nas cidades intermediadoras da Amazônia Legal',
    subtitle = '2009 e 2019'
  ) %>%
  cols_hide(
    columns = cod_muni
  ) %>% 
  fmt_markdown(
    columns = c(muni, class_matr_100_mil.x, class_matr_100_mil.y)
  ) %>% 
  fmt_number(
    columns = c(matr_100_mil.x, matr_100_mil.y),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elaboração própria. Censo Escolar (INEP) via Carabetta, João; Dahis, Ricardo; Israel, Fred; Scovino, Fernanda (2020) \n Base dos Dados: Repositório de Dados Abertos em https://basedosdados.org.')


tabela.educacao

gtsave(tabela.educacao, 'Outputs/03_mapas/Educação/03_tabela_educacao_matric.png')


# tabela por faixas - Escolas 2009 e 2019
escol.tab.2009 <- dados.2009 %>% 
  select('cod_muni','muni','escol_100_mil','class_escol_100_mil')

escol.tab.2019 <- dados.2019 %>% 
  select('cod_muni','muni','escol_100_mil','class_escol_100_mil')

escol.2009.2019 <- left_join(escol.tab.2009, escol.tab.2019, by = c('cod_muni','muni')) %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras) %>% 
  arrange(desc(escol_100_mil.x))


tabela.educacao <- gt(escol.2009.2019) %>%
  cols_label(
    muni = 'Município',
    escol_100_mil.x = 'Escolas públicas a cada 100 mil\nhabitantes em 2009',
    class_escol_100_mil.x = 'Classificação das escolas\npúblicas  em 2009',
    escol_100_mil.y = 'Escolas públicas a cada 100 mil\nhabitantes em 2019',
    class_escol_100_mil.y = 'Classificação das escolas públicas\nem 2019'
  ) %>% 
  tab_header(
    title = 'Quantidade de escolas públicas a cada 100 mil habitantes \n nas cidades intermediadoras da Amazônia Legal',
    subtitle = '2009 e 2019'
  ) %>%
  cols_hide(
    columns = cod_muni
  ) %>% 
  fmt_markdown(
    columns = c(muni, class_escol_100_mil.x, class_escol_100_mil.y)
  ) %>% 
  fmt_number(
    columns = c(escol_100_mil.x, escol_100_mil.y),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Dados do Censo Escolar (INEP) via Carabetta, João; Dahis, Ricardo; Israel, Fred; Scovino, Fernanda (2020) \n Base dos Dados: Repositório de Dados Abertos em https://basedosdados.org.')


tabela.educacao

gtsave(tabela.educacao, 'Outputs/03_mapas/Educação/03_tabela_educacao_escol.png')


# CAMPI IES Federais
# Fonte: http://www.litoral.ufpr.br/portal/observatoriolitoral/geodados/arquivos-shapefile/
# http://www.litoral.ufpr.br/portal/observatoriolitoral/wp-content/uploads/sites/20/2019/09/DADOS-GEORREFERENCIADOS-DOS-CAMPI-DAS-UNIVERSIDADES-FEDERAIS-COMO-INSTRUMENTO-DE-GEST%C3%83O-DA-INFORMA%C3%87%C3%83O-PELO-FORCAMPI.pdf

x <- st_read('Input/shape IES/334-IFES-shape-e-kml/Forcampi_c_Litoral_Final.shp')
shape.estad <- st_read('Outputs/00_shapes_e_dados/shape.estad.shp')


ggplot() +
  geom_sf(data = shape.estad, aes(geometry = geometry), fill = NA) +
  geom_sf(data = x, aes(geometry = geometry, 
                        col = 'Campi e multicampi das Universidades Federais'), 
          size = .7, 
          show.legend = 'point') +
  scale_color_manual(values = c("Campi e multicampi das Universidades Federais" = "#31a354"),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("blank"),
                                                              shape=c(16)))) +
  labs(x = NULL, y = NULL) +
  ggtitle('Campi e multicampi das Universidades Federais em 2018') +
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 14))

ggsave('Outputs/03_mapas/Educação/03_mapa_campi_IES.png', width = 10, height = 8, dpi = 300, units = "in", device='png')  






