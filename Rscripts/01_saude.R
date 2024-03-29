# Dados da Sa�de
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# 1 - CNES-ST - Dados dos estabelecimentos
# estabelecimentos de sa�de 

# baixar dados de interna��es pelo SIH e cruzar com geobr
# https://github.com/rfsaldanha/microdatasus
# https://www.scielo.br/j/csp/a/gdJXqcrW5PPDHX8rwPDYL7F/?lang=pt#ModalFigf1 explica��o do pacote!
# https://bigdata-metadados.icict.fiocruz.br/dataset/cadastro-nacional-de-estabelecimentos-de-saude-cnes/resource/7bcf4f68-f2e9-4e06-87b5-229358702efc
# TP_UNID  ver as legendas em http://cnes2.datasus.gov.br/Mod_Ind_Unidade.asp?VEstado=00

#  n�vel AMBULATORIAL: consultas, exames, atendimentos de urg�ncia e emerg�ncia limitados a interna��es de 12 horas (?)
# GESPRG1E  aten��o b�sica Estadual = 1
# GESPRG1M  aten��o b�sica Municipal = 1
# GESPRG2E  m�dia complexidade estadual = 1
# GESPRG2M  m�dia complexidade municipal = 1
# GESPRG4E  Alta complexidade estadual = 1
# GESPRG4M  Alta complexidade municipal = 1
# 
# # n�vel HOSPITALAR: interna��es e procedimentos hospitalares
# GESPRG5E  m�dia complexidade estadual = 1
# GESPRG5M  m�dia complexidade municipal = 1
# GESPRG6E  Alta complexidade estadual = 1
# GESPRG6M  Alta complexidade municipal = 1

# Dados de dezembro de 2015
x <- read_csv('Outputs/00_shapes_e_dados/00_cnes_st_2015.csv', col_types = c('c',rep('n',3),rep('c',3),rep('n',13)))
shape.cnes <- st_read('Outputs/00_shapes_e_dados/shape.cnes.amzl.shp')
shape.cnes$cd_cnes <- as.character(shape.cnes$cd_cnes)

pontos.cnes <- left_join(x, shape.cnes, by=c('CNES' = 'cd_cnes')) %>%    
  dplyr::filter(VINC_SUS == 1)

atend.hospitalar <- pontos.cnes %>% 
  dplyr::filter(NIVATE_H == 1) # Tem atendimento hospitalar municipal ou estadual?

atend.ambulat <- pontos.cnes %>% 
  dplyr::filter(NIVATE_A == 1) # Tem atendimento ambulatorial municipal ou estadual?
 
atend.urgemerg <- pontos.cnes %>% 
  dplyr::filter(URGEMERG == 1) # Indica a exist�ncia de INSTALA��O F�SICA de URG�NCIA/EMERG�NCIA

sum(is.na(pontos.cnes$cd_mn)) # 36 estabelecimentos sem coordenadas

# As coordenadas s�o de 2015, por isso os dados devem ser do mesmo ano.

# mapa
shape.muni.amzl <- st_read('Outputs/00_shapes_e_dados/shape.muni.amzl.shp')
shape.estad.amzl <- st_read('Outputs/00_shapes_e_dados/shape.estad.amzl.shp')
shape.hidrovia <- read_sf('Input/shapes log�stica/hidrovias/Hidrovias.shp') %>% 
  dplyr::filter(cla_icacao %in% c('Naveg�vel', 'Navega��o sazonal'))
sf_use_s2(FALSE)
shape.hidrovia <- st_intersection(shape.estad.amzl,shape.hidrovia)

cor.estab <- "#636363"
cor.hidrov <- "#bdbdbd"

# Todos os casos tem v�nculo com sus
# dados de 2015 dos pontos
# CNES com v�nculo com SUS
a <- ggplot() +
  geom_sf(data = shape.estad.amzl, aes(geometry = geometry), fill = NA) +
  geom_sf(data = shape.hidrovia, aes(col = 'Hidrovias naveg�veis'), size = 0.4, show.legend = 'line') +
  geom_sf(data = pontos.cnes, aes(geometry = geometry, col = 'Estabelecimentos\nde sa�de'), stat = "sf_coordinates", size = .1, show.legend = 'point') +
  scale_color_manual(values = c("Estabelecimentos\nde sa�de" = alpha(cor.estab, 0.7), "Hidrovias naveg�veis" = cor.hidrov),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("blank", "solid"),
                                                              shape=c(16, NA)))) +
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  #ggtitle('Estabelecimentos de sa�de vinculados\nao SUS em 2015') +
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))

b <- ggplot() +
  geom_sf(data = shape.estad.amzl, aes(geometry = geometry), fill = NA) +
  geom_sf(data = shape.hidrovia, aes(col = 'Hidrovias naveg�veis'), size = 0.4, show.legend = 'line') +
  geom_sf(data = atend.hospitalar, aes(geometry = geometry, col = 'Estabelecimento com\natendimento hospitalar'), stat = "sf_coordinates", size = .1, show.legend = 'point') +
  scale_color_manual(values = c("Estabelecimento com\natendimento hospitalar" = alpha(cor.estab,0.7), "Hidrovias naveg�veis" = cor.hidrov),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("blank", "solid"),
                                                              shape=c(16, NA)))) +
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  #ggtitle('Estabelecimentos de sa�de vinculados ao SUS\ncom atendimento hospitalar em 2015') +
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))

c <- ggplot() +
  geom_sf(data = shape.estad.amzl, aes(geometry = geometry), fill = NA) +
  geom_sf(data = shape.hidrovia, aes(col = 'Hidrovias naveg�veis'), size = 0.4, show.legend = 'line') +
  geom_sf(data = atend.ambulat, aes(geometry = geometry, col = 'Estabelecimento com\natendimento ambulatorial'), stat = "sf_coordinates", size = .1, show.legend = 'point') +
  scale_color_manual(values = c("Estabelecimento com\natendimento ambulatorial" = alpha(cor.estab,0.7), "Hidrovias naveg�veis" = cor.hidrov),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("blank", "solid"),
                                                              shape=c(16, NA)))) +
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  #ggtitle('Estabelecimentos de sa�de vinculados ao SUS\ncom atendimento ambulatorial em 2015') +
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))

d <- ggplot() +
  geom_sf(data = shape.estad.amzl, aes(geometry = geometry), fill = NA) +
  geom_sf(data = shape.hidrovia, aes(col = 'Hidrovias naveg�veis'), size = 0.4, show.legend = 'line') +
  geom_sf(data = atend.urgemerg, aes(geometry = geometry, col = 'Estabelecimento com atendimento\nde urg�ncia e emerg�ncia'), stat = "sf_coordinates", size = .1, show.legend = 'point') +
  scale_color_manual(values = c("Estabelecimento com atendimento\nde urg�ncia e emerg�ncia" = alpha(cor.estab,0.7), "Hidrovias naveg�veis" = cor.hidrov),
                     name = NULL,
                     guide = guide_legend(override.aes = list(linetype=c("blank", "solid"),
                                                              shape=c(16, NA)))) +
  labs(x = NULL, y = NULL) + #Muda o nome da legenda com o fill.
  #ggtitle('Estabelecimentos de sa�de vinculados ao SUS\ncom atendimento de urg�ncia e emerg�ncia em 2015') +
  coord_sf(crs = 4674) +
  annotation_scale(location = 'br')+
  annotation_north_arrow(location = 'tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5))

(a|c)/
(b|d)

ggsave('Outputs/03_mapas/Sa�de/03_cnes_pontos_amzl_2015_grey.png', scale = 1.3, width = 9, height = 6, dpi = 600)

# POR TIPO DE ATENDIMENTO
# atendimento de urg�ncia e emerg�ncia
urge.e.emerg <- atend.urgemerg %>% 
  select(CODUFMUN, URGEMERG) %>% 
  group_by(CODUFMUN) %>% 
  count(URGEMERG) 

# aten��o b�sica
saude.basica <- pontos.cnes %>% 
  dplyr::filter(GESPRG1E == 1 | GESPRG1M == 1) %>% 
  mutate(saude_basica = ifelse(GESPRG1E + GESPRG1M >= 1, 1, 0)) %>% 
  select(CODUFMUN, saude_basica) %>% 
  group_by(CODUFMUN) %>% 
  count(saude_basica) 
  
# M�dia complexidade ambulatorial ou hospitalar
med.complex <- pontos.cnes %>% 
  dplyr::filter(GESPRG2E == 1 | GESPRG2M == 1 | GESPRG5E == 1 | GESPRG5M == 1) %>% 
  mutate(med_complex = ifelse((GESPRG2E + GESPRG2M + GESPRG5E + GESPRG5M) >= 1, 1, 0)) %>%
  select(CODUFMUN, med_complex) %>% 
  group_by(CODUFMUN) %>% 
  count(med_complex) 
  
# Alta complexidade ambulatorial ou hospitalar
alta.complex <- pontos.cnes %>% 
  dplyr::filter(GESPRG4E == 1 | GESPRG4M == 1 | GESPRG6E == 1 | GESPRG6M == 1) %>% 
  mutate(alta_complex = ifelse((GESPRG4E + GESPRG4M + GESPRG6E + GESPRG6M) >= 1, 1, 0)) %>%
  select(CODUFMUN, alta_complex) %>% 
  group_by(CODUFMUN) %>% 
  count(alta_complex) 

# Calcular a cada 100 mil habitantes
# importar dados de popula��o
pop.2010 <- read_excel('Input/tabela202.xlsx', skip = 4) %>% select(1,2,4)
pop.2005.2019 <- read_excel('Input/tabela6579.xlsx', skip = 3)

populacao <- left_join(pop.2005.2019, pop.2010) %>% 
  select(1:10,21,11:20) 

colnames(populacao)[1] <- 'cod_muni'
colnames(populacao)[2] <- 'muni'

populacao <- populacao %>% 
  dplyr::filter(cod_muni %in% cidades.amazonia.legal)

populacao[3:21] <- lapply(populacao[3:21], as.numeric) # mudar colunas para numeric

# descobrir quantos NAs existem em cada coluna do Dataframe
sapply(populacao, function(x) sum(is.na(x)))

# reorganizar dataframe como se fosse uma tabela din�mica
populacao <- populacao %>% 
  pivot_longer(!c(cod_muni,muni), names_to = "ano", values_to = "populacao")

populacao <- populacao %>% 
  dplyr::filter(ano == 2019)

populacao$cod_muni <-  str_sub(populacao$cod_muni, end=-2) # remove o �ltimo caractere do cod_muni
populacao$cod_muni <- as.numeric(populacao$cod_muni)


# Estabelecimentos de sa�de que atendem urg�ncia e emerg�ncia
urge.e.emerg <- left_join(urge.e.emerg, populacao, by = c('CODUFMUN'='cod_muni')) %>% 
  select(CODUFMUN,muni,n,populacao) %>% 
  na.omit() %>% 
  mutate(urgemerg_100_mil = (n/populacao) * 100000) %>% 
  classificar.variavel('urgemerg_100_mil','class_urgemerg') %>% 
  arrange(desc(urgemerg_100_mil))

# Estabelecimentos que atendem a n�vel de sa�de b�sica
saude.basica <- left_join(saude.basica, populacao, by = c('CODUFMUN'='cod_muni')) %>% 
  select(CODUFMUN,muni,n,populacao) %>% 
  na.omit() %>% 
  mutate(saude_basica_100_mil = (n/populacao) * 100000) %>% 
  classificar.variavel('saude_basica_100_mil','class_saude_basica') %>% 
  arrange(desc(saude_basica_100_mil))

# Estabelecimentos que atendem procedimentos de m�dia complexidade
med.complex <- left_join(med.complex, populacao, by = c('CODUFMUN'='cod_muni')) %>% 
  select(CODUFMUN,muni,n,populacao) %>% 
  na.omit() %>% 
  mutate(med_complex_100_mil = (n/populacao) * 100000) %>% 
  classificar.variavel('med_complex_100_mil','class_med_complex') %>% 
  arrange(desc(med_complex_100_mil))

# Estabelecimentos que atendem procedimentos de alta complexidade
alta.complex <- left_join(alta.complex, populacao, by = c('CODUFMUN'='cod_muni')) %>% 
  select(CODUFMUN,muni,n,populacao) %>% 
  na.omit() %>% 
  mutate(alta_complex_100_mil = (n/populacao) * 100000) %>% 
  classificar.variavel('alta_complex_100_mil','class_alta_complex') %>% 
  arrange(desc(alta_complex_100_mil))



# Mapa atendimento de alta complexidade
shape.muni.amzl$cd_mn <- as.numeric(str_sub(shape.muni.amzl$cd_mn, end = -2))
alta.complex.shape <- left_join(shape.muni.amzl, alta.complex, by = c('cd_mn' = 'CODUFMUN')) 
coord.cidades <- st_read('Outputs/00_shapes_e_dados/coord.cidades.shp')


# Fazer tabelas das intermedi�rias com gt table
urge.e.emerg.inter <- urge.e.emerg %>%   
  dplyr::filter(CODUFMUN %in% cidades.inter.6.dig) %>% 
  ungroup() %>% 
  select(muni, urgemerg_100_mil, class_urgemerg) %>% 
  arrange(desc(urgemerg_100_mil))
  
saude.basica.inter <- saude.basica %>%   
  dplyr::filter(CODUFMUN %in% cidades.inter.6.dig) %>% 
  ungroup() %>% 
  select(muni, saude_basica_100_mil, class_saude_basica) %>% 
  arrange(desc(saude_basica_100_mil))

med.complex.inter <- med.complex %>%   
  dplyr::filter(CODUFMUN %in% cidades.inter.6.dig) %>% 
  ungroup() %>% 
  select(muni, med_complex_100_mil, class_med_complex) %>% 
  arrange(desc(med_complex_100_mil))

alta.complex.inter <- alta.complex %>%   
  dplyr::filter(CODUFMUN %in% cidades.inter.6.dig) %>% 
  ungroup() %>% 
  select(muni, alta_complex_100_mil, class_alta_complex) %>% 
  arrange(desc(alta_complex_100_mil))


# Tabela urg�ncia e emerg�ncia
tabela.datasus <- gt(urge.e.emerg.inter) %>%
  cols_label(
    muni = 'Munic�pio',
    urgemerg_100_mil = 'Estabelecimentos com atendimento de urg�ncia e emerg�ncia \n para cada 100 mil habitantes',
    class_urgemerg = 'Classifica��o dos estabelecimentos com atendimento \n de urg�ncia e emerg�ncia'
  ) %>% 
  tab_header(
    title = 'Atendimento de urg�ncia e emerg�ncia nas cidades intermediadoras da Amaz�nia Legal',
    subtitle = '2019'
  ) %>% 
  fmt_markdown(
    columns = c(muni,class_urgemerg)
  ) %>% 
  fmt_number(
    columns = c(urgemerg_100_mil),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elabora��o pr�pria. SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pr�-processamento de microdados do Departamento de Inform�tica do SUS (DATASUS). Cad. Sa�de P�blica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019 . Available from http://ref.scielo.org/dhcq3y.')

tabela.datasus
gtsave(tabela.datasus, 'Outputs/03_mapas/Sa�de/03_tabela_urg_emerg.png')

# Tabela sa�de b�sica
tabela.datasus <- gt(saude.basica.inter) %>%
  cols_label(
    muni = 'Munic�pio',
    saude_basica_100_mil = 'Estabelecimentos com atendimento de sa�de b�sica \n para cada 100 mil habitantes',
    class_saude_basica = 'Classifica��o dos estabelecimentos com atendimento \n de sa�de b�sica'
  ) %>% 
  tab_header(
    title = 'Atendimento de sa�de b�sica nas cidades intermediadoras da Amaz�nia Legal',
    subtitle = '2019'
  ) %>% 
  fmt_markdown(
    columns = c(muni,class_saude_basica)
  ) %>% 
  fmt_number(
    columns = c(saude_basica_100_mil),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elabora��o pr�pria. SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pr�-processamento de microdados do Departamento de Inform�tica do SUS (DATASUS). Cad. Sa�de P�blica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019 . Available from http://ref.scielo.org/dhcq3y.')

tabela.datasus
gtsave(tabela.datasus, 'Outputs/03_mapas/Sa�de/03_tabela_saude_basica.png')


# Tabela atendimento de m�dia complexidade
tabela.datasus <- gt(med.complex.inter) %>%
  cols_label(
    muni = 'Munic�pio',
    med_complex_100_mil = 'Estabelecimentos com atendimento de m�dia complexidade \n para cada 100 mil habitantes',
    class_med_complex = 'Classifica��o dos estabelecimentos com atendimento \n de m�dia complexidade'
  ) %>% 
  tab_header(
    title = 'Atendimento de m�dia complexidade nas cidades intermediadoras da Amaz�nia Legal',
    subtitle = '2019'
  ) %>% 
  fmt_markdown(
    columns = c(muni,class_med_complex)
  ) %>% 
  fmt_number(
    columns = c(med_complex_100_mil),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elabora��o pr�pria. SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pr�-processamento de microdados do Departamento de Inform�tica do SUS (DATASUS). Cad. Sa�de P�blica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019 . Available from http://ref.scielo.org/dhcq3y.')

tabela.datasus
gtsave(tabela.datasus, 'Outputs/03_mapas/Sa�de/03_tabela_media_complex.png')


# Tabela atendimento de alta complexidade
tabela.datasus <- gt(alta.complex.inter) %>%
  cols_label(
    muni = 'Munic�pio',
    alta_complex_100_mil = 'Estabelecimentos com atendimento de alta complexidade \n para cada 100 mil habitantes',
    class_alta_complex = 'Classifica��o dos estabelecimentos com atendimento \n de alta complexidade'
  ) %>% 
  tab_header(
    title = 'Atendimento de alta complexidade nas cidades intermediadoras da Amaz�nia Legal',
    subtitle = '2019'
  ) %>% 
  fmt_markdown(
    columns = c(muni, class_alta_complex)
  ) %>% 
  fmt_number(
    columns = c(alta_complex_100_mil),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elabora��o pr�pria. SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pr�-processamento de microdados do Departamento de Inform�tica do SUS (DATASUS). Cad. Sa�de P�blica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019 . Available from http://ref.scielo.org/dhcq3y.')

tabela.datasus
gtsave(tabela.datasus, 'Outputs/03_mapas/Sa�de/03_tabela_alta_complex.png')




# 2 - Fazer consulta para a base CNES-LT para ver a quantidade de leitos
# explica��o leitos (porque escolhi a vari�vel QT_SUS, j� que os dados incluem a rede privada):
# https://wiki.saude.gov.br/cnes/index.php/Principais_Conceitos 
# Dados de dezembro de 2019

# AMZL
x <- read.csv('Outputs/00_shapes_e_dados/00_cnes_lt_2019.csv')

# N�o houve diferencia��o pelo tipo de leito (por isso cada estabelecimento tem mais de uma linha).
# Foram contabilizados apenas os leitos do SUS.
leitos <- read.csv('Outputs/00_shapes_e_dados/00_cnes_lt_2019.csv') %>%  
  left_join(populacao, by = c('CODUFMUN'='cod_muni')) %>%  
  select(CODUFMUN, muni, QT_SUS, populacao) %>% 
  group_by(CODUFMUN, muni, populacao) %>% 
  summarise(qtd_leitos_sus = sum(QT_SUS)) %>% 
  mutate(qtd_leitos_sus_100_mil = (qtd_leitos_sus/populacao)*100000) %>% 
  classificar.variavel('qtd_leitos_sus_100_mil','class_leitos_sus') %>% 
  arrange(desc(qtd_leitos_sus_100_mil)) %>% 
  na.omit() # remove aqueles munic�pios n�o est�o dentro da AMZL, apesar de parte do estado pertencer a AMZL
  




# Cidades intermediadoras
leitos.intermediadoras <- leitos %>% 
  ungroup(CODUFMUN, muni) %>% 
  dplyr::filter(CODUFMUN %in% cidades.inter.6.dig)

# tabela leitos
tabela.leitos <- gt(leitos.intermediadoras) %>%
  cols_label(
    muni = 'Munic�pio',
    qtd_leitos_sus_100_mil = 'Quantidade de leitos do SUS \n para cada 100 mil habitantes',
    class_leitos_sus = 'Classifica��o da quantidade de leitos do SUS'
  ) %>% 
  tab_header(
    title = 'Leitos do SUS nas cidades intermediadoras da Amaz�nia Legal',
    subtitle = '2019'
  ) %>% 
  cols_hide(
   columns = c(CODUFMUN, populacao, qtd_leitos_sus)
  ) %>% 
  fmt_markdown(
    columns = c(muni, class_leitos_sus)
  ) %>% 
  fmt_number(
    columns = c(qtd_leitos_sus_100_mil),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elabora��o pr�pria. SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pr�-processamento de microdados do Departamento de Inform�tica do SUS (DATASUS). Cad. Sa�de P�blica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019 . Available from http://ref.scielo.org/dhcq3y.')

tabela.leitos
gtsave(tabela.leitos, 'Outputs/03_mapas/Sa�de/03_tabela_leitos_sus.png')