rm(list=ls()) # limpar as variáveis carregadas
# REGIC Mapas estáticos
source('Rscripts/00_funcoes_globais.R')
source('Rscripts/00_variaveis_globais.R')
# Carregar bibliotecas
library(geobr)
library(sf)
library(tidyverse)
library(readxl)
library(nngeo) # pacote para remover buracos quando uso st_union em shapefiles
library(ggspatial)
library(RColorBrewer)
library(ggrepel) # impede que as labels do ggplot se sobreponham
library(stringr) # para usar a função str_replace_all()
library(sidrar)

options(scipen=999)
sf_use_s2(F) # permite que o sf fucione como na versão anterior

# Baixar mapa estados
shape.estados <- read_state(code_state = 'all')

# Importar shapefiles da REGIC
ligacoes.regic <- st_read("Input/REGIC2018_ligacoes_cidades/REGIC2018_Ligacoes_entre_Cidades.shp", options = "ENCODING=UTF-8")
cidades.ponto <- st_read("Input/REGIC2018_cidades_ponto/REGIC2018_Cidades_ponto.shp", options = "ENCODING=UTF-8")

amzl <- read_amazon()
uf.amzl <- c('RO','AM','AC','RO','RR','PA','MA','AP','TO','MT')

########## ligações de saúde de alta complexidade
ligacoes.s <- ligacoes.regic %>% 
  dplyr::filter(uf_dest %in% uf.amzl,
                quest_4 > 0)

pontos <- cidades.ponto %>% 
  dplyr::filter(uf %in% uf.amzl)

shape.selec <- st_intersection(amzl, shape.estados)


# pegar dados do IBGE
info_sidra(1384) 
dados.norte <- get_sidra(x = 1384, 
                         geo = 'City',
                         geo.filter = list("Region" = 1),
                         variable = 1000140)

dados.nordeste <- get_sidra(x = 1384, 
                            geo = 'City',
                            geo.filter = list("Region" = 2),
                            variable = 1000140)

dados.co <- get_sidra(x = 1384, 
                      geo = 'City',
                      geo.filter = list("Region" = 5),
                      variable = 1000140)

dados.ibge <- rbind(dados.norte,dados.nordeste,dados.co)

x <- read_municipality()

ate.um.sm <- dados.ibge %>% 
  janitor::clean_names() %>% 
  dplyr::filter(classes_de_rendimento_nominal_mensal_codigo %in% c(92980,92973,92974),
                municipio_codigo %in% cidades.amazonia.legal) %>% 
  unique() %>% 
  select(7,6,9,13,4,5) %>% 
  group_by(municipio, municipio_codigo) %>% 
  summarise(percent_ate_1_sm = sum(valor)) %>%
  mutate(municipio_codigo = as.numeric(municipio_codigo)) %>% 
  left_join(x, by = c('municipio_codigo'='code_muni')) 

mycolors <- brewer.pal(9,'Greens')

saude <- ggplot() +
  geom_sf(data = shape.selec) +
  geom_sf(data = ate.um.sm, aes(fill = percent_ate_1_sm, geometry = geom), color = NA) +
  geom_sf(data = ligacoes.s, aes(color = quest_4)) +
  geom_sf(data = pontos, size = .1) +
  scale_color_manual(values = c("#f03b20","#feb24c","#ffeda0")) + # cor das ligações
  scale_fill_gradientn(colors = mycolors,
                       breaks = c(40,60,80,100),
                       limits = c(40,100),
                       labels = c("40","60","80","100")
                       ) +
  labs(fill = 'Percentual com\nrenda até 1 SM')+
  annotation_scale(location = 'br') +
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering()) +
  theme_classic() + # retira o grid e coloca o fundo branco
  labs(color = 'Ordem de\nfrequência') +
  theme(legend.position = 'bottom',
        legend.title = element_text(vjust = 1)) # ajuste na altura da legenda
 

saude
ggsave(saude,filename = 'Outputs/03_mapas/Saúde/regic_alta_complexidade.png', width = 9, height = 6)


dest.saude <- ligacoes.s %>% 
  select('nome_dest') %>% 
  count(nome_dest)

########### ligações de transporte público
ligacoes.tp <- ligacoes.regic %>% 
  dplyr::filter(uf_dest %in% uf.amzl,
                quest_10 > 0)

ggplot() +
  geom_sf(data = amzl) +
  geom_sf(data = ligacoes.tp) +
  geom_sf(data = pontos, size = .1)



############ ligações agropecuárias de qualquer tipo para culturas selecionadas da sociobioeconomia
# obs: Alguns shapes estão faltando.

# criar uma variável chave para ligar shape e dados (match)
x <- read_country()
m <- read_municipality() %>% 
  mutate(code_muni = as.character(code_muni))
# Dados do valor da produção de culturas selecionadas
# valores em mil reais
prod.agro.censo <- read_xlsx("Input/censoagro.xlsx", skip = 5)# dados de 2017
pevs <- read_xlsx("Input/pevs.xlsx", skip = 5) # dados de 2020


prod.cacau <- prod.agro %>% 
  slice_max(order_by = cacau, n = 30)
  
# ligações
ligacoes.agro <- ligacoes.regic %>% 
  dplyr::filter(uf_ori %in% uf.amzl,
                agro > 0) %>% 
  mutate(match = as.numeric(paste0(cod_ori,cod_dest))) %>% 
  select('match')

# ligações
dados.agro <- read_xlsx('Input/REGIC2018 - Fluxos agropecuários por produto e Municipio.xlsx', sheet = 5) %>% 
  janitor::clean_names() %>%   
  dplyr::filter(produto %in% 'Cacau',
                uf_origem %in% uf.amzl) %>%    
  mutate(match = as.numeric(paste0(mun_origem,mun_destino))) %>% 
  left_join(ligacoes.agro)    
  
ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.cacau, aes(fill = cacau, geometry = geom), colour = NA) +
  geom_sf(data = dados.agro, aes(geometry = geometry)) 

# Açaí
prod.acai <- prod.agro %>% 
  slice_max(order_by = acai, n = 30)

dados.agro <- read_xlsx('Input/REGIC2018 - Fluxos agropecuários por produto e Municipio.xlsx', sheet = 5) %>% 
  janitor::clean_names() %>%   
  dplyr::filter(produto %in% 'Açaí',
                uf_origem %in% uf.amzl) %>%    
  mutate(match = as.numeric(paste0(mun_origem,mun_destino))) %>% 
  left_join(ligacoes.agro)  

ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.acai, aes(fill = acai, geometry = geom), colour = NA) +
  geom_sf(data = dados.agro, aes(geometry = geometry)) 

# Cupuaçu
prod.cupuacu <- prod.agro %>% 
  slice_max(order_by = cupuacu, n = 30)

dados.agro <- read_xlsx('Input/REGIC2018 - Fluxos agropecuários por produto e Municipio.xlsx', sheet = 5) %>% 
  janitor::clean_names() %>%   
  dplyr::filter(produto %in% 'Cupuaçu',
                uf_origem %in% uf.amzl) %>%    
  mutate(match = as.numeric(paste0(mun_origem,mun_destino))) %>% 
  left_join(ligacoes.agro)  

ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.acai, aes(fill = cupuacu, geometry = geom), colour = NA) +
  geom_sf(data = dados.agro, aes(geometry = geometry, color = percentual)) 

# Babaçu
prod.babacu <- prod.agro %>% 
  slice_max(order_by = babacu, n = 30)

dados.agro <- read_xlsx('Input/REGIC2018 - Fluxos agropecuários por produto e Municipio.xlsx', sheet = 5) %>% 
  janitor::clean_names() %>%   
  dplyr::filter(produto %in% 'Babaçu',
                uf_origem %in% uf.amzl) %>%    
  mutate(match = as.numeric(paste0(mun_origem,mun_destino))) %>% 
  left_join(ligacoes.agro)  

ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.babacu, aes(fill = babacu, geometry = geom), colour = NA) +
  geom_sf(data = dados.agro, aes(geometry = geometry, color = percentual))

# Catanha-do-pará
prod.para <- prod.agro %>% 
  slice_max(order_by = castanha_do_para, n = 30)

dados.agro <- read_xlsx('Input/REGIC2018 - Fluxos agropecuários por produto e Municipio.xlsx', sheet = 5) %>% 
  janitor::clean_names() %>%   
  dplyr::filter(produto %in% 'Castanha-do-pará',
                uf_origem %in% uf.amzl) %>%    
  mutate(match = as.numeric(paste0(mun_origem,mun_destino))) %>% 
  left_join(ligacoes.agro)  

ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.para, aes(fill = castanha_do_para, geometry = geom), colour = NA) +
  geom_sf(data = dados.agro, aes(geometry = geometry))




# Mapa conjunto


# prod agro
prod.agro <- left_join(prod.agro.censo, pevs) %>% 
  select(1,2,3,4,6,7,8) %>% 
  mutate(cacau = as.numeric(cacau),
         cupuacu = as.numeric(cupuacu),
         acai = as.numeric(acai),
         castanha_do_para = as.numeric(castanha_do_para),
         babacu = as.numeric(babacu)
  ) %>% 
  replace(is.na(.), 0) %>% 
  left_join(m, by = c('cod_muni'='code_muni')) 

prod.cacau <- prod.agro %>% 
  dplyr::filter(cacau > 0) %>%  
  classificar.variavel('cacau','cacau_cat') %>% 
  mutate(cacau_cat = as.factor(cacau_cat))
prod.cacau$cacau_cat <- factor(prod.cacau$cacau_cat, levels = c('Muito Baixo','Baixo','Médio Baixo','Médio Alto','Alto','Muito Alto'))

prod.acai <- prod.agro %>% 
  dplyr::filter(acai > 0) %>%  
  classificar.variavel('acai','acai_cat') %>% 
  mutate(acai_cat = as.factor(acai_cat))
prod.acai$acai_cat <- factor(prod.acai$acai_cat, levels = c('Muito Baixo','Baixo','Médio Baixo','Médio Alto','Alto','Muito Alto'))

prod.cupuacu <- prod.agro %>% 
  dplyr::filter(cupuacu > 0) %>%  
  classificar.variavel('cupuacu','cupuacu_cat') %>% 
  mutate(cupuacu_cat = as.factor(cupuacu_cat))
prod.cupuacu$cupuacu_cat <- factor(prod.cupuacu$cupuacu_cat, levels = c('Muito Baixo','Baixo','Médio Baixo','Médio Alto','Alto','Muito Alto'))

prod.castanha_do_para <- prod.agro %>% 
  dplyr::filter(castanha_do_para > 0) %>%  
  classificar.variavel('castanha_do_para','castanha_do_para_cat') %>% 
  mutate(castanha_do_para_cat = as.factor(castanha_do_para_cat))
prod.castanha_do_para$castanha_do_para_cat <- factor(prod.castanha_do_para$castanha_do_para_cat, levels = c('Muito Baixo','Baixo','Médio Baixo','Médio Alto','Alto','Muito Alto'))

library(patchwork)
# Castanha-do-pará
a <- ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.castanha_do_para, aes(fill = castanha_do_para_cat, geometry = geom), colour = NA) +
  ggtitle('Castanha-do-pará')+
  scale_fill_manual(values = brewer.pal(6, "Greens")) + 
  theme_classic()+
  labs(fill = 'Valor da produção') +
  theme(plot.title = element_text(hjust = 0.5))+
  annotation_scale(location = 'br') +
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(.25, "cm"),
                         pad_x = unit(-.12, "cm"))


# Cacau
b <- ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.cacau, aes(fill = cacau_cat, geometry = geom), colour = NA) +
  ggtitle('Cacau')+
  scale_fill_manual(values = brewer.pal(6, "Greens")) + 
  theme_classic()+
  labs(fill = 'Valor da produção') +
  theme(plot.title = element_text(hjust = 0.5))+
  annotation_scale(location = 'br') +
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(.25, "cm"),
                         pad_x = unit(-.12, "cm"))

# Cupuaçu
c <- ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.cupuacu, aes(fill = cupuacu_cat, geometry = geom), colour = NA) +
  ggtitle('Cupuaçu')+
  scale_fill_manual(values = brewer.pal(6, "Greens")) + 
  theme_classic()+
  labs(fill = 'Valor da produção') +
  theme(plot.title = element_text(hjust = 0.5))+
  annotation_scale(location = 'br') +
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(.25, "cm"),
                         pad_x = unit(-.12, "cm"))

# Açaí
d <- ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.acai, aes(fill = acai_cat, geometry = geom), colour = NA) +
  ggtitle('Açaí')+
  scale_fill_manual(values = brewer.pal(6, "Greens")) + 
  theme_classic()+
  labs(fill = 'Valor da produção') +
  theme(plot.title = element_text(hjust = 0.5))+
  annotation_scale(location = 'br') +
  annotation_north_arrow(location='tl', 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         pad_y = unit(.25, "cm"),
                         pad_x = unit(-.12, "cm"))
                         


(d|b)/
(c|a) + plot_layout(guides = 'collect') & theme(legend.position = 'bottom') # precisa usar o & nesse caso para mandar a legenda para baixo


# salva o mapa Cacau
ggsave(path = 'Outputs/03_mapas/bioeconomia/', filename = 'bioeconomia_amzl.png', width = 9, height = 6)


































cadeias_regic <- function(cultura){
    for(i in cultura) {
# incluir mecanismo para passar mais de uma cultura de uma vez
label.cultura <- i

ligacoes.agro <- ligacoes.regic %>% 
   dplyr::filter(uf_ori %in% uf.amzl,
                 agro > 0) %>% 
  mutate(match = as.numeric(paste0(cod_ori,cod_dest))) %>% 
  select('match')

# Dados
dados.agro <- read_xlsx('Input/REGIC2018 - Fluxos agropecuários por produto e Municipio.xlsx', sheet = 5) %>% 
  janitor::clean_names() %>% 
  dplyr::filter(produto %in% i,
                uf_origem %in% uf.amzl) %>%   
  mutate(match = as.numeric(paste0(mun_origem,mun_destino))) %>% 
  left_join(ligacoes.agro) 


mapa <- ggplot() +
  geom_sf(data = x) +
  geom_sf(data = prod.agro, aes(fill = acai, geometry = geom), colour = NA) +
  geom_sf(data = dados.agro, aes(geometry = geometry, color = percentual)) 


arquivo.graf.nom <- paste('Mapa',label.cultura,'.png')

# salva o mapa
ggsave(plot = mapa, path = 'Outputs/03_mapas/bioeconomia/', filename = arquivo.graf.nom, width = 9, height = 6)


  }
}

#debug(cadeias_regic)
cadeias_regic('Açaí')

# Açaí, cacau, castanha do pará, cupuaçu, babaçu
culturas <- c('Cacau','Açaí','Cupuaçu','Babaçu','Castanha-do-pará')
cadeias_regic(culturas)


ggplot() +
  geom_sf(data = x) + 
  geom_sf(data = prod.agro, aes(fill = cacau, geometry = geom), colour = NA)
  
  



# incluir dados do censo agro no mapa. Regic tem apenas os principais.

# PEVS https://sidra.ibge.gov.br/tabela/289



# ver Censo Escolar e alunos de outros municípios.







