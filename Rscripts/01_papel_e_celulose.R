# Papel e celulose
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# Empregos RAIS papel e celulose - v�nculos ativos em 31-12-2019
cnae.cidades <- read_csv('Outputs/00_shapes_e_dados/00_rais_ativos_2019.csv',
                         col_types = list(
                           id_municipio = 'n',
                           ano = 'n',
                           cnae_2 = 'c',
                           vinculos_ativos = 'n'))

which(is.na(cnae.cidades$vinculos_ativos)) # retorna a linha dos NAs caso existam
cnae.cidades$id_municipio <- as.numeric(cnae.cidades$id_municipio)

# CNAE: 17 FABRICA��O DE CELULOSE, PAPEL E PRODUTOS DE PAPEL
cnae.celulose <- cnae.cidades %>% 
  select(id_municipio,cnae_2,vinculos_ativos) %>% 
  filter(str_detect(cnae_2, "^17")) %>%  # filtrar empregos relacionados produ��o de papel e celulose
  group_by(id_municipio) %>% 
  summarise(empregos_celulose = sum(vinculos_ativos, na.rm = TRUE))

cnae.celulose$empregos_celulose <- as.numeric(cnae.celulose$empregos_celulose)

cnae.celulose <- left_join(cidades.amazonia.legal.nome, cnae.celulose, by = c('cod_muni'='id_municipio')) %>% 
  dplyr::filter(empregos_celulose > 0) %>% 
  arrange(desc(empregos_celulose))

# Classifica empregos de energia el�trica
cnae.celulose <- classificar.variavel(cnae.celulose, 'empregos_celulose', 'class_empregos_celulose')

cnae.celulose.inter <- cnae.celulose %>%
  dplyr::filter(cod_muni %in% cidades.intermediadoras)

x <- cnae.celulose %>% 
  group_by(class_empregos_celulose) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# Manaus, Imperatriz, Almeirim (tr�s maiores munic�pios)