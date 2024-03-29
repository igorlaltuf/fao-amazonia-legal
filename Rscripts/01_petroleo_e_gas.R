# PETR�LEO - V�nculos ativos em 31-12-2019 por munic�pio e relacionados � petr�leo e g�s natural
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# Importar dados
cnae.cidades <- read_csv('Outputs/00_shapes_e_dados/00_rais_ativos_2019.csv',
                         col_types = list(
                           id_municipio = 'n',
                           ano = 'n',
                           cnae_2 = 'c',
                           vinculos_ativos = 'n'))

which(is.na(cnae.cidades$vinculos_ativos)) # retorna a linha dos NAs caso existam
cnae.cidades$id_municipio <- as.numeric(cnae.cidades$id_municipio)


# c�digo CNAE
# 06 Extra��o de petr�leo e g�s natural
cnae.petroleo <- cnae.cidades %>% 
  select(id_municipio,cnae_2,vinculos_ativos) %>% 
  filter(str_detect(cnae_2, "^06")) %>%  
  group_by(id_municipio) %>% 
  summarise(empregos_petroleo = sum(vinculos_ativos, na.rm = TRUE))

cnae.petroleo$empregos_petroleo <- as.numeric(cnae.petroleo$empregos_petroleo)

cnae.petroleo <- left_join(cidades.amazonia.legal.nome,cnae.petroleo, by = c('cod_muni'='id_municipio')) %>%  
  dplyr::filter(empregos_petroleo>0 &
                  cod_muni %in% cidades.amazonia.legal) %>% 
  arrange(desc(empregos_petroleo))

# Classifica empregos de petr�leo 
cnae.petroleo <- classificar.variavel(cnae.petroleo,'empregos_petroleo','class_empregos_petroleo')

x <- cnae.petroleo %>% 
  group_by(class_empregos_petroleo) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# Coari, Manaus e Santo Ant�nio dos Lopes