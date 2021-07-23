# Grandes e médias propriedades
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# 1 - Quantidade de estabelecimentos agropecuários com mais de 1.000 hectares em cada cidade (grandes propriedades).
grandes.propriedades <- read.csv(file = "Input/tabela6778-grandes.csv", sep = ";", dec = ",", fileEncoding = 'UTF-8-BOM')
grandes.propriedades <- grandes.propriedades %>% 
                        select(1,2,5,6) 
# Limpar dados
grandes.propriedades$quantidade_de_grandes_estabelecimentos <- as.character(grandes.propriedades$quantidade_de_grandes_estabelecimentos)
grandes.propriedades$quantidade_de_grandes_estabelecimentos[grandes.propriedades$quantidade_de_grandes_estabelecimentos %in% "-"] <- '0'
grandes.propriedades$quantidade_de_grandes_estabelecimentos <- as.numeric(grandes.propriedades$quantidade_de_grandes_estabelecimentos)

grandes.propriedades <- grandes.propriedades %>%
                        select(cod_muni,muni,quantidade_de_grandes_estabelecimentos) %>% 
                        group_by(cod_muni,muni) %>% 
                        summarise(total_grandes_estab = sum(quantidade_de_grandes_estabelecimentos, na.rm = TRUE)) %>% 
                        dplyr::filter(total_grandes_estab>0 &
                        cod_muni %in% cidades.amazonia.legal)

grandes.propriedades <- classificar.variavel(grandes.propriedades,'total_grandes_estab','class_total_grandes_estab')

# teste para ver quantos itens existem em cada categoria
x <- grandes.propriedades %>% 
     group_by(class_total_grandes_estab) %>%
     mutate(N_category = n()) %>%
     count(N_category)

# 2 - Quantidade de estabelecimentos agropecuários com tamanho entre de 200 a 1.000 hectares (médias propriedades).
medias.propriedades <- read.csv(file = "Input/tabela6778-medias.csv", sep = ";", dec = ",", fileEncoding = 'UTF-8-BOM')
medias.propriedades <- medias.propriedades %>% 
                        select(1,2,5,6) 
# Limpar dados
medias.propriedades$qtd_medios_estabelecimentos <- as.character(medias.propriedades$qtd_medios_estabelecimentos)
medias.propriedades$qtd_medios_estabelecimentos[medias.propriedades$qtd_medios_estabelecimentos %in% "-"] <- '0'
medias.propriedades$qtd_medios_estabelecimentos <- as.numeric(medias.propriedades$qtd_medios_estabelecimentos)

medias.propriedades <- medias.propriedades %>%
                       select(cod_muni,muni,qtd_medios_estabelecimentos) %>% 
                       group_by(cod_muni,muni) %>% 
                       summarise(total_medios_estab = sum(qtd_medios_estabelecimentos, na.rm = TRUE)) %>% 
                       dplyr::filter(total_medios_estab > 0 &
                       cod_muni %in% cidades.amazonia.legal)

medias.propriedades <- classificar.variavel(medias.propriedades,'total_medios_estab','class_total_medios_estab')

# teste para ver quantos itens existem em cada categoria
x <- medias.propriedades %>% 
  group_by(class_total_medios_estab) %>%
  mutate(N_category = n()) %>%
  count(N_category)

medios.e.grandes <- full_join(medias.propriedades,grandes.propriedades) 
medios.e.grandes$total_medios_estab[is.na(medios.e.grandes$total_medios_estab)] <- 0
medios.e.grandes$total_grandes_estab[is.na(medios.e.grandes$total_grandes_estab)] <- 0

medios.e.grandes <- medios.e.grandes %>% 
                    mutate(medios_e_grandes_estab = total_medios_estab + total_grandes_estab)
medios.e.grandes <- classificar.variavel(medios.e.grandes,'medios_e_grandes_estab','class_medios_e_grandes_estab')

x <- medios.e.grandes %>% 
      group_by(class_medios_e_grandes_estab) %>%
      mutate(N_category = n()) %>%
      count(N_category)

medios.e.grandes.inter <- full_join(cidades.amazonia.legal.nome,medios.e.grandes) %>% 
                    dplyr::filter(cod_muni %in% cidades.intermediadoras)

# Salvar arquivo
write.csv(medios.e.grandes,file='Outputs/01_tabelas/01_medios_e_grandes_estab_agropec.csv',na = '0')