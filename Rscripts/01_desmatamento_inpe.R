# Desmatamento nos municípios da AMZL -INPE
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar todos os arquivos 
lista.de.arquivos <- list.files(path = "Input/dados_desmatamento_inpe/", recursive = TRUE,
                                pattern = "\\.txt$", 
                                full.names = TRUE)

# importar arquivos do INPE de 2005 a 2020
ano <- c(2005:2020)
i <- 1
while (i<=length(lista.de.arquivos)) {
  assign(paste("desmatamento",ano[i], sep="."),read.delim(paste(lista.de.arquivos[i]), header = T, sep = ',', dec = '.'))  
  i <- i + 1
}

# juntar valores de 2005 a 2020 e selecionar colunas que vou usar
dados.inpe <- list(desmatamento.2005,desmatamento.2006,desmatamento.2007,desmatamento.2008,desmatamento.2009,desmatamento.2010,
                desmatamento.2011,desmatamento.2012,desmatamento.2013,desmatamento.2014,desmatamento.2015,desmatamento.2016,
                desmatamento.2017,desmatamento.2018,desmatamento.2019,desmatamento.2020) %>% 
                reduce(left_join, by = 'CodIbge') %>% 
                select('CodIbge','Municipio.x','Estado.x','AreaKm2.x','Desmatado2005','Desmatado2006','Desmatado2007','Desmatado2008',
                       'Desmatado2009','Desmatado2010','Desmatado2011','Desmatado2012','Desmatado2013','Desmatado2014','Desmatado2015',
                       'Desmatado2016','Desmatado2017','Desmatado2018','Desmatado2019','Desmatado2020','Hidrografia2005','Hidrografia2006',
                       'Hidrografia2007','Hidrografia2008','Hidrografia2009','Hidrografia2010','Hidrografia2011','Hidrografia2012',
                       'Hidrografia2013','Hidrografia2014','Hidrografia2015','Hidrografia2016','Hidrografia2017','Hidrografia2018',
                       'Hidrografia2019','Hidrografia2020')

colnames(dados.inpe) <- c('cod_muni','muni','uf','areakm2','desm_2005','desm_2006','desm_2007','desm_2008',
                            'desm_2009','desm_2010','desm_2011','desm_2012','desm_2013','desm_2014','desm_2015',
                            'desm_2016','desm_2017','desm_2018','desm_2019','desm_2020','hidro_2005','hidro_2006',
                            'hidro_2007','hidro_2008','hidro_2009','hidro_2010','hidro_2011','hidro_2012',
                            'hidro_2013','hidro_2014','hidro_2015','hidro_2016','hidro_2017','hidro_2018',
                            'hidro_2019','hidro_2020')
# desmatamento AMZL
desmatamento <- dados.inpe %>% 
                select(1:20) %>% 
                replace(is.na(.), 0) %>% 
                mutate(total_desm = rowSums(across(desm_2005:desm_2020))) %>% 
                dplyr::filter(total_desm>0)
              
desmatamento <- classificar.variavel(desmatamento,'total_desm','class_desmat')


x <- desmatamento %>% 
  group_by(class_desmat) %>%
  mutate(N_category = n()) %>%
  count(N_category)


# desmatamento nas regiões de bacias hidrográficas da AMZL
desmatamento.bacias <- dados.inpe %>% 
                       select(1:4,21:36) %>% 
                       replace(is.na(.), 0) %>% 
                       mutate(total_hidro = rowSums(across(hidro_2005:hidro_2020))) %>% 
                       dplyr::filter(total_hidro>0)

desmatamento.bacias <- classificar.variavel(desmatamento.bacias,'total_hidro','class_hidro')


x <- desmatamento.bacias %>% 
  group_by(class_hidro) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# dados das cidades intermediadoras
desmatamento.interm <- desmatamento %>% 
                       dplyr::filter(cod_muni %in% cidades.intermediadoras)

desmatamento.interm.bacias <- desmatamento.bacias %>% 
                       dplyr::filter(cod_muni %in% cidades.intermediadoras)


# Exportar
write.csv(desmatamento, file='Outputs/01_tabelas/01_desmatamento_agro.csv',row.names = F)
write.csv(desmatamento.bacias, file='Outputs/01_tabelas/01_desmatamento_bacias_energia.csv',row.names = F)