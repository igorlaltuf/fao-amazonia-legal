# Dados do Censo Escolar
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# contar a quantidade de escolas por muni e verificar a quantidade de escolas públicas.
df <- read.csv(file = 'Outputs/00_shapes_e_dados/00_base_escolas_2009_2020.csv') 
escolas <- df %>% 
  dplyr::filter(tipo_situacao_funcionamento == 1) %>% # filtrar escolas ativas (= 1)
  select(ano, sigla_uf, id_municipio, rede) %>%  
  group_by(ano, sigla_uf, id_municipio, rede) %>%
  count(rede)

# matrículas por turma (juntar com a tabela acima para saber o número de matrículas por escola)
df <- read.csv(file = 'Outputs/00_shapes_e_dados/00_base_matriculas_2009_2020.csv') 
matriculas <- df %>% 
  select(ano, sigla_uf, id_municipio, rede, quantidade_matriculas) %>%  
  group_by(ano, sigla_uf, id_municipio, rede) %>% 
  summarise(qtd_matriculas = sum(quantidade_matriculas))
  
# dados com total de escolas públicas e matrículas COM separação por esfera governamental
tabela.educacao <- left_join(matriculas, escolas) %>% 
  dplyr::filter(id_municipio %in% cidades.amazonia.legal)

colnames(tabela.educacao)[6] <- 'qtd_escolas'
colnames(tabela.educacao)[3] <- 'cod_muni'

# dados com total de escolas públicas e matrículas SEM separação por esfera governamental
tabela.educacao <- tabela.educacao %>% 
  group_by(ano, sigla_uf, cod_muni) %>% 
  summarise_at(c("qtd_matriculas", "qtd_escolas"), sum, na.rm = TRUE)


# importar dados de população
pop.2010 <- read_excel('Input/tabela202.xlsx', skip = 4) %>% select(1,2,4)
pop.2005.2019 <- read_excel('Input/tabela6579.xlsx', skip = 3)

populacao <- left_join(pop.2005.2019,pop.2010) %>% 
  select(1:10,21,11:20) 

colnames(populacao)[1] <- 'cod_muni'
colnames(populacao)[2] <- 'muni'

populacao <- populacao %>% 
  dplyr::filter(cod_muni %in% cidades.amazonia.legal)


populacao[3:21] <- lapply(populacao[3:21], as.numeric) # mudar colunas para numeric

# descobrir quantos NAs existem em cada coluna do Dataframe
sapply(populacao, function(x) sum(is.na(x)))

# reorganizar dataframe como se fosse uma tabela dinâmica
populacao <- populacao %>% 
  pivot_longer(!c(cod_muni,muni), names_to = "ano", values_to = "populacao")

tabela.educacao$cod_muni <- as.character(tabela.educacao$cod_muni)
tabela.educacao$ano <- as.character(tabela.educacao$ano)

tabela <- left_join(tabela.educacao, populacao, by=c('cod_muni','ano')) %>% 
  select(1,2,3,6,4,5,7) %>% 
  na.omit() %>% 
  mutate(matr_100_mil = (qtd_matriculas/populacao)*100000) %>% 
  mutate(escol_100_mil = (qtd_escolas/populacao)*100000)

# Salvar dados
write.csv(tabela,'Outputs/01_tabelas/01_escolas_publicas_AMZL_2009_2020.csv', row.names = F)