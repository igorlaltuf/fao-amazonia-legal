####################################################################################
# Datasus - mortalidade por município de 2005 a 2019 (excluindo 2007, pq não tem estimativa populacional)
# Fontes: DATASUS via Base dos Dados. 
####################################################################################
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# importar csv
datasus <- read.csv('Outputs/00_shapes_e_dados/00_datasus_mortalidade_anual.csv')

# Organizar dados de 2005 até 2019
datasus$ano <- format(datasus$ano, format="%Y")
datasus$sigla_uf <- as.character(datasus$sigla_uf)
datasus$causa_basica <- as.character(datasus$causa_basica)
datasus$id_municipio <- as.numeric(datasus$id_municipio)

# C00-C97 - neoplasias malignas
cid.neomalig1 <- as.character(c(01:9)) 
cid.neomalig1 <- paste0("0", cid.neomalig1)
cid.neomalig2 <- as.character(c(10:97))
cid.neomalig <- append(cid.neomalig1,cid.neomalig2)
cid.neomalig <- paste0("^C", cid.neomalig)
cid.neomalig <- paste(cid.neomalig, collapse = '|')

datasus.amzl <- datasus %>% 
  dplyr::filter(id_municipio %in% cidades.amazonia.legal &
                ano %in% c(2005:2019)) %>% 
  filter(str_detect(causa_basica, cid.neomalig)) %>% 
  select('ano','id_municipio','sigla_uf','numero_obitos') %>% 
  group_by(ano,id_municipio,sigla_uf) %>% 
  summarise(numero_de_obitos_anual = sum(numero_obitos, na.rm = TRUE))

datasus.amzl <- left_join(datasus.amzl,cidades.amazonia.legal.nome, by = c('id_municipio'='cod_muni')) %>% 
  select(1,2,5,3,4)

# Importar estimativas populacionais de 2005 a 2019, dados do censo de 2010 e da contagem de 2007
# contagem 2007 https://www.ibge.gov.br/estatisticas/sociais/habitacao/9065-contagem-da-populacao.html?edicao=10189&t=resultados
pop.2010 <- read_excel('Input/tabela202.xlsx', skip = 4) %>% select(1,2,4)
pop.2005.2019 <- read_excel('Input/tabela6579.xlsx', skip = 3)
pop.2007 <- read_excel('Input/popmunic2007layoutTCU14112007.xls', skip = 2, range = 'A3:E5567') %>% janitor::clean_names() %>% unite("cod_muni", 2:3,sep = '') %>% rename('2007' = 'populacoes')

populacao <- left_join(pop.2005.2019,pop.2010) %>% 
  select(1:10,21,11:20)  
 
colnames(populacao)[1] <- 'cod_muni'
colnames(populacao)[2] <- 'muni'

populacao <-left_join(populacao, pop.2007) %>% 
  select(1:8,24,9:21) %>% 
  dplyr::filter(cod_muni %in% cidades.amazonia.legal)

# converter em numeric
populacao[3:22] <- lapply(populacao[3:22], as.numeric)

# descobrir quantos NAs existem em cada coluna do Dataframe
sapply(populacao, function(x) sum(is.na(x)))

# reorganizar dataframe como se fosse uma tabela dinâmica
populacao <- populacao %>% 
  pivot_longer(!c(cod_muni,muni), names_to = "ano", values_to = "populacao")

# exportar csv com dados populacionais
write.csv(populacao,'Outputs/00_shapes_e_dados/populacao_amzl_2001-20.csv',row.names = F)


tabela <- left_join(datasus.amzl,populacao,by=c('muni','ano')) %>% 
  select(-'cod_muni') %>% 
  na.omit() %>% 
  mutate(cada_100_mil = (numero_de_obitos_anual/populacao)*100000)  


# limpar variáveis
# rm(populacao,pop.2005.2019,pop.2010,datasus.amzl,datasus)


# separar em dataframes por ano e classificar:
ano <- c(2005,2006,2008:2019)
i <- 1
while (i <= length(ano)) {
  y <- ano[i] # não foi possível buscar o elemento do vetor pela posição dentro do filter
  x <- tabela %>% 
      dplyr::filter(ano == y)
assign(paste("ano",ano[i], sep="."), x)
i <- i + 1
}


t <- as.vector(c('ano.2005','ano.2006','ano.2008','ano.2009','ano.2010','ano.2011','ano.2012',
                 'ano.2013','ano.2014','ano.2015','ano.2016','ano.2017','ano.2018','ano.2019'))
# classificar por ano
ano.2005 <- classificar.variavel(ano.2005,'cada_100_mil','class_cada_100_mil')
ano.2006 <- classificar.variavel(ano.2006,'cada_100_mil','class_cada_100_mil')
ano.2008 <- classificar.variavel(ano.2008,'cada_100_mil','class_cada_100_mil')
ano.2009 <- classificar.variavel(ano.2009,'cada_100_mil','class_cada_100_mil')
ano.2010 <- classificar.variavel(ano.2010,'cada_100_mil','class_cada_100_mil')
ano.2011 <- classificar.variavel(ano.2011,'cada_100_mil','class_cada_100_mil')
ano.2012 <- classificar.variavel(ano.2012,'cada_100_mil','class_cada_100_mil')
ano.2013 <- classificar.variavel(ano.2013,'cada_100_mil','class_cada_100_mil')
ano.2014 <- classificar.variavel(ano.2014,'cada_100_mil','class_cada_100_mil')
ano.2015 <- classificar.variavel(ano.2015,'cada_100_mil','class_cada_100_mil')
ano.2016 <- classificar.variavel(ano.2016,'cada_100_mil','class_cada_100_mil')
ano.2017 <- classificar.variavel(ano.2017,'cada_100_mil','class_cada_100_mil')
ano.2018 <- classificar.variavel(ano.2018,'cada_100_mil','class_cada_100_mil')
ano.2019 <- classificar.variavel(ano.2019,'cada_100_mil','class_cada_100_mil')

tabela <- rbind(ano.2005,ano.2006,ano.2008,ano.2009,ano.2010,ano.2011,ano.2012,ano.2013,
  ano.2014,ano.2015,ano.2016,ano.2017,ano.2018,ano.2019) %>% 
  dplyr::filter(class_cada_100_mil %in% c('Alto','Muito Alto') &
                ano %in% c(2005:2019)) %>% # anos analisados
  ungroup(ano,id_municipio) %>% 
  select(id_municipio,muni)


tabela <- aggregate(cbind(tabela[0],  numdup = 1), tabela, length) # conta quantas vezes cada município aparece no df com casos altos e muito altos.

tabela <- tabela %>% 
  dplyr::filter(numdup >= 12) %>% # quantidade de anos que o valor precisa ter sido alto ou muito alto
  mutate(datasus = 1)

# municípios que foram classificados como alto ou muito alto em pelo menos 12 dos 14 anos (alto ou muito altos em pelos menos 85% das vezes).
# Isso exclui por exemplo:
# exemplo de Lajeado (TO) que em 2019 era alto, mas que antes não era, assim como outros casos mais inconsistentes.

write.csv(tabela, 'Outputs/01_tabelas/01_datasus_cancer_2005-19.csv', row.names = F)


# Exportar dados apenas para 2019
write.csv(ano.2019, 'Outputs/01_tabelas/01_datasus_cancer_2019.csv', row.names = F)