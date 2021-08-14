# Datasus - mortalidade por município
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar credenciais do data lake base dos dados
load_dot_env()
credencial <- Sys.getenv('CREDENCIAL_BASE_DOS_DADOS')
basedosdados::set_billing_id(credencial)   

# Fazer a query
query <- "SELECT * FROM `basedosdados.br_ms_sim.municipio_causa`
          WHERE ano = 2019"

query <- "SELECT * FROM `basedosdados.br_ms_sim.municipio_causa`" # baixas todos os dados de mortalidade do datasus
# WHERE ano IN(2019,2018,2017,2016,2015) usar para vários anos

datasus <- read_sql(query)
write.csv(datasus,file = 'Input/datasus_mortalidade_anual.csv')


############################################## Rodar a partir daqui
datasus <- read.csv('Input/datasus_mortalidade_anual.csv') %>% 
           select(-'X')

# filtrar a partir de 2005 daqui em diante
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

# filtrar dados
# datasus.amzl <- datasus %>% 
#                 dplyr::filter(id_municipio %in% cidades.amazonia.legal) %>% 
#                 filter(str_detect(causa_basica, cid.neomalig)) %>% 
#                 select('id_municipio','sigla_uf','numero_obitos') %>% 
#                 group_by(id_municipio,sigla_uf) %>% 
#                 summarise(numero_de_obitos_2019 = sum(numero_obitos, na.rm = TRUE))

datasus.amzl <- datasus %>% 
  dplyr::filter(id_municipio %in% cidades.amazonia.legal &
                ano %in% c(2005:2019)) %>% 
  filter(str_detect(causa_basica, cid.neomalig)) %>% 
  select('ano','id_municipio','sigla_uf','numero_obitos') %>% 
  group_by(ano,id_municipio,sigla_uf) %>% 
  summarise(numero_de_obitos_anual = sum(numero_obitos, na.rm = TRUE))

datasus.amzl <- left_join(datasus.amzl,cidades.amazonia.legal.nome, by = c('id_municipio'='cod_muni')) %>% 
  select(1,2,5,3,4)




### CONTINUAR DAQUI!!!!!!
# Importar estimativas populacionais de 2005 a 2019 e dados do censo de 2010
pop.2010 <- read_excel('Input/tabela202.xlsx', skip = 4) %>% select(1,2,4)
pop.2005.2019 <- read_excel('Input/tabela6579.xlsx', skip = 3)
populacao <- left_join(pop.2005.2019,pop.2010) %>% 
  select(1:10,21,11:20)

colnames(populacao)[1] <- 'cod_muni'
colnames(populacao)[2] <- 'muni'

# left_join pelo ano e pelo código (criar uma coluna que junta essas duas colunas e dar o join por ela)
# Calcular a cada 100 mil hab p cada ano, calcular a média e classificar a média.









# Importar estimativa populacional IBGE
pop.2019 <- read_excel('Input/tabela6579_pop_estimada_2019.xlsx')
datasus.amzl <- left_join(datasus.amzl, pop.2019)
datasus.amzl <- datasus.amzl %>% 
                select(1,5,4,2,3,6) %>% 
                ungroup() %>% # preciso fazer ungroup para remover coluna usada para fazer o join
                select(-1) %>% 
                mutate(casos_cada_100_mil = (numero_de_obitos_2019/pop_resid_estimada_2019)*100000)

# Classificar 
datasus.amzl <- classificar.variavel(datasus.amzl,'casos_cada_100_mil','class_cada_100_mil')

datasus.amzl.inter <- datasus.amzl %>% 
                      dplyr::filter(cod_muni %in% cidades.intermediadoras)####

x <- datasus.amzl %>% 
  group_by(class_cada_100_mil) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# Exportar
write.csv(datasus.amzl,'Outputs/01_tabelas/01_datasus_agro.csv')

# query <- "SELECT * FROM `basedosdados.br_ms_sim.dicionario`"
# df <- read_sql(query)
