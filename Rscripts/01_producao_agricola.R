# Dados sobre produção agrícola do agronegócio
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Arquivo com o valor da produção de soja, milho e arroz de estabelecimentos agropecuários com mais de 200 hectares (médios e grandes)
prod.agro <- read.csv(file = "Input/tabela6959-prod-agro.csv", sep = ";", dec = ",", fileEncoding = 'UTF-8-BOM') %>% 
             dplyr::slice(1:27775) # remove linhas finais que não pertencem a tabela
# Valores estão em mil reais
# Grandes e médias estabelecimentos agrícolas são responsáveis por 79,5% do valor de produção brasileira de arroz, 74,6% de milho e 83,7% de soja. 

# transformar factors em character
prod.agro <- data.frame(lapply(prod.agro, as.character), stringsAsFactors=FALSE) 

# renomear colunas
colnames(prod.agro) <- c('cod_muni','muni','.','arroz_em_casca','milho_em_grao','soja_em_grao')

# transformar colunas em numeric (transforma X e - em NA)
prod.agro$arroz_em_casca <- as.numeric(prod.agro$arroz_em_casca)
prod.agro$milho_em_grao <- as.numeric(prod.agro$milho_em_grao)
prod.agro$soja_em_grao <- as.numeric(prod.agro$soja_em_grao)

prod.agro[is.na(prod.agro)] <- 0

# Agrupar linhas de categorias, para ficar uma linha por cidade e criar uma coluna com o valor total
prod.agro <- prod.agro %>% 
             select(cod_muni,muni,arroz_em_casca,milho_em_grao,soja_em_grao) %>% 
             group_by(cod_muni,muni) %>% 
             summarise(across(everything(), list(sum))) %>% 
             mutate(valor_producao = arroz_em_casca_1 + milho_em_grao_1 + soja_em_grao_1) %>% 
             dplyr::filter(valor_producao > 0 &
             cod_muni %in% cidades.amazonia.legal) 

prod.agro <- classificar.variavel(prod.agro,'valor_producao','class_valor_producao')

# teste para ver quantos itens existem em cada categoria
x <- prod.agro %>% 
  group_by(class_valor_producao) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# Lembrar que valores estão em mil reais
prod.agro$cod_muni <- as.numeric(prod.agro$cod_muni)
prod.agro <- full_join(prod.agro,cidades.amazonia.legal.nome)


inter <- prod.agro %>% 
         dplyr::filter(cod_muni %in% cidades.intermediadoras)

# Exportar arquivo
write.csv(prod.agro,file='Outputs/01_tabelas/01_producao_agro.csv',na = '0',row.names = F)