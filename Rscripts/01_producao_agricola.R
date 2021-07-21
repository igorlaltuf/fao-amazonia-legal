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
             dplyr::filter(valor_producao>0) 


# até aqui ok! Esperando resposta sobre função com R
# CLASSIFICAR E FILTRAR APENAS AQUELAS DA AMAZONIA LEGAL!


summary(prod.agro$valor_producao) # aqui eu mostro as estatísticas básicas, como os quartis
round(quantile(prod.agro$valor_producao, probs = seq(0, 1, 1/10),na.rm = TRUE),2) # ver decis

# Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
q1 <- as.vector(summary(prod.agro$valor_producao)[2])
mediana <- as.vector(summary(prod.agro$valor_producao)[3])
q3 <- as.vector(summary(prod.agro$valor_producao)[5])

# armazenar o primeiro e o último decil
x <- quantile(prod.agro$valor_producao, probs = seq(0, 1, 1/10),na.rm = TRUE) 
round(x, 5) # ver os decis 

# armazenar o primeiro (10%) e o último decil (90%)
dec1 <- as.vector(round(x, 5)[2])
dec9 <- as.vector(round(x, 5)[10])


prod.agro <- prod.agro %>% 
  mutate(class_valor_producao = case_when(valor_producao <= dec1 ~ 'Muito Baixo',  # muito baixo 10% (primeiro decil)
                                          valor_producao <=  q1 ~ 'Baixo',         # baixo entre 10% e 25%
                                          valor_producao <= mediana ~ 'Médio Baixo',    # médio-baixo entre 25% e 50%
                                          valor_producao <= q3 ~ 'Médio Alto',     # médio-alto entre 50% e 75%
                                          valor_producao <= dec9 ~ 'Alto',         # alto entre entre 75% e 90%
                                          valor_producao > dec9 ~ 'Muito Alto'))   # muito alto acima de 90% (último decil)


# teste para ver quantos itens existem em cada categoria
x <- prod.agro %>% 
  group_by(class_valor_producao) %>%
  mutate(N_category = n()) %>%
  count(N_category)



cidades.inter <- prod.agro %>% 
                 dplyr::filter(cod_muni %in% cod.cidades) %>% 
                 arrange(desc(valor_producao)) 

# Lembrar que valores estão em mil reais


# produtividade
area <- read_excel('Input/tabela6771-area-estab-agro.xlsx') %>% 
        dplyr::filter(area_estab_agro_hecta > 0) %>% 
        select(1,3)

prod.agro <- left_join(prod.agro,area,by=c('cod_muni' = 'cod_muni'))

prod.agro <- prod.agro %>% 
             mutate(valor_prod_por_hectare = valor_producao/area_estab_agro_hecta) 
            
prod.agro$valor_prod_por_hectare <- round(prod.agro$valor_prod_por_hectare,2)
             





#calcular a produtividade por área plantada, comparar classificação das intermediadoras
# para ver se mantém (agro é )






