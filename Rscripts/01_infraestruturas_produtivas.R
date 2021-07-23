# Dados sobre armazens, portos, pontos multimodais, ferrovias, usinas de energia e projetos que serão implementados.
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Dados de 2020 armazens
armazens <- read_excel(path = './Input/armazens.xlsx') 
armazens <- armazens %>% 
            mutate(existe_armazem = ifelse(armazens$armazens_e_silos_2_sem_2020 >0,1,0)) %>% 
            mutate(existe_armazem2 = ifelse(armazens$armazens_e_silos_2_sem_2020 >0,'sim','não')) %>% 
            dplyr::filter(cod_muni %in% cidades.amazonia.legal)

# Dados de geração de energia



# Falta incluir dados de:
# usinas de energia eu já tenho
# portos, pontos multimodais, ferrovias, usinas de energia e projetos que serão implementados.
# Incluir dados sobre portos que escoam Soja, Milho e Arroz
# Incluir dados sobre terminais ferroviários que escoam Soja, Milho e Arroz, minério de ferro etc

# Exportar o arquivo
#write.csv(prod.agro,file='Outputs/01_tabelas/01_producao_agro.csv',sep = ';',na = '0')

# Verificar as cidades intermediadoras
