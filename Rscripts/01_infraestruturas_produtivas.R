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

# Dados de geração de energia na Amazônia Legal
infra.energetica <- read_excel(path = 'Input/Banco_de_dados_nao_padronizados_energia.xls',col_types = c("text","numeric","numeric","date","text","numeric","numeric")) %>% 
                    dplyr::filter(cod_muni %in% cidades.amazonia.legal)

infra.energetica <- left_join(infra.energetica,cidades.amazonia.legal.nome) 
colnames(infra.energetica)[8] <- 'muni_principal'

infra.energetica <- left_join(infra.energetica,cidades.amazonia.legal.nome, by=c('cod_muni_2'='cod_muni')) %>% 
  select(1,2,8,6,9,3,4,5,7)
colnames(infra.energetica)[5] <- 'muni_secundario'

infra.energetica <- infra.energetica %>% 
                    dplyr::filter(tipo_de_energia %in% 'Hidrelétrica - UHE') 
                    

# ver se nos financiamentos do BNDES também filtro apenas essas UHE de energia hidreletrica
# Ver região de influencia dos empreendimentos EIA RIMA e tabelular

# Belo monte começou a operar depois de 2015, por isso não consta aqui. Ver nos financiamentos do BNDES as que surgiram depois!
# e incluir aqui!!!


# EXPORTAR em csv e enviar no excel!
write.csv(infra.energetica,file = 'Outputs/01_tabelas/01_infra_energia_eletrica_amz_legal.csv')







# Falta incluir dados de:

# portos, pontos multimodais, ferrovias, usinas de energia e projetos que serão implementados.
# Incluir dados sobre portos que escoam Soja, Milho e Arroz
# Incluir dados sobre terminais ferroviários que escoam Soja, Milho e Arroz, minério de ferro etc

# Exportar o arquivo
#write.csv(prod.agro,file='Outputs/01_tabelas/01_producao_agro.csv',sep = ';',na = '0')

# Verificar as cidades intermediadoras
