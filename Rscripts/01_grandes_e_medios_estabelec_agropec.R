# Grandes e médias propriedades
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar arquivos (dados do censo agro 2017)
# Quantidade de estabelecimentos agropecuários com mais de 1.000 hectares em cada cidade (grandes propriedades).
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
                        dplyr::filter(total_grandes_estab>0)

# Classificar grandes
cod.cidades <- c('1100023','1100122','1100205','1200401','1200203','1302405','1302504','1301902','1304203',
                 '1304062','1303403','1400472','1400100','1502400','1500602','1501808','1504208','1506138',
                 '1506807','1600303','1600501','1702109','1709500','1721000','2101202','2109908','2109106',
                 '2105302','2103000','5102504','5101803','5107602','5103403','5107909')

summary(grandes.propriedades$total_grandes_estab) # aqui eu mostro as estatísticas básicas, como os quartis

round(quantile(grandes.propriedades$total_grandes_estab, probs = seq(0, 1, 1/10),na.rm = TRUE),2) # ver decis

# Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
q1 <- as.vector(summary(grandes.propriedades$total_grandes_estab)[2])
mediana <- as.vector(summary(grandes.propriedades$total_grandes_estab)[3])
q3 <- as.vector(summary(grandes.propriedades$total_grandes_estab)[5])

# armazenar o primeiro e o último decil
x <- quantile(grandes.propriedades$total_grandes_estab, probs = seq(0, 1, 1/10),na.rm = TRUE) 
round(x, 5) # ver os decis 

# armazenar o primeiro (10%) e o último decil (90%)
dec1 <- as.vector(round(x, 5)[2])
dec9 <- as.vector(round(x, 5)[10])


grandes.propriedades <- grandes.propriedades %>% 
  mutate(class_grandes_propriedades = case_when(total_grandes_estab <= dec1 ~ 'Muito Baixo',  # muito baixo 10% (primeiro decil)
                                                total_grandes_estab <=  q1 ~ 'Baixo',         # baixo entre 10% e 25%
                                                total_grandes_estab <= mediana ~ 'Médio Baixo',    # médio-baixo entre 25% e 50%
                                                total_grandes_estab <= q3 ~ 'Médio Alto',     # médio-alto entre 50% e 75%
                                                total_grandes_estab <= dec9 ~ 'Alto',         # alto entre entre 75% e 90%
                                                total_grandes_estab > dec9 ~ 'Muito Alto'))   # muito alto acima de 90% (último decil)


# teste para ver quantos itens existem em cada categoria
x <- grandes.propriedades %>% 
     group_by(class_grandes_propriedades) %>%
     mutate(N_category = n()) %>%
     count(N_category)



# Quantidade de estabelecimentos agropecuários com tamanho entre de 200 a 1.000 hectares (médias propriedades).
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
                       dplyr::filter(total_medios_estab>0)





# Criar uma coluna que soma os dois tipos de propriedades e classificar esta quantidade dentro da Amazônia legal!





cidades.inter <- grandes.propriedades %>% 
  dplyr::filter(cod_muni %in% cod.cidades) %>% 
  arrange(desc(total_grande_prop)) 

