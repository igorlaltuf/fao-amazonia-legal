# Dados sobre royalties
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Royalties recebidos pelos municípios (dados do IPEA)
royalties <- read_excel(path = './Input/royalties_municipios_ipea.xlsx', col_types = c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","text"))
royalties[is.na(royalties)] <- 0

# 1 - Classificar Royalties de energia CFH
royalties.cfh <- royalties %>% 
             select(1,2,4) %>% 
             dplyr::filter(royalties_cfh_energia_eletrica > 0)

# Estatísticas
summary(royalties.cfh$royalties_cfh_energia_eletrica) # aqui eu mostro as estatísticas básicas, como os quartis
round(quantile(royalties.cfh$royalties_cfh_energia_eletrica, probs = seq(0, 1, 1/10),na.rm = TRUE),2) # mostrar os decis

# Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
q1 <- as.vector(summary(royalties.cfh$royalties_cfh_energia_eletrica)[2])
mediana <- as.vector(summary(royalties.cfh$royalties_cfh_energia_eletrica)[3])
q3 <- as.vector(summary(royalties.cfh$royalties_cfh_energia_eletrica)[5])

# armazenar o primeiro e o último decil
x <- quantile(royalties.cfh$royalties_cfh_energia_eletrica, probs = seq(0, 1, 1/10),na.rm = TRUE) 
round(x, 5) # ver os decis 

# armazenar o primeiro (10%) e o último decil (90%)
dec1 <- as.vector(round(x, 5)[2])
dec9 <- as.vector(round(x, 5)[10])

royalties.cfh <- royalties.cfh %>% 
      mutate(class_energia_cfh = case_when(royalties_cfh_energia_eletrica <= dec1 ~ 'Muito Baixo',  # muito baixo 10% (primeiro decil)
                                  royalties_cfh_energia_eletrica <=  q1 ~ 'Baixo',         # baixo entre 10% e 25%
                                  royalties_cfh_energia_eletrica <= mediana ~ 'Médio Baixo',    # médio-baixo entre 25% e 50%
                                  royalties_cfh_energia_eletrica <= q3 ~ 'Médio Alto',     # médio-alto entre 50% e 75%
                                  royalties_cfh_energia_eletrica <= dec9 ~ 'Alto',         # alto entre entre 75% e 90%
                                  royalties_cfh_energia_eletrica > dec9 ~ 'Muito Alto'))   # muito alto acima de 90% (último decil)

# teste para ver quantos itens existem em cada categoria
x <- royalties.cfh %>% 
  group_by(class_energia_cfh) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# 2 - Classificar Royalties de energia Itaipu
royalties.itaipu <- royalties %>% 
  select(1,2,7) %>% 
  dplyr::filter(royalties_ita_itaipu > 0)

# Estatísticas
summary(royalties.itaipu$royalties_ita_itaipu) # aqui eu mostro as estatísticas básicas, como os quartis
round(quantile(royalties.itaipu$royalties_ita_itaipu, probs = seq(0, 1, 1/10),na.rm = TRUE),2) # mostrar os decis

# Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
q1 <- as.vector(summary(royalties.itaipu$royalties_ita_itaipu)[2])
mediana <- as.vector(summary(royalties.itaipu$royalties_ita_itaipu)[3])
q3 <- as.vector(summary(royalties.itaipu$royalties_ita_itaipu)[5])

# armazenar o primeiro e o último decil
x <- quantile(royalties.itaipu$royalties_ita_itaipu, probs = seq(0, 1, 1/10),na.rm = TRUE) 
round(x, 5) # ver os decis 

# armazenar o primeiro (10%) e o último decil (90%)
dec1 <- as.vector(round(x, 5)[2])
dec9 <- as.vector(round(x, 5)[10])

royalties.itaipu <- royalties.itaipu %>% 
  mutate(class_energia_itaipu = case_when(royalties_ita_itaipu <= dec1 ~ 'Muito Baixo',  # muito baixo 10% (primeiro decil)
                                       royalties_ita_itaipu <=  q1 ~ 'Baixo',         # baixo entre 10% e 25%
                                       royalties_ita_itaipu <= mediana ~ 'Médio Baixo',    # médio-baixo entre 25% e 50%
                                       royalties_ita_itaipu <= q3 ~ 'Médio Alto',     # médio-alto entre 50% e 75%
                                       royalties_ita_itaipu <= dec9 ~ 'Alto',         # alto entre entre 75% e 90%
                                       royalties_ita_itaipu > dec9 ~ 'Muito Alto'))   # muito alto acima de 90% (último decil)

# teste para ver quantos itens existem em cada categoria
x <- royalties.itaipu %>% 
  group_by(class_energia_itaipu) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# 3 - Classificar Royalties de ANP Petróleo
royalties.anp.petroleo <- royalties %>% 
  select(1,2,3) %>% 
  dplyr::filter(royalties_anp_petroleo > 0)

# Estatísticas
summary(royalties.anp.petroleo$royalties_anp_petroleo) # aqui eu mostro as estatísticas básicas, como os quartis
round(quantile(royalties.anp.petroleo$royalties_anp_petroleo, probs = seq(0, 1, 1/10),na.rm = TRUE),2) # mostrar os decis

# Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
q1 <- as.vector(summary(royalties.anp.petroleo$royalties_anp_petroleo)[2])
mediana <- as.vector(summary(royalties.anp.petroleo$royalties_anp_petroleo)[3])
q3 <- as.vector(summary(royalties.anp.petroleo$royalties_anp_petroleo)[5])

# armazenar o primeiro e o último decil
x <- quantile(royalties.anp.petroleo$royalties_anp_petroleo, probs = seq(0, 1, 1/10),na.rm = TRUE) 
round(x, 5) # ver os decis 

# armazenar o primeiro (10%) e o último decil (90%)
dec1 <- as.vector(round(x, 5)[2])
dec9 <- as.vector(round(x, 5)[10])

royalties.anp.petroleo <- royalties.anp.petroleo %>% 
  mutate(class_anp_petroleo = case_when(royalties_anp_petroleo <= dec1 ~ 'Muito Baixo',  # muito baixo 10% (primeiro decil)
                                          royalties_anp_petroleo <=  q1 ~ 'Baixo',         # baixo entre 10% e 25%
                                          royalties_anp_petroleo <= mediana ~ 'Médio Baixo',    # médio-baixo entre 25% e 50%
                                          royalties_anp_petroleo <= q3 ~ 'Médio Alto',     # médio-alto entre 50% e 75%
                                          royalties_anp_petroleo <= dec9 ~ 'Alto',         # alto entre entre 75% e 90%
                                          royalties_anp_petroleo > dec9 ~ 'Muito Alto'))   # muito alto acima de 90% (último decil)

# teste para ver quantos itens existem em cada categoria
x <- royalties.anp.petroleo %>% 
     group_by(class_anp_petroleo) %>%
     mutate(N_category = n()) %>%
     count(N_category)

# 4 - Classificar Royalties de Petróleo FEP - Repartição todos os municípios
royalties.fep.petroleo <- royalties %>% 
                          select(1,2,6) %>% 
                          dplyr::filter(royalties_fep_petroleo > 0)

# Estatísticas
summary(royalties.fep.petroleo$royalties_fep_petroleo) # aqui eu mostro as estatísticas básicas, como os quartis
round(quantile(royalties.fep.petroleo$royalties_fep_petroleo, probs = seq(0, 1, 1/10),na.rm = TRUE),2) # mostrar os decis

# Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
q1 <- as.vector(summary(royalties.fep.petroleo$royalties_fep_petroleo)[2])
mediana <- as.vector(summary(royalties.fep.petroleo$royalties_fep_petroleo)[3])
q3 <- as.vector(summary(royalties.fep.petroleo$royalties_fep_petroleo)[5])

# armazenar o primeiro e o último decil
x <- quantile(royalties.fep.petroleo$royalties_fep_petroleo, probs = seq(0, 1, 1/10),na.rm = TRUE) 
round(x, 5) # ver os decis 

# armazenar o primeiro (10%) e o último decil (90%)
dec1 <- as.vector(round(x, 5)[2])
dec9 <- as.vector(round(x, 5)[10])

royalties.fep.petroleo <- royalties.fep.petroleo %>% 
  mutate(class_fep_petroleo = case_when(royalties_fep_petroleo <= dec1 ~ 'Muito Baixo',  # muito baixo 10% (primeiro decil)
                                        royalties_fep_petroleo <=  q1 ~ 'Baixo',         # baixo entre 10% e 25%
                                        royalties_fep_petroleo <= mediana ~ 'Médio Baixo',    # médio-baixo entre 25% e 50%
                                        royalties_fep_petroleo <= q3 ~ 'Médio Alto',     # médio-alto entre 50% e 75%
                                        royalties_fep_petroleo <= dec9 ~ 'Alto',         # alto entre entre 75% e 90%
                                        royalties_fep_petroleo > dec9 ~ 'Muito Alto'))   # muito alto acima de 90% (último decil)

# teste para ver quantos itens existem em cada categoria
x <- royalties.fep.petroleo %>% 
  group_by(class_fep_petroleo) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# 5 - Classificar Royalties de Mineração CFEM
royalties.cfem <- royalties %>% 
                  select(1,2,5) %>% 
                  dplyr::filter(royalties_cfem_mineracao > 0)

# Estatísticas
summary(royalties.cfem$royalties_cfem_mineracao) # aqui eu mostro as estatísticas básicas, como os quartis
round(quantile(royalties.cfem$royalties_cfem_mineracao, probs = seq(0, 1, 1/10),na.rm = TRUE),2) # mostrar os decis

# Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
q1 <- as.vector(summary(royalties.cfem$royalties_cfem_mineracao)[2])
mediana <- as.vector(summary(royalties.cfem$royalties_cfem_mineracao)[3])
q3 <- as.vector(summary(royalties.cfem$royalties_cfem_mineracao)[5])

# armazenar o primeiro e o último decil
x <- quantile(royalties.cfem$royalties_cfem_mineracao, probs = seq(0, 1, 1/10),na.rm = TRUE) 
round(x, 5) # ver os decis 

# armazenar o primeiro (10%) e o último decil (90%)
dec1 <- as.vector(round(x, 5)[2])
dec9 <- as.vector(round(x, 5)[10])

royalties.cfem <- royalties.cfem %>% 
  mutate(class_cfem = case_when(royalties_cfem_mineracao <= dec1 ~ 'Muito Baixo',  # muito baixo 10% (primeiro decil)
                                royalties_cfem_mineracao <=  q1 ~ 'Baixo',         # baixo entre 10% e 25%
                                royalties_cfem_mineracao <= mediana ~ 'Médio Baixo',    # médio-baixo entre 25% e 50%
                                royalties_cfem_mineracao <= q3 ~ 'Médio Alto',     # médio-alto entre 50% e 75%
                                royalties_cfem_mineracao <= dec9 ~ 'Alto',         # alto entre entre 75% e 90%
                                royalties_cfem_mineracao > dec9 ~ 'Muito Alto'))   # muito alto acima de 90% (último decil)

# teste para ver quantos itens existem em cada categoria
x <- royalties.cfem %>% 
  group_by(class_cfem) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# 6 - Classificar Royalties PEA (bacias de grande volume)
royalties.pea <- royalties %>% 
                  select(1,2,8) %>% 
                  dplyr::filter(royalties_pea > 0)

# Estatísticas
summary(royalties.pea$royalties_pea) # aqui eu mostro as estatísticas básicas, como os quartis
round(quantile(royalties.pea$royalties_pea, probs = seq(0, 1, 1/10),na.rm = TRUE),2) # mostrar os decis

# Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
q1 <- as.vector(summary(royalties.pea$royalties_pea)[2])
mediana <- as.vector(summary(royalties.pea$royalties_pea)[3])
q3 <- as.vector(summary(royalties.pea$royalties_pea)[5])

# armazenar o primeiro e o último decil
x <- quantile(royalties.pea$royalties_pea, probs = seq(0, 1, 1/10),na.rm = TRUE) 
round(x, 5) # ver os decis 

# armazenar o primeiro (10%) e o último decil (90%)
dec1 <- as.vector(round(x, 5)[2])
dec9 <- as.vector(round(x, 5)[10])

royalties.pea <- royalties.pea %>% 
  mutate(class_pea = case_when(royalties_pea <= dec1 ~ 'Muito Baixo',  # muito baixo 10% (primeiro decil)
                               royalties_pea <=  q1 ~ 'Baixo',         # baixo entre 10% e 25%
                               royalties_pea <= mediana ~ 'Médio Baixo',    # médio-baixo entre 25% e 50%
                               royalties_pea <= q3 ~ 'Médio Alto',     # médio-alto entre 50% e 75%
                               royalties_pea <= dec9 ~ 'Alto',         # alto entre entre 75% e 90%
                               royalties_pea > dec9 ~ 'Muito Alto'))   # muito alto acima de 90% (último decil)

# teste para ver quantos itens existem em cada categoria
x <- royalties.pea %>% 
  group_by(class_pea) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# Juntar todas as classificações a nível nacional
royalties <- full_join(royalties.cfh,royalties.itaipu)
royalties <- full_join(royalties,royalties.anp.petroleo)
royalties <- full_join(royalties,royalties.fep.petroleo)
royalties <- full_join(royalties,royalties.cfem)
royalties <- full_join(royalties,royalties.pea)

# Royalties das cidades da Amazônia Legal
royalties.amz.legal <- royalties %>% 
  dplyr::filter(cod_muni %in% cidades.amazonia.legal)

# Royalties das cidades intermediadoras da Amazônia Legal
royalties.intermediadoras <- royalties %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras)
