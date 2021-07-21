# Dados de empregos formais e informais (Censo Agro 2017)
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

empregos.agro.censo <- read_excel(path = './Input/empregos agro censo 2017.xlsx')

# População estimada de 2017
pop.2017 <- read_excel(path = './Input/tabela6579_pop_estimada_2017.xlsx')

empregos.agro.censo <- left_join(empregos.agro.censo, pop.2017[-2], by = c('cod_muni' = 'cod'))
empregos.agro.censo <- empregos.agro.censo %>% 
                       mutate(empr_agro_100mil_ha = pessoal_ocupado_agro/pop_estimada_2017*100000) %>% 
                       mutate(empr_agro_familiar_100mil_ha = pessoal_ocupado_agro_familiar/pop_estimada_2017*100000) %>% 
                       mutate(percent_agro_familiar = pessoal_ocupado_agro_familiar/pop_estimada_2017) %>% 
                       mutate(percent_agro = pessoal_ocupado_agro/pop_estimada_2017)
  
empregos.agro.censo[6:9] <- round(empregos.agro.censo[6:9],2)


# Classificar empregos agro censo
summary(empregos.agro.censo$percent_agro) # aqui eu mostro as estatísticas básicas, como os quartis
quantile(empregos.agro.censo$percent_agro, probs = seq(0, 1, 1/10),na.rm = TRUE) #decis

# Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
q1 <- as.vector(summary(empregos.agro.censo$percent_agro)[2])
mediana <- as.vector(summary(empregos.agro.censo$percent_agro)[3])
q3 <- as.vector(summary(empregos.agro.censo$percent_agro)[5])


x <- quantile(empregos.agro.censo$percent_agro, probs = seq(0, 1, 1/10),na.rm = TRUE) # Descobrir decis
round(x, 5) # ver os decis 

# armazenar o primeiro (10%) e o último decil (90%)
dec1 <- as.vector(round(x, 5)[2])
dec9 <- as.vector(round(x, 5)[10])

empregos.agro.censo <- empregos.agro.censo %>% 
  mutate(class_agro = case_when(percent_agro <= dec1 ~ 'Muito Baixo',   # muito baixo 10% (primeiro decil)
                                percent_agro <=  q1 ~ 'Baixo',          # baixo entre 10% e 25%
                                percent_agro <= mediana ~ 'Médio Baixo',# médio-baixo entre 25% e 50%
                                percent_agro <= q3 ~ 'Médio Alto',      # médio-alto entre 50% e 75%
                                percent_agro <= dec9 ~ 'Alto',          # alto entre entre 75% e 90%
                                percent_agro > dec9 ~ 'Muito Alto')) %>%# muito alto acima de 90% (último decil)

  mutate(class_familiar = case_when(percent_agro_familiar <= dec1 ~ 'Muito Baixo',   # muito baixo 10% (primeiro decil)
                                    percent_agro_familiar <=  q1 ~ 'Baixo',          # baixo entre 10% e 25%
                                    percent_agro_familiar <= mediana ~ 'Médio Baixo',# médio-baixo entre 25% e 50%
                                    percent_agro_familiar <= q3 ~ 'Médio Alto',      # médio-alto entre 50% e 75%
                                    percent_agro_familiar <= dec9 ~ 'Alto',          # alto entre entre 75% e 90%
                                    percent_agro_familiar > dec9 ~ 'Muito Alto')) %>%# muito alto acima de 90% (último decil)
  arrange(desc(percent_agro_familiar)) 


cod.cidades <- c('1100023','1100122','1100205','1200401','1200203','1302405','1302504','1301902','1304203',
                 '1304062','1303403','1400472','1400100','1502400','1500602','1501808','1504208','1506138',
                 '1506807','1600303','1600501','1702109','1709500','1721000','2101202','2109908','2109106',
                 '2105302','2103000','5102504','5101803','5107602','5103403','5107909')

cidades.intermed <- empregos.agro.censo %>% 
                    dplyr::filter(cod_muni %in% cod.cidades) %>% 
                    arrange(desc(percent_agro)) %>% 
                    select(2,3,4,5,8,9,10,11)




# Usar emprego total, quando eu uso emprego familiar, cai naquele problema da classificação.

x <- empregos.agro.censo %>% 
      group_by(class_agro) %>%
      mutate(N_category = n()) %>%
      count(N_category)

y <- empregos.agro.censo %>% 
      group_by(class_familiar) %>%
      mutate(N_category = n()) %>%
      count(N_category)

















censo.agro <- gt(cidades.intermed) %>%
  cols_label(
    muni = 'Cidade',
    pessoal_ocupado_agro = 'Pessoal ocupado em estabelecimento agropecuário',
    pessoal_ocupado_agro_familiar = 'Pessoal ocupado em estabelecimento agropecuário com vínculo familiar com o produtor',
    pop_estimada_2017 = 'População estimada em 2017',
    percent_agro = 'Pessoal ocupado agro/população estimada',
    class_agro= 'Classificação frente às demais cidades'
  ) %>% 
  tab_header(
    title = 'Pessoal ocupado em estabelecimento agropecuário por cidades intermediadoras da Amazônia Legal',
    subtitle = '2017'
  ) %>%
  fmt_markdown(
    columns = c(muni,class_agro)
  ) %>% 
  fmt_number(
    columns = c(pessoal_ocupado_agro,pessoal_ocupado_agro_familiar,pop_estimada_2017),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  fmt_percent(
    columns = percent_agro,
    decimals = 1,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Censo Agropecuário 2017.')


censo.agro

gtsave(censo.agro, 'Censo agro ocupação.png')

