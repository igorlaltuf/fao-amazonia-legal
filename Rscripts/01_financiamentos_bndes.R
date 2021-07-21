# Dados sobre Financiamentos do BNDES
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# 1 - FINANCIAMENTOS PARA GERA��O DE ENERGIA EL�TRICA
# Grandes projetos de gera��o de energia financiados pelo BNDES na regi�o da Amaz�nia Legal
fin.bndes.original <- read_excel('Input/bndes_financiamentos.xlsx')
which(is.na(fin.bndes.original$valor_contratado_corrente)) # retorna a linha dos NAs se existirem

fin.bndes.deflac <- fin.bndes.original %>% 
                    mutate(fin.bndes.original,desembolso_deflac = deflate(valor_desembolsado_corrente,as.Date(ymd(fin.bndes.original$data_contrato)),'04/2021','igpdi')) %>% 
                    dplyr::filter(desembolso_deflac>=40000000 & 
                    porte_cliente == 'GRANDE' &
                    natureza_cliente == 'PRIVADA') #filtra os grandes investimentos (acima de R$ 40 milh�es de 2021)
  
bndes.energia <- fin.bndes.deflac %>% 
                 filter(str_detect(nome_sub_setor_cnae, "^GERACAO DE ENERGIA ELETRICA"))


# IMPORTANTE!!!!!!
amz.legal.uppercase <- mutate_all(cidades.amazonia.legal.nome, .funs=toupper) 
amz.legal.uppercase$muni <- str_sub(amz.legal.uppercase$muni ,1,nchar(amz.legal.uppercase$muni )-5)
# Falta remover �,~,acentos,' etc e incluir no filtro abaixo:##### IMPORTANTE!

# Como muitas delas est�o categorizadas como "SEM MUNIC�PIO", vou checar se o nome das cidades est� na descri��o:
intermed.descricao <- bndes.energia %>% 
  dplyr::filter(muni == 'SEM MUNIC�PIO') %>% 
  dplyr::filter(grepl('RIO BRANCO|ITACOATIARA|LABREA|MANACAPURU|PARINTINS|TABATINGA|TEFE|MACAPA
                      |OIAPOQUE|BACABAL|CAXIAS|IMPERATRIZ|PRESIDENTE DUTRA|SANTA INES|BARRA DO GARCAS
                      |CACERES|CUIABA|RONDONOPOLIS|SINOP|ALTAMIRA|BREVES|CASTANHAL|MARABA|REDENCAO
                      |SANTAREM|ARIQUEMES|JI-PARANA|PORTO VELHO|BOA VISTA|RORAINOPOLIS|ARAGUAINA|GURUPI
                      |PALMAS', descricao)) 

# agrupar por munic�pio
bndes.energia <- bndes.energia %>% 
                 select(5,6,28) %>% 
                 group_by(muni,cod_muni) %>%
                 summarise(desembolsos_energia_deflac = sum(desembolso_deflac)) %>% 
                 arrange(desc(desembolsos_energia_deflac))

  

# 2 - FINANCIAMENTOS PARA MINERA��O

# REVISAR ESTE FILTRO DE MINERA��O (perguntar para Luis)
bndes.mineracao <- fin.bndes.deflac %>% 
                   dplyr::filter(nome_sub_setor_cnae %in% c('EXT MIN COBRE CHMBO ZNCO OUTR MIN MET NAO-FERRO NAO ESPEC ANTER',
                                                            'EXTRACAO DE MINERIO DE FERRO',
                                                            'EXTRACAO DE MINERIO DE ALUMINIO',
                                                            'BNFC MIN CBRE CHMBO ZNCO OUTR MIN MET NAO-FERRO NAO ESPEC ANTER',
                                                            'EXTRACAO DE MINERIO DE NIQUEL',
                                                            'BENEFICIAMENTO DE MINERIO DE ALUMINIO',
                                                            'PELOTIZACAO, SINTERIZACAO OUTROS BENEF DE MINERIO DE FERRO',
                                                            'EXTRACAO DE MINERAIS RADIOATIVOS'))
                   
# investigar pela descri��o aquelas sem munic�pio

# Por munic�pio
bndes.mineracao <- bndes.mineracao %>% 
                    select(5,6,28) %>% 
                    group_by(muni,cod_muni) %>%
                    summarise(desembolsos_mineracao_deflac = sum(desembolso_deflac)) %>% 
                    arrange(desc(desembolsos_mineracao_deflac))



# 3 - FINANCIAMENTOS PARA PETR�LEO
bndes.petroleo <- fin.bndes.deflac %>% 
                  dplyr::filter(nome_sub_setor_cnae %in% 'EXTRACAO DE PETROLEO E GAS NATURAL')

bndes.petroleo <- bndes.petroleo %>% 
                  select(5,6,28) %>% 
                  group_by(muni,cod_muni) %>%
                  summarise(desembolsos_petroleo_deflac = sum(desembolso_deflac)) %>% 
                  arrange(desc(desembolsos_petroleo_deflac))

# 4 - FINANCIAMENTOS PARA AGRO (aqui n�o limitei aos grandes projetos e grandes empresas)
bndes.agro <- fin.bndes.original %>% 
              mutate(fin.bndes.original,desembolso_deflac = deflate(valor_desembolsado_corrente,as.Date(ymd(fin.bndes.original$data_contrato)),'04/2021','igpdi')) %>% 
              dplyr::filter(natureza_cliente == 'PRIVADA'&
                            setor_cnae == 'AGROPECU�RIA E PESCA')


# agrupar por munic�pios
bndes.agro <- bndes.agro %>% 
              select(5,6,28) %>% 
              group_by(muni,cod_muni) %>%
              summarise(desembolsos_agro_deflac = sum(desembolso_deflac)) %>% 
              arrange(desc(desembolsos_agro_deflac))


# Juntar tudo (menos os agro)
grandes.projetos.bndes <- full_join(bndes.energia,bndes.mineracao)
grandes.projetos.bndes <- full_join(grandes.projetos.bndes,bndes.petroleo)


# Financiamentos na regi�o da Amaz�nia Legal
bndes.amz.legal <- grandes.projetos.bndes %>% 
  dplyr::filter(cod_muni %in% cidades.amazonia.legal) 

# Financiamentos nas cidades intermediadoras
bndes.cidades.intermed <- grandes.projetos.bndes %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras) 