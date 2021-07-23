# Dados sobre Financiamentos do BNDES
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# 1 - FINANCIAMENTOS PARA GERAÇÃO DE ENERGIA ELÉTRICA
# Grandes projetos de geração de energia financiados pelo BNDES na Amazônia Legal
fin.bndes.original <- read_excel('Input/bndes_financiamentos.xlsx')
which(is.na(fin.bndes.original$valor_contratado_corrente)) # retorna a linha dos NAs se existirem

fin.bndes.deflac <- fin.bndes.original %>% 
                    mutate(fin.bndes.original,desembolso_deflac = deflate(valor_desembolsado_corrente,as.Date(ymd(fin.bndes.original$data_contrato)),'04/2021','igpdi')) %>% 
                    dplyr::filter(desembolso_deflac>=40000000 & 
                    porte_cliente == 'GRANDE' &
                    natureza_cliente == 'PRIVADA') #filtra os grandes investimentos (acima de R$ 40 milhões de 2021)
                 

bndes.energia <- fin.bndes.deflac %>% 
                 filter(str_detect(nome_sub_setor_cnae, "^GERACAO DE ENERGIA ELETRICA"))

bndes.energia.cidades <- bndes.energia %>% 
                         select(5,6,23) %>% 
                         unique() 
                         

### Continuar daqui!!! (juntar as cidades encontradas na descrição abaixo com as cidades da tabela acima e depois filtrar pela amazonia legal)

amz.legal.uppercase <- mutate_all(cidades.amazonia.legal.nome, .funs=toupper) 
amz.legal.uppercase$muni <- str_sub(amz.legal.uppercase$muni,1,nchar(amz.legal.uppercase$muni )-5)
# Falta remover Ç,~,acentos,' etc e incluir no filtro abaixo:##### IMPORTANTE!

# Como muitas delas estão categorizadas como "SEM MUNICÍPIO", vou checar se o nome das cidades está na descrição:
intermed.descricao <- bndes.energia %>% 
  dplyr::filter(muni == 'SEM MUNICÍPIO') %>% 
  dplyr::filter(grepl('RIO BRANCO|ITACOATIARA|LABREA|MANACAPURU|PARINTINS|TABATINGA|TEFE|MACAPA
                      |OIAPOQUE|BACABAL|CAXIAS|IMPERATRIZ|PRESIDENTE DUTRA|SANTA INES|BARRA DO GARCAS
                      |CACERES|CUIABA|RONDONOPOLIS|SINOP|ALTAMIRA|BREVES|CASTANHAL|MARABA|REDENCAO
                      |SANTAREM|ARIQUEMES|JI-PARANA|PORTO VELHO|BOA VISTA|RORAINOPOLIS|ARAGUAINA|GURUPI
                      |PALMAS', descricao)) 

# Valor agrupado por município
bndes.energia <- bndes.energia %>% 
                 select(5,6,28) %>% 
                 group_by(muni,cod_muni) %>%
                 summarise(desembolsos_energia_deflac = sum(desembolso_deflac)) %>% 
                 arrange(desc(desembolsos_energia_deflac))


# 2 - FINANCIAMENTOS PARA MINERAÇÃO

# REVISAR ESTE FILTRO DE MINERAÇÃO (perguntar para Luis)
bndes.mineracao <- fin.bndes.deflac %>% 
                   dplyr::filter(nome_sub_setor_cnae %in% c('EXT MIN COBRE CHMBO ZNCO OUTR MIN MET NAO-FERRO NAO ESPEC ANTER',
                                                            'EXTRACAO DE MINERIO DE FERRO',
                                                            'EXTRACAO DE MINERIO DE ALUMINIO',
                                                            'BNFC MIN CBRE CHMBO ZNCO OUTR MIN MET NAO-FERRO NAO ESPEC ANTER',
                                                            'EXTRACAO DE MINERIO DE NIQUEL',
                                                            'BENEFICIAMENTO DE MINERIO DE ALUMINIO',
                                                            'PELOTIZACAO, SINTERIZACAO OUTROS BENEF DE MINERIO DE FERRO',
                                                            'EXTRACAO DE MINERAIS RADIOATIVOS'))
                   

# Por município
bndes.mineracao <- bndes.mineracao %>% 
                    select(5,6,28) %>% 
                    group_by(muni,cod_muni) %>%
                    summarise(desembolsos_mineracao_deflac = sum(desembolso_deflac)) %>% 
                    arrange(desc(desembolsos_mineracao_deflac))


# investigar pela descrição aquelas sem município
# criar um df que reúne as classificações por descrição e município 


# 3 - FINANCIAMENTOS PARA PETRÓLEO (não foram financiados grandes projetos na Amazonia Legal)
bndes.petroleo <- fin.bndes.deflac %>% 
                  dplyr::filter(nome_sub_setor_cnae %in% 'EXTRACAO DE PETROLEO E GAS NATURAL')


# Reunir dados
grandes.projetos.bndes <- full_join(bndes.energia,bndes.mineracao)



# exportar tabela

# Financiamentos nas cidades intermediadoras
bndes.cidades.intermed <- grandes.projetos.bndes %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras) 