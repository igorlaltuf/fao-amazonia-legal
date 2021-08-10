# Dados sobre inraestruturas logística de suporte aos subconjuntos: armazens, portos, ferrovias, usinas de energia (térmicas e hidrelétricas)
# e projetos que serão implementados.
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# 1 - Verificar mercadorias embarcadas nos portos da Amazônia Legal em 2019 (AMZL)
muni.nome.amzl <- cidades.amazonia.legal.nome[2]
muni.nome.amzl <- stri_sub(muni.nome.amzl$muni,1,-6)
portos <- st_read('./Input/shapes logística/portos/Portos.shp') %>% 
          dplyr::filter(MUNICIPIO %in% muni.nome.amzl)
portos$CODPORTUAR <- as.character(portos$CODPORTUAR)

cod.portos.amzl <- st_set_geometry(portos['CODPORTUAR'],value = NULL)
cod.portos.amzl <- cod.portos.amzl[['CODPORTUAR']]      
cod.portos.amzl <- as.character(cod.portos.amzl)

# Fonte dos shapes: https://www.gov.br/infraestrutura/pt-br/assuntos/dados-de-transportes/bit/bitmodosmapas
# Classificação NCM https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta#Tabelas_Correlacoes

# Importar dados da ANTAQ de 2019 e verificar portos que são de empresas de logística ou públicos
mercadoria <- read.delim(file = 'Input/ANTAQ dados/Mercadoria.txt', sep = ';', dec = ',', fileEncoding = 'UTF-8-BOM') 
mercadoria.conteiner <- read.delim(file = 'Input/ANTAQ dados/MercadoriaConteinerizada.txt', sep = ';', dec = ',', fileEncoding = 'UTF-8-BOM') 
carga.cont <- read.delim(file = 'Input/ANTAQ dados/2019Carga_Conteinerizada.txt', sep = ';', dec = ',', fileEncoding = 'UTF-8-BOM') 
antaq <- read_csv2("Input/ANTAQ dados/2019Carga.txt") 

# Função que mostra o que é embarcado ou desembarcado em cada porto da AMZL
# Esta função retorna a quantidade em toneladas e o percetual sobre o total do porto.
# Importante: No valor em toneladas, foi descontado o peso dos containers, assim como foi discriminado o seu conteúdo interno.
# Por isso, o valor retornado pro esta função sempre será menor daquele exibido pelo sistema da ANTAQ, que mostra o valor bruto.
mercadoria.portos <- function(porto.origem,sentido){
# Filtrar por Porto
antaq <- antaq %>% 
         dplyr::filter(Origem %in% porto.origem & # Porto de Origem
                       Sentido %in% sentido) # Produção que sai do porto 'Embarcados' ou 'Desembarcados'

antaq <- left_join(antaq,mercadoria)
antaq <- left_join(antaq,carga.cont)
antaq <- left_join(antaq, mercadoria.conteiner)

antaq$VLPesoCargaConteinerizada <- as.numeric(sub(",", ".", antaq$VLPesoCargaConteinerizada, fixed = TRUE))

carga.solta <- antaq %>% 
               select(3,32,27,31,5) %>% 
               group_by(Origem,Nomenclatura.Simplificada.Mercadoria,CDMercadoria,Mercadoria) %>% 
               summarise(toneladas = sum(VLPesoCargaBruta)) %>% 
               arrange(desc(toneladas))

carga.solta$toneladas[is.na(carga.solta$toneladas)] <- 0

carga.conteiner <- antaq %>% 
                   select(3,38,34,37,33) %>% 
                   group_by(Origem,Nomenclatura.Simplificada.Mercadoria.Conteinerizada,CDMercadoriaConteinerizada,Mercadoria.Conteinerizada) %>% 
                   summarise(toneladas = sum(VLPesoCargaConteinerizada)) %>% 
                   arrange(desc(toneladas))

carga.conteiner$toneladas[is.na(carga.conteiner$toneladas)] <- 0

colnames(carga.conteiner) <- c('origem','nomenclatura','mercadoria','cod_mercadoria','embarque_toneladas')
colnames(carga.solta) <- c('origem','nomenclatura','mercadoria','cod_mercadoria','embarque_toneladas')

total.porto <- rbind(carga.solta,carga.conteiner) %>% 
               group_by(origem,nomenclatura,mercadoria,cod_mercadoria) %>% 
               summarise(embarque_toneladas_2019 = sum(embarque_toneladas)) %>% 
               arrange(desc(embarque_toneladas_2019))

total.porto <- total.porto[total.porto$nomenclatura != 'CONTÊINERES', ] # Remove peso dos containers (para não ter dupla contagem)
total.porto <- na.omit(total.porto) # Remove linhas com NAs 

valor.total <- sum(total.porto$embarque_toneladas_2019)
total.porto <- total.porto %>% 
               mutate(percentual = embarque_toneladas_2019/valor.total)

total.porto$percentual <- round(total.porto$percentual, digits=2)
return(total.porto) 
}

# Teste
# mercadoria.portos('BRAM004','Embarcados')


# Verificar todas as mercadorias embarcadas nos os portos da AMZL 
total.embarcado <- mercadoria.portos(cod.portos.amzl,'Embarcados') # aqui o % sobre o total não vai funcionar

# Ver categorias que aparecem na amzl para selecionar os códigos de interesse
categorias.amzl <- total.embarcado %>% 
                   ungroup(1) %>% 
                   select(2:4) %>% 
                   group_by(nomenclatura,mercadoria,cod_mercadoria) %>% 
                   unique()

# Verificar os portos que não tenho dados da ANTAQ e aqueles que tenho
portos.com.dados <- unique(total.embarcado$origem)
portos.sem.dados <- setdiff(cod.portos.amzl,portos.com.dados) 

# Isso pode ser confirmado por
embarq.por.porto<- mercadoria.portos('BRPA028','Embarcados') 

# filtrar no shape os portos sem dados para ver as empresas que são donas
portos.sem.dados.antaq <- portos %>% 
                          dplyr::filter(CODPORTUAR %in% portos.sem.dados)

# criar a tabela final dos portos
tabela.portos <- cidades.amazonia.legal.nome
tabela.portos$muni <- substr(tabela.portos$muni,1,nchar(tabela.portos$muni)-5)
tabela.portos <- left_join(portos,tabela.portos,by=c('MUNICIPIO'='muni'))

# Pontuação pela empresa proprietária do porto
# Mineração
minera <- str_extract(tabela.portos$COMPANHIA, regex("mineração|cadam|caulim|rio doce|alumar|alcoa", ignore_case = TRUE))
# Agro
agro <- str_extract(tabela.portos$COMPANHIA, regex("grãos|cargill|bunge", ignore_case = TRUE))
# Petróleo e gás
petr.e.gas <- str_extract(tabela.portos$COMPANHIA, regex("petróleo|petro|fogás", ignore_case = TRUE))

tabela.portos <- tabela.portos %>% 
                 select('cod_muni','MUNICIPIO','CODPORTUAR','NOMEPORTO','COMPANHIA') %>% 
                 mutate(porto_petroleo_e_gas = ifelse(str_detect(tabela.portos$COMPANHIA,petr.e.gas)==T,1,0)) %>% 
                 mutate(porto_agropecuaria = ifelse(str_detect(tabela.portos$COMPANHIA,agro)==T,1,0)) %>% 
                 mutate(porto_mineracao = ifelse(str_detect(tabela.portos$COMPANHIA,minera)==T,1,0)) 

# Pontuação pelas mercadorias embarcadas no porto 
# agropecuária
agro <- c('0102','0201','0202','1201','1005','1006','1208') # gado vivo, carne, milho, soja, arroz (não inclui derivados, como óleo de soja)
agro <- total.embarcado %>% 
        dplyr::filter(mercadoria %in% agro) 
agro <- unique(agro$origem)

# mineração
minera <- c('2507','2601','2602','2603','2606','2608','2609','2614','2615') # caulim, minérios de ferro, manganês, cobre,zinco, estanho, diversos e bauxita, 
minera <- total.embarcado %>% 
          dplyr::filter(mercadoria %in% minera)
minera <- unique(minera$origem)

# petróleo e gás
petr.e.gas <- c('2709','2710','2711','2713') # óleos brutos de petróleo, coque de petróleo, gás de petróleo
petr.e.gas <- total.embarcado %>% 
              dplyr::filter(mercadoria %in% petr.e.gas)
petr.e.gas <- unique(petr.e.gas$origem)


tabela.portos <- tabela.portos %>% 
  mutate(porto_petroleo_e_gas = ifelse(CODPORTUAR %in% petr.e.gas, 1,porto_petroleo_e_gas)) %>% 
  mutate(porto_agropecuaria = ifelse(CODPORTUAR %in% agro, 1,porto_agropecuaria)) %>% 
  mutate(porto_mineracao = ifelse(CODPORTUAR %in% minera, 1,porto_mineracao)) 

tabela.portos[is.na(tabela.portos)] <- 0


# Portos que a ANTAQ não tem dados e que não foram pontuados. Verificar no google se pertencem a algum subconjunto
verificar.informacao <- tabela.portos %>% 
         dplyr::filter(CODPORTUAR %in% portos.sem.dados)

# Terminal Portuário do Mearim Ltda (BRMA003) é um consórcio da Vale com uma empresa de petróleo e gás.
tabela.portos[tabela.portos$CODPORTUAR=='BRMA003',7] <- 1
tabela.portos[tabela.portos$CODPORTUAR=='BRMA003',9] <- 1
tabela.portos <- tabela.portos %>% 
                 mutate(portos_total = porto_petroleo_e_gas + porto_agropecuaria + porto_mineracao) %>% 
                 dplyr::filter(portos_total > 0)

# 25 portos dos 47 portos da AMZL são ligados a pelo menos 1 subconjunto.

# tabela final dos portos
tabela.portos <- tabela.portos %>%  
  as.data.frame() %>% 
  select(1,2,7,8,9) %>% 
  group_by(cod_muni) %>% 
  summarise(porto_agro = ifelse(sum(porto_agropecuaria) > 0,1,0),
            porto_petr_e_gás = ifelse(sum(porto_petroleo_e_gas) > 0,1,0),
            porto_mineracao = ifelse(sum(porto_mineracao) > 0,1,0))


# 2 - Verificar dados dos armazens existentes na AMZL em 2020
armazens <- read_excel(path = './Input/armazens.xlsx') 
armazens <- armazens %>% 
  mutate(existe_armazem = ifelse(armazens$armazens_e_silos_2_sem_2020 >0,1,0)) %>% 
  select(1,4) %>% 
  dplyr::filter(cod_muni %in% cidades.amazonia.legal)


# 3 - Ferrovias na AMZL
# foram considerados apenas os municípios que carregam os trens com produtos da mineração ou do agronegócio na AMZL

# Usar o shape ferrovias  para plotar as ferrovias na cor daquilo que é transportado
ferrovias <- st_read('./Input/shapes logística/ferrovias/Ferrovias.shp') %>% 
  dplyr::filter(UF %in% uf.amz.legal) %>% 
  mutate(ferrovia_mineral = ifelse(PRODUTOS_5 %in% 'Derivados de petróleo, álcool, cimento e ferro-gusa',1,0)) %>% 
  mutate(ferrovia_petroleo = ifelse(PRODUTOS_5 %in% 'Derivados de petróleo, álcool, cimento e ferro-gusa',1,0)) %>% 
  mutate(ferrovia_agropec = ifelse(PRODUTOS_5 %in% 'Minério de ferro, minério de manganês, ferro-gusa, veículos, combustível e soja',1,0)) %>% 
  mutate(ferrovia_mineral = ifelse(PRODUTOS_5 %in% 'Minério de ferro, minério de manganês, ferro-gusa, veículos, combustível e soja',1,ferrovia_mineral)) 

# Declaração de rede das ferrovias de 2020 - usar para pontuar as cidades em que as mercadorias são carregadas
# https://antt-hml.antt.gov.br/declaracao-de-rede-2020

# RMN - Rumo Malha Norte (antiga ALLMN)
rmn.prod <- read_excel('Input/ANTT declaração de rede/DR_2020_RMN.xlsx', sheet = 4, skip = 1)
####### RENOMEAR O TERMINAL Terminal Olacyr F. Morais??? QUAL CIDADE????###################
rmn.prod <- rmn.prod %>% 
            filter(str_detect(Origem, paste0(muni.nome.amzl,collapse = '|')))


# EFC - Estrada de Ferro dos Carajás (VALE)
efc.prod <- read_excel('Input/ANTT declaração de rede/DR_2020_EFC.xlsx', sheet = 4, skip = 1) 
# Colocar nomes dos municípios no lugar do nome dos terminais (Serra Leste é em Curionópolis, Serra Sul em Canaã dos Carajás e Carajás é em Paraupebas).
efc.prod$Origem<-replace(efc.prod$Origem, efc.prod$Origem=='Serra Leste (QSL, EFC)','Curionópolis')
efc.prod$Origem<-replace(efc.prod$Origem, efc.prod$Origem=='Serra Sul (QSS, EFC)','Canaã dos Carajás')
efc.prod$Origem<-replace(efc.prod$Origem, efc.prod$Origem=='Carajás (QCA, EFC)','Parauapebas')
efc.prod <- efc.prod %>% 
            filter(str_detect(Origem, paste0(muni.nome.amzl,collapse = '|')))


# FNS-TN (Ferr. Norte Sul Tramo Norte)
# aqui apenas um terminal da Suzano que o nome não é igual ao nome do município
fnstn.prod <- read_excel('Input/ANTT declaração de rede/DR_2020_FNSTN.xlsx', sheet = 4, skip = 1) %>%   
              filter(str_detect(Origem, paste0(muni.nome.amzl,collapse = '|')))

# Projetos ferroviários como ferrogrão, ferrovia do Pará, Ferrovia de integração do centro oeste não foram considerados na pontuação.

# Planilha das ferrovias
ferrovias.muni <- rbind(rmn.prod,efc.prod)
ferrovias.muni <- rbind(ferrovias.muni,fnstn.prod) %>% select(4,12) %>% unique()
colnames(ferrovias.muni) <- c('origem','produtos')

mineral <- str_extract(ferrovias.muni$produtos, regex("minério|mineral|ferro", ignore_case = TRUE))
agro <- str_extract(ferrovias.muni$produtos, regex("soja", ignore_case = TRUE))

ferrovias.muni <- ferrovias.muni %>% 
  mutate(ferrov_mineral = ifelse(str_detect(ferrovias.muni$produtos,mineral)==T,1,0)) %>% 
  mutate(ferrov_agropec = ifelse(str_detect(ferrovias.muni$produtos,agro)==T,1,0)) 


muni.nome.amzl <- cidades.amazonia.legal.nome
muni.nome.amzl$muni <- stri_sub(muni.nome.amzl$muni,1,-6)
tabela.ferrovias <- left_join(muni.nome.amzl,ferrovias.muni, by=c('muni'='origem'))
ferrovias.muni$origem <- substr(ferrovias.muni$origem,1,nchar(ferrovias.muni$origem)-11)
tabela.ferrovias <- left_join(tabela.ferrovias,ferrovias.muni, by=c('muni'='origem'))
ferrovias.muni$origem <- substr(ferrovias.muni$origem,1,nchar(ferrovias.muni$origem)-2)
tabela.ferrovias <- left_join(tabela.ferrovias,ferrovias.muni, by=c('muni'='origem'))

tabela.ferrovias <- tabela.ferrovias %>% 
                    select(1,2,4,5,7,8,10,11)

tabela.ferrovias[is.na(tabela.ferrovias)] <- 0

tabela.ferrovias <- tabela.ferrovias %>% 
                    mutate(total_mineral = ferrov_mineral.x + ferrov_mineral.y + ferrov_mineral) %>% 
                    mutate(total_agro = ferrov_agropec.x + ferrov_agropec.y + ferrov_agropec) %>% 
                    select(1,9,10) %>% 
                    group_by(cod_muni) %>% 
                    summarise(ferrov_mineral = sum(total_mineral),
                              ferrov_agro = sum(total_agro)) %>% 
                    dplyr::filter(ferrov_mineral > 0 | ferrov_agro > 0) %>% 
                    mutate(ferrov_mineral = ifelse(ferrov_mineral > 0, 1, 0)) %>% 
                    mutate(ferrov_agro = ifelse(ferrov_agro > 0, 1, 0))


# 4 - Dutovias na AMZL (não inclui na tabela final)
muni.nome.amzl <- cidades.amazonia.legal.nome[2]
muni.nome.amzl <- stri_sub(muni.nome.amzl$muni,1,-6)
muni.nome.amzl <- paste0(muni.nome.amzl,collapse = "|")

dutovias <- st_read('./Input/shapes logística/dutovias/Dutovias.shp') %>% 
            as.data.frame() %>% 
            select(2,15,22,23) %>% 
            dplyr::filter(grepl(muni.nome.amzl,MUNIC_ORIG,ignore.case = T))
          
# 5 - Rodovias 
# Ainda não foi decidido se será utilizado. Ver artigo do Banco Mundial.


# 6 - tabela síntese
infra.logistica <- left_join(cidades.amazonia.legal.nome,tabela.portos, by = c('cod_muni'='cod_muni'))
infra.logistica <- left_join(infra.logistica,armazens, by = c('cod_muni'='cod_muni'))
infra.logistica <- left_join(infra.logistica,tabela.ferrovias, by = c('cod_muni'='cod_muni'))

infra.logistica[is.na(infra.logistica)] <- 0

# aqui as duas últimas coluna sintetizam os valores de agro e mineração, caso um município tenha apresentado pelo menos um tipo de infraestrutura
infra.logistica <- infra.logistica %>% 
                   mutate(infra_agro = ifelse(porto_agro+existe_armazem+ferrov_agro>0,1,0)) %>% 
                   mutate(infra_mineral = ifelse(porto_mineracao+ferrov_mineral>0,1,0))  

# É o recorte das duas últimas colunas acima.
infra.logistica.acumulada <- infra.logistica %>% select(1,2,9,10)


# Exportar 
write.csv(infra.logistica.acumulada, file='Outputs/01_tabelas/01_infra_logistica.csv')

# Verificar as cidades intermediadoras
intermed <- infra.logistica.acumulada %>% 
            dplyr::filter(cod_muni %in% cidades.intermediadoras)
