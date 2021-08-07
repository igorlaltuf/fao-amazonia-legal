# Dados sobre inraestruturas de suporte aos subconjuntos: armazens, portos, ferrovias, usinas de energia (térmicas e hidrelétricas)
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

# Exportar (refazer e exportar um arquivo único)
write.csv(tabela.portos, file='Outputs/01_tabelas/01_infraestrutura_produtiva.csv')


# 2 - Verificar dados dos armazens existentes na AMZL em 2020
armazens <- read_excel(path = './Input/armazens.xlsx') 
armazens <- armazens %>% 
  mutate(existe_armazem = ifelse(armazens$armazens_e_silos_2_sem_2020 >0,1,0)) %>% 
  mutate(existe_armazem2 = ifelse(armazens$armazens_e_silos_2_sem_2020 >0,'sim','não')) %>% 
  dplyr::filter(cod_muni %in% cidades.amazonia.legal)


# 3 - Ferrovias na AMZL













# 4 - Dutovias na AMZL







# 5 - Infraestrutura energética



# Dados de geração de energia na Amazônia Legal (AGUARDANDO DADOS DA AMANDA DAS SUB-BACIAS E TERMOELÉTRICAS)
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
# Incluir dados sobre terminais ferroviários que escoam Soja, Milho e Arroz, minério de ferro etc

# Exportar o arquivo
#write.csv(prod.agro,file='Outputs/01_tabelas/01_producao_agro.csv',sep = ';',na = '0')

# Verificar as cidades intermediadoras
