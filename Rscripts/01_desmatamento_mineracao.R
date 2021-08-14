### Uso da terra voltado para mineração para os anos de 2004,2008,2010,2012 e 2014
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar todos os arquivos 
lista.de.arquivos <- list.files(path = "Input/desmatamento mineracao 2014/", recursive = TRUE,
                                pattern = "\\.xlsx$", 
                                full.names = TRUE)

# importar arquivos de 2014 dos estados em que há desmatamento por mineração
arquivo <- c(1:9)
i <- 1
while (i<=length(lista.de.arquivos)) {
  ano <- c(2004,2008,2010,2012,2014)
  j <- 1
  while (j <= 5) {
    assign(paste("desmat.miner",arquivo[i],ano[j], sep="."),read_excel(paste(lista.de.arquivos[i]), skip = 2,sheet = j)) 
    j <- j + 1
  }
  j <- 1
  i <- i + 1
}


# Agrupar por ano (não preciso agrupar aqueles estados em que não existiu mineração):
miner.2004 <- bind_rows(desmat.miner.1.2004, desmat.miner.2.2004, desmat.miner.3.2004, desmat.miner.4.2004,
                        desmat.miner.5.2004,desmat.miner.6.2004,desmat.miner.7.2004, desmat.miner.8.2004,
                        desmat.miner.9.2004) %>% 
              dplyr::select('Municípios','Mineração') %>% 
              mutate(ano = 2004)

miner.2008 <- bind_rows(desmat.miner.1.2008, desmat.miner.2.2008, desmat.miner.3.2008, desmat.miner.4.2008,
                        desmat.miner.5.2008,desmat.miner.6.2008,desmat.miner.7.2008, desmat.miner.8.2008,
                        desmat.miner.9.2008) %>% 
              dplyr::select('Municípios','Mineração') %>% 
              mutate(ano = 2008)

miner.2010 <- bind_rows(desmat.miner.1.2010, desmat.miner.2.2010, desmat.miner.3.2010, desmat.miner.4.2010,
                        desmat.miner.5.2010,desmat.miner.6.2010,desmat.miner.7.2010, desmat.miner.8.2010,
                        desmat.miner.9.2010) %>% 
              dplyr::select('Municípios','Mineração') %>% 
              mutate(ano = 2010)

miner.2012 <- bind_rows(desmat.miner.1.2012, desmat.miner.2.2012, desmat.miner.3.2012, desmat.miner.4.2012,
                        desmat.miner.5.2012,desmat.miner.6.2012,desmat.miner.7.2012, desmat.miner.8.2012,
                        desmat.miner.9.2012) %>% 
              dplyr::select('Municípios','Mineração') %>% 
              mutate(ano = 2012)

miner.2014 <- bind_rows(desmat.miner.1.2014, desmat.miner.2.2014, desmat.miner.3.2014, desmat.miner.4.2014,
                        desmat.miner.5.2014,desmat.miner.6.2014,desmat.miner.7.2014, desmat.miner.8.2014,
                        desmat.miner.9.2014) %>% 
              dplyr::select('Municípios','Mineração') %>% 
              mutate(ano = 2014)


desm.minerac.total <- bind_rows(miner.2004,miner.2008,miner.2010,miner.2012,miner.2014)

desm.minerac.total$Mineração[is.na(desm.minerac.total$Mineração)] <- 0
desm.minerac.total$Mineração <- round(desm.minerac.total$Mineração, 2)

desm.minerac.total <- desm.minerac.total %>% 
                      dplyr::select(1,2) %>% 
                      group_by(Municípios) %>% 
                      summarise(total = sum(Mineração)) %>% 
                      dplyr::filter(total > 0)


desmatamento.mineracao <- cidades.amazonia.legal.nome
desmatamento.mineracao[2] <- substr(cidades.amazonia.legal.nome$muni,1,nchar(cidades.amazonia.legal.nome$muni)-5)

# remover acentos com função do script global 
df_teste_sem_acento <- rm_accent(desmatamento.mineracao$muni)
desmatamento.mineracao[2] <- df_teste_sem_acento

# continuar daqui!!! Fazer o join abaixo
 



desmatamento.mineracao <- left_join(desm.minerac.total,desmatamento.mineracao,by = c('Municípios'='muni'))





# desmatamento.mineracao os códigos estão com NA nos municípios que não estão com o mesmo nome! resolver isso!
# remover pontuação


# CONTINUAR DAQUI!
# verificar nomes duplicados (Rio Branco)
# Verificar nomes com escrita diferente (D'Oeste, caixa alta etc)
duplicated(desmatamento.mineracao$muni)





# Fazer bater com os valores das cidades da amz legal para pegar os códigos
# Remover a linha com o total (será feito automaticamente)
# classificar



