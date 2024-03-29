### Uso da terra voltado para minera��o para os anos de 2004,2008,2010,2012 e 2014
# munic�pios com pelo menos 30 km� desmatados no per�odo em fun��o da minera��o
rm(list = ls()) # limpar as vari�veis carregadas
source("Rscripts/00_bibliotecas.R")
source("Rscripts/00_variaveis_globais.R")
source("Rscripts/00_funcoes_globais.R")
setwd("F:/Meu reposit�rio/fao-amazonia-legal/")

# Carregar todos os arquivos
lista.de.arquivos <- list.files(
  path = "Input/desmatamento mineracao 2014/", recursive = TRUE,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# importar arquivos de 2014 dos estados em que h� desmatamento por minera��o
arquivo <- c(1:9)
i <- 1
while (i <= length(lista.de.arquivos)) {
  ano <- c(2004, 2008, 2010, 2012, 2014)
  j <- 1
  while (j <= 5) {
    assign(paste("desmat.miner", arquivo[i], ano[j], sep = "."), read_excel(paste(lista.de.arquivos[i]), skip = 2, sheet = j))
    j <- j + 1
  }
  j <- 1
  i <- i + 1
}


# Agrupar por ano (n�o preciso agrupar aqueles estados em que n�o existiu minera��o):
miner.2004 <- bind_rows(
  desmat.miner.1.2004, desmat.miner.2.2004, desmat.miner.3.2004, desmat.miner.4.2004,
  desmat.miner.5.2004, desmat.miner.6.2004, desmat.miner.7.2004, desmat.miner.8.2004,
  desmat.miner.9.2004
) %>%
  dplyr::select("Munic�pios", "Minera��o") %>%
  mutate(ano = 2004)

miner.2008 <- bind_rows(
  desmat.miner.1.2008, desmat.miner.2.2008, desmat.miner.3.2008, desmat.miner.4.2008,
  desmat.miner.5.2008, desmat.miner.6.2008, desmat.miner.7.2008, desmat.miner.8.2008,
  desmat.miner.9.2008
) %>%
  dplyr::select("Munic�pios", "Minera��o") %>%
  mutate(ano = 2008)

miner.2010 <- bind_rows(
  desmat.miner.1.2010, desmat.miner.2.2010, desmat.miner.3.2010, desmat.miner.4.2010,
  desmat.miner.5.2010, desmat.miner.6.2010, desmat.miner.7.2010, desmat.miner.8.2010,
  desmat.miner.9.2010
) %>%
  dplyr::select("Munic�pios", "Minera��o") %>%
  mutate(ano = 2010)

miner.2012 <- bind_rows(
  desmat.miner.1.2012, desmat.miner.2.2012, desmat.miner.3.2012, desmat.miner.4.2012,
  desmat.miner.5.2012, desmat.miner.6.2012, desmat.miner.7.2012, desmat.miner.8.2012,
  desmat.miner.9.2012
) %>%
  dplyr::select("Munic�pios", "Minera��o") %>%
  mutate(ano = 2012)

miner.2014 <- bind_rows(
  desmat.miner.1.2014, desmat.miner.2.2014, desmat.miner.3.2014, desmat.miner.4.2014,
  desmat.miner.5.2014, desmat.miner.6.2014, desmat.miner.7.2014, desmat.miner.8.2014,
  desmat.miner.9.2014
) %>%
  dplyr::select("Munic�pios", "Minera��o") %>%
  mutate(ano = 2014)


desm.minerac.total <- bind_rows(miner.2004, miner.2008, miner.2010, miner.2012, miner.2014)

desm.minerac.total$Minera��o[is.na(desm.minerac.total$Minera��o)] <- 0
desm.minerac.total$Minera��o <- round(desm.minerac.total$Minera��o, 2)

desm.minerac.total <- desm.minerac.total %>%
  dplyr::select(1, 2) %>%
  group_by(Munic�pios) %>%
  summarise(total = sum(Minera��o)) %>%
  dplyr::filter(total > 0)


desmatamento.mineracao <- cidades.amazonia.legal.nome
desmatamento.mineracao[2] <- substr(cidades.amazonia.legal.nome$muni, 1, nchar(cidades.amazonia.legal.nome$muni) - 5)

# remover acentos com fun��o do script global
df_teste_sem_acento <- rm_accent(desmatamento.mineracao$muni)
desmatamento.mineracao[2] <- df_teste_sem_acento


desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "Tucume", "Tucuma")
desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "Poxoreo", "Poxoreu")
desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "Machadinho DOeste", "Machadinho D'Oeste")
desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "Espigao D Oeste", "Espigao D'Oeste")
desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "Mirassol d Oeste", "Mirassol d'Oeste")
desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "Eldorado dos Carajas", "Eldorado do Carajas")
desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "Santa Isabel do Para", "Santa Izabel do Para")
desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "Pau DArco", "Pau D'Arco")
desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "Igarape Miri", "Igarape-Miri")
desm.minerac.total$Munic�pios <- replace(desm.minerac.total$Munic�pios, desm.minerac.total$Munic�pios == "JiParana", "Ji-Parana")

desmatamento.mineracao <- left_join(desm.minerac.total, desmatamento.mineracao, by = c("Munic�pios" = "muni"))

desmatamento.mineracao$cod_muni <- replace(desmatamento.mineracao$cod_muni, desmatamento.mineracao$cod_muni == "1505551", NA) # excluir cidade com o mesmo nome em dois estados diferentes

desmatamento.mineracao <- na.omit(desmatamento.mineracao) # verificar NA

desmatamento.mineracao <- desmatamento.mineracao %>% select(3, 1, 2)

desmatamento.mineracao <- classificar.variavel(desmatamento.mineracao, "total", "class_desmatamento_minerac")

desmatamento.mineracao <- left_join(desmatamento.mineracao, cidades.amazonia.legal.nome, by = "cod_muni") %>% # colocar nomes corretos dos munic�pios
  select(1, 5, 3)

desmatamento.mineracao <- classificar.variavel(desmatamento.mineracao, "total", "class_desmat_min")

write.csv(desmatamento.mineracao, "Outputs/01_tabelas/01_desmatamento_mineracao.csv", row.names = F)
