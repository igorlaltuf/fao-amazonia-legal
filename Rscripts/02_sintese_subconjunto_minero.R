# TABELA MINERA��O
rm(list=ls()) # limpar as vari�veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit�rio/fao-amazonia-legal/')

# Carregar tabelas output
vab <- read.csv('Outputs/01_tabelas/01_vab_extrativista_2017.csv')
empregos <- read.csv('Outputs/01_tabelas/01_empregos_rais.csv')
royalties <- read.csv('Outputs/01_tabelas/01_royalties_mineracao_energia.csv')
# Financ BNDES falta acabar o levantamento das descri��es dos SEM MUNIC�PIOS
# read.csv('Outputs/01_tabelas/01_royalties_mineracao_energia.csv') infraestruturas produtivas


# Juntar tudo em uma tabela que tenha 0 ou 1 de acordo com o crit�rio.





# Criar uma coluna que some as colunas



# Exportar os arquivos
