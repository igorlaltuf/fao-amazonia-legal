# Subconjunto energético
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Carregar tabelas output
vab.energia <- read.csv('Outputs/01_tabelas/01_vab_energia_2017.csv')
empregos.energia <- read.csv('Outputs/01_tabelas/01_empregos_rais.csv')
royalties <- read.csv('Outputs/01_tabelas/01_royalties_mineracao_energia.csv')
bndes <- read.csv('Outputs/01_tabelas/01_financ_bndes.csv') 

# Coari gás financiamento (não teve), empregos (ver municípios) e royalties (incluir municípios com valor acima de 1 milhão)
# Explicar que o critério do petróleo será diferente.




# Incluir as infraestruturas de energia elétrica (já tenho os dados. Ver financiamentos do BNDES para ver usinas pós 2015 e contrapor os dados (belo monte não consta)
# emprego em construção civil na época da obra
# Ver área de influencia dos empreendimentos UHE