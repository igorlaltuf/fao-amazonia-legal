# Rodar todos os scripts da primeira etapa e gerar os arquivos na pasta /Output/01-tabelas
# Dados sobre produção agrícola do agronegócio
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
source('Rscripts/01_empregos_agro_inclusive_informais.R')
source('Rscripts/01_empregos_formais_RAIS.R')
source('Rscripts/01_financiamentos_bndes.R')
source('Rscripts/01_grandes_e_medios_estabelec_agropec.R')
source('Rscripts/01_infraestruturas_produtivas.R')
source('Rscripts/01_producao_agricola.R')
source('Rscripts/01_producao_pecuaria.R')
source('Rscripts/01_royalties_municipios.R')
source('Rscripts/01_vab_entre_tres_maiores.R')
source('Rscripts/01_itr_classificacao.R')
beep(sound = 3)
rm(list=ls()) # limpar as variáveis carregadas