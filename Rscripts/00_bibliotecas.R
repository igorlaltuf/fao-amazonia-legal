# Carregar bibliotecas
library(readxl) # ler arquivos xls
library(tidyverse) # manipulação de dados
library(gt) # criar tabelas
library(dotenv) # gerenciamento de credenciais base dos dados
library(deflateBR) # deflacionar valores
library(lubridate) # manipular datas
library(geobr) # baixar shapes do IBGE
library(basedosdados) # acessar dados da RAIS
library(udunits2) # precisa para iniciar o pacote sf no renv
library(sf) # Ler shapefiles
library(nngeo) # pacote para remover buracos quando uso st_union em shapefiles
library(ggspatial) 
library(RColorBrewer) #paleta de cores
library(beepr) # aviso ao termianr
library(ggrepel) # impede que as labels do ggplot se sobreponham
library(stringr) # para usar a função str_replace_all()
library(stringi) # para remover caracteres especiais
library(RColorBrewer) # paletas de cores
library(ggspatial) # elementos para os mapas
library(patchwork) # juntar gráficos na mesma figura
library(microdatasus) # carrega dados do datasus

# removi o pacote plyr porque estava dando conflito com as funções do dplyr que eu já havia utilizado