# Base dos dados 
rm(list=ls()) # limpar as vari?veis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu reposit?rio/fao-amazonia-legal/')

# credenciais do data lake base dos dados
dotenv::load_dot_env()
credencial <- Sys.getenv('CREDENCIAL_BASE_DOS_DADOS')
basedosdados::set_billing_id(credencial) 





# sinal de gps de ônibus Rio de Janeiro:
library(basedosdados)
query <- "SELECT DISTINCT data FROM `datario.transporte_rodoviario_municipal.viagem_onibus`"
df <- read_sql(query)
nrow(df) > 107 # se for true é pq atualizou a tabela


# Uma linha para cada viagem (ida ou volta) que ocorreu aos domingos de 07/08/2022 até 27/11/2022
# query <- "SELECT data FROM `datario.transporte_rodoviario_municipal.viagem_onibus` 
#           WHERE data IN ('2022-08-07','2022-08-14','2022-08-21','2022-08-28',
#           '2022-09-04','2022-09-11','2022-09-18','2022-09-25',
#           '2022-10-02', '2022-10-09','2022-10-16','2022-10-23',
#           '2022-10-30', '2022-11-06', '2022-11-13', '2022-11-20', '2022-11-27')"
# 
# df <- read_sql(query)
# 
# write.csv2(df, 'viagens_ida_volta_domingos_rj.csv', row.names = F)


library(tidyverse)
library(magrittr)

dados_dia <- df %>% 
  group_by(data) %>% 
  count()



# 2 e 30 de outubro
2022-10-02
2022-10-30




# ----------------------------------------------------------------------------
# Dicionário de dados do CAGED
query <- 'SELECT * FROM `basedosdados.br_me_caged.dicionario`'
dicio_caged<- read_sql(query)


# Query CAGED Manaus (2020-2021)
# Filtrar tipo_movimentacao == 60 Desligamento Por Morte - 2020 e 2021

# óbitos por CBO em 2020
query <- 'SELECT ano, id_municipio, tipo_movimentacao, cbo_2002 FROM `basedosdados.br_me_caged.microdados_movimentacao`
          WHERE id_municipio = "1302603" AND
          ano = 2020 AND
          tipo_movimentacao = "60"'

df_pandemia_2020 <- read_sql(query)

write.csv2(df_pandemia_2020,'mortes_manaus_2020.csv')


# óbitos por CBO em 2021
query <- 'SELECT ano, id_municipio, tipo_movimentacao, cbo_2002 FROM `basedosdados.br_me_caged.microdados_movimentacao`
          WHERE id_municipio = "1302603" AND
          ano = 2021 AND
          tipo_movimentacao = "60"'

df_pandemia_2021 <- read_sql(query)

write.csv2(df_pandemia_2021,'mortes_manaus_2021.csv')


# Query CAGED Manaus (2019)
# filtrar tipo_movimentacao_desagregado: 08 Desligamentos Por Morte - até 2019
query <- 'SELECT ano, id_municipio, cbo_2002, tipo_movimentacao_desagregado FROM `basedosdados.br_me_caged.microdados_antigos_ajustes` 
          WHERE id_municipio = "1302603" AND
          ano = 2019 AND
          tipo_movimentacao_desagregado = "08"'

df_pre_pandemia_2019 <- read_sql(query)

write.csv2(df_pre_pandemia_2019,'mortes_manaus_2019.csv')


# Query CAGED Manaus (2018)
# filtrar tipo_movimentacao_desagregado: 08 Desligamentos Por Morte - até 2018
query <- 'SELECT ano, id_municipio, cbo_2002, tipo_movimentacao_desagregado FROM `basedosdados.br_me_caged.microdados_antigos_ajustes` 
          WHERE id_municipio = "1302603" AND
          ano = 2018 AND
          tipo_movimentacao_desagregado = "08"'

df_pre_pandemia_2018 <- read_sql(query)

write.csv2(df_pre_pandemia_2018,'mortes_manaus_2018.csv')


# Agrupar os 4 anos em um único CSV
df_pre_pandemia_2019 <- dplyr::rename(df_pre_pandemia_2019, 
                                      tipo_movimentacao = tipo_movimentacao_desagregado)

df_pre_pandemia_2018 <- dplyr::rename(df_pre_pandemia_2018, 
                                      tipo_movimentacao = tipo_movimentacao_desagregado)

base_covid_manaus_cbo <- rbind(df_pandemia_2021,df_pandemia_2020,df_pre_pandemia_2019, df_pre_pandemia_2018)

write.csv2(base_covid_manaus_cbo,'mortes_manaus_cbo.csv')




# RAIS Dicionário de dados
query <- 'SELECT * FROM `basedosdados.br_me_rais.dicionario`'

dicio_rais <- read_sql(query)



# RAIS 
query <- 'SELECT * FROM `basedosdados.br_me_rais.microdados_estabelecimentos` LIMIT 100'

estab_rais <- read_sql(query)








# chamados iluminação pública

library(basedosdados)
query <- "SELECT data_inicio, id_bairro, id_logradouro, id_unidade_organizacional_mae, tipo, subtipo, tipo_situacao, dentro_prazo, longitude, latitude  FROM `datario.administracao_servicos_publicos.chamado_1746`
          WHERE data_inicio >= '2022-01-01 00:00:00'
          AND data_inicio < '2022-10-15 00:00:00'
          "
df <- read_sql(query)

write.csv2(df, 'chamados_prefeitura_2022.csv', row.names = F)




# RAIS Teres?polis
query <- "SELECT id_municipio, ano, quantidade_vinculos_ativos, cnae_2, cnae_2_subclasse, subsetor_ibge, tamanho,natureza_juridica, tipo, cep  FROM `basedosdados.br_me_rais.microdados_estabelecimentos`
          WHERE id_municipio IN ('3305802') AND
          quantidade_vinculos_ativos >= 1 AND
          ano IN (2019)"

df <- read_sql(query)
write.csv(df, 'Outputs/00_shapes_e_dados/tere_rais_estab_2019.csv', row.names = F)



# ANATEL
query <- "SELECT ano, sigla_uf, id_municipio, empresa, sinal, produto, SUM(acessos) as acessos FROM `basedosdados.br_anatel_telefonia_movel.municipio`
          WHERE ano = 2019
          GROUP BY ano, sigla_uf, id_municipio, empresa, sinal, produto"
internet <- read_sql(query)
write.csv(internet, 'Outputs/00_shapes_e_dados/00_internet_anatel.csv', row.names = F)

# DATASUS - SIM Municipal
query <- "SELECT * FROM `basedosdados.br_ms_sim.municipio_causa`" # baixas todos os dados de mortalidade do datasus
datasus <- read_sql(query)
write.csv(datasus, file = 'Outputs/00_shapes_e_dados/00_datasus_mortalidade_anual.csv', row.names = F)

# INEP - Censo Escolar - Escolas
query <- "SELECT ano,sigla_uf,id_municipio,rede,id_escola,id_orgao_regional,tipo_situacao_funcionamento,
          tipo_localizacao,tipo_categoria_escola_privada,conveniada_poder_publico,tipo_convenio_poder_publico,
          mantenedora_escola_privada_emp,mantenedora_escola_privada_ong,mantenedora_escola_privada_sind,mantenedora_escola_privada_sist_s,
          mantenedora_escola_privada_s_fins FROM `basedosdados.br_inep_censo_escolar.escola`
          WHERE sigla_uf IN ('AC', 'RR', 'AM', 'RO', 'PA', 'AP', 'MA', 'TO', 'MT') AND
          rede IN ('municipal','estadual','federal') "
df <- read_sql(query)
write.csv(df,'Outputs/00_shapes_e_dados/00_base_escolas_2009_2020.csv', row.names = F)

# INEP - Censo Escolar - Matr?culas
query <- "SELECT ano,sigla_uf,id_municipio,rede,id_escola,quantidade_matriculas FROM `basedosdados.br_inep_censo_escolar.turma`
          WHERE sigla_uf IN ('AC', 'RR', 'AM', 'RO', 'PA', 'AP', 'MA', 'TO', 'MT') AND
          rede IN ('municipal','estadual','federal')"
df <- read_sql(query)
write.csv(df,'Outputs/00_shapes_e_dados/00_base_matriculas_2009_2020.csv', row.names = F)



# Abaixo foram filtrados os docentes de escolas p?blicas que trabalharam nos 20 munic?pios que mais receberam recursos via royalties cfm e cfh.
# INEP - Censo Escolar - Professores escolas p?blicas CFM
query <- "SELECT id_municipio, id_escola, ano, sigla_uf, rede FROM `basedosdados.br_inep_censo_escolar.docente`
          WHERE sigla_uf IN ('AC', 'RR', 'AM', 'RO', 'PA', 'AP', 'MA', 'TO', 'MT') AND
          rede IN ('municipal','estadual','federal') AND
          id_municipio IN ('1505536','1502152','1504208','1505304','1505502','1503903','1503457',
                           '1600154','1507979','1303536','1502772','5105903','1600808','1503606',
                           '5105507','1507300','1100023','1503044','1100205','2104305')"
df <- read_sql(query)
write.csv(df,'Outputs/00_shapes_e_dados/00_base_docentes_cfm_2009_2020.csv', row.names = F)



# INEP - Censo Escolar - Professores escolas p?blicas CFH
query <- "SELECT id_municipio, id_escola, ano, sigla_uf, rede FROM `basedosdados.br_inep_censo_escolar.docente`
          WHERE sigla_uf IN ('AC', 'RR', 'AM', 'RO', 'PA', 'AP', 'MA', 'TO', 'MT') AND
          rede IN ('municipal','estadual','federal') AND
          id_municipio IN ('1505064','1100205','1508100','1503093','1503804','1501782','1718204','5104609',
                           '1503705','1500602','5106299','1716208','1508357','1303536','1504976','1600238',
                           '1721000','2102804','5101407','5103007')"
          
df <- read_sql(query)
write.csv(df,'Outputs/00_shapes_e_dados/00_base_docentes_cfh_2009_2020.csv', row.names = F)




# RAIS - V?nculos ativos em 31-12-2019 por munic?pio de acordo com a CNAE 2.0
query <- "SELECT id_municipio, ano, cnae_2, SUM(numero_vinculos) as vinculos_ativos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE ano = 2019 AND vinculo_ativo_3112 = 1
          GROUP BY id_municipio, ano, cnae_2"
df <- read_sql(query)
df$cnae_2 <- as.character(df$cnae_2)
write.csv(df,'Outputs/00_shapes_e_dados/00_rais_ativos_2019.csv', row.names = F)

# RAIS - V?nculos totais em 2019 por munic?pio de acordo com a CNAE 2.0
query <- "SELECT id_municipio, ano, cnae_2, SUM(numero_vinculos) as vinculos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE ano = 2019
          GROUP BY id_municipio, ano, cnae_2"
df <- read_sql(query)
df$cnae_2 <- as.character(df$cnae_2)
write.csv(df,'Outputs/00_shapes_e_dados/00_rais_vinculos_2019.csv', row.names = F)

# Pacote microdatasus
# 1 - Dados de estabelecimentos

# Cruzar dados abaixo com tipo de estabelecimento de sa?de do datasus para saber onde est?o os hospitais, upas etc
# algumas colunas s? aparecem quando eu fa?o a requisi??o especificamente pelo seu nome (NIVATE_H e URGEMERG).
# Por isso eu selecionei as colunas que preciso.

var.cnes.st <- c('CNES','CODUFMUN','COD_CEP','VINC_SUS','TPGESTAO','TP_UNID','TURNO_AT','GESPRG1E', 
                 'GESPRG1M','GESPRG2E','GESPRG2M','GESPRG4E','GESPRG4M','GESPRG5E','GESPRG5M', 
                 'GESPRG6E','GESPRG6M','NIVATE_A','NIVATE_H','URGEMERG')

x <- fetch_datasus(year_start = 2015,
                   year_end = 2015, 
                   month_start = 12, 
                   month_end = 12, 
                   uf = uf.amz.legal, 
                   information_system = "CNES-ST", 
                   vars = var.cnes.st)

write.csv(x,'Outputs/00_shapes_e_dados/00_cnes_st_2015.csv', row.names = F)

# 2 - Dados de Leitos
var.cnes.lt <- c('CNES','CODUFMUN','TP_UNID','TP_LEITO','CODLEITO','QT_EXIST','QT_CONTR','QT_SUS','QT_NSUS')

y <- fetch_datasus(year_start = 2019,
                   year_end = 2019, 
                   month_start = 12, 
                   month_end = 12, 
                   uf = uf.amz.legal, 
                   information_system = "CNES-LT",
                   var = var.cnes.lt)

write.csv(y,'Outputs/00_shapes_e_dados/00_cnes_lt_2019.csv', row.names = F)









