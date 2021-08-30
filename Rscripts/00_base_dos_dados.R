# Base dos dados 
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# credenciais do data lake base dos dados
load_dot_env()
credencial <- Sys.getenv('CREDENCIAL_BASE_DOS_DADOS')
basedosdados::set_billing_id(credencial)   

# ANATEL
query <- "SELECT ano, sigla_uf, id_municipio, empresa, sinal, produto, SUM(acessos) as acessos FROM `basedosdados.br_anatel_telefonia_movel.municipio`
          WHERE ano = 2019
          GROUP BY ano, sigla_uf, id_municipio, empresa, sinal, produto"
internet <- read_sql(query)
write.csv(internet, 'Outputs/00_shapes_e_dados/00_internet_anatel.csv',row.names = F)

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

# INEP - Censo Escolar - Matrículas
query <- "SELECT ano,sigla_uf,id_municipio,rede,id_escola,quantidade_matriculas FROM `basedosdados.br_inep_censo_escolar.turma`
          WHERE sigla_uf IN ('AC', 'RR', 'AM', 'RO', 'PA', 'AP', 'MA', 'TO', 'MT') AND
          rede IN ('municipal','estadual','federal')"
df <- read_sql(query)
write.csv(df,'Outputs/00_shapes_e_dados/00_base_matriculas_2009_2020.csv', row.names = F)

# RAIS - Vínculos ativos em 31-12-2019 por município de acordo com a CNAE 2.0
query <- "SELECT id_municipio, ano, cnae_2, SUM(numero_vinculos) as vinculos_ativos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE ano = 2019 AND vinculo_ativo_3112 = 1
          GROUP BY id_municipio, ano, cnae_2"
df <- read_sql(query)
df$cnae_2 <- as.character(df$cnae_2)
write.csv(df,'Outputs/00_shapes_e_dados/00_rais_ativos_2019.csv', row.names = F)

# RAIS - Vínculos totais em 2019 por município de acordo com a CNAE 2.0
query <- "SELECT id_municipio, ano, cnae_2, SUM(numero_vinculos) as vinculos FROM `basedosdados.br_me_rais.agregado_vinculos_municipio_vinculo_ativo_cbo_cnae_natureza_juridica_idade_sexo_raca`
          WHERE ano = 2019
          GROUP BY id_municipio, ano, cnae_2"
df <- read_sql(query)
df$cnae_2 <- as.character(df$cnae_2)
write.csv(df,'Outputs/00_shapes_e_dados/00_rais_vinculos_2019.csv', row.names = F)