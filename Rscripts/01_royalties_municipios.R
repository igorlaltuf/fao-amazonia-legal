# Dados sobre royalties
rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')
setwd('F:/Meu repositório/fao-amazonia-legal/')

# Royalties recebidos pelos municípios (dados do IPEA)
royalties <- read_excel(path = './Input/royalties_municipios_ipea.xlsx', col_types = c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","text"))
royalties[is.na(royalties)] <- 0

# 1 - Classificar Royalties de energia CFH
royalties.cfh <- royalties %>% 
             select(1,2,4) %>% 
             dplyr::filter(royalties_cfh_energia_eletrica > 0 &
                           cod_muni %in% cidades.amazonia.legal) %>% 
             arrange(desc(royalties_cfh_energia_eletrica)) 

royalties.cfh <- classificar.variavel(royalties.cfh,'royalties_cfh_energia_eletrica','class_royalties_cfh')

# teste para ver quantos itens existem em cada categoria
x <- royalties.cfh %>%
  group_by(class_royalties_cfh) %>%
  mutate(N_category = n()) %>%
  count(N_category)



# 2 - Classificar Royalties de ANP Petróleo

royalties.anp.petroleo <- royalties %>% 
  select(1,2,3) %>% 
  dplyr::filter(royalties_anp_petroleo > 0 &
                cod_muni %in% cidades.amazonia.legal) %>% 
  arrange(desc(royalties_anp_petroleo))

royalties.anp.petroleo <- classificar.variavel(royalties.anp.petroleo,'royalties_anp_petroleo','class_royalties_anp_petroleo')
summary(royalties.anp.petroleo$royalties_anp_petroleo)

# teste para ver quantos itens existem em cada categoria
x <- royalties.anp.petroleo %>%
     group_by(class_royalties_anp_petroleo) %>%
     mutate(N_category = n()) %>%
     count(N_category)

inter <- royalties.anp.petroleo %>% 
  dplyr::filter(royalties_anp_petroleo > 1000000) %>% 
  arrange(desc(royalties_anp_petroleo))

tabela.agro <- gt(inter) %>%
  cols_label(
    muni = 'Município',
    royalties_anp_petroleo = 'Valor dos Royalties ANP'
  ) %>% 
  cols_hide(
    columns = c(cod_muni,class_royalties_anp_petroleo)
  ) %>% 
  fmt_markdown(
    columns = c(muni)
  ) %>% 
  fmt_number(
    columns = c(royalties_anp_petroleo),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  )

tabela.agro
gtsave(tabela.agro, 'Outputs/03_mapas/Outros/03_tabela_royalties_petroleo.png')


# 3 - Classificar Royalties de Petróleo FEP - Repartição todos os municípios
royalties.fep.petroleo <- royalties %>% 
                          select(1,2,6) %>% 
                          dplyr::filter(royalties_fep_petroleo > 0 &
                                        cod_muni %in% cidades.amazonia.legal)

royalties.fep.petroleo <- classificar.variavel(royalties.fep.petroleo,'royalties_fep_petroleo','class_royalties_fep_petroleo')

# teste para ver quantos itens existem em cada categoria
x <- royalties.fep.petroleo %>% 
  group_by(class_royalties_fep_petroleo) %>%
  mutate(N_category = n()) %>%
  count(N_category)




# 4 - Classificar Royalties de Mineração CFEM
royalties.cfem <- royalties %>% 
                  select(1,2,5) %>% 
                  dplyr::filter(royalties_cfem_mineracao > 0 &
                  cod_muni %in% cidades.amazonia.legal) %>% 
                  arrange(desc(royalties_cfem_mineracao)) 

royalties.cfem <- classificar.variavel(royalties.cfem,'royalties_cfem_mineracao','class_royalties_cfem_mineracao') 


# teste para ver quantos itens existem em cada categoria
x <- royalties.cfem %>%
  group_by(class_royalties_cfem_mineracao) %>%
  mutate(N_category = n()) %>%
  count(N_category)


# 5 - Classificar Royalties PEA (bacias de grande volume)
royalties.pea <- royalties %>% 
                  select(1,2,8) %>% 
                  dplyr::filter(royalties_pea > 0 &
                  cod_muni %in% cidades.amazonia.legal)




# Juntar royalties mineração e energia
royalties <- full_join(royalties.cfh, royalties.cfem) %>% 
             select(-2)

royalties$cod_muni <- as.numeric(royalties$cod_muni)

royalties <- full_join(royalties, cidades.amazonia.legal.nome) 

royalties.pontuac <- royalties %>% 
  select(cod_muni, muni, class_royalties_cfh, class_royalties_cfem_mineracao)
      
# Exportar arquivo
write.csv(royalties.pontuac, file='Outputs/01_tabelas/01_royalties_mineracao_energia.csv', row.names = F)

# Royalties das cidades intermediadoras da Amazônia Legal
royalties.intermediadoras <- royalties.pontuac %>% 
  dplyr::filter(cod_muni %in% cidades.intermediadoras)

