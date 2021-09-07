rm(list=ls()) # limpar as variáveis carregadas
source('Rscripts/00_bibliotecas.R')
source('Rscripts/00_variaveis_globais.R')
source('Rscripts/00_funcoes_globais.R')

setwd('F:/Meu repositório/fao-amazonia-legal/')

# ler os arquivos
internet <- read.csv('Outputs/00_shapes_e_dados/00_internet_anatel.csv')
pop.estimada.2019 <- read_excel(path = 'Input/tabela6579_pop_estimada_2019.xlsx')

unique(internet$produto) #Ver as categorias

internet.amzl.2019 <- internet %>% 
                      dplyr::filter(sinal %in% '4G' &
                                    produto %in% c('DADOS','VOZ+DADOS') &
                                    id_municipio %in% cidades.amazonia.legal) %>% 
                      select(3,7) %>% 
                      group_by(id_municipio) %>% 
                      summarise(acessos_2019 = sum(acessos, na.rm = TRUE))

internet.amzl.2019$acessos_2019 <- as.numeric(internet.amzl.2019$acessos_2019)
internet.amzl.2019$id_municipio <- as.numeric(internet.amzl.2019$id_municipio)
internet.amzl.2019 <- left_join(internet.amzl.2019,pop.estimada.2019, by = c('id_municipio'='cod_muni')) %>% 
                      select(1,3,2,4) %>% 
                      mutate(acessos_cada_100_mil_hab = (acessos_2019/pop_resid_estimada_2019)*100000)

# classificar
internet.amzl.2019 <- classificar.variavel(internet.amzl.2019,'acessos_cada_100_mil_hab','class_acessos_100mil_2019')

write.csv(internet.amzl.2019,'Outputs/02_tabelas/02_internet_4g_AMZL.csv', row.names = F)

x <- internet.amzl.2019 %>% 
     group_by(class_acessos_100mil_2019) %>%
     mutate(N_category = n()) %>%
     count(N_category)

intermed <- internet.amzl.2019 %>% 
  dplyr::filter(id_municipio %in% cidades.intermediadoras) %>% 
  arrange(desc(acessos_cada_100_mil_hab))


# fazer tabela
tabela.internet <- gt(intermed) %>%
  cols_label(
    muni = 'Município',
    acessos_cada_100_mil_hab = 'Acessos a cada\n100 mil habitantes',
    class_acessos_100mil_2019 = 'Classificação dos acessos'
  ) %>% 
  tab_header(
    title = 'Acessos à internet via rede 4G',
    subtitle = '2019'
  ) %>% 
  cols_hide(
    columns = c(id_municipio,acessos_2019,pop_resid_estimada_2019)
  ) %>% 
  fmt_markdown(
    columns = c(muni, class_acessos_100mil_2019)
  ) %>% 
  fmt_number(
    columns = c(acessos_cada_100_mil_hab),
    decimals = 0,
    sep_mark = '.',
    dec_mark = ','
  ) %>% 
  cols_align(
    align = 'center'
  ) %>% 
  tab_source_note('Fonte: Elaboração própria. Microdados da ANATEL (2019) via Carabetta, João; Dahis, Ricardo; Israel, Fred; Scovino, Fernanda (2020) Base dos Dados: Repositório de Dados Abertos em https://basedosdados.org.')

tabela.internet
gtsave(tabela.internet, 'Outputs/03_mapas/Internet/03_tabela_internet.png')
