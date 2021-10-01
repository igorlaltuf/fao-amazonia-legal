####################################################################################
# Amálise dos dados da rede 4G para a Amazônia Legal
# Fontes: ANATEL(dados de comunicação) e IBGE (estimativa populacional)
####################################################################################
# Limpar as variáveis carregadas
rm(list = ls())

# Carregar pacotes, variáveis e funções
source("Rscripts/00_bibliotecas.R")
source("Rscripts/00_variaveis_globais.R")
source("Rscripts/00_funcoes_globais.R")

# Importar arquivo
pop.estimada.2019 <- read_excel(path = "Input/tabela6579_pop_estimada_2019.xlsx")

# unique(internet$produto) ver as categorias

# Calcular e classificar os acessos via rede 4G a cada 100 mil habitantes
internet.amzl.2019 <- read.csv("Outputs/00_shapes_e_dados/00_internet_anatel.csv") %>%
  dplyr::filter(sinal %in% "4G" &
    produto %in% c("DADOS", "VOZ+DADOS") &
    id_municipio %in% cidades.amazonia.legal) %>%
  select(3, 7) %>%
  group_by(id_municipio) %>%
  summarise(acessos_2019 = sum(acessos, na.rm = TRUE)) %>%
  mutate(
    acessos_2019 = as.numeric(acessos_2019),
    id_municipio = as.numeric(id_municipio)
  ) %>%
  left_join(pop.estimada.2019, by = c("id_municipio" = "cod_muni")) %>%
  select(1, 3, 2, 4) %>%
  mutate(acessos_cada_100_mil_hab = (acessos_2019 / pop_resid_estimada_2019) * 100000) %>%
  classificar.variavel("acessos_cada_100_mil_hab", "class_acessos_100mil_2019")

# Exportar em csv
write.csv(internet.amzl.2019, "temp/01_internet_4g_AMZL.csv", row.names = F)

# Quantidade em cada categoria classificada
x <- internet.amzl.2019 %>%
  group_by(class_acessos_100mil_2019) %>%
  mutate(N_category = n()) %>%
  count(N_category)

# Filtrar as cidades intermediadoras
intermed <- internet.amzl.2019 %>%
  dplyr::filter(id_municipio %in% cidades.intermediadoras) %>%
  arrange(desc(acessos_cada_100_mil_hab))


# Tabela das cidades intermediadoras
tabela.internet <- gt(intermed) %>%
  cols_label(
    muni = "Município",
    acessos_cada_100_mil_hab = "Acessos a cada\n100 mil habitantes",
    class_acessos_100mil_2019 = "Classificação dos acessos"
  ) %>%
  tab_header(
    title = "Acessos à internet via rede 4G",
    subtitle = "2019"
  ) %>%
  cols_hide(
    columns = c(id_municipio, acessos_2019, pop_resid_estimada_2019)
  ) %>%
  fmt_markdown(
    columns = c(muni, class_acessos_100mil_2019)
  ) %>%
  fmt_number(
    columns = c(acessos_cada_100_mil_hab),
    decimals = 0,
    sep_mark = ".",
    dec_mark = ","
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_source_note("Fonte: Elaboração própria. Microdados da ANATEL (2019) via Carabetta, João; Dahis, Ricardo; Israel, Fred; Scovino, Fernanda (2020) Base dos Dados: Repositório de Dados Abertos em https://basedosdados.org.")

tabela.internet

# Exportar tabela das cidades intermediadoras
gtsave(tabela.internet, "Outputs/Internet/03_tabela_internet.png")
