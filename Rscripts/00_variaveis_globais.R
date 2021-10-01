# Variáveis Globais
source('Rscripts/00_bibliotecas.R')

# cidades brasileiras
cidades.brasil.nome <- read_excel(path = './Input/municipios_brasil.xlsx')
cidades.brasil <- as.vector(cidades.brasil.nome$cod_muni)

# cidades Amazônia Legal
cidades.amazonia.legal.nome <- read_excel(path = './Input/municipios_amazonia_legal.xlsx')
cidades.amazonia.legal <- as.vector(cidades.amazonia.legal.nome$cod_muni)
  
# cidades intermediadoras
cidades.intermediadoras <- c('1100023','1100122','1100205','1200401','1200203','1302405','1302504','1301902','1304203',
                            '1304062','1303403','1400472','1400100','1502400','1500602','1501808','1504208','1506138',
                            '1506807','1600303','1600501','1702109','1709500','1721000','2101202','2109908','2109106',
                            '2105302','2103000','5102504','5101803','5107602','5103403','5107909')

cidades.inter.6.dig <- as.numeric(str_sub(cidades.intermediadoras, end=-2)) # cod intermediadoras sem dígito verificador

# Estados
uf.amz.legal <- c('AC','RR','AM','RO','PA','AP','MA','TO','MT')

# valores
em_milhoes <- 1000000
em_bilhoes <- 1000000000