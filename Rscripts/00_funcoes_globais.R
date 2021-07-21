# Funções Globais
library(tidyverse)
data.frame.teste <- data.frame(salario = sample(1:100, 50, replace=FALSE),
                               stringsAsFactors = FALSE)

classificar.variavel <- function(nome.do.dataframe,nome.da.coluna) {
  summary(nome.do.dataframe$nome.da.coluna) # aqui eu mostro as estatísticas básicas, como os quartis
  round(quantile(nome.do.dataframe$nome.da.coluna, probs = seq(0, 1, 1/10),na.rm = TRUE),2) # ver decis
  
  # Agora eu vou usar variáveis para armazenar o primeiro quartil, o terceiro quartil e a mediana.
  q1 <- as.vector(summary(nome.do.dataframe$nome.da.coluna)[2])
  mediana <- as.vector(summary(nome.do.dataframe$nome.da.coluna)[3])
  q3 <- as.vector(summary(nome.do.dataframe$nome.da.coluna)[5])
  
  # armazenar o primeiro e o último decil
  x <- quantile(nome.do.dataframe$nome.da.coluna, probs = seq(0, 1, 1/10),na.rm = TRUE) 
  round(x, 5) # ver os decis 
  
  # armazenar o primeiro (10%) e o último decil (90%)
  dec1 <- as.vector(round(x, 5)[2])
  dec9 <- as.vector(round(x, 5)[10])
  
  nome.do.dataframe <- nome.do.dataframe %>% 
    mutate(class_nome_da_coluna = case_when(nome.da.coluna <= dec1 ~ 'Muito Baixo',   # muito baixo 10% (primeiro decil)
                                              nome.da.coluna <=  q1 ~ 'Baixo',          # baixo entre 10% e 25%
                                              nome.da.coluna <= mediana ~ 'Médio Baixo',# médio-baixo entre 25% e 50%
                                              nome.da.coluna <= q3 ~ 'Médio Alto',      # médio-alto entre 50% e 75%
                                              nome.da.coluna <= dec9 ~ 'Alto',          # alto entre entre 75% e 90%
                                              nome.da.coluna > dec9 ~ 'Muito Alto'))    # muito alto acima de 90% (último decil)
  
  # teste para ver quantos itens existem em cada categoria
  x <- nome.do.dataframe %>% 
    group_by(class_(class_nome_da_coluna)) %>%
    mutate(N_category = n()) %>%
    count(N_category)
}

# Rodar a função
classificar.variavel(data.frame.teste,salario)
