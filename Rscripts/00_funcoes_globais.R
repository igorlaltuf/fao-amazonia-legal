# Funções Globais

# 1 - Função de categorização dos dados
classificar.variavel <- function(dataframe, variavel.analisada = NULL, nova.variavel = NULL) {
  sym <- deparse(substitute(dataframe))
  if (!exists(sym, parent.frame())) stop("Dados passados não existe")
  if (!is.data.frame(dataframe)) stop("Dados passados não é do tipo data.frame")
  if (is.null(variavel.analisada) || !is.character(variavel.analisada) || variavel.analisada == "") stop("Variável analisada deve ser uma string")
  if (is.null(nova.variavel) || !is.character(nova.variavel) || nova.variavel == "") stop("Variável analisada deve ser uma string")
  if (!any(names(dataframe) == variavel.analisada)) stop("Variável analisada não existe do dataframe")
   
  resumo <- summary(dataframe[[variavel.analisada]])
  # armazenar o primeiro (10%) e o último decil (90%)
  decil <- quantile(dataframe[variavel.analisada], probs = c(0.1, 0.9), na.rm = TRUE)

  novo.dataframe <- dataframe %>%
    mutate(!!nova.variavel := case_when(
      !!as.name(variavel.analisada) <= decil[["10%"]] ~ "Muito Baixo",
      !!as.name(variavel.analisada) <= resumo[["1st Qu."]] ~ "Baixo",
      !!as.name(variavel.analisada) <= resumo[["Median"]] ~ "Médio Baixo",
      !!as.name(variavel.analisada) <= resumo[["3rd Qu."]] ~ "Médio Alto",
      !!as.name(variavel.analisada) <= decil[["90%"]] ~ "Alto",
      !!as.name(variavel.analisada) > decil[["90%"]] ~ "Muito Alto"))
      
    invisible(novo.dataframe)
}

# 2 - Função que transforma valores alto e muito alto em 1
pontuacao.criterio1 <- function(dataframe, variavel.analisada = NULL, nova.variavel = NULL) {
  novo.dataframe <- dataframe %>%
    mutate(nova.variavel = ifelse(dataframe$variavel.analisada %in% c('Alto','Muito Alto'), 1,0))
  }