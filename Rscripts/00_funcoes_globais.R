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

# 3 - Remover acentos
rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}