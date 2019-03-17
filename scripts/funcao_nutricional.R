#### Funcao utilizada para avaliar o valor nutricional para determinado nutriente
# valor_nutriente_cardaio: ? a qauntidade, na recei??o, do nutriente a ser avaliado
#valor_minino_esperado: ? a menor quantidade aceita, para que esteja dentro do esperado, para aquele nutrientre na refei??o
#valor_maximo_esperado: ? a maior quantidade aceita, para que esteja dentro do esperado, para aquele nutrientre na refei??o
#valor_maximo_nutriente_soma_base: ? a maior quantidade que possa ser obtida em uma refei??o para o nutriente esecifico, 
#ela provem da soma dos valores da receita de cada categoria que tem a maior quantidade do nutriente em analise

# Carregar pacotes
if(!require(dplyr)) {
  install.packages("dplyr"); 
  require(dplyr)
}

base_com_receitas <- read.csv2("bases/receitas.csv")


funcao_valor_maximo_nutriente <- function(nutriente) {
  
  selecionar_valor_maximo <- 
    base_com_receitas %>% 
    select(categoria, nutriente_ingrediente = nutriente, receita) %>%
    group_by(categoria, receita) %>%
    dplyr::summarise(valor_nutriente_receita = sum(as.double(as.character(nutriente_ingrediente)))) %>%
    group_by(categoria) %>% 
    top_n(1, valor_nutriente_receita) %>% 
    distinct()
  
  soma_nutriente <- 
    sum(selecionar_valor_maximo$valor_nutriente_receita )
  
  return(soma_nutriente)
}


valor_max_proteina <- funcao_valor_maximo_nutriente(nutriente = "proteina")
valor_max_gordura <- funcao_valor_maximo_nutriente(nutriente = "gordura")
valor_max_carboidrato <- funcao_valor_maximo_nutriente(nutriente = "carboidrato")
valor_max_energetico <- valor_max_proteina + valor_max_gordura + valor_max_carboidrato
valores_maximos <- 
  tibble(proteina = valor_max_proteina, gordura = valor_max_gordura, carboidrato = valor_max_carboidrato, calorias = valor_max_energetico)


funcao_nutricional <- function(valor_nutriente_cardapio, valor_minino_esperado,  
                                     valor_maximo_esperado, nutriente) {

  valor_maximo_nutriente_soma_base <- as.double(valores_maximos[nutriente])
  
  valor <- 
    (
      ( 
        abs(valor_nutriente_cardapio-valor_minino_esperado) +
          abs(valor_nutriente_cardapio-valor_maximo_esperado) - 
          abs(valor_maximo_esperado - valor_minino_esperado) 
      ) / valor_maximo_nutriente_soma_base 
    )
  
  valor <- (1-valor)
  
  return(valor)
}

