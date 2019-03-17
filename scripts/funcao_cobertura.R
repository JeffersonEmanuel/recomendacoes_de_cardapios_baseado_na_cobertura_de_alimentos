# Carregar pacotes
if(!require(dplyr)) {
  install.packages("dplyr"); 
  require(dplyr)
}

source("scripts/funcao_montar_cardapio.R")

# Seleciona a despensa dentre todas as encontradas na base de dados
funcao_selecionar_despensa <- function(base_de_despensas, numero_despensa){
  despensa <- base_de_despensas %>% filter(despensa == numero_despensa)
  return(despensa)
}

# Filtra o conjunto de alimentos presentes no cardápio que estão presente na despensa já com a quantidade utilizada no cardapio
funcao_selecionar_ingredientes_em_comum <- function(despensa, cardapio, numero_de_porcoes) {
  alimentos_em_comum <- cardapio %>% 
    select(ingrediente, quantidade_por_pocao) %>% 
    filter(ingrediente %in% despensa$alimento) %>% 
    group_by(ingrediente) %>% 
    dplyr::summarise(quantidade_porcao = sum(as.numeric(as.character(quantidade_por_pocao)))*numero_de_porcoes)
    
    return(alimentos_em_comum)
}

# Seleciona os alimentos na despensa que estão presentes no cardápio e a quantidade disponivel na despensa
ingredientes_em_comum_despensa <- function(despensa, cardapio) {
  alimentos_em_comum <- despensa %>% 
    select(alimento, quantidade_disponivel = quantidade) %>% 
    filter(alimento %in% cardapio$ingrediente) 
  
  return(alimentos_em_comum)
}

# Seleciona a quantidade do aliemto que tem menor quantidade
funcao_selecionar_menor_valor <- function(quantidade_despensa, quantidade_necessaria) {
  if (quantidade_despensa <= quantidade_necessaria) {
    return(quantidade_despensa)
  } else {
    return(quantidade_necessaria)
  }
}


# Funcao utilizada para verificar a viabilidade do cardapio quando considerado os alimentos da cesta de compras do usuario
funcao_cobertura <- function(despensa, cardapio, numero_de_porcoes) {
   
  # Seleciona Alimentos que tambem estaão na despensa para compor o cardápio. Seleciona também a quantidade necessária de acordo com o numero de porções
  alimentos_comum_cardapio_em_quantidades_necessarias <- funcao_selecionar_ingredientes_em_comum(despensa = despensa, cardapio = cardapio, numero_de_porcoes = numero_de_porcoes)
  
  # Seleciona os alimentos na despensa que estão presentes no cardápio selecionado. seleciona também a quantiade disponivel
  alimentos_comum_despensa_em_quantidades_disponivel <- ingredientes_em_comum_despensa(despensa = despensa, cardapio = cardapio)
  
  if(nrow(alimentos_comum_cardapio_em_quantidades_necessarias)>0 && nrow(alimentos_comum_despensa_em_quantidades_disponivel)>0) {
    
    # Cria um datafreme com os alimentos presentes no cardapio e na despensa e com a respectiva quantidade
    alimentos_e_quantidades <- 
      inner_join(alimentos_comum_cardapio_em_quantidades_necessarias, alimentos_comum_despensa_em_quantidades_disponivel, 
                 by = c("ingrediente" = "alimento"))
    
    # Adicioana uma coluna com a menor quantidade para cada um dos ingredientes e 
    # uma coluna com a quantidade presente na despensa após retirar a quantidade necessaria para compor o cardápio selecionado
    calcula_valores <- alimentos_e_quantidades %>% mutate(menor_quantidade = mapply(funcao_selecionar_menor_valor,
                                    quantidade_despensa=quantidade_disponivel, 
                                    quantidade_necessaria=quantidade_porcao))
    
    # Soma o valor referente ao menor valor de cada ingrediente
    soma_menor_quantidade <- sum(as.double(as.character(calcula_valores$menor_quantidade)))
    
    # Soma a quantidade referente ao necessário para compor o cardápio
    cardapio$quantidade_por_pocao <- as.double(as.character(cardapio$quantidade_por_pocao))*numero_de_porcoes
    soma_quantidade_necessaria <- sum(cardapio$quantidade_por_pocao)
    
    # Calcula o valor referente a cobertura
    cobertura <- soma_menor_quantidade/soma_quantidade_necessaria

  } else {
    cobertura <- 0
 }

  return(cobertura)
}