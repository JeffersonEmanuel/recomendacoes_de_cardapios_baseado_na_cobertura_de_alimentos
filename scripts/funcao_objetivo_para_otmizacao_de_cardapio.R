source("scripts/funcao_cobertura.R")
source("scripts/funcao_nutricional.R")
source("scripts/funcao_harmonia.R")
source("scripts/funcao_montar_cardapio.R")
base_com_receitas <- read.csv2("bases/receitas.csv")

# funcao objetivo utilizada no Algoritmo Genetico
funcao_objetivo_para_otmizacao_de_cardapio <- function(x) {
  
  # Monta o cardapio
  cardapio <- funcao_montar_cardapio(base_de_receitas = base_com_receitas, x)
  
  # Verifica a viabilidade
  f1 <- funcao_cobertura(numero_despensa = despensa, cardapio = cardapio, numero_de_porcoes = numero_de_porcoes)

  # Valor Nutricional
  cardapio_nutricional <- cardapio %>% dplyr::summarise(proteina =  sum(proteina), carboidrato =  sum(carboidrato), gordura = sum(gordura))

  # # valor nuticional proteico presente no cardapio
  f2 <- funcao_nutricional(valor_nutriente_cardapio = cardapio_nutricional$proteina, 
                           valor_minino_esperado = 60, valor_maximo_esperado = 120, 
                           nutriente = "proteina") # 120

  # # valor nuticional de gordura presente no cardapio
  f3 <- funcao_nutricional(valor_nutriente_cardapio = cardapio_nutricional$gordura, 
                           valor_minino_esperado = 90, valor_maximo_esperado = 240, 
                           nutriente = "gordura") # 240

  # # valor nuticional de crboidrato presente no cardapio
  f4 <- funcao_nutricional(valor_nutriente_cardapio = cardapio_nutricional$carboidrato, 
                           valor_minino_esperado = 330, valor_maximo_esperado = 600, 
                           nutriente = "carboidrato") # 600

  # Valor Energetico Total
  vet <- cardapio_nutricional$proteina + cardapio_nutricional$gordura + cardapio_nutricional$carboidrato
  
  f5 <- funcao_nutricional(valor_nutriente_cardapio = vet, 
                           valor_minino_esperado = 600, valor_maximo_esperado = 800, 
                           nutriente = "calorias")

  # Harmonia entre aliemtos
  f6 <- funcao_harmonia(cardapio)
  
  
  # Retorno
  c(f1=f1, f2=f2, f3=f3, f4=f4, f5=f5, f6=f6) # Completo
}
