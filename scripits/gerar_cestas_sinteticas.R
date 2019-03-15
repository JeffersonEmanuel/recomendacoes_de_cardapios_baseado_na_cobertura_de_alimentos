# Carregar pacotes
if(!require(dplyr)) {
  install.packages("dplyr"); 
  require(dplyr)
}

# Base com cestas de compras
base_com_cestas_de_compras <- read.csv2('bases/cestas_de_compras.csv')

# Selecionar aleatóriamente uma despensa 
selecionar_despensa_aleatoriamente <- function(base_cestas_de_compras) {
  quantidade_de_despensa <- unique(base_cestas_de_compras$despensa)
  numero_da_despensa <- sample(quantidade_de_despensa, 1)
  despensa <- base_com_cestas_de_compras %>% filter(despensa == numero_da_despensa)
  return(despensa)
}

# Mutriplicar quantidade de alimento por um valor entre 0.5 e 1.5
gerar_nova_quantidade_alimento <- function(quantidade) {
  nova_quantidade <- quantidade * runif(1, 0.5, 1.5)
  return(nova_quantidade)
}

# Gera uma amostra de cesta de compras com alimentos presentes em outras cestas sem repetir alimentos
# porém com quantidade de alimentos variando entre 0.5 e 1.5 da quantidade da amostra original
gerar_nova_cesta_de_comprar <- function(numero_da_nova_despensa, base_cestas_de_compras) {
  
  # Selecionar aleatoriamente a primeira das despensas
  despensa_aleatoria_1 <- selecionar_despensa_aleatoriamente(base_cestas_de_compras)
  
  # Selecionar aleatoriamente a segunda das despensas
  despensa_aleatoria_2 <- selecionar_despensa_aleatoriamente(base_cestas_de_compras)
  
  # juntar as duas despensas desconsiderando os alimentos já presentes  na primeira despensa 
  # e recalculando a quantidade de alimentos
  nova_despensa <- despensa_aleatoria_2 %>% 
    filter(!alimento %in% despensa_aleatoria_1$alimento) %>% 
    bind_rows(despensa_aleatoria_1) %>% 
    mutate(quantidade = gerar_nova_quantidade_alimento(quantidade))
  
  nova_despensa$despensa <- numero_da_nova_despensa
  
  return(nova_despensa)
}


# Gerar nova base de despensas sinteticas
gerar_base_despensa_sintetica <- function(numero_de_cestas, base_cestas_de_compras) {
  
  # Nova base com cestas de compras geradas 
  cestas_de_compras_sinteticas <- tibble()
  
  # loop para gerar a nova base de cestas sinteticas
  for(i in 1:numero_de_cestas) {
    despensa_sintetica <- 
      gerar_nova_cesta_de_comprar(numero_da_nova_despensa = i, base_cestas_de_compras = base_cestas_de_compras)
    cestas_de_compras_sinteticas <- bind_rows(cestas_de_compras_sinteticas, despensa_sintetica)
  }
  
  return(cestas_de_compras_sinteticas)
  
}

# gerar base sintetica de cestas de compras com 1000 exemplares
base_sintetica_de_cestas_de_compras <- 
  gerar_base_despensa_sintetica(numero_de_cestas = 1000, base_cestas_de_compras = base_com_cestas_de_compras)


# Salvar base com cestas de compras sinteticas
write.csv2(file = 'bases/base_sintetica_com_cestas_de_compras.csv', base_sintetica_de_cestas_de_compras, row.names = FALSE)
