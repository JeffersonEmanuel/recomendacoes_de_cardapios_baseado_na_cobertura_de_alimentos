# Carregar pacotes
if(!require(dplyr)) {
  install.packages("dplyr"); 
  require(dplyr)
}


# Funcao utilizadas para separar as preparacoes de acordo com a categoria de cada prato
funcao_separar_receita_por_categoria <- function(base, tipo_de_prato) {
  base_selecionada <- base %>% filter(categoria == tipo_de_prato) %>% select(-categoria)
  r <-  base_selecionada %>% select(receita) %>% distinct()
  r$id <- 1:nrow(r)
  base_selecionada <- merge(r, base_selecionada)
  return(base_selecionada)
}

# Montar Cardapio contend contendo apenas um representante de cada categoria. x representa o conjunto de entrada
funcao_montar_cardapio <- function(base_de_receitas, x) {
  
  # Carregar as preparacoes de acordo com a categoria e selecionar receita pelo id
  carne <- funcao_separar_receita_por_categoria(base_de_receitas,"carne") %>% filter(id == as.integer(x[1]))
  
  acompanhamento_massa <- funcao_separar_receita_por_categoria(base_de_receitas,"acompanhamento_massa") %>% filter(id == as.integer(x[2]))
  
  acompanhamento_arroz <- funcao_separar_receita_por_categoria(base_de_receitas,"acompanhamento_arroz") %>% filter(id == as.integer(x[3]))
  
  acompanhamento_feijao <- funcao_separar_receita_por_categoria(base_de_receitas,"acompanhamento_feijao") %>% filter(id == as.integer(x[4]))
  
  salada <- funcao_separar_receita_por_categoria(base_de_receitas,"salada") %>% filter(id == as.integer(x[5]))
  
  suco <- funcao_separar_receita_por_categoria(base_de_receitas,"suco") %>% filter(id == as.integer(x[6]))
  
  sobremesa <- funcao_separar_receita_por_categoria(base_de_receitas,"sobremesa") %>% filter(id == as.integer(x[7]))
  
  # montar cardápio com uma receita de cada categoria
  cardapio <- bind_rows(carne, acompanhamento_massa, acompanhamento_arroz, acompanhamento_feijao, salada, suco, sobremesa)
  
  return(cardapio)
}