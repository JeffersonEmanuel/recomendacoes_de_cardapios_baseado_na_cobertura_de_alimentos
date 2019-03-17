
## Get harmina csv --- Executar caso não tenha a tabela com harmonia dos ingredientes
# funcao_harmonia_geral <- function(base_com_preparacoes){
#   ingredientes_receitas <- base_com_preparacoes %>% 
#     select(ingrediente) %>%
#     distinct()
#   ingredientes_receitas <- tibble::rowid_to_column(ingredientes_receitas, "id")
#   
#   ### Funcao para obter receitas com o ingrediente especifico
#   funcao_get_receitas_by_ingrediente <- function(id_find) {
#     i_ingrediente <- ingredientes_receitas %>% 
#       filter(id == id_find) %>% 
#       select(ingrediente)
#     receitas_com_iesimo_ingrediente <- base_com_preparacoes %>%
#       filter(ingrediente == i_ingrediente$ingrediente) %>%
#       select(receita) %>% distinct()
#     return(receitas_com_iesimo_ingrediente)
#   }
#   
#   harmonia <- 0
#   harmonia <- tibble()
#   
#   for(i in 1:(nrow(ingredientes_receitas)-1)) {
#     
#     # Get receitas do i-ésimo ingrediente
#     receitas_com_i_esimo_ingrediente <- funcao_get_receitas_by_ingrediente(id_find = i)
#     
#     for(j in 2:nrow(ingredientes_receitas)) {
#       
#       # Get receitas do j-ésimo ingrediente
#       receitas_com_j_esimo_ingrediente <- funcao_get_receitas_by_ingrediente(id_find = j)
#       
#       intercessao <- intersect(receitas_com_i_esimo_ingrediente$receita, receitas_com_j_esimo_ingrediente$receita)
#       intercessao <- as.double(length(intercessao))
#       uniao <- union(receitas_com_i_esimo_ingrediente$receita, receitas_com_j_esimo_ingrediente$receita)
#       uniao <- as.double(length(uniao))
#       
#       i_ing <- ingredientes_receitas %>% 
#         filter(id == i) %>% 
#         select(ingrediente)
#       j_ing <- ingredientes_receitas %>% 
#         filter(id == j) %>% 
#         select(ingrediente)
#       
#       har_tible <- tibble(ingrediente_1 = i_ing$ingrediente,
#                           ingrediente_2 = j_ing$ingrediente,
#                           harmonia = ((intercessao)/(uniao)))
#       
#       harmonia <- bind_rows(harmonia, har_tible)
#       
#     }
#     write.csv2(harmonia, 'scripts/harmonia_todos_ingredientes.csv' )
#   }
#   
#   return(harmonia)
# }

# Carregar pacotes
if(!require(dplyr)) {
  install.packages("dplyr"); 
  require(dplyr)
}

base_harmonia <- read.csv2('bases/harmonia_entre_ingredientes_na_base_de_receitas.csv')

funcao_harmonia <- function(cardapio_avaliado){
  
  base_harmonia <- base_harmonia %>% 
    filter(as.character(base_harmonia$ingrediente_1) != as.character(base_harmonia$ingrediente_2))
  
  ingredientes <- cardapio_avaliado %>% 
    select(ingrediente) %>%
    distinct()
  
  harmonia <- base_harmonia %>%
    filter(as.character(ingrediente_1) %in% as.character(ingredientes$ingrediente), 
           as.character(ingrediente_2) %in% as.character(ingredientes$ingrediente),
           as.character(ingrediente_2) != as.character(ingrediente_1))
  
  harmonia_cardapio <- sum(harmonia$harmonia)
  
  return(harmonia_cardapio)
}