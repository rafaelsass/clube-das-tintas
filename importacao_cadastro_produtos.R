library(readxl)
library(dplyr)
library(stringr)


# importacao de dados -----------------------------------------------------


fornecedor <- read_excel("dados/importacao/blascor/blascor_0322.xlsx", skip = 1)
interno <- read_excel("dados/sede_0222.xlsx", skip = 1)


 #Uso com codigo de fábrica --------------------------------------------


 interno <- interno %>%
   filter(`Código de fábrica` %in% fornecedor$codigo[is.na(fornecedor$codigo) == F] & 
            Fornecedor %in% c(90001943,90000011,90000919)) %>% unique() %>%
   arrange(as.numeric(`Código de fábrica`)) 

 fornecedor <- fornecedor %>%
   filter(codigo %in% interno$`Código de fábrica`) %>%
   arrange(as.numeric(codigo))

# criacao de tabela para importacao ---------------------------------------

# necessaria a adaptaçao dos valores e colunas para importacao no sistema
organizar_colunas <- function(data1, data2){
  table <- select(data2, Cód.Item) %>%
      mutate(#`Código de Barras` = data1$cod_barras,
      Cód.Item = str_pad(data2$Cód.Item,5,"left",0))
  
  df_pad <- data.frame(matrix(
    vector(), 0, 51,dimnames = (list(c(),c( #0-50 com código de barras 0-51 sem
      LETTERS[2:26],paste0("A",LETTERS))))), #2;26 com cod barras 3;26 sem
    stringsAsFactors=F)
  
  table <- full_join(table, df_pad,by = character()) %>%
    mutate(#S = data1$NCM,
      #Z = str_replace_all(data1$Peso_Bruto,"\\.",","),
      #AE = data1$Mult_venda,
      AI = str_replace_all(data1$`Preco_Compra`,"\\.",","),
      AJ = data1$desconto,
      E = str_pad(data2$Departamento, 3, "left", pad = 0),
      #G = data2$Descrição
      )
}
table <- organizar_colunas(fornecedor,interno)

# exporta??o da tabela finalizada -----------------------------------------


write.table(filter(table, E == "003"| E == "004"), 
            file = "arq/data_import_sw_222_BC.csv", quote = F, sep = ";", 
            row.names = F, col.names = F, dec = ",", na = "")



# criação de tabela para checagem de diferença em produtos ----------------

check <- interno %>%
  mutate(Preço_Sistema = Pr.Compra, 
         Preço_Tabela = as.numeric(fornecedor$Preco_Compra),
         Variação = (Preço_Tabela/Preço_Sistema) - 1) %>%
  select(Cód.Item, Descrição, `Código de fábrica`, Preço_Sistema, Preço_Tabela, 
         Variação, Departamento) %>%
  filter(Variação != 0)
  

write.table(check, file = "arq/data_check_sw.csv", quote = F, sep = ";", 
            row.names = F, col.names = T, dec = ",", na = "")

# Dicionario --------------------------------------------------------------

#codigo
#cod_barras
#Mult_Venda
#Preco_Compra
#Peso_Bruto
#NCM
#desconto