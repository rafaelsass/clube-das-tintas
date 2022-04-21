library(readxl)
library(dplyr)
library(stringr)

# importacao de dados -----------------------------------------------------

fornecedor <- read_excel("dados/importacao/tabela_sw_clube.xlsx", skip = 1)
interno <- read_excel("dados/importacao/produtos_20abril_2022.xlsx", skip = 1)

interno <- interno %>%
  filter(`Código de fábrica` %in% fornecedor$codigo[is.na(fornecedor$codigo) == F] & 
           Fornecedor %in% c(90000919,90000011,90001943)) %>% unique() %>%
  arrange(as.numeric(`Código de fábrica`)) 

fornecedor <- fornecedor %>%
  filter(codigo %in% interno$`Código de fábrica`) %>%
  arrange(as.numeric(codigo))

# criacao de tabela para importacao ---------------------------------------
# necessaria a adaptaçao dos valores e colunas para importacao no sistema

tabela <- data.frame(matrix(
  vector(), nrow(fornecedor), 52,dimnames = (list(c(),c(
    LETTERS,paste0("A",LETTERS))))), 
  stringsAsFactors=F)

tabela <- tabela %>%
  mutate(A = str_pad(interno$Cód.Item,5,"left",0),
         #B = interno$cod_barras,
         #S = fornecedor$NCM,
         #Z = str_replace_all(fornecedor$Peso_Bruto,"\\.",","),
         #AE = fornecedor$Mult_venda,
         AJ = fornecedor$desconto*100,
         E = str_pad(interno$Departamento, 3, "left", pad = 0),
         #G = interno$Descrição,
         AI = str_replace_all(fornecedor$`Preco_Compra`,"\\.",",")
  )

# exportação da tabela finalizada -----------------------------------------

#import A e AA
write.table(filter(tabela, E == "001"| E == "002"), 
            file = "arq/data_import_sw_222_AAA.csv", quote = F, sep = ";", 
            row.names = F, col.names = F, dec = ",", na = "")
#import B e C
write.table(filter(tabela, E == "003"| E == "004"), 
            file = "arq/data_import_sw_222_BC.csv", quote = F, sep = ";", 
            row.names = F, col.names = F, dec = ",", na = "")

# criação de tabela para checagem de diferença em produtos ----------------

checagem <- interno %>%
  mutate(Preço_Sistema = Pr.Compra, 
         Preço_Tabela = as.numeric(fornecedor$Preco_Compra),
         Variação = (Preço_Tabela/Preço_Sistema) - 1) %>%
  select(Cód.Item, Descrição, `Código de fábrica`, Preço_Sistema, Preço_Tabela, 
         Variação, Departamento) %>%
  filter(Variação != 0)


write.table(checagem, file = "arq/data_check_sw.csv", quote = F, sep = ";", 
            row.names = F, col.names = T, dec = ",", na = "")

# Dicionario --------------------------------------------------------------

#codigo
#cod_barras
#Mult_Venda
#Preco_Compra
#Peso_Bruto
#NCM
#desconto