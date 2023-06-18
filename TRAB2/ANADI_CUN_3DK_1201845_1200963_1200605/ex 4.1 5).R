#Crie um diagrama de correlação entre todos os atributos. Comente o que observa. 
#EX5

#As correlações ajudam a compreender a relação entre duas ou mais variáveis. 
#Pode variar de -1 a +1. Um valor de +1 indica uma correlação positiva perfeita, -1 indica uma correlação negativa perfeita, 0 indica que não há correlação linear entre as variáveis.
#São utilizadas para determinar se existe uma relação linear entre as variáveis e se essa relação é:
# positiva- quando uma variável aumenta, a outra também aumenta (1)
# negativa - quando uma variável aumenta, a outra diminui(-1)

# Carregar a biblioteca
library(dplyr)
library(corrplot)
library(readr)

# Carreguei o dataset
dataset <- ciclismo

# Selecionei apenas as colunas para a matriz
selected_columns <- c("ID", "gender", "Team", "Background", "Pro level", "Winter Training Camp", "altitude_results", "vo2_results", "hr_results", "dob", "Continent")

# cópia do dataset com as colunas selecionadas
selected_dataset <- dataset[, selected_columns]

# Converter as colunas categóricas para fatores
# mutate() para aplicar uma transformação para fator a todas as colunas que são do tipo caracter. 
# across() aplicar a várias colunas em simultâneo. 
# where(is.character) seleciona apenas as colunas que são do tipo carácter. 
selected_dataset <- selected_dataset %>%
  mutate(across(where(is.character), as.factor))

# Converter todas as colunas para numéricas
# transfornaçao para numerico
selected_dataset <- selected_dataset %>%
  mutate(across(everything(), as.numeric))

#Calcular a matriz de correlação
#compara as variáveis numéricas duas a duas e retorna os coeficientes de correlação
correlation_matrix <- cor(selected_dataset)

# Cria o gráfico de mapa de calor da matriz de correlação
corrplot(correlation_matrix, method = "color", tl.col = "black", tl.srt = 30)



#############################################################################
