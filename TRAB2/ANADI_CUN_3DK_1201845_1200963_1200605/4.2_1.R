####4.2

##1

#carregamento de pacotes necessarios
library(rpart)
library(neuralnet)

#Preparação dos dados
dados -> ciclismo
dados$dob <- as.Date(dados$dob, format = "%Y-%m-%d")
dados$Age <- as.integer(format(Sys.Date(), "%Y")) - as.integer(format(dados$dob, "%Y"))

dados_filtered <- subset(dados, select = -c(ID))  # Remover a coluna ID

colnames(dados_filtered)

# Definir uma semente aleatória para reprodutibilidade
#set.seed(123)

# Divisão dos dados num conjunto de treino e de teste 
# 70% - treino
# 30% - teste
# Dividir os dados em conjuntos de treino e teste
proporcao_treino <- 0.7  # Proporção de dados para treino
indices_treino <- sample(1:nrow(dados_filtered), floor(proporcao_treino * nrow(dados_filtered)))

# Amostragem aleatória dos dados dos ciclistas
index_normalized <- sample(1:nrow(selected_dataset), 0.7 * nrow(selected_dataset))
dados.train <- selected_dataset[index_normalized,]
dados.test <- selected_dataset[-index_normalized,]

dados_treino <- dados_filtered[indices_treino, ]
dados_teste <- dados_filtered[-indices_treino, ]

# Codificar variáveis categóricas usando a função model.matrix
dados_treino_encoded <- model.matrix(~ . - 1, data = dados_treino)
dados_teste_encoded <- model.matrix(~ . - 1, data = dados_teste)

# Converter os dados codificados de volta em data frames
dados_treino_encoded_df <- as.data.frame(dados_treino_encoded)
dados_teste_encoded_df <- as.data.frame(dados_teste_encoded)

# Verificar as colunas presentes nos dados codificados
colnames(dados_treino_encoded_df)

# Rename the column with a simpler name
colnames(dados_treino_encoded_df)[colnames(dados_treino_encoded_df) == "`Pro level`World Tour"] <- "Pro_level_World_Tour"

str(dados_treino_encoded_df)


#### arvore decisao
# Criar o modelo de árvore de decisão
modelo_arvore <- rpart(Pro_level_World_Tour ~ ., data = dados_treino_encoded_df[, -1])




# Previsão nos dados de teste
previsao_arvore <- predict(modelo_arvore, newdata = dados_teste, type = "class")

# Calculo da taxa de accuracy
accuracy_arvore <- sum(previsao_arvore == dados_teste$`Pro level`) / nrow(dados_teste)
###


###rede neural

###



