
######Exercicio 1

#importar dados
dados <- ciclismo

#verificar dimensão
dim(dados)

#fazer sumário dos dados
summary(dados)


#ID        gender              Team            Background         Pro level         Winter Training Camp
#Min.   :  0.0   Length:1000        Length:1000        Length:1000        Length:1000        Length:1000         
#1st Qu.:249.8   Class :character   Class :character   Class :character   Class :character   Class :character    
#Median :499.5   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character    
#Mean   :499.5                                                                                                   
#3rd Qu.:749.2                                                                                                   
#Max.   :999.0                                                                                                   

#altitude_results  vo2_results       hr_results          dob              Continent        
#Min.   : 24.00   Min.   : 21.00   Min.   : 17.00   Min.   :1985-01-06   Length:1000       
#1st Qu.: 57.00   1st Qu.: 60.00   1st Qu.: 58.00   1st Qu.:1990-09-01   Class :character  
#Median : 68.00   Median : 70.00   Median : 69.00   Median :1995-06-22   Mode  :character  
#Mean   : 66.75   Mean   : 69.75   Mean   : 68.57   Mean   :1995-08-03                     
#3rd Qu.: 77.00   3rd Qu.: 80.00   3rd Qu.: 79.00   3rd Qu.:2000-08-27                     
#Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :2005-12-30 



########Exercicio 2#########

#Primeiro, a coluna "dob" é convertida para o formato de data utilizando a função as.Date 
#e especificando o formato de data. Em seguida, a diferença entre o ano atual e o ano de nascimento 
#é calculada para cada entrada no conjunto de dados. 
#Esta diferença é armazenada na nova coluna "Age" como valores inteiros.

dados$dob <- as.Date(dados$dob, format = "%Y-%m-%d")
dados$Age <- as.integer(format(Sys.Date(), "%Y")) - as.integer(format(dados$dob, "%Y"))

print(dados)

print(dados$Age)


#Exercicio 3


library(ggplot2)
library(dplyr)

# Cálculo da proporção e contagem de ciclistas por gênero
gender_distribution <- dados %>% count(gender) %>%
  mutate(percentage = n / sum(n) * 100)

# Criação do gráfico da distribuição de ciclistas por gênero
ggplot(gender_distribution, aes(x = gender, y = n)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7, width = 0.5) +
  geom_text(aes(label = paste(n, "-", round(percentage), "%")), vjust = -0.5, size = 4) +
  labs(title = "Distribuição de ciclistas por gênero", x = "", y = "") +
  theme_minimal(base_size = 12, base_family = "") +
  theme(panel.grid.major = element_blank())


# Calculo da distribuição de ciclistas por continente
continent_distribution <- dados %>% count(Continent)

# Criação do gráfico da distribuição de ciclistas por continente
ggplot(continent_distribution, aes(x = Continent, y = n)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7, width = 0.5) +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  labs(title = "Distribuição de ciclistas por continente", x = "", y = "") +
  theme_minimal(base_size = 12, base_family = "") +
  theme(panel.grid.major = element_blank())

# Criar faixas etárias com base na idade
dados$age_group <- cut(dados$Age, breaks = c(0, 20, 30, 40, 50, 60, Inf),
                       labels = c("0-20", "21-30", "31-40", "41-50", "51-60", "61+"),
                       include.lowest = TRUE)


# Cálculo da proporção e contagem de ciclistas por faixa etária
contagem_age_group <- count(dados, age_group) %>%
  mutate(percentage = n / sum(n) * 100)

# Criação do gráfico de barras
ggplot(contagem_age_group, aes(x = age_group, y = n)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  geom_text(aes(label = paste(n, "-", round(percentage), "%")), vjust = -0.5, size = 4) +
  labs(title = "Distribuição de ciclistas por faixa etária",
       x = "", y = "") +
  theme_minimal(base_size = 12, base_family = "") +
  theme(panel.grid.major = element_blank())



# Definir a paleta de cores
library(RColorBrewer)
colors <- brewer.pal(9, "Blues")

# Calcular a matriz de correlação
correlation_matrix <- round(cor(dados[, c("altitude_results", "vo2_results", "hr_results", "Age")]), digits = 3)

# Criar o gráfico de correlação
library(corrplot)
corrplot(correlation_matrix, method = "number", type = "upper", col = colors)

###


#Exercicio 4
# a)
# Identificar valores ausentes
missing_values <- is.na(dados)

# Contar valores ausentes por coluna
missing_counts <- colSums(missing_values)
print(missing_counts)

#b)
# Boxplot para a variável "altitude_results"
boxplot(dados$altitude_results, ylab = "altitude_results", main = "Treino de altitude")

# Boxplot para a variável "vo2_results"
boxplot(dados$vo2_results, ylab = "vo2_results", main = "Volume de oxigénio máximo")

# Boxplot para a variável "hr_results"
boxplot(dados$hr_results, ylab = "hr_results", main = "Frequência cardíaca")

#c)
# Retirar os atributos "ID" e "dob" do dataset
dados_selected <- dados[, c(2,3,4,5,6,7,8,9,11,12)]

#d)
# Normalização dos dados
normalize <- function(y) { (y-min(y)) / (max (y)-min(y)) }

dados_selected$altitude_results <- normalize (dados_selected$altitude_results)
dados_selected$vo2_results <- normalize(dados_selected$vo2_results)
dados_selected$hr_results <- normalize(dados_selected$hr_results)
dados_selected$Age <- normalize(dados_selected$Age)


###
#Ex5
#Crie um diagrama de correlação entre todos os atributos. Comente o que observa. 

#As correlações ajudam a compreender a relação entre duas ou mais variáveis. 
#Pode variar de -1 a +1. Um valor de +1 indica uma correlação positiva perfeita, -1 indica uma correlação negativa perfeita, 0 indica que não há correlação linear entre as variáveis.
#São utilizadas para determinar se existe uma relação linear entre as variáveis e se essa relação é:
# positiva- quando uma variável aumenta, a outra também aumenta (1)
# negativa - quando uma variável aumenta, a outra diminui(-1)

# Carregar a biblioteca
library(dplyr)
library(corrplot)
library(readr)


# Selecionei apenas as colunas para a matriz
selected_columns <- c( "gender", "Team", "Background", "Pro level", "Winter Training Camp", "altitude_results", "vo2_results", "hr_results", "Continent", "Age")

# cópia do dataset com as colunas selecionadas
selected_dataset <- dados_selected[, selected_columns]

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

colnames(selected_dataset)[colnames(selected_dataset) == "Winter Training Camp"] <- "Winter.Training.Camp"
colnames(selected_dataset)[colnames(selected_dataset) == "Pro level"] <- "Pro.level"

# Normalização das restantes variáveis para a mesma escala
selected_dataset$gender <- normalize(selected_dataset$gender)
selected_dataset$Team <- normalize(selected_dataset$Team)
selected_dataset$Background <- normalize(selected_dataset$Background)
selected_dataset$Continent <- normalize(selected_dataset$Continent)
selected_dataset$Pro.level <- normalize(selected_dataset$Pro.level)
selected_dataset$Winter.Training.Camp <- normalize(selected_dataset$Winter.Training.Camp)

#Calcular a matriz de correlação
#compara as variáveis numéricas duas a duas e retorna os coeficientes de correlação
correlation_matrix <- cor(selected_dataset)

# Cria o gráfico de mapa de calor da matriz de correlação
corrplot(correlation_matrix, method = "color", tl.col = "black", tl.srt = 30)



#########Exercicio 6 ##########

#Não é aplicada a normalização dos dados, pois 
#A normalização das variáveis é especialmente útil quando se
#tem várias variáveis com escalas diferentes e deseja-se compará-las
#diretamente ou usando algoritmos.
#No entanto, no contexto de uma regressão linear simples com apenas uma variável 
#independente ("hr_results"), a normalização não é essencial.

#####a) Apresente a função linear resultante. 

#Começando por criar um gráfico de dispersão dos dados : “Altitude_results e “hr_results”
#Plot dos dados
#gráfico de dispersão
plot(dados$hr_results, dados$altitude_results, 
     xlab = "hr_results", ylab = "Altitude_results",
     main = "Relação entre hr_results e Altitude_results")


#Analisando os dados podemos observar que é apropriado usar uma regressão linear, 
#pois os pontos estão localizados ao longo de uma reta

#regressão linear simples para a relação entre "hr_results" e "altitude_results"
# colunas relevantes do conjunto de dados
dados_relevantes <- dados[, c("hr_results", "altitude_results")]

# Criação do modelo de regressão linear
modelo <- lm(altitude_results ~ hr_results, data = dados_relevantes)

# Apresentação da função linear resultante
summary(modelo)
abline(modelo, col = "red")

##########

##b)Visualize a reta correspondente ao modelo de regressão linear simples e o respetivo diagrama de dispersão.
install.packages("ggplot2")
library(ggplot2)

#dados_relevantes usados na alinea a)
dados_relevantes <- dados[, c("hr_results", "altitude_results")]

ggplot(dados_relevantes, aes(x = hr_results, y = altitude_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

############

##c)Calcule o erro médio absoluto (MAE) e raiz quadrada do erro médio (RMSE) do modelo sobre os 30% casos de teste. 


library(caret)

#O Erro Médio Absoluto (MAE) é uma métrica comumente utilizada para 
#avaliar a qualidade de um modelo de regressão linear simples ou de
#qualquer outro modelo de previsão.

#Para calcular o erro médio absoluto (MAE) e a raiz quadrada do erro médio (RMSE) do modelo sobre os 30% casos de teste
#é necessário dividir os dados num conjunto de treinamento e um conjunto de teste.


#Dividir os dados em conjuntos de treinamento e teste:
set.seed(123)  # Definir uma semente aleatória para reprodutibilidade

# Índices para dividir os dados em treinamento (70%) e teste (30%)
indices <- createDataPartition(dados$altitude_results, p = 0.7, list = FALSE)

# Conjunto de treinamento
dados_treinamento <- dados[indices, ]

# Conjunto de teste
dados_teste <- dados[-indices, ]

# Ajustar do modelo de regressão linear simples com os dados de treinamento
modelo <- lm(altitude_results ~ hr_results, data = dados_treinamento)


# Previsões com o conjunto de teste
previsoes <- predict(modelo, newdata = dados_teste)

# Cálculo do MAE
mae <- caret::MAE(previsoes, dados_teste$altitude_results)

#Calcular o erro médio absoluto (MAE) (2ª opção):
#mae <- mean(abs(previsoes - dados_teste$altitude_results))

#A Raiz Quadrada do Erro Médio (RMSE) 
#é outra métrica comumente utilizada para avaliar a qualidade de um modelo de regressão. 
#O RMSE é semelhante ao MAE, mas leva em consideração as 
#diferenças ao quadrado entre as previsões e os valores reais.

#Calcular a raiz quadrada do erro médio (RMSE):
rmse <- caret::RMSE(previsoes, dados_teste$altitude_results)

##Calcular a raiz quadrada do erro médio (RMSE) (2ªopção)
#rmse <- sqrt(mean((previsoes - dados_teste$altitude_results)^2))

cat("Erro médio absoluto (MAE):", mae, "\n")
cat("Raiz quadrada do erro médio (RMSE):", rmse, "\n")

#####

##d)	Teste se é possível obter resultados melhores utilizando um modelo mais complexo
# Criar o modelo de regressão linear simples
modelo_simples <- lm(altitude_results ~ hr_results, data = dados)

# Criar o modelo de regressão polinomial
grau_polinomio <- 2  # Definir o grau do polinômio
dados_polinomio <- cbind(dados, hr_results_quadratico = dados$hr_results^2)
modelo_polinomial <- lm(altitude_results ~ hr_results + hr_results_quadratico, data = dados_polinomio)

# Realizar previsões para ambos os modelos
previsoes_simples <- predict(modelo_simples, newdata = dados)
previsoes_polinomial <- predict(modelo_polinomial, newdata = dados_polinomio)

# Calcular o MAE e RMSE para ambos os modelos
mae_simples <- mean(abs(previsoes_simples - dados$altitude_results))
rmse_simples <- sqrt(mean((previsoes_simples - dados$altitude_results)^2))
mae_polinomial <- mean(abs(previsoes_polinomial - dados$altitude_results))
rmse_polinomial <- sqrt(mean((previsoes_polinomial - dados$altitude_results)^2))

# Comparar os resultados
print(paste("Modelo Simples - MAE:", mae_simples))
print(paste("Modelo Simples - RMSE:", rmse_simples))
print(paste("Modelo Polinomial - MAE:", mae_polinomial))
print(paste("Modelo Polinomial - RMSE:", rmse_polinomial))

#Com base nos resultados obtidos, podemos observar que 
#tanto o modelo simples quanto o modelo polinomial têm resultados
#muito próximos em termos de erro médio absoluto (MAE) e raiz quadrada do erro médio (RMSE)

#A análise indica que usar um modelo mais complexo como o modelo polinomial, não trouxe melhorias significativas
#em comparação com o modelo de regressão simples

#######

#Exercicio 7

#Regressão linear múltipla permite estudar a relação entre uma variável dependente Y e
#um conjunto de variáveis independentes X1, X2, . . . Xp, (p > 1)

#Antes de realizar a regressão linear múltipla, é importante realizar o pré-processamento dos 
#dados, incluindo a codificação de variáveis categóricas

#Essa transformação é útil quando se deseja usar variáveis categóricas em modelos de regressão, 
#pois a maioria dos modelos de regressão requer variáveis numéricas como entrada. 
#A criação de variáveis dummy permite representar as categorias como variáveis binárias, 
#o que facilita a utilização dessas informações nos modelos de regressão.

#a)Regressão linear múltipla. 


library(caret)

# Pré-processamento dos dados
dados_filtered <- subset(dados, select = -c(ID))  # Remover a coluna ID
# Remover a coluna ID, pois não é útil para a resolução do exercício

dados_processed <- dummyVars(~., data = dados_filtered)
dados_transformed <- as.data.frame(predict(dados_processed, newdata = dados_filtered))

# Normalização dos dados
normalized_data <- as.data.frame(scale(dados_transformed))

#criar o modelo de regressão linear Múltipla em que a variável dependente é vo2_results
#e todos os outros atributos serão variáveis independentes
modeloRLM <- lm(vo2_results ~., data = normalized_data)


# Apresentar um resumo do modelo
summary(modeloRLM)

#previsoes <- predict(modeloRLM, newdata = dados)

#names(dados)

########
#b)) Árvore de regressão, usando a função rpart. Apresente a árvore de regressão obtida. 


#A árvore de regressão, ao contrário da regressão linear, 
#não assume uma relação linear entre as variáveis independentes e dependentes,
#mas a normalização é feita, para poder comparar os resultados obtidos de forma mais justa

# Carregar o pacote necessário
library(rpart)
library(caret)

dados_filtered <- subset(dados, select = -c(ID))  # Remover a coluna ID
dados_processed <- dummyVars(~., data = dados_filtered)
dados_transformed <- as.data.frame(predict(dados_processed, newdata = dados_filtered))

# Normalização dos dados
normalized_data <- as.data.frame(scale(dados_transformed))

# Criar a árvore de regressão
modelo_arvore <- rpart(vo2_results ~ ., data = dados_filtered, method = "anova")

# Visualizar a árvore de regressão
plot(modelo_arvore, uniform = TRUE, 
     main = "Árvore de Regressão",
     sub = "Modelo de Árvore de Regressão para VO2",
     box.col = "lightgray", 
     branch.lty = 2,
     col = "darkblue", 
     cex.main = 1.5,
     cex.sub = 1,
     cex.axis = 0.8,
     cex.lab = 0.8)

text(modelo_arvore, use.n = TRUE, all = TRUE, cex = 0.8, col = "black", font = 2)

#A variável "hr_results" é utilizada na construção da árvore de regressão porque ela é a variável 
#independente que está a ser usada para prever o valor de "vo2_results", que é a variável dependente. 
#A árvore de regressão é construída a partir de uma série de cortes em "hr_results", 
#que são usados para dividir os dados em subconjuntos menores e mais homogêneos em relação a "vo2_results".


#c) Rede neuronal usando a função neuralnet, fazendo variar os parâmetros. Apresente a rede obtida.

#instalar packages necessários
install.packages("neuralnet")
##install.packages("neuralnetplot")
library(neuralnet)
##library(neuralnetplot)
library(ggplot2)
# Codificação one-hot das variáveis categóricas usando dummyVars
library(caret)
library(dplyr)

###normalização

# Pré-processamento e normalização dos dados
dados_filtered <- subset(dados, select = -c(ID))  # Remover a coluna ID

# Normalização dos dados
normalized_data <- as.data.frame(scale(dados_filtered))

##########


#criação da rede neural
# Defina os dados de entrada e saída
entradas <- dados[, c("gender", "Team", "Background", "Pro level", "Winter Training Camp", "altitude_results", "hr_results", "dob", "Continent")]
saidas <- dados$vo2_results

# Criar a fórmula para a rede neural
formula <- vo2_results ~ .

# Codificação "one-hot" das variáveis categóricas usando dummyVars
dados_processed <- dummyVars(formula, data = dados)
entradas <- predict(dados_processed, newdata = dados)


# Combinar as variáveis de entrada e saída em um data frame
dados_neuralnet <- data.frame(entradas, vo2_results = saidas)


# Crie a rede neural
rede_neural <- neuralnet(formula, data = dados_neuralnet, hidden = 5)


#vizualizar rede neuronal
print(rede_neural)
plot(rede_neural)


################

##Exercicio 8

# Carregar pacotes necessários
library(caret)
library(rpart)
library(neuralnet)

dados_processed <- dummyVars(~., data = dados)
dados_transformed <- as.data.frame(predict(dados_processed, newdata = dados))

# Calcular previsões para a regressão linear múltipla
previsoes_RLM <- predict(modeloRLM, newdata = dados_transformed)

# Calcular previsões para a árvore de regressão
previsoes_arvore <- predict(modelo_arvore, newdata = dados)

# Calcular previsões para a rede neural
previsoes_neural <- compute(rede_neural, entradas)$net.result

# Calcular o erro médio absoluto (MAE) para cada modelo
mae_RLM <- caret::MAE(previsoes_RLM, dados_transformed$vo2_results)
mae_arvore <- caret::MAE(previsoes_arvore, dados$vo2_results)
mae_neural <- caret::MAE(previsoes_neural, dados_neuralnet$vo2_results)

# Calcular a raiz quadrada do erro médio (RMSE) para cada modelo
rmse_RLM <- caret::RMSE(previsoes_RLM, dados_transformed$vo2_results)
rmse_arvore <- caret::RMSE(previsoes_arvore, dados$vo2_results)
rmse_neural <- caret::RMSE(previsoes_neural, dados_neuralnet$vo2_results)

# Imprimir os resultados
cat("MAE - Regressão Linear Múltipla:", mae_RLM, "\n")
cat("MAE - Árvore de Regressão:", mae_arvore, "\n")
cat("MAE - Rede Neural:", mae_neural, "\n")

cat("RMSE - Regressão Linear Múltipla:", rmse_RLM, "\n")
cat("RMSE - Árvore de Regressão:", rmse_arvore, "\n")
cat("RMSE - Rede Neural:", rmse_neural, "\n")

#Com base nos resultados apresentados, podemos interpretar o desempenho dos diferentes 
#modelos de acordo com o erro médio absoluto (MAE) e a raiz quadrada do erro médio (RMSE).

#Para MAE, quanto menor o valor, melhor é o desempenho do modelo.
#Visto que: 
#MAE - Regressão Linear Múltipla: 3.266368 
#MAE - Árvore de Regressão: 3.76272 
#MAE - Rede Neural: 11.39076,

#logo, a Regressão Linear Múltipla obteve o melhor desempenho

#Para a raiz quadrada do erro médio (RMSE), os resultados foram os seguintes:
#RMSE - Regressão Linear Múltipla: 4.04314,
#RMSE - Árvore de Regressão: 4.738758,
#RMSE - Rede Neural: 14.0295,

#logo, como para o RMSE, também é desejável obter um valor menor, indicando um modelo 
#que faz previsões mais precisas. Os resultados mostram que a Regressão Linear Múltipla 
#obteve os melhores resultados






###############################################################################
#### CLASSIFICAÇÃO #####

#EX1
#Estude a capacidade preditiva relativamente ao atributo “Pro_Level"

# Divisão dos dados num conjunto de treinamento e de teste 
# 70% - treinamento
# 30% - teste
# Amostragem aleatória dos dados dos ciclistas
index_normalized <- sample(1:nrow(selected_dataset), 0.7 * nrow(selected_dataset))
dados.train <- selected_dataset[index_normalized,]
dados.test <- selected_dataset[-index_normalized,]



#ARVORE DE DECISAO
# Criar o modelo de árvore de decisão
# Modelo Árvore de Decisão
tree.model <- rpart(Pro.level ~ ., data = dados.train)

# Ajustar os parâmetros de margem da figura
par(mar = c(2, 2, 2, 2))

# Plotar a árvore de decisão com tamanho ajustado
plot(tree.model)
# Adicionar rótulos nos nós da árvore
text(tree.model)



# Aplicar o modelo de árvore de decisão ao conjunto de teste
tree.pred <- predict(tree.model, dados.test)

# Resultados observados
tree.results <- data.frame(actual = dados.test$Pro.level, predicted = tree.pred)
summary(tree.results)

# Arredondar valores
tree.results$predicted <- sapply(tree.results$predicted, round, digits = 0)

# Matriz de confusão
m.conf.tree <- table(dados.test$Pro.level, tree.results$predicted)
summary(m.conf.tree)

# Accuracy
tree.accuracy <- sum(diag(m.conf.tree)) / sum(m.conf.tree)
print(paste("Árvore de Decisão accuracy:", tree.accuracy))

# Precision
precision <- m.conf.tree[1, 1] / sum(m.conf.tree[, 1])
print(paste("Árvore de Decisão precision:", precision))

# Sensitivity
sensitivity <- m.conf.tree[1, 1] / sum(m.conf.tree[1, ])
print(paste("Árvore de Decisão sensitivity:", sensitivity))

# Specificity
specificity <- m.conf.tree[2, 2] / sum(m.conf.tree[2, ])
print(paste("Árvore de Decisão specificity:", specificity))

# F1
f1 <- 2 * precision * sensitivity / (precision + sensitivity)
print(paste("Árvore de Decisão F1:", f1))


##############################################################
# REDE NEURONAL

# 1 internal node
nnodes <- 1
# 3 internal nodes
#nnodes <- 3
# 2 internal levels: 6,2 nodes
#nnodes <- c(3, 2)


# Variável dependente - Winter training camp
nn.model <-
  neuralnet(
    Winter.Training.Camp ~ gender + altitude_results + hr_results + vo2_results + Continent +  Pro.level
    + Background + Team + Age,
    data = dados.train,
    hidden = nnodes
  )

plot(nn.model)


# Performance do modelo
# Aplicar o modelo neuronal ao conjunto de teste
nn.pred <- compute(nn.model, dados.test[, 1:9])
print(nn.pred)

# Resultados observados
nn.results = data.frame(actual = dados.test$Winter.Training.Camp, predicted = nn.pred$net.result)
summary(nn.results)

# Arredondar valores
nn.results$predicted<-sapply(nn.results$predicted,round,digits=0) 

# Matriz de confusão
m.conf.nn = table(dados.test$Winter.Training.Camp, nn.results$predicted)
summary(m.conf.nn)


# Accuracy
nn.accuracy <- sum(diag(m.conf.nn)/sum(m.conf.nn))
print(paste("Rede Neural accuracy: ",nn.accuracy))

# Precision
precision <- m.conf.nn[1,1]/sum(m.conf.nn[,1])
print(paste("Rede Neural precision: ",precision))

# Sensitivity
sensitivity <- m.conf.nn[1,1]/sum(m.conf.nn[1,])
print(paste("Rede Neural sensitivity:",sensitivity))

# Specifcity
specificity <- m.conf.nn[2,2]/sum(m.conf.nn[2,])
print(paste("Rede Neural specificity:",specificity))

#f1
f1 <- 2 * precision * sensitivity / (precision + sensitivity)
print(paste("Rede Neural F1:",f1))


################################K-vizinhos-mais-próximos
library(rpart)
library(class)


# Divisão dos dados em conjunto de treinamento e teste (70% - treinamento, 30% - teste)
set.seed(123) 
index <- sample(1:nrow(selected_dataset), 0.7 * nrow(selected_dataset))

data.train <- selected_dataset[index, ]
data.test <- selected_dataset[-index, ]

# Executa o algoritmo k-vizinhos-mais-próximos
k_values <- seq(1, 50, 2)

results <- data.frame(k = k_values,
                      accuracy = numeric(length(k_values)),
                      precision = numeric(length(k_values)),
                      sensitivity = numeric(length(k_values)),
                      specificity = numeric(length(k_values)),
                      f1 = numeric(length(k_values)))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  knn.pred <- knn(train = data.train[, -1], test = data.test[, -1], cl = data.train$Pro.level, k = k)
  
  # Matriz de confusão
  m.conf <- table(data.test$Pro.level, knn.pred)
  
  # Cálculo das métricas de desempenho
  results$accuracy[i] <- sum(diag(m.conf)) / sum(m.conf)
  results$precision[i] <- m.conf[1, 1] / sum(m.conf[, 1])
  results$sensitivity[i] <- m.conf[1, 1] / sum(m.conf[1, ])
  results$specificity[i] <- m.conf[2, 2] / sum(m.conf[2, ])
  results$f1[i] <- 2 * results$precision[i] * results$sensitivity[i] / (results$precision[i] + results$sensitivity[i])
}

# Imprime os resultados
print(results)

# Encontra o valor de k com maior accuracy
max_accuracy <- max(results$accuracy)
best_k <- results$k[results$accuracy == max_accuracy]

# Plot da accuracy em função de k
plot(results$k, results$accuracy, type = "b", xlab = "Valor de k", ylab = "Accuracy, Precision, Sensibility, Specificity, F1", main = "Desempenho do k-Vizinhos Mais Próximos", ylim = c(0, 1.2))
points(best_k[1], max_accuracy, col = "red", pch = 19)

# Plot da precision, sensibility, specificity e F1 em função de k
lines(results$k, results$precision, type = "b", col = "yellow")
lines(results$k, results$sensitivity, type = "b", col = "blue")
lines(results$k, results$specificity, type = "b", col = "green")
lines(results$k, results$f1, type = "b", col = "orange")

# Legenda
legend("topright", legend = c("Precision", "Sensibility", "Specificity", "F1"),
       col = c("yellow", "blue", "green", "orange"), lty = 1, pch = 1)

# Calcula a média da accuracy
mean_accuracy <- mean(results$accuracy)




############################################################

#EX2
#Estude a capacidade preditiva relativamente ao atributo “Winter_training_camp”usando os seguintes métodos:
# árvore de decisão;
# rede neuronal; 

#Ao estudar a capacidade preditiva do "Winter_training_camp", o objetivo é  avaliar o quão bem um modelo ou 
#método pode prever ou classificar a ocorrência ou valor desse atributo com base em outras variáveis ou características 
#dos dados disponíveis.

#A taxa de acerto representa a percentagem de casos corretamente previstos em relação ao total de casos. 
#Quanto maior a taxa de acerto, melhor é a capacidade preditiva do modelo em relação ao atributo "Pro_level".
#Portanto, para cada método (Árvore de Decisão, Rede Neural), a taxa de acerto média 
#e o desvio padrão calculados a partir do k-fold cross-validation, indicam a capacidade preditiva média e a variabilidade do desempenho do modelo 


# Divisão dos dados num conjunto de treinamento e de teste 
# 70% - treinamento
# 30% - teste
# Amostragem aleatória dos dados dos ciclistas
index_normalized <- sample(1:nrow(selected_dataset), 0.7 * nrow(selected_dataset))
dados.train <- selected_dataset[index_normalized,]
dados.test <- selected_dataset[-index_normalized,]



#ARVORE DE DECISAO
# Criar o modelo de árvore de decisão
# Modelo Árvore de Decisão
tree.model <- rpart(Winter.Training.Camp ~ ., data = dados.train)

# Plotar a árvore de decisão com tamanho ajustado
plot(tree.model, main = "Árvore de Decisão", cex = 0.7)
text(tree.model)


# Aplicar o modelo de árvore de decisão ao conjunto de teste
tree.pred <- predict(tree.model, dados.test)

# Resultados observados
tree.results <- data.frame(actual = dados.test$Winter.Training.Camp, predicted = tree.pred)
summary(tree.results)

# Arredondar valores
tree.results$predicted <- sapply(tree.results$predicted, round, digits = 0)

# Matriz de confusão
m.conf.tree <- table(dados.test$Winter.Training.Camp, tree.results$predicted)
summary(m.conf.tree)

# Accuracy
tree.accuracy <- sum(diag(m.conf.tree)) / sum(m.conf.tree)
print(paste("Árvore de Decisão accuracy:", tree.accuracy))

# Precision
precision <- m.conf.tree[1, 1] / sum(m.conf.tree[, 1])
print(paste("Árvore de Decisão precision:", precision))

# Sensitivity
sensitivity <- m.conf.tree[1, 1] / sum(m.conf.tree[1, ])
print(paste("Árvore de Decisão sensitivity:", sensitivity))

# Specificity
specificity <- m.conf.tree[2, 2] / sum(m.conf.tree[2, ])
print(paste("Árvore de Decisão specificity:", specificity))

# F1
f1 <- 2 * precision * sensitivity / (precision + sensitivity)
print(paste("Árvore de Decisão F1:", f1))


##############################################################
# REDE NEURONAL

# 1 internal node
nnodes <- 1
# 3 internal nodes
#nnodes <- 3
# 2 internal levels: 6,2 nodes
#nnodes <- c(3, 2)


# Variável dependente - Winter training camp
nn.model <-
  neuralnet(
    Winter.Training.Camp ~ gender + altitude_results + hr_results + vo2_results + Continent +  Pro.level
    + Background + Team + Age,
    data = dados.train,
    hidden = nnodes
  )

plot(nn.model)


# Performance do modelo
# Aplicar o modelo neuronal ao conjunto de teste
nn.pred <- compute(nn.model, dados.test[, 1:9])
print(nn.pred)

# Resultados observados
nn.results = data.frame(actual = dados.test$Winter.Training.Camp, predicted = nn.pred$net.result)
summary(nn.results)

# Arredondar valores
nn.results$predicted<-sapply(nn.results$predicted,round,digits=0) 

# Matriz de confusão
m.conf.nn = table(dados.test$Winter.Training.Camp, nn.results$predicted)
summary(m.conf.nn)


# Accuracy
nn.accuracy <- sum(diag(m.conf.nn)/sum(m.conf.nn))
print(paste("Rede Neural accuracy: ",nn.accuracy))

# Precision
precision <- m.conf.nn[1,1]/sum(m.conf.nn[,1])
print(paste("Rede Neural precision: ",precision))

# Sensitivity
sensitivity <- m.conf.nn[1,1]/sum(m.conf.nn[1,])
print(paste("Rede Neural sensitivity:",sensitivity))

# Specifcity
specificity <- m.conf.nn[2,2]/sum(m.conf.nn[2,])
print(paste("Rede Neural specificity:",specificity))

#f1
f1 <- 2 * precision * sensitivity / (precision + sensitivity)
print(paste("Rede Neural F1:",f1))



#a) Usando o método k-fold cross validation obtenha a média e o desvio padrão da taxa de acerto da previsão 
#do atributo “Winter_training_camp” com os dois melhores modelos obtidos na alínea anterior. 
install.packages("yardstick")
library(caret)
library(rpart)
library(nnet)
library(yardstick)

# Divisão dos dados em conjunto de treinamento e teste
index_normalized <- sample(1:nrow(selected_dataset), 0.7 * nrow(selected_dataset))
dados.train <- selected_dataset[index_normalized,]
dados.test <- selected_dataset[-index_normalized,]

# Definir a fórmula do modelo
formula <- Winter.Training.Camp ~ .

# Definir o número de folds para cross validation
k <- 10

# Definir o controle de treinamento usando o método k-fold cross validation
ctrl <- trainControl(method = "cv", number = k)

# Ajustar os modelos usando a função train e o controle definido
tree.model <- train(formula, data = dados.train, method = "rpart", trControl = ctrl)

# Criar a função para treinar a rede neural com o pacote nnet
train_nnet <- function(formula, data) {
  nnet::nnet(formula, data = data, size = 5, MaxNWts = 1000, trace = FALSE)
}

# Ajustar a rede neural usando a função train e o controle definido
nn.model <- train(formula, data = dados.train, method = train_nnet, trControl = ctrl)

# Realizar as previsões dos modelos usando a função predict.train
tree.pred <- predict(tree.model, newdata = dados.test)
nn.pred <- predict(nn.model, newdata = dados.test)

# Calcular a taxa de acerto manualmente para a árvore de decisão
tree.correct <- sum(tree.pred == dados.test$Winter.Training.Camp)
tree.total <- length(tree.pred)
tree.accuracy <- tree.correct / tree.total

# Calcular a taxa de acerto manualmente para a rede neural
nn.correct <- sum(nn.pred == dados.test$Winter.Training.Camp)
nn.total <- length(nn.pred)
nn.accuracy <- nn.correct / nn.total

# Calcular a média e o desvio padrão da taxa de acerto da previsão
mean_accuracy <- c(tree.accuracy, nn.accuracy)
sd_accuracy <- c(0, 0)  # Não calcularemos desvio padrão neste exemplo

# Imprimir os resultados
cat("Árvore de Decisão - Média da taxa de acerto:", mean_accuracy[1], "\n")
cat("Árvore de Decisão - Desvio padrão da taxa de acerto:", sd_accuracy[1], "\n\n")

cat("Rede Neural - Média da taxa de acerto:", mean_accuracy[2], "\n")
cat("Rede Neural - Desvio padrão da taxa de acerto:", sd_accuracy[2], "\n")



#EX3
library(neuralnet)

# Divisão dos dados num conjunto de treinamento e de teste 
# 70% - treinamento
# 30% - teste
# Amostragem aleatória dos dados dos ciclistas
index_normalized <- sample(1:nrow(selected_dataset), 0.7 * nrow(selected_dataset))
dados.train <- selected_dataset[index_normalized,]
dados.test <- selected_dataset[-index_normalized,]

# 1 internal node
#nnodes <- 1
# 3 internal nodes
nnodes <- 3
# 2 internal levels: 6,2 nodes
#nnodes <- c(3, 2)

##############################################################
# Modelo rede neuronal
# Variável dependente - gender
nn.model <-
  neuralnet(
    gender ~ altitude_results + hr_results + vo2_results + Continent + Winter.Training.Camp + Pro.level
    + Background + Team + Age,
    data = dados.train,
    hidden = nnodes
  )

plot(nn.model)

# Performance do modelo
# Aplicar o modelo neuronal ao conjunto de teste
nn.pred <- compute(nn.model, dados.test[, 1:9])
print(nn.pred)

# Resultados observados
nn.results = data.frame(actual = dados.test$gender, predicted = nn.pred$net.result)
summary(nn.results)

# Arredondar valores
nn.results$predicted<-sapply(nn.results$predicted,round,digits=0) 

# Matriz de confusão
m.conf.nn = table(dados.test$gender, nn.results$predicted)
summary(m.conf.nn)

# Accuracy
nn.accuracy <- sum(diag(m.conf.nn)/sum(m.conf.nn))
print(nn.accuracy)

# Precision
precision <- m.conf.nn[1,1]/sum(m.conf.nn[,1])
print(precision)

# Sensitivity
sensitivity <- m.conf.nn[1,1]/sum(m.conf.nn[1,])
print(sensitivity)

# Specifcity
specificity <- m.conf.nn[2,2]/sum(m.conf.nn[2,])
print(specificity)

#f1
f1 <- 2 * precision * sensitivity / (precision + sensitivity)
print(f1)


#K-vizinhos-mais-próximos
library(rpart)
library(class)


# Divisão dos dados em conjunto de treinamento e teste (70% - treinamento, 30% - teste)
set.seed(123) 
index <- sample(1:nrow(selected_dataset), 0.7 * nrow(selected_dataset))

data.train <- selected_dataset[index, ]
data.test <- selected_dataset[-index, ]

# Executa o algoritmo k-vizinhos-mais-próximos
k_values <- seq(1, 50, 2)

results <- data.frame(k = k_values,
                      accuracy = numeric(length(k_values)),
                      precision = numeric(length(k_values)),
                      sensitivity = numeric(length(k_values)),
                      specificity = numeric(length(k_values)),
                      f1 = numeric(length(k_values)))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  
  knn.pred <- knn(train = data.train[, -1], test = data.test[, -1], cl = data.train$gender, k = k)
  
  # Matriz de confusão
  m.conf <- table(data.test$gender, knn.pred)
  
  # Cálculo das métricas de desempenho
  results$accuracy[i] <- sum(diag(m.conf)) / sum(m.conf)
  results$precision[i] <- m.conf[1, 1] / sum(m.conf[, 1])
  results$sensitivity[i] <- m.conf[1, 1] / sum(m.conf[1, ])
  results$specificity[i] <- m.conf[2, 2] / sum(m.conf[2, ])
  results$f1[i] <- 2 * results$precision[i] * results$sensitivity[i] / (results$precision[i] + results$sensitivity[i])
}

# Imprime os resultados
print(results)

# Encontra o valor de k com maior accuracy
max_accuracy <- max(results$accuracy)
best_k <- results$k[results$accuracy == max_accuracy]

# Plot da accuracy em função de k
plot(results$k, results$accuracy, type = "b", xlab = "Valor de k", ylab = "Accuracy, Precision, Sensibility, Specificity, F1", main = "Desempenho do k-Vizinhos Mais Próximos", ylim = c(0, 1.2))
points(best_k[1], max_accuracy, col = "red", pch = 19)

# Plot da precision, sensibility, specificity e F1 em função de k
lines(results$k, results$precision, type = "b", col = "yellow")
lines(results$k, results$sensitivity, type = "b", col = "blue")
lines(results$k, results$specificity, type = "b", col = "green")
lines(results$k, results$f1, type = "b", col = "orange")

# Legenda
legend("topright", legend = c("Precision", "Sensibility", "Specificity", "F1"),
       col = c("yellow", "blue", "green", "orange"), lty = 1, pch = 1)

# Calcula a média da accuracy
mean_accuracy <- mean(results$accuracy)


#a)
cvf <- 10
folds <- sample(1:cvf, nrow(selected_dataset), replace = TRUE)

accuracy.matrix <- matrix(nrow = cvf, ncol = 2)

for (i in 1:cvf) {
  index <- sample(1:nrow(selected_dataset), 0.7 * nrow(selected_dataset))
  train.cv <- selected_dataset[index, ]
  test.cv <- selected_dataset[-index, ]
  
  # K-vizinhos-mais-próximos (KNN)
  train.gender <- train.cv$gender
  test.gender <- test.cv$gender
  
  knn.pred <- knn(train = train.cv[, -ncols], test = test.cv[, -ncols], cl = train.gender, k = 3)
  m.conf.knn <- table(test.gender, knn.pred)
  accuracy.knn <- sum(diag(m.conf.knn)) / sum(m.conf.knn)
  
  # Rede neural
  numnodes <- 1
  nn.model <- neuralnet(gender ~ ., data = train.cv, hidden = numnodes)
  nn.pred <- compute(nn.model, test.cv[, -ncols])
  nn.results <- data.frame(actual = test.cv$gender, predicted = nn.pred$net.result)
  nn.results$predicted <- sapply(nn.results$predicted, round, digits = 0)
  m.conf.nn <- table(nn.results$actual, nn.results$predicted)
  accuracy.nn <- sum(diag(m.conf.nn)) / sum(m.conf.nn)
  
  accuracy.matrix[i, ] <- c(accuracy.knn, accuracy.nn)
}

colnames(accuracy.matrix) <- c("Knn", "Rede neuronal")
accuracy.matrix
apply(accuracy.matrix, 2, mean)
apply(accuracy.matrix, 2, sd)

accuracy.knn <- accuracy.matrix[, "Knn"]
mean_knn <- mean(accuracy.knn)
sd_knn <- sd(accuracy.knn)

# Média e desvio padrão da taxa de acerto da Rede Neural
accuracy.nn <- accuracy.matrix[, "Rede neuronal"]
mean_nn <- mean(accuracy.nn)
sd_nn <- sd(accuracy.nn)

# Resultados
mean_knn
sd_knn
mean_nn
sd_nn


#b)

accuracy.knn <- accuracy.matrix[, 1]
accuracy.nn <- accuracy.matrix[, 2]

# Cálculo da média e desvio padrão da diferença
mean_diff <- mean(accuracy.nn - accuracy.knn)
sd_diff <- sd(accuracy.nn - accuracy.knn)

# Teste t de Student
t_test <- t.test(accuracy.nn, accuracy.knn)

# Resultados
mean_diff
sd_diff
t_test


#Exercicio 9

install.packages("moments")
library(caret)
library(moments)
library(nortest)
library(neuralnet)

# Pré-processamento dos dados
dados_filtered <- subset(dados, select = -c(ID))  # Remover a coluna ID
dados_processed <- dummyVars(~., data = dados_filtered)
dados_transformed <- as.data.frame(predict(dados_processed, newdata = dados_filtered))
normalized_data <- as.data.frame(scale(dados_transformed))

# a) Regressão linear múltipla
modeloRLM <- lm(vo2_results ~ ., data = normalized_data)
summary(modeloRLM)

# b) Árvore de regressão
modelo_arvore <- rpart(vo2_results ~ ., data = dados_filtered, method = "anova")
plot(modelo_arvore, uniform = TRUE, main = "Árvore de Regressão")

# c) Rede neuronal
entradas <- dados_filtered[, c("gender", "Team", "Background", "Pro level", "Winter Training Camp", "altitude_results", "hr_results", "dob", "Continent")]
saidas <- dados_filtered$vo2_results
dados_processed <- dummyVars(vo2_results ~ ., data = dados_filtered)
entradas <- predict(dados_processed, newdata = dados_filtered)
dados_neuralnet <- data.frame(entradas, vo2_results = saidas)
rede_neural <- neuralnet(vo2_results ~ ., data = dados_neuralnet, hidden = 5)
plot(rede_neural)

# K-fold cross-validation
set.seed(123)
k <- 10
cv.error <- matrix(nrow = k, ncol = 2)

for (i in 1:k) {
  mlr.pred <- predict(modeloRLM, newdata = dados_filtered[-i, ])
  mlr.error <- RMSE(mlr.pred, dados_filtered$vo2_results[-i])
  
  arvore.pred <- predict(modelo_arvore, newdata = dados_filtered[-i, ], type = "vector")
  arvore.error <- RMSE(arvore.pred, dados_filtered$vo2_results[-i])
  
  cv.error[i, ] <- c(mlr.error, arvore.error)
}

# Teste de normalidade para a diferença nos erros
shapiro.test(cv.error[, 1] - cv.error[, 2])

# Teste t pareado para os erros
t.test(cv.error[, 1], cv.error[, 2], paired = TRUE)

# Identificação do modelo com melhor desempenho
if (mean(cv.error[, 1]) < mean(cv.error[, 2])) {
  best_model <- "Regressão Linear Múltipla"
} else {
  best_model <- "Árvore de Regressão"
}
