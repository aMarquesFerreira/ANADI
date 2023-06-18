
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


#########Exercicio 5#############

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
selected_columns <- c("ID", "gender", "Team", "Background", "Pro level", "Winter Training Camp", "altitude_results", "vo2_results", "hr_results", "dob", "Continent","Age")

# cópia do dataset com as colunas selecionadas
selected_dataset <- dados[, selected_columns]

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

########

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