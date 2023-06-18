
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

dados$dob <- as.Date(dados$dob, format = "%Y-%m-%d")
dados$Age <- as.integer(format(Sys.Date(), "%Y")) - as.integer(format(dados$dob, "%Y"))

print(dados)

print(dados$Age)

#########Exercicio 6 ##########

#####a)

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

##b)
install.packages("ggplot2")
library(ggplot2)

#dados_relevantes usados na alinea a)
dados_relevantes <- dados[, c("hr_results", "altitude_results")]

ggplot(dados_relevantes, aes(x = hr_results, y = altitude_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

############

##c)

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

##d)
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

library(caret)


dados_processed <- dummyVars(~., data = dados)
dados_transformed <- predict(dados_processed, newdata = dados)

#criar o modelo de regressão linear Múltipla em que a variável dependente é vo2_results
#e todos os outros atributos serão variáveis independentes
modeloRLM <- lm(vo2_results ~., data = dados_transformed)


# Apresentar um resumo do modelo
summary(modeloRLM)

# Fazer previsões com novos dados
novos_dados <- data.frame(
  gender = "female",
  Team = "group C",
  Background = "Cobblestones",
  "Pro level" = "World Tour",
  "Winter Training Camp" = "completed",
  altitude_results = 60,
  hr_results = 70,
  dob = as.Date("1990-01-01"),
  Continent = "Europe"
)

novas_previsoes <- predict(modeloRLM, newdata = novos_dados)

########

#b)

# Carregar o pacote necessário
library(rpart)

# Criar a árvore de regressão
modelo_arvore <- rpart(vo2_results ~ ., data = dados, method = "anova")

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
#independente que está sendo usada para prever o valor de "vo2_results", que é a variável dependente. 
#A árvore de regressão é construída a partir de uma série de cortes em "hr_results", 
#que são usados para dividir os dados em subconjuntos menores e mais homogêneos em relação a "vo2_results".


#c)

#instalar packages necessários
install.packages("neuralnet")
##install.packages("neuralnetplot")
library(neuralnet)
##library(neuralnetplot)
library(ggplot2)
# Codificação one-hot das variáveis categóricas usando dummyVars
library(caret)

#criação da rede neural
# Defina os dados de entrada e saída
entradas <- dados[, c("gender", "Team", "Background", "Pro level", "Winter Training Camp", "altitude_results", "hr_results", "dob", "Continent")]
saidas <- dados$vo2_results

# Criar a fórmula para a rede neural
formula <- vo2_results ~ .

# Codificação "one-hot" das variáveis categóricas usando dummyVars
dados_processed <- dummyVars(formula, data = dados)
entradas <- predict(dados_processed, newdata = dados)


# Combine as variáveis de entrada e saída em um data frame
dados_neuralnet <- data.frame(entradas, vo2_results = saidas)


# Crie a rede neural
rede_neural <- neuralnet(formula, data = dados_neuralnet, hidden = 5)


#vizualizar rede neuronal
print(rede_neural)
plot(rede_neural)


################



