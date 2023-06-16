
#Exercicio 1

#importar dados
dados <- ciclismo

#verificar dimensão
dim(dados)

#fazer sumário dos dados
summary(dados)

##

#Exercicio 2

dados$dob <- as.Date(dados$dob, format = "%Y-%m-%d")
dados$Age <- as.integer(format(Sys.Date(), "%Y")) - as.integer(format(dados$dob, "%Y"))

print(dados)

print(dados$Age)

#Exercicio 6

##a)

#regressão linear simples para a relação entre "hr_results" e "altitude_results"

# colunas relevantes do seu conjunto de dados
dados_relevantes <- dados[, c("hr_results", "altitude_results")]


modelo <- lm(altitude_results ~ hr_results, data = dados_relevantes)


summary(modelo)


##b)
install.packages("ggplot2")
library(ggplot2)

ggplot(dados_relevantes, aes(x = hr_results, y = altitude_results)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

##c)

#Para calcular o erro médio absoluto (MAE) e a raiz quadrada do erro médio (RMSE) do modelo sobre os 30% casos de teste
#é necessário dividir os dados num conjunto de treinamento e um conjunto de teste.


#Dividir os dados em conjuntos de treinamento e teste:
set.seed(123)  # Definir uma semente aleatória para reprodutibilidade
n <- nrow(dados_relevantes)
indice_teste <- sample(1:n, round(0.3 * n))  # Amostra aleatória de 30% dos casos para teste
conjunto_treinamento <- dados_relevantes[-indice_teste, ]  # Conjunto de treinamento
conjunto_teste <- dados_relevantes[indice_teste, ]  # Conjunto de teste

#Ajustar o modelo usando o conjunto de treinamento:

modelo <- lm(altitude_results ~ hr_results, data = conjunto_treinamento)

#Calcular as previsões do modelo para o conjunto de teste:

previsoes <- predict(modelo, newdata = conjunto_teste)

#Calcular o erro médio absoluto (MAE):

mae <- mean(abs(previsoes - conjunto_teste$altitude_results))

#Calcular a raiz quadrada do erro médio (RMSE):

rmse <- sqrt(mean((previsoes - conjunto_teste$altitude_results)^2))

cat("Erro médio absoluto (MAE):", mae, "\n")
cat("Raiz quadrada do erro médio (RMSE):", rmse, "\n")


#Exercicio 7





