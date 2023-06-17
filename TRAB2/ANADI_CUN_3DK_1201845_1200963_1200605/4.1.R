
######Exercicio 1

#importar dados
dados <- ciclismo

#verificar dimensão
dim(dados)

#fazer sumário dos dados
summary(dados)

#######

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





