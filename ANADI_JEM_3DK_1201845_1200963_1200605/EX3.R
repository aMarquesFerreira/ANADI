#Alinea a)
#se nao tiverem distribuicao normal, a media nao é uma representacao dos dados
#A ANOVA pressupõe que os dados em cada grupo sejam normalmente distribuídos, e violações dessa pressuposição podem levar a resultados incorretos.

#Normalmente Distribuidos?
#Teste de Shapiro-Wilk: É amplamente utilizado e é recomendado para amostras de tamanho pequeno a moderado (n <= 50). 
# A hipótese nula (H0) do teste é que os dados são normalmente distribuídos, enquanto a hipótese alternativa (H1) é que os dados não são normalmente distribuídos.
# Valor-p > 0,05: aceitar a hipótese nula,dados podem seguir uma distribuição normal.
# Valor-p < 0,05: rejeitar a hipótese nula, dados não seguem uma distribuição normal.


# Carregar os dados do arquivo CSV
dados <- DADOS3

# armazenar os dados de aceleração para cada grupo
cilindros4 <- as.numeric(dados$Acceleration[dados$Cylinders == 4])
cilindros6 <- as.numeric(dados$Acceleration[dados$Cylinders == 6])
cilindros8 <- as.numeric(dados$Acceleration[dados$Cylinders == 8])

# O teste de normalidade de Shapiro-Wilk para cada 
res_shapiro_cilindros4 <- shapiro.test(cilindros4)
res_shapiro_cilindros6 <- shapiro.test(cilindros6)
res_shapiro_cilindros8 <- shapiro.test(cilindros8)

# Extrair o pvalue para mostrar
p_cilindros4 <- res_shapiro_cilindros4$p.value
p_cilindros6 <- res_shapiro_cilindros6$p.value
p_cilindros8 <- res_shapiro_cilindros8$p.value

# Verificar se os grupos podem ser considerados normalmente distribuídos (p-valor > 0,05)
if (res_shapiro_cilindros4$p.value > 0.05 & 
    res_shapiro_cilindros6$p.value > 0.05 & 
    res_shapiro_cilindros8$p.value > 0.05) {
  
  #  normalmente distribuídos -> abordagem paramétrica, como ANOVA.
  print("Os grupos seguem uma distribuição normal.")
  cat("4_p-valor =", p_cilindros4, "\n")
  cat("6_p-valor =", p_cilindros6, "\n")
  cat("8_p-valor =", p_cilindros8, "\n")
  
} else {
  # não normalmente distribuídos -> abordagem não paramétrica, como Kruskal-Wallis.
  print("Os grupos não seguem uma distribuição normal. Considere usar uma abordagem não paramétrica.")
  cat("4_p-valor =", p_cilindros4, "\n")
  cat("6_p-valor =", p_cilindros6, "\n")
  cat("8_p-valor =", p_cilindros8, "\n")
}


## teste de Kruskal-Wallis 

# analisar as diferenças entre os grupos
res_kruskal_wallis <- kruskal.test(list(cilindros4, cilindros6, cilindros8))

# Extrair p-value para apresentar
p_kruskal_wallis <- resultado_kruskal_wallis$p.value

# Verificar se há diferenças significativas entre os grupos (p-valor < 0,05)
if (p_kruskal_wallis < 0.05) {
 cat("p-value =", p_kruskal_wallis, " < 0,05 -> Há diferenças significativas entre os 3 grupos.")
} else {
  cat("p-value =", p_kruskal_wallis, " > 0,05 -> Não há diferenças significativas entre os 3 grupos.")
}




#############################################################################################################

#Alinea b)
#Supondo que a aceleração é a variável dependente e as restantes variáveis são independentes:

#i)
# A variável dependente é "Acceleration" e as variáveis independentes são "Cylinders" (como uma variável dummy/fator), "Weight" e "Horsepower". 
# A função summary() é então usada para obter um resumo do modelo ajustado, que inclui informações como os coeficientes de regressão, estatísticas de ajuste, e 
#valores de p-valor para avaliar a significância dos coeficientes.
#Um valor de p baixo (geralmente menor que 0,05) indica uma relação estatisticamente significativa entre a variável independente e a variável dependente.

# Transformar a variável "Cylinders" numa variável dummy (fator) 
dados$Cylinders <- as.factor(dados$Cylinders)

#O objetivo é encontrar os coeficientes de regressão que melhor explicam a relação linear entre as variáveis.
modelo <- lm(Acceleration ~ Cylinders + Weight + Horsepower, data = dados)

# obter um resumo do modelo (coeficientes de regressão, estatísticas, e p-values) para avaliar a significância dos coeficientes.
summary(modelo)



#ii)
# Use o modelo encontrado na alínea anterior para estimar a aceleração de uma viatura com: 
# um peso de 2950 kg, potência de 100 Hp e 4 cilindros.

# Criar um novo data frame com os valores de entrada
#A transformação o 4 em fator é necessária, é um valor específicos(variável categórica, que representa características discretas)
#a variável dependente precisa ser contínua, enquanto as variáveis independentes podem ser tanto contínuas quanto categóricas. 
# O weight e horsepower são variáveis contínuas, ou seja, representam quantidades numéricas em uma escala contínua.
novos_dados <- data.frame(Cylinders = as.factor(4), Weight = 2950, Horsepower = 100)

# Usar a função predict() para estimar a aceleração com base no modelo ajustado e nos novos dados
estimativa <- predict(modelo, newdata = novos_dados)

# Imprimir a estimativa de aceleração
cat("Estimativa de aceleração de uma viatura com um peso de 2950 kg, potência de 100 Hp e 4 cilindros:", estimativa, "\n")