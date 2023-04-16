
# Importar os dados
#dados <- read.csv("DADOS2.csv")
dados <- DADOS2

# Realizar o teste de Shapiro-Wilk para cada coluna
shapiro.test(dados$SVM) # p-value=0.2687 > alfa, logo não se rejeita H0
shapiro.test(dados$DT)  # p-value=0.06772 > alfa, logo não se rejeita H0
shapiro.test(dados$KN)  # p-value=0.06926 > alfa, logo não se rejeita H0
shapiro.test(dados$RF)  # p-value=0.3138 > alfa, logo não se rejeita H0
shapiro.test(dados$ML)  # p-value=0.02138 < alfa=0.05, logo rejeita-se H0 e conclui-se que não segue uma distribuição normal 
shapiro.test(dados$GB)  # p-value=0.5125 > alfa, logo não se rejeita H0

#Todas as variáveis, exceto "ML", possuem uma distribuição normal, 
#com p-values maiores que 0,05. A variável "ML" tem um p-value de 0,02138, 
#o que é menor que 0,05, indicando que ela não está distribuída normalmente.

#Como um dos conjuntos de dados "ML"
#não segue uma distribuição normal, 
#é mais apropriado usar um teste não paramétrico, 
#como o teste de Friedman que não prossupoe que os dados estejam normalizados


# Perform Friedman rank sum test
friedman.test(cbind(dados$SVM, dados$DT, dados$KN, dados$RF, dados$ML, dados$GB))



#O resultado do teste de Friedman mostrou um valor de chi-squared de 8.7097 
#df de 5 graus de liberdade e um p-value de 0.1212. Como o p-value(0.1212) é maior que o nível de significância (0.05), 
#não há evidências estatisticamente significativas para rejeitar a hipótese nula, o que significa que não há diferenças 
#significativas entre a precisão dos diferentes algoritmos.


