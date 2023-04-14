#Testes paramétricos vs testes não paramétricos:


#Os testes estatísticos podem ser classificados em dois tipos: 

#testes paramétricos e não paramétricos:

#Testes Paramétricos:

#--> Testes paramétricos são mais robustos

#--> são aqueles que assumem que os dados seguem uma distribuição de probabilidade específica (como a distribuição normal)

#---> e que possuem um conjunto de parâmetros fixos (como a média e a variância).

##

#Nao Paramétricos:

#--> testes não paramétricos, por outro lado, não assumem nenhuma distribuição de probabilidade 
    específica para os dados e não dependem de parâmetros fixos. 

#--> Esses testes são geralmente usados ​​quando os dados são categóricos ou não seguem uma distribuição normal. 


#---------------

#Para realizacao do exercicio 2. b)
#
#install.packages("broom")
#Determinar se os dados seguem uma distribuição Normal ou não.

#Teste de Normalidade (teste de Shapiro-Wilk)

# Importar o conjunto de dados
dados <- DADOS2

# Realizar o teste de Shapiro-Wilk para cada coluna
# Pode não ser necessário e usamos logo um teste paramétrico
shapiro.test(dados$SVM)
shapiro.test(dados$DT)
shapiro.test(dados$KN)
shapiro.test(dados$RF)
shapiro.test(dados$ML)
shapiro.test(dados$GB)


#Com base nos resultados do teste de Shapiro-Wilk realizado para cada coluna do conjunto de dados, 
#podemos afirmar que nem todas as colunas apresentam distribuição normal. Especificamente, 
#as colunas DT e ML apresentaram valores de p abaixo de 0,05, indicando que podemos rejeitar
#a hipótese nula de que os dados seguem uma distribuição normal. 
#Por outro lado, as demais colunas apresentaram valores de p acima de 0,05, 
#o que significa que não podemos rejeitar a hipótese nula de que os dados seguem uma distribuição normal.

#Com base nos resultados do teste de normalidade, podemos utilizar testes paramétricos para as colunas 
#SVM, KN, RF e GB, uma vez que não há evidências suficientes para rejeitar a hipótese nula de normalidade 
#para essas colunas. Para as colunas DT e ML, que apresentaram evidências de não normalidade, seria mais apropriado 
#utilizar testes não paramétricos ou buscar alguma transformação nos dados para torná-los mais próximos de uma distribuição 
#normal antes de aplicar testes paramétricos

#No caso dos dados deste problema, não temos informações sobre a distribuição dos dados, mas como o tamanho das amostras é 
#relativamente grande (n=10), podemos considerar utilizar a ANOVA. 

#realizar a ANOVA para testar se há diferenças significativas entre os algoritmos:

modelo <- aov(SVM + DT + KN + RF + ML + GB ~ dsets, data = dados)


#Nesse modelo, estamos a testar se há diferenças significativas entre as médias dos modelos SVM, DT, KN, RF, ML e GB 
#em relação aos diferentes conjuntos de dados (D1-D10). A variável data especifica que os dados estão no conjunto de dados dados.


#Para visualizar os resultados:
summary(modelo)

#para ser mais legível
library(broom)
tidy(modelo)


#O resultado da função tidy() mostra que o teste ANOVA encontrou uma diferença significativa entre as médias dos modelos 
#SVM, DT, KN, RF, ML e GB em relação aos diferentes conjuntos de dados (D1-D10), pois o valor de p associado à variável dsets é menor que 0,05.
#A coluna df indica o grau de liberdade associado a cada fator, a coluna sumsq indica a soma de quadrados e a coluna meansq indica a média dos 
#quadrados para cada fator.

#Em resumo, podemos concluir que o conjunto de dados utilizado tem um impacto significativo nos resultados dos modelos SVM, DT, KN, RF, ML e GB.


#O resultado da função summary() também mostra que houve uma diferença significativa entre as médias dos modelos SVM, DT, KN, RF, ML e GB em relação 
#aos diferentes conjuntos de dados (D1-D10), como indicado pelo valor de p associado à variável dsets ser menor que 0,05.
#Além disso, a tabela indica o número de graus de liberdade (Df), a soma de quadrados (Sum Sq) e a média de quadrados (Mean Sq) para cada fator.
#Em suma, tanto a função tidy() quanto a função summary() confirmam que o conjunto de dados tem um efeito significativo nos resultados dos modelos 
#SVM, DT, KN, RF, ML e GB.
