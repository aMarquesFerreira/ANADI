#alinea c)

#Na alínea anterior, realizamos um teste de hipótese para verificar se existem diferenças significativas 
#entre a precisão dos diferentes algoritmos de machine learning.
#Primeiro, verificamos a normalidade dos dados usando o teste de Shapiro-Wilk para cada coluna. 
#Todos os dados, exceto o conjunto de dados "ML", seguiram uma distribuição normal, com p-values maiores que 0,05. 
#O conjunto de dados "ML" teve um p-value menor que 0,05, indicando que não seguiu uma distribuição normal.
#Portanto, como um dos conjuntos de dados não seguiu uma distribuição normal, achamos mais apropriado usar um teste não paramétrico, 
#como o teste de Friedman. O resultado do teste de Friedman mostrou um valor de chi-squared de 8.7097, 
#com 5 graus de liberdade e um p-value de 0.1212.
#Como o p-value é maior que o nível de significância (0.05), 
#não há evidências estatisticamente significativas para rejeitar a hipótese nula, 
#o que significa que não há diferenças significativas entre a precisão dos diferentes algoritmos.
#No entanto, se houvesse diferenças significativas, poderíamos realizar um estudo post-hoc para 
#identificar quais os algoritmos que têm diferenças significativas entre si. 