#Ex2

#Alínea a)

# Carregar os dados do arquivo CSV
dados <- DADOS2

# Carregar o pacote Hmisc
library(Hmisc)

# Extrair as colunas de precisão de cada algoritmo
precisao_svm <- dados$SVM
precisao_dt <- dados$DT
precisao_kn <- dados$KN
precisao_rf <- dados$RF
precisao_ml <- dados$ML
precisao_gb <- dados$GB

# Criar a matriz de correlações usando a função rcorr
matriz_correlacoes <- rcorr(cbind(precisao_svm, precisao_dt, precisao_kn, precisao_rf, precisao_ml, precisao_gb))

# Exibir a matriz de correlações
print(matriz_correlacoes$r)



#Alínea b)