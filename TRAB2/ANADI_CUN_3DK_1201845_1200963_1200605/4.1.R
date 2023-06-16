
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

#Exercicio 3


###

#Exercicio 4





