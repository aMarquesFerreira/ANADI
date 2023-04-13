#Alinea a)

#Nova variável com dados carregados de "DADOS1.CSV"
dados <- DADOS1

#FORMATAÇÃO DA TABELA

#Eliminar linha 1 e 2 da tabela (linhas desncessárias)
dados <- subset(dados, !(row.names(dados) %in% c("1", "2")))

#Formatar nome das colunas
colnames(dados) <- c("Time","DischargePressureESP01", "IntakePressureESP01.1", "IntakeTemperatureESP01.2", "MotorTemperatureESP01.3", "VSDFREQOUTESP01.4", "VSDMOTAMPSESP01.5", "DischargePressureESP02", "IntakePressureESP02.1","IntakeTemperatureESP02.2","MotorTemperatureESP02.3", "VSDFREQOUTESP02.4", "VSDMOTAMPSESP02.5", "DischargePressureESP03", "IntakePressureESP03.1", "IntakeTemperatureESP03.2", "MotorTemperatureESP03.3", "VSDFREQOUTESP03.4", "VSDMOTAMPSESP03.5","ChokePositionICO1","Pressure1ICO1.1","Pressure2ICO1.2", "Temperature1ICO1.3","Temperature2ICO1.4","WaterCutICO1.5","LiquidRateICO1.6","WaterRateICO1.7","OilRateICO1.8","ChokePositionICO2","Pressure1ICO2.1","Pressure2ICO2.2", "Temperature1ICO2.3","Temperature2ICO2.4","WaterCutICO2.5","LiquidRateICO2.6","WaterRateICO2.7","OilRateICO2.8")

#Criar nova coluna com a data. formatada como pedido no enunciado
dados$TimeFormatted <- as.POSIXct(dados$Time, origin = "1970-01-01", tz = "GMT", format = "%s")

#Exportar a nova tabela formatada para um ficheiro csv
write.csv(dados, file = "dadosTime.csv", row.names = FALSE)

#Alinea b)

#Dados filtrados para o dia 4 de Agosto de 2013
dados_subset <- subset(dados, format(TimeFormatted, "%Y-%m-%d") == "2013-08-04")

#Conversão das temperaturas em numérico
dados_subset$MotorTemperatureESP01.3 <- as.numeric(dados_subset$MotorTemperatureESP01.3)
dados_subset$MotorTemperatureESP02.3 <- as.numeric(dados_subset$MotorTemperatureESP02.3)
dados_subset$MotorTemperatureESP03.3 <- as.numeric(dados_subset$MotorTemperatureESP03.3)

library(ggplot2)

# Criar o gráfico com as temperaturas do motor das bombas 1, 2 e 3 no dia 4 de Agosto de 2013
grafico <- ggplot(dados_subset, aes(x = TimeFormatted)) +
  geom_line(aes(y = MotorTemperatureESP01.3, color = "Bomba 1", group=1), linetype = "solid") +
  geom_line(aes(y = MotorTemperatureESP02.3, color = "Bomba 2", group=2), linetype = "solid") +
  geom_line(aes(y = MotorTemperatureESP03.3, color = "Bomba 3", group=3), linetype = "solid") +
  labs(x = "Tempo", y = "Temperatura da bomba", title = "Comparação da Temperatura do motor das bombas 1, 2 e 3 no dia 4 de Agosto de 2013") +
  scale_color_manual(values = c("Bomba 1" = "red", "Bomba 2" = "green", "Bomba 3" = "blue")) +
  theme_minimal() 

grafico

#Alinea c
#Criar um boxplot com os dados da alínea anterior

#Criar uma data frame nova para os dados do boxplot
dados_boxplot <- data.frame(Bombas = rep(c("Bomba 1", "Bomba 2", "Bomba 3"), each = nrow(dados_subset)),
                           Temperaturas = c(dados_subset$MotorTemperatureESP01.3,
                                           dados_subset$MotorTemperatureESP02.3,
                                           dados_subset$MotorTemperatureESP03.3))

#Criar o boxplot com as 3 bombas no eixo do x e o valor das temperaturas do motor no eixo dos y
grafico_boxplot <- ggplot(data = dados_boxplot) +
  geom_boxplot(aes(x = Bombas, y = Temperaturas, color = Bombas), 
               width = 0.5, linetype = "solid", size = 0.3, outlier.shape = NA, 
               position = position_dodge(width = 0.1)) +
  guides(color = guide_legend(title = "Bomba")) + labs(title = "Boxplot das Temperaturas do motor das bombas 1, 2 e 3 no dia 4 de Agosto de 2013") 

#Visualizar Boxplot
grafico_boxplot

#Alínea d) ii.

#Filtrar os dados compreendidos entre Junho de 2013 e Maio de 2014
barris <- dados[dados$TimeFormatted >= as.Date("2013-06-01") & dados$TimeFormatted <= as.Date("2014-05-31"), ]

#Agrega as médias de produção de barris de óleo em função do mês
dados_mensais_bomba1 <- aggregate(as.numeric(barris$OilRateICO1.8) ~ format(as.Date(barris$TimeFormatted), "%Y-%m"), data = barris, FUN = mean)

#Edição do nome das colunas
colnames(dados_mensais_bomba1) <- c("Mês", "Média de barris produzidos")

#Cor das colunas
cores <- rep("#234F1E", length(dados_mensais_bomba1[,1]))

#Cálculo da coluna com a maior média
mes_max <- which(dados_mensais_bomba1$Mês == dados_mensais_bomba1$Mês[which.max(dados_mensais_bomba1$`Média de barris produzidos`)])

#Cor da coluna com maior média
cores[mes_max] <- "#B2D3C2"
  
#Criação do barplot
barplot(dados_mensais_bomba1$`Média de barris produzidos`, col = cores, names.arg = dados_mensais_bomba1$Mês, xlab = "Meses", ylab = "Média da produção de barris de óleo", main = "Produção de barris de óleo entre Junho de 2013 e Maio de 2014 da Bomba 1")

