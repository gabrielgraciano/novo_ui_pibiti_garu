library(plyr)
data <- read.csv("data/food-choices/clean_data.csv")[,-1]
altura <- read.csv("data/altura.csv")[-1]
idade <- read.csv("data/idade.csv")[-1]
labs <- read.csv("data/lab_results.csv")[-1]
colnames(data) <- gsub(".", ' ',colnames(data), fixed = TRUE)


data$Cozinha <- factor(data$Cozinha, levels = c("Nunca", "Durante as férias", "Às vezes", "Quase todo dia", "Todo dia"))
data$`Come fora` <- factor(data$`Come fora`, levels = c("Nunca", "1-2 vezes", "2-3 vezes", "3-5 vezes", "Todo dia"))
data$`Pratica exercícios` <- factor(data$`Pratica exercícios`, levels = c("Nunca", "Uma vez por semana", "2-3 vezes por semana", "Todo dia"))
data$`Vegetais nas refeições` <- factor(data$`Vegetais nas refeições`, levels = c("Difícil", "Um pouco", "Regular", "Frequente", "Bastante"))
data$Trabalha <- factor(data$Trabalha, levels = c("Não trabalha", "Meio Período", "Tempo Integral"))
data$Relacionamento <- factor(data$Relacionamento, levels = c("Solteirx", "Em um relacionamento", "Casadx"))
data$Relacionamento <- revalue(data$Relacionamento, c("Solteirx" = "Solteiro",
                                                      "Casadx" = "Casado"))
data$Altura <- as.numeric(unlist(altura))

data$Idade <- idade$Idade
data <- cbind(data, labs)
colnames(data)[c(16:21)] <- c("HDL", "LDL", "Triglicérides", "Álcool: Consumo mensal", 
                              "Álcool: Dose média",
                              "Consumo de tabaco")
data$`Consumo de tabaco` <- factor(data$`Consumo de tabaco`, levels = c(
  "Não fuma",
  "Até 1 cigarro",
  "Até 5 cigarros",
  "Até 1 maço",
  "2 maços ou mais"
))

data$`Álcool: Consumo mensal` <- factor(data$`Álcool: Consumo mensal`, 
                                        levels = c(
                                          "Não bebe",
                                          "Raramente",
                                          "Ocasional",
                                          "Frequente",
                                          "Muito frequente"
                                        ))

data$`Álcool: Dose média` <- factor(data$`Álcool: Dose média`,
                                    levels = c(
                                      "Uma dose",
                                      "Até 2 doses",
                                      "3 a 4 doses",
                                      "5 a 11 doses",
                                      "12 ou mais doses"
                                    ))

example_dataframe <- as.data.frame(rbind(
  c("Sexo", "'Masculino', 'Feminino'", "Qualitativa Nominal"),
  c("Ano letivo", "1, 2, 3, 4...", "Quantitativa Discreta"),
  c("Peso (kg)", "61,2; 85; 119,3; ...", "Quantitativa Contínua"),
  c("Altura (m)", "1,75; 1,67; 1,68; ...", "Quantitativa Contínua"),
  c("Idade (anos)", "17, 21, 20,...", "Quantitativa Contínua"),
  c("Trabalho", "'Tempo Integral', 'Meio Período', 'Não trabalha'", "Qualitativa Nominal"),
  c("Relacionamento", "'Solteiro', 'Em um relacionamento', 'Morando junto', ...", "Qualitativa Nominal"),
  c("Cozinha", "'Sempre', 'Quase todo dia', 'Nunca', ...", "Qualitativa Ordinal"),
  c("Come Fora", "'Sempre', 'Quase todo dia', 'Nunca', ...", "Qualitativa Ordinal"),
  c("Culinária Favorita", "'Árabe', 'Oriental', 'Africana', ...", "Qualitativa Nominal"),
  c("Pratica exercícios", "'Sempre', 'Quase todo dia', 'Nunca', ...", "Qualitativa Ordinal"),
  c("Pratica esportes", "'Sim', 'Não'", "Qualitativa Nominal"),
  c("Toma Vitamina", "'Sim', 'Não'", "Qualitativa Nominal"),
  c("HDL (mg/dL)", "50,76; 56,43; 56,87;", "Quantitativa Contínua"),
  c("LDL (mg/dL)", "84,93; 100,65; 101,47;", "Quantitativa Contínua"),
  c("Triglicérides (mg/dL)", "61,32; 55,4; 91,62;", "Quantitativa Contínua"),
  c("Álcool: Consumo mensal", "'Não bebe', 'Raramente', 'Ocasional', 'Frequente', ...", "Qualitativa Ordinal"),
  c("Álcool: Dose média", "'Não bebe', 'Até 2 doses', '3 a 4 doses', ...", "Qualitativa Ordinal")
))

colnames(example_dataframe) <- c("Variável", "Possíveis Valores", "Tipo de Variável")

tab_frequencia_relacionamento <- as.data.frame(table(data$Relacionamento))
tab_frequencia_relacionamento$prop <- round(tab_frequencia_relacionamento$Freq/sum(tab_frequencia_relacionamento$Freq), digits = 4)
tab_frequencia_relacionamento$porc <- scales::percent(tab_frequencia_relacionamento$prop)
colnames(tab_frequencia_relacionamento) <- c("Relacionamento", "Frequência", "Proporção", "Porcentagem")

freq_peso <- cut(data$Peso, breaks = c(45, 60, 75, 90, 105, 120))
tab_frequencia_peso <- as.data.frame(with(data, table(freq_peso, useNA = 'ifany')))
tab_frequencia_peso$prop <- round(tab_frequencia_peso$Freq/sum(tab_frequencia_peso$Freq), digits = 4)
tab_frequencia_peso$porc<- scales::percent(tab_frequencia_peso$prop) 
colnames(tab_frequencia_peso) <- c("Faixa de peso", "Frequência", "Proporção", "Porcentagem")

tab_frequencia_ano_letivo <- as.data.frame(table(data$`Ano letivo`))
tab_frequencia_ano_letivo$prop <- round(tab_frequencia_ano_letivo$Freq/sum(tab_frequencia_ano_letivo$Freq), digits = 4)
tab_frequencia_ano_letivo$porc <- scales::percent(tab_frequencia_ano_letivo$prop)
colnames(tab_frequencia_ano_letivo) <- c("Ano Letivo", "Frequência", "Proporção", "Porcentagem")

#tabela culinária sexo (DEPRECATED)

# tb <- table(data$`Culinária favorita`, data$Sexo)
# tb <- as.data.frame(cbind(rownames(tb), as.numeric(tb[,1]), as.numeric(tb[,2])))
# colnames(tb) <- c("Culinária/Sexo", "Feminino", "Masculino")
# tb$Feminino <- as.numeric(as.character(tb$Feminino))
# tb$Masculino <- as.numeric(as.character(tb$Masculino))
# tb$`Culinária/Sexo` <- as.character(tb$`Culinária/Sexo`)
# tb <- rbind(tb, c("Total", sum(tb$Feminino), sum(tb$Masculino)))
# tb$Feminino <- as.numeric(as.character(tb$Feminino))
# tb$Masculino <- as.numeric(as.character(tb$Masculino))
# tb$Total <- tb$Feminino + tb$Masculino
# tb$`Culinária/Sexo` <- as.factor(tb$`Culinária/Sexo`)

#handling NA
#sapply(data, function(x) sum(is.na(x)))

data[is.na(data$Peso),]$Peso <- mean(data$Peso, na.rm = TRUE)
data[is.na(data$Trabalha),]$Trabalha <- "Não trabalha"
data[is.na(data$Relacionamento),]$Relacionamento <- "Solteiro"
data[is.na(data$Cozinha),]$Cozinha <- "Às vezes"
data$`Culinária favorita` <- as.character(data$`Culinária favorita`)
data$`Culinária favorita`[is.na(data$`Culinária favorita`)] <- "Nenhuma"
data$`Culinária favorita` <- as.factor(data$`Culinária favorita`)
data$`Pratica esportes`[is.na(data$`Pratica esportes`)] <- "Não"
data$Peso <- round(data$Peso)

head(data)
write.csv(data, "handled_data.csv")

native_factors <- sapply(data, is.factor)
native_numbers <- sapply(data, is.numeric) | sapply(data, is.integer)
native_shortLevels <- sapply(data, nlevels) < 6 & sapply(data, is.factor)
native_continuous <- sapply(data, is.numeric)
print(native_shortLevels)
