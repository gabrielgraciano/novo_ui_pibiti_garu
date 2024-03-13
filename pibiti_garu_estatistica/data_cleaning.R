 data <- read.csv("data/food-choices/food_coded.csv")
head(data)
ncol(data)
colnames(data)
data$comfort_food_reasons_coded.1 <- NULL
data <- data[,c(2, 31, 60, 20, 41, 11, 19, 33, 26, 57, 22, 52, 58)]
dim(data)
head(data$Gender)
typeof(data$Gender)

data$Gender <- factor(data$Gender, levels = 1:2, labels = c("Feminino", "Masculino"))
data$weight <- as.numeric(as.character(data$weight))
data$weight <- round(data$weight/2.205)
head(data$weight, n=30)

head(data$employment)
data$employment <- factor(data$employment, levels = 1:4, labels = c("Período Integral", "Meio Período", 
                                                                    "Não trabalha", "Outro"))
data$marital_status <- factor(data$marital_status,
                              levels = 1:6, labels = c("Solteirx", "Em um relacionamento",
                                                       "Morando junto", "Casadx", 
                                                       "Divorciadx", "Víuvx"))
head(data)

data$cook <- factor(data$cook, levels = 1:5, labels = c("Todo dia", "Quase todo dia", "Às vezes",
                                                        "Durante as férias", "Nunca"))

data$eating_out <- factor(data$eating_out, levels = 1:5, labels = c("Nunca", 
                                                                    "1-2 vezes",
                                                                    "2-3 vezes",
                                                                    "3-5 vezes",
                                                                    "Todo dia"))

data$fav_cuisine_coded <- factor(data$fav_cuisine_coded, levels = 1:8,
                                 labels = c("Italiana/Francesa", 
                                            "Espanhola/Mexicana",
                                            "Árabe",
                                            "Asiática",
                                            "Americana",
                                            "Africana",
                                            "Jamaicana",
                                            "Indiana"))

data$veggies_day <- factor(data$veggies_day, levels = 1:5,
                           labels = c("Difícil", "Um pouco", "Regular", "Frequente", "Bastante"))

data$exercise <- factor(data$exercise, levels = 1:5, labels = c("Todo dia", 
                                                                "2-3 vezes por semana", 
                                                                "Uma vez por semana", 
                                                                "De vez em quando",
                                                                "Nunca"))

data$sports <- factor(data$sports, levels=1:2, labels = c("Sim", "Não"))
data$vitamins <- factor(data$vitamins, levels = 1:2, labels = c("Sim", "Não"))

View(data)
colnames(data) <- c("Sexo", "Ano letivo", "Peso", "Trabalha", "Relacionamento",
                    "Cozinha", "Come fora", "Percepção de Saúde", "Culinária favorita",
                    "Vegetais nas refeições", "Pratica exercícios", "Pratica esportes", 
                    "Toma vitaminas")

data[is.na(data$`Pratica exercícios`),]$`Pratica exercícios` <- "Nunca"
write.csv(data, file = "data/food-choices/clean_data.csv")
