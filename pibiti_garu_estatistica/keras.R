setwd("/home/joao/Documents/BIG/v2/")
#install.packages("keras")
library(plyr)
library(keras)

model <- keras_model_sequential() %>%
  layer_dense(units=64, activation = "relu", input_shape = 2) %>%
  layer_dense(units=32, activation = "relu") %>%
  layer_dense(units=1, activation = "linear")

model %>% compile(
  loss = "mse",
  optimizer = "adam",
  metrics = list("mean_absolute_error")
)

model %>% summary()

data <- read.csv("data/food-choices/clean_data.csv")[,-1]
colnames(data) <- gsub(".", ' ',colnames(data), fixed = TRUE)

metrics <- read.csv("data/weight-height.csv")
metrics$Height <- 2.54*metrics$Height
metrics$Weight <- 0.453592*metrics$Weight

sex <- as.numeric(revalue(metrics$Gender, c("Female" = 1, "Male" = 0)))

sd(metrics$Height)

model %>% fit(as.matrix(data.frame(sex, metrics$Weight)), metrics$Height, epochs = 100, verbose = 0)
scores = model %>% evaluate(as.matrix(data.frame(sex, metrics$Weight)), metrics$Height, verbose = 0)
print(scores)
height_predict <- model %>% predict(as.matrix(data.frame(sex, metrics$Weight)))

ggplot(data=data.frame(metrics$Weight, metrics$Height, height_predict), aes(x=metrics.Weight)) + 
  geom_point(aes(y=metrics.Height), color="blue") + 
  geom_point(aes(y=height_predict), color="red")

x_peso <- data$Peso
x_sexo <- as.numeric(revalue(data$Sexo, c("Masculino" = 0, "Feminino" = 1)))
altura_predict <- model %>% predict(as.matrix(data.frame(x_sexo, x_peso)))
head(altura_predict)
data$altura <- round((altura_predict + runif(125, 0, sd(metrics$Weight)))/100, 2)
data$altura

ggplot(data, aes(x=Peso, y=altura)) + 
  geom_point(aes(color = Sexo))

ggplot(data, aes(x=altura)) + 
  geom_histogram(breaks = seq(min(data$altura), max(data$altura), by=0.05), aes(y=..density..))

altura <- as.data.frame(data$altura, col.names = c("Altura"))
head(altura)
colnames(altura) <- c("Altura")
write.csv(altura, file = "data/altura.csv")

#idade
idade <- read.csv("data/idade_sem.csv")[-1]
head(idade)

ano1 <- data$`Ano letivo` == 1
ano2 <- data$`Ano letivo` == 2
ano3 <- data$`Ano letivo` == 3
ano4 <- data$`Ano letivo` == 4
data$Idade[ano1] <- pmax(17, round(rnorm(table(ano1)["TRUE"],
                                         mean(idade$idade[idade$ano==1]),
                                         sd(idade$idade[idade$ano==1]))))
data$Idade[ano2] <- pmax(18, round(rnorm(table(ano2)["TRUE"],
                                         mean(idade$idade[idade$ano==2]),
                                         sd(idade$idade[idade$ano==2]))))
data$Idade[ano3] <- pmax(19, round(rnorm(table(ano3)["TRUE"],
                                         mean(idade$idade[idade$ano==3]),
                                         sd(idade$idade[idade$ano==3]))))
data$Idade[ano4] <- pmax(20, round(rnorm(table(ano4)["TRUE"],
                                         mean(idade$idade[idade$ano==4]),
                                         sd(idade$idade[idade$ano==4]))))

ggplot(data=data, aes(x=as.factor(data$`Ano letivo`), y=Idade)) + geom_boxplot()


idade_nova <- as.data.frame(data$Idade, col.names = c("Idade"))
head(idade_nova)
colnames(idade_nova) <- c("Idade")
write.csv(idade_nova, file = "data/idade.csv")

#LAB data
lab <- read.csv("data/lab_data.csv")[-1]
head(lab)
#HDL
levels(data$`Pratica exercícios`)
lab$exercise_tax <- cut(lab$exercise_week, breaks=c(-Inf, 1, 2, 4, Inf))
table(lab$exercise_tax)
lab$exercise_tax <- revalue(lab$exercise_tax, c("(-Inf,1]" = "1", "(1,2]" = "2", 
                                                "(2,4]" = "3", "(4, Inf]" = "4"))

is.numeric(lab$gender)
is.numeric(lab$age)
is.numeric(lab$exercise_tax)
lab$exercise_tax <- as.numeric(as.character(lab$exercise_tax))
is.numeric(lab$weight)
is.numeric(lab$height)

model2 <- keras_model_sequential() %>%
  layer_dense(units=64, activation = "relu", input_shape = 5) %>%
  layer_dense(units=32, activation = "relu") %>%
  layer_dense(units=1, activation = "linear")

model2 %>% compile(
  loss = "mse",
  optimizer = "adam",
  metrics = list("mean_absolute_error")
)

model2 %>% summary()

model2 %>% fit(as.matrix(data.frame(lab$gender, lab$age, lab$exercise_tax, lab$weight, lab$height)), lab$hdl, epochs = 100, verbose = 1)
scores = model2 %>% evaluate(as.matrix(data.frame(lab$gender, lab$age, lab$exercise_tax, lab$weight, lab$height)), lab$hdl, verbose = 0)
print(scores)
x_sexo <- as.numeric(as.character(revalue(data$Sexo, c("Masculino" = 1, "Feminino" = 2))))
x_idade <- as.numeric(as.character(idade_nova$Idade))
x_exercicio <- as.numeric(as.character(revalue(data$`Pratica exercícios`, 
                                               c(
                                                 "Nunca" = "1",
                                                 "Uma vez por semana" = "2",
                                                 "2-3 vezes por semana" = "3",
                                                 "Todo dia" = "4"
                                               ))))
x_peso <- data$Peso
x_altura <- data$Altura*100
head(cbind(x_sexo, x_idade, x_exercicio, x_peso, x_altura))
head(lab)

hdl_prev <- model2 %>% predict(as.matrix(data.frame(x_sexo, x_idade, x_exercicio, x_peso, x_altura)))
head(hdl_prev)

ggplot(data=NULL, aes(x=cut(x_idade, 5), y=hdl_prev)) + geom_boxplot()

model2 %>% fit(as.matrix(data.frame(lab$gender, lab$age, lab$exercise_tax, lab$weight, lab$height)), lab$ldl, epochs = 100, verbose = 1)
ldl_prev <- model2 %>% predict(as.matrix(data.frame(x_sexo, x_idade, x_exercicio, x_peso, x_altura)))

model2 %>% fit(as.matrix(data.frame(lab$gender, lab$age, lab$exercise_tax, lab$weight, lab$height)), lab$triglycerides, epochs = 100, verbose = 1)
trig_prev <- model2 %>% predict(as.matrix(data.frame(x_sexo, x_idade, x_exercicio, x_peso, x_altura)))

#visualization
lab1 <- lab[lab$age <= 21,]
lab2 <- lab[lab$age > 21 & lab$age <= 24,]
lab3 <- lab[lab$age > 24 & lab$age <= 30,]
lab4 <- lab[lab$age > 30 & lab$age <= 40,]
lab5 <- lab[lab$age > 40,]

mean(lab1$alc_month)
mean(lab4$alc_month)

#categorization

table(cut(lab$alc_month, 5))
table(cut(lab$alc_month, breaks = c(-Inf, 1, 4, 8, 15, Inf)))

table(cut(lab$alc_dose, 5))
table(cut(lab$alc_dose, breaks = c(-Inf, 1, 2, 4, 12, Inf)))

lab$alc_month_cat <- cut(lab$alc_month, breaks = c(-Inf, 1, 4, 8, 15, Inf))
lab$alc_dose_cat <- cut(lab$alc_dose, breaks=c(-Inf, 1, 2, 4, 12, Inf))

summary(lab$alc_month_cat)
summary(lab$alc_dose_cat)
lab$alc_month_cat <- revalue(lab$alc_month_cat, c(
  "(-Inf,1]" = 1,
  "(1,4]" = 2,
  "(4,8]" = 3,
  "(8,15]" = 4,
  "(15, Inf]" = 5
))
lab$alc_dose_cat <- revalue(lab$alc_dose_cat, c(
  "(-Inf,1]" = 1,
  "(1,2]" = 2,
  "(2,4]" = 3,
  "(4,12]" = 4,
  "(12, Inf]" = 5
))

#use knn to categorize
summary(lab)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

train <- data.frame(normalize(lab$gender), 
                    normalize(lab$age),
                    normalize(lab$weight),
                    normalize(lab$height),
                    normalize(lab$exercise_tax),
                    lab$alc_month_cat,
                    lab$alc_dose_cat
)

preddata <- data.frame(normalize(x_sexo),
                   normalize(x_idade),
                   normalize(x_peso),
                   normalize(x_altura),
                   normalize(x_exercicio))
library(class)
alc_month_prev <- knn(train[,c(-6, -7)], preddata, train[,6], k=3)
alc_dose_prev <- knn(train[,c(-6, -7)], preddata, train[,7], k=3)
summary(alc_month_prev)
summary(alc_dose_prev)

table(cut(lab$smoke_day, 5))
table(cut(lab$smoke_day, breaks = c(-Inf, 1, 4, 8, 16, Inf)))

lab$smoke_cat <- cut(lab$smoke_day, breaks = c(-Inf, 1, 4, 8, 16, Inf))
summary(lab$smoke_cat)
lab$smoke_cat <- revalue(lab$smoke_cat, c(
  "(-Inf,1]" = 1,
  "(1,4]" = 2,
  "(4,8]" = 3,
  "(8,16]" = 4,
  "(16, Inf]" = 5
))

train <- cbind(train, lab$smoke_cat)
smoke_prev <- knn(train[,c(-6, -7, -8)], preddata, train[,8], k=3)
summary(smoke_prev)

addition <- data.frame(hdl_prev, ldl_prev, trig_prev, revalue(
  alc_month_prev, 
  c(
    "1" = "Não bebe",
    "2" = "Raramente",
    "3" = "Ocasional",
    "4" = "Frequente",
    "5" = "Muito frequente"
  )
), revalue(
  alc_dose_prev,
  c(
    "1" = "Uma dose",
    '2' = "Até 2 doses", 
    "3" = "3 a 4 doses",
    "4" = "5 a 11 doses", 
    "5" = "12 ou mais doses"
  )
), revalue(
  smoke_prev,
  c(
    "1" = "Não fuma",
    '2' = "Até 1 cigarro",
    "3" = "Até 5 cigarros",
    "4" = "Até 1 maço",
    '5' = "2 maços ou mais"
  )
  ))
summary(addition)

colnames(addition) <- c("hdl", "ldl", "trig", "alc_month", "alc_dose", "smoke_day")
write.csv(addition, "data/lab_results.csv")
