library(ggplot2)
library(ggokabeito)
library(ggthemes)

#Estou usando essa paleta de cores pois permite maior acessibilidade

cores_okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", '#000000')


#Questão 2 abaixo
#Adaptei do food-choices


#Histograma


q2_hist_v1 <- q2%>%
  ggplot(aes(x = Peso, fill = after_stat(count))) +  
  geom_histogram(binwidth = 10)+
  ylab('')+
  xlab('')+
  scale_color_colorblind()+
  guides(fill = FALSE)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())
q2_hist_v1

contagem <- table(q2$`Pratica esportes`)

#Pie chart

q2_pie_v1 <- ggplot(data.frame(respostas = names(contagem), contagem), aes(x = "", y = Freq, fill = respostas)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = cores_okabe_ito)+
  labs(fill = " ", y = NULL)+
  guides(fill = FALSE) 

q2_pie_v1

q2 <- mutate(veg = `Vegetais nas refeições`)

#Barplot

q3_bar <- q2%>%
  ggplot(aes(x = `Vegetais nas refeições`, fill = `Vegetais nas refeições`))+
  scale_fill_manual(values = cores_okabe_ito)+
  geom_bar()+
  scale_color_colorblind()+
  guides(fill = FALSE)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())+
  ylab('')+
  xlab('')

q3_bar

#Salvando tudo, estou colocando nessa pasta para depois mover para a wwww

ggsave('imagens_app/q2_histv1.png', plot = q2_hist, width = 4, height = 2.5, units = 'in') 
ggsave('imagens_app/q2_pie_v1.png', plot = q2_pie_v1, width = 4, height = 2.5, units = 'in') 
ggsave('imagens_app/q3_bar.png', plot = q3_bar, width = 4, height = 2.5, units = 'in') 




#q23

a_23 <- data.frame(
  peso = c(3.5, 3.7, 3.1, 4.2, 2.8, 3.5, 3.2),
  comprimento = c(51, 49, 48, 53, 48, 50, 49)
  )

a_23

q_23_a <- a_23%>%
  ggplot(aes(x = peso, y = comprimento))+
  geom_point(color = '#0072b2')+
  theme_minimal()+
  theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())+
  ylab('Comprimento')+
  xlab('Peso')


b_23 <- data.frame(
  peso = c(4.0, 3.5, 3.2, 4.8, 3, 3.5, 3.2),
  comprimento = c(51, 49, 50, 52, 48, 54, 49)
)
q_23_b <- b_23%>%
  ggplot(aes(x = peso, y = comprimento))+
  geom_point(color = '#E69F00')+
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank())+
  ylab('Comprimento')+
  xlab('Peso')

c_23 <- data.frame(
  peso = c(51, 49, 48, 53, 48, 50, 49),
  comprimento = c(3.5, 3.7, 3.1, 4.2, 2.8, 3.5, 3.2)
)

c_23

q_23_c <- c_23%>%
  ggplot(aes(x = peso, y = comprimento))+
  geom_point(color = '#CC79A7')+
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank())+
  ylab('Comprimento')+
  xlab('Peso')

d_23 <- data.frame(
  peso = c(3.2, 3.5, 2.8, 4.2, 3.1, 3.7, 3.5),
  comprimento = c(51, 49, 48, 53, 48, 50, 49)
)

q_23_d <- d_23%>%
  ggplot(aes(x = peso, y = comprimento))+
  geom_point(color = '#D55E00')+
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank())+
  ylab('Comprimento')+
  xlab('Peso')
  

ggsave('imagens_app/q_23_a.png', plot = q_23_a, width = 4, height = 2.5, units = 'in')
ggsave('imagens_app/q_23_b.png', plot = q_23_b, width = 4, height = 2.5, units = 'in')
ggsave('imagens_app/q_23_c.png', plot = q_23_c, width = 4, height = 2.5, units = 'in')
ggsave('imagens_app/q_23_d.png', plot = q_23_d, width = 4, height = 2.5, units = 'in')


#questão 27

q_27 <- data.frame(
  Cardiopata = c('Cardiopata', 'Não Cardiopata'),
  Fumante = c(38, 6),
  Nfumante = c(63, 94)
)

q_27_long <- tidyr::pivot_longer(q_27, cols = c(Fumante, Nfumante), names_to = "Fumante", values_to = "Count")

q_27_long <- transform(q_27_long, Proporcao = Count / tapply(Count, Cardiopata, sum)[Cardiopata])

q_27_long$Fumante <- factor(q_27_long$Fumante, levels = c("Nfumante", "Fumante"))


q27_barplot <- ggplot(q_27_long, aes(x = Cardiopata, y = Proporcao, fill = Fumante)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Formatar eixo y como percentual
  labs(title = "Hábito de fumar em relação à cardiopatia",
       x = "Condição Cardiopata",
       y = "Proporção (%)",
       fill = "Hábito de Fumar") +
  theme_minimal()+
  scale_fill_manual(values = cores_okabe_ito, labels = c('Não Fumante', 'Fumante'))
  

ggsave('imagens_app/q27_barplot.png', plot = q27_barplot, width = 4, height = 2.5, units = 'in')

  
