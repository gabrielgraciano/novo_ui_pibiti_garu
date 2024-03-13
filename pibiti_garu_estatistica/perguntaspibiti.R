library(ggplot2)


library(dplyr)
library(tidyverse)
library(shiny)
library(htmltools)
library(imager)


perguntas_respostas <- list(
  
  list(
    pergunta = 'A faixa etária é uma variável ___________; um tipo de gráfico apropriado para representar sua distribuição é o ________. A pressão arterial é uma variável __________; um tipo de gráfico apropriado para representar sua distribuição é o _________________. 
Assinale a alternativa que preenche corretamente os espaços em branco acima.',
    respostas = c('Qualitativa ordinal; de barras; quantitativa discreta; de boxplot.',
                  'Quantitativa discreta; de barras; quantitativa contínua; histograma.',
                  'Qualitativa ordinal; de pizza; quantitativa contínua; boxplot.',
                  'Qualitativa ordinal; de barras; quantitativa contínua; histograma.'),
    resposta_correta = 4
  ),
  
  list(
    pergunta = 'Mantenho essa pergunta aqui',
    respostas = c('O valor que ocorre com mais frequência dentro de um conjunto de dados.',
                  'É a soma de todos os valores de um conjunto de dados dividida pela quantidade de valores n que se está somando.',
                  'É a variabilidade dos dados.',
                  'É o valor que está no centro de um conjunto de dados. (50% abaixo e 50% acima)'),
    resposta_correta = 4
  ),
  
  
  list(
    pergunta =  HTML('Em um estudo sobre mortalidade de crianças com menos de 5 anos de idade em grandes cidades do Brasil
    coletaram-se dados sobre várias características demográficas, incluindo-se o relativo a renda mensal
    per capita (em reais). Os dados encontrados foram os seguintes:
    <center><img src="images/questionario/q3.png"></center>
    \n
    Para verificar uma possível relação entre mortalidade de crianças com menos de 5 anos de idade
    e renda familiar per capita deve-se utilizar o gráfico de:'),
    respostas = c('Barras',
                  'Boxplot',
                  'Histograma',
                  'Dispersão'),
    resposta_correta = 4
  ),
  
  list(
    pergunta =  HTML('Qual(is) medida(s) de variação é/são sensível(is) a valores discrepantes?'),
    respostas = c('Margem de erro',
                  'Amplitude interquartílica',
                  'Desvio padrão',
                  'Primeira e terceira alternativas'),
    resposta_correta = 4
  ),
  
  list(
    pergunta = 'Considerando as assertivas abaixo, e assinale a alternativa correta:
<br>
<br>
I – É durante a análise estatística descritiva que os dados de um estudo são organizados e resumidos.
<br>
II – Quando se está diante de uma variável de natureza qualitativa, é possível calcular média, mediana e desvio-padrão.
<br>
III - A média não é afetada por valores extremos do conjunto de dados.
<br>
IV - O desvio-padrão é uma medida de variabilidade dos dados individuais em torno da média.
',
    respostas = c('Apenas I e II estão corretas',
                  'Apenas II e III estão corretas',
                  'Apenas III e IV estão corretas',
                  'Apenas I e IV estão corretas'),
    resposta_correta = 4
  ),
  
  list(
    pergunta =  HTML('Abaixo, são dadas medidas resumo do salário mensal e da carga horária semanal
                     (em horas) de 100 funcionários de um determinado hospital:
                      <center><img src="images/questionario/q5_tabela.png"></center>
    <br>
    Com base nessas informações, assinale a alternativa CORRETA.'),
    respostas = c('O salário possui menor variabilidade que a carga horária semanal, pois o coeficiente de variação é menor.',
                  'O salário possui menor variabilidade que a carga horária semanal, pois tem maior desvio padrão.',
                  'A carga horária semanal possui menor variabilidade que o salário, pois o desvio padrão é menor.',
                  'A carga horária semanal possui menor variabilidade que o salário, pois o coeficiente de variação é menor.'),
    resposta_correta = 1
  ),
  
  list(
    pergunta =  HTML('Considere as informações contidas no boxplot abaixo, referente ao número de casos de COVID-19 acumulado até o 24/02/2020 nos estados brasileiros, por região.
                      <center><img src="images/questionario/q6_boxplot.png"></center>
    <br>
    Assinale a alternativa CORRETA:'),
    respostas = c('O desvio interquartílico do número de casos de covid-19 do Nordeste é maior do que o desvio interquartílico da região Norte.',
                  'O número de casos mediano da região Nordeste é superior ao da região Sudeste.',
                  'A distribuição do número de casos de covid-19 do Sudeste é simétrica.',
                  'O número médio de casos de covid-19 no Nordeste é superior ao da região Norte.'),
    resposta_correta = 1
  ),
  
  list(
    pergunta =  HTML('Observe abaixo a distribuição do peso de 50 pacientes antes e após a realização de um tratamento.
                      <center><img src="images/questionario/q7_boxplot.png"></center>
    <br>
    
Com base nas informações da figura, analise as afirmativas abaixo:
<br>
I. Antes do tratamento, o peso mediano dos pacientes era superior a 87 kg.
<br>
II. Antes do tratamento, 25% dos pacientes pesavam 93 kg ou mais.
<br>
III. Após o tratamento, 75% ou mais dos pacientes passaram a pesar menos de 84 kg.
<br>
IV. Após o tratamento, 25% ou menos dos pacientes passaram a pesar de 83 kg a 87 kg.
<br>
<br>
Está(ão) correta(s) apenas a(s) afirmativa(s):
    '),
    respostas = c('III',
                  'I e II',
                  'II e IV',
                  'I, II e III'),
    resposta_correta = 2
  ),
  #q8
  list(
    pergunta = HTML('Com relação as propriedades da distribuição Normal, considere as assertivas abaixo.
    <br>
    <br>
    I – É simétrica
    <br>
II – Média e Mediana são iguais
<br>
III – Os valores mais comuns estão próximos a média; os valores menos comuns estão longe dela.
<br>
IV – O desvio padrão marca a distância da média ao ponto de inflexão.
<br>
V – Aproximadamente 68% das observações estão entre mais ou menos 2 desvios padrão.
<br>
<br>
Está(ão) correta(s) apenas a(s) afirmativa(s):'),
    respostas = c('I, II e IV',
                  'I, II, III e V',
                  'I, II, III e IV',
                  'Todas as afirmativas estão corretas'),
    resposta_correta = 3
  ),
  
  #q9 - tentar arrumar o IC95 depois
  list(
    pergunta = HTML('Em um estudo sobre o perfil lipídico de estudantes de medicina de uma universidade federal (n=590),
  a média de colesterol total das mulheres foi de 184 mg/dL (IC95%: 175 – 193 mg/dL),
  enquanto a dos homens foi de 188 mg/dL (IC95%: 180 – 196 mg/dL).
  Acerca dos resultados, assinale a alternativa correta: 
  <br>
    <br>
      I – Não há indícios de diferença entre as médias de colesterol total quando
      comparamos homens e mulheres.
        <br>
      II – 95% dos homens têm valor de colesterol total entre 180 e 196 mg/dL.
        <br>
      III - Há 95% de confiança de que o verdadeiro valor da média de colesterol total
      das mulheres esteja entre 175 e 193 mg/dL
'),
    respostas = c(' I e II são corretas.',
                  'I, II e III são corretas.',
                  'II e III são corretas.',
                  ' I e III são corretas.'),
    resposta_correta = 4
  ),
  
  #q10
  list(
    pergunta = 'Qual das seguintes afirmativas diminuirá a margem de erro de um intervalo de confiança?',
    respostas = c('Aumentar o tamanho amostral de 200 para 1.000 elementos.',
                  'Diminuir o nível de confiança de 95% para 90%.',
                  'Aumentar o nível de confiança de 95% para 98%.',
                  'Primeira e segunda alternativas.'),
    resposta_correta = 4
  ),
  
  #q11
  list(
    pergunta = 'Ao afirmar que a estimativa, por intervalo com 95% de confiança,
    da prevalência de hipertensão, entre adultos, residentes em um determinado
    município, é de 25% a 35%, o pesquisador quer dizer que:
',
    respostas = c('Ele aceita a hipótese de que a prevalência seja baixa, com 95% de confiança.
',
                  'A prevalência de hipertensão foi estimada com confiança.
',
                  'Ele rejeita a hipótese de que a prevalência seja baixa com 95% de confiança.
',
                  'Em 95% das vezes que repetisse esse estudo, com amostras de mesmo tamanho, encontraria valores dessa prevalência entre 25% a 35%.
'),
    resposta_correta = 4
  ),

  
  #q12
  list(
    pergunta = ' Para exemplificar os conceitos relacionados a testes de hipóteses,
    o professor introduz o seguinte problema.  A fim de determinar se um novo medicamento
    é mais efetivo do que o convencional no tratamento de cefaleia, foi realizado um estudo
    com pacientes selecionados ao acaso e alocados em dois grupos. Um grupo foi submetido à nova droga
    (grupo tratado), enquanto o outro recebeu tratamento padrão (grupo controle). A variável resposta
    do estudo foi o tempo de alívio da cefaleia, em minutos. A pesquisa concluiu que a diferença
    do tempo de alívio da cefaleia encontrada entre os dois grupos foi estatisticamente
    significativa (p-valor<0,05). Com base nesse problema, o professor quer saber
    qual das seguintes afirmações está correta. 
',
    respostas = c('O teste de hipótese concluiu pela não rejeição da hipótese nula, ao nível de significância de 5%.
',
                  'O p-valor é uma medida da probabilidade de a hipótese nula ser verdadeira.
',
                  'O p-valor é a probabilidade de observarmos um valor da estatística de teste igual ou mais extremo daquele que observamos na amostra, na suposição da hipótese nula verdadeira.',
                  '"Estatisticamente significativo" quer dizer que há alta probabilidade de que o resultado obtido se deva ao acaso. 
'),
    resposta_correta = 3 
  ),
  
  #Q13
  list(
    pergunta = HTML('Numa amostra aleatória de 200 pacientes investigadas no ano de 2010,
    no serviço de ginecologia e obstetrícia da UFSCAR, metade eram casadas e dessas,
    70 faziam uso regular de algum método contraceptivo. Entre as mulheres não casadas
    50 faziam uso regular de algum método contraceptivo. A partir do valor da estatística
    Qui-Quadrado (χ2 = 8,33) e do nível descritivo do teste (p = 0,004), pode-se
    afirmar, ao nível de significância de 5%, que:'),
    respostas = c('Não há associação entre estado cível e uso de contraceptivos.',
                  'Há associação entre estado conjugal e uso de métodos contraceptivos, pois as mulheres casadas usam mais contraceptivos.',
                  'Há associação entre estado conjugal e uso de métodos contraceptivos, pois as mulheres não casadas usam mais contraceptivos.',
                  'Não se pode tomar uma decisão, pois as mulheres casadas costumam se esterilizar e não recorrem a contraceptivos.'),
    resposta_correta = 2 
  ),
  
  
  #q14
  list(
    pergunta = HTML('Considere as assertivas abaixo e assinale a alternativa correta:
    <br>
    <br>
    I – Em um teste de hipóteses, o erro do tipo II consiste em rejeitar a hipótese nula quando ela é verdadeira.
     <br>
II - Em um teste de hipóteses, β é a probabilidade de não rejeitar a hipótese nula quando ela é falsa.
 <br>
III – O nível de significância corresponde a probabilidade de rejeitar a hipótese nula quando ela é verdadeira.
 <br>
IV – Rejeita-se a hipótese nula quando p-valor é maior ou igual ao nível de significância.

'),
    respostas = c('I e II estão corretas',
                  'I e II estão corretas',
                  'II e IV estão corretas',
                  'I e IV estão corretas '),
    resposta_correta = 2
  ),
  
  list(
    pergunta = 'Qual teste estatístico poderia ser utilizado em um experimento que objetiva verificar a associação entre faixa etária e óbito (sim ou não) por COVID-19?
',
    respostas = c('Qui-quadrado
',
                  'Teste t-Student
',
                  'Correlação de Pearson
',
                  'ANOVA
'),
    resposta_correta = 1
  ),
  
  #q16
  list(
    pergunta = HTML('Julgue os itens abaixo como FALSO (F) ou VERDADEIRO (V) 
    <br>
    <br>
    ( )  A variável Número de erros de impressão dos livros de uma editora é do tipo quantitativa contínua.
    <br>
( ) Para verificar se a presença de depressão em estudantes do ensino médio está associada estatisticamente ao sexo, o teste estatístico que deve ser empregado é o teste de correlação de Pearson. 
<br>
( ) A área total sob a curva de uma distribuição normal é 95%.
<br>
( ) As alturas dos alunos de uma determinada escola são normalmente distribuídas com média de 1,50 m e variância de 0,09. Então, P(X < 1,50) < P(X > 1,60).

'),
    respostas = c('VVVV',
                  'VFVF',
                  'FVVF',
                  'FFFF'),
    resposta_correta = 4
  ),
  

  #q17
  list(
    pergunta = HTML('Considere o boxplot que mostra o resultado de um estudo sobre hábitos de sono, realizado com 261 alunos de medicina da EPM em 2011. Assinale a alternativa correta: 
    <br>
    <center><b>Figura 1:<b> Boxplot das horas dormidas segundo dia da semana (n=261). EPM (2011)
    <br>
    <center><img src="images/questionario/q17_boxplot.png"></center>
    '),
    respostas = c('Na segunda-feira, os alunos dormem em média de horas dormidas foi de 7,5 horas.
',
                  'As medianas de horas dormidas foram idênticas na sexta-feira e no sábado.
',
                  'Às quintas-feiras, 50% dos alunos dorme menos do que 6 horas. ',
                  'No domingo, o desvio-padrão da variável “horas dormida” foi de 0,5 hora.
'),
    resposta_correta = 3
  ),
  
  #q18
  list(
    pergunta = HTML('Considere as três afirmações abaixo:
    <br>
    <br>
      I - A média aritmética não é afetada por valores extremos.
      <br>
      II - A mediana é afetada por valores extremos.
      <br>
      III - O desvio-padrão é uma medida de variabilidade dos dados individuais em torno da média.
'),
    respostas = c(' I é correta.
',
                  'II é correta.
',
                  ' III é correta.',
                  ' I e II são corretas.	'),
    resposta_correta = 3
  ),
  
  #q19
  list(
    pergunta = HTML('Observe o gráfico abaixo e assinale com <b>V<b>
    se a afirmação for <b>verdadeira<b> ou com <b>F<b> se a afirmação for <b>falsa<b>
    <br>
    <center>Distribuição de mulheres de acordo com a religião. Município Beta, 2016.<center>
    <br>
    <center><img src="images/questionario/q19_barplot.png"></center>
    <br>
    <br>
    ( ) A maioria das mulheres entrevistadas tinham religião católica, cerca de 70%.
<br>
B - () A variável descrita no gráfico é do tipo qualitativa ordinal.
<br>
C - () O gráfico apresentado é um histograma. 
    
'),
    respostas = c('VVV',
                  'FFF',
                  'VFV',
                  'VFF'),
    resposta_correta = 4
  ),
  
  #q20
  list(
    pergunta = 'Dado um conjunto de dados, qual das medidas de tendência central (média, moda, mediana) corresponde sempre a um valor numérico do conjunto?
',
    respostas = c('Média
',
                  'Moda
',
                  'Mediana
',
                  'Nenhuma das anteriores'),
    resposta_correta = 2
  ),
  
  #q21
  list(
    pergunta = HTML('Você está fazendo um estudo para determinar a opinião de estudantes de sua universidade com respeito ao PSIU - Programa de Silêncio Urbano de combate à poluição sonora na cidade de São Paulo. A amostra pode ser obtida de vários modos, dentre eles:
<br>
    I. Seleciona-se aleatoriamente uma classe e, em seguida, faz-se perguntas a cada um dos estudantes desta classe.
    <br>
II. Divide-se a população estudantil com relação às especialidades estudadas, realiza-se uma seleção aleatória e em seguida faz-se perguntas a alguns estudantes de cada especialidade.
<br>
III. Atribui-se um número a cada estudante, faz-se um sorteio e, em seguida, questiona-se cada estudante que foi selecionado ao acaso.
<br>

Assinale a alternativa que traz corretamente o tipo de amostragem que você está utilizando em I, II e III, respectivamente:
'),
    respostas = c('sistemática, aleatória simples e por conglomerados.
',
                  'estratificada, por conglomerados e sistemática.
',
                  'por conglomerados, estratificada e sistemática.
',
                  'por conglomerados, estratificada e aleatória simples.
'),
    resposta_correta = 4
  ),
  
  #q22
  list(
    pergunta = 'A média das idades de quatro pessoas que estão reunidas em uma sala é 20 anos e a variância é zero. Se uma pessoa de 40 anos entrar na sala, qual será a idade média do novo grupo e qual será a variância? 
',
    respostas = c('24 anos e 80, respectivamente
',
                  '30 anos e 80, respectivamente
',
                  '24 anos e 15, respectivamente
',
                  '30 anos e 15, respectivamente
'),
    resposta_correta = 1
  ),
  
  #q23
  list(
    pergunta = HTML('Qual dos diagramas de dispersão abaixo melhor se adequa à seguinte tabela?
<br>
<center><img src="images/questionario/q23_tabela.png"></center>
    '),
    respostas = c(HTML('<img src="images/questionario/q23_a.png">
'),
                  HTML('<img src="images/questionario/q23_b.png">'),
                  HTML('<img src="images/questionario/q23_c.png">'),
                  HTML('<img src="images/questionario/q23_d.png">')
    ),
    resposta_correta = 1
  )
  ,
  
  #q24
  list(
    pergunta = HTML(' Na Figura abaixo representam-se duas distribuições, uma para gatos fêmeas (F) e outra para machos (M). Os resultados referem-se à variável peso (kg) de gatos domésticos adultos. Assinale a alternativa correta acerca das distribuições.
<br>
    <center><img src="images/questionario/q24_boxplot.png"></center>
'),
    respostas = c('O peso corporal dos gatos do sexo feminino apresenta maior variabilidade do que o peso corporal dos gatos do sexo masculino.
',
                  's gatos do sexo masculino possuem peso mediano maior do que os gatos do sexo feminino.
',
                  'Não há gatos com peso discrepante em relação aos demais de seu mesmo grupo.
',
                  'Cerca de 75% dos gatos do sexo feminino apresentam peso maior do que 2,5 kg.
'),
    resposta_correta = 2
  ),
  
  #q25
  list(
    pergunta = HTML(' Considere a Tabela 1 referente à pesquisa realizada com trabalhadores de uma indústria metalúrgica. O objetivo da pesquisa é verificar se existe associação entre sexo e perda auditiva. 

    <br>
    <b><center>Tabela 1<b> - Número de funcionários da indústria metalúrgica segundo sexo e diagnósticoo de perda auditiva <center>
    <br>
        <center><img src="images/questionario/q25_tabela.png"></center>
<br>
    Com base nessas informações assinale a alternativa correta
'),
    respostas = c('A hipótese nula é a de que o sexo não está associado à perda auditiva e o teste mais adequado para verificar a hipótese de interesse é o teste t de Student.',
                  'A hipótese alternativa é a de que o sexo não está associado à perda auditiva e o teste mais adequado para verificar a hipótese de interesse é o qui-quadrado.
',
                  'A hipótese nula é a de que o sexo não está associado à perda auditiva e o teste mais adequado para verificar a hipótese de interesse é o qui-quadrado.
',
                  'A hipótese alternativa é a de que o sexo está associado à perda auditiva e o teste mais adequado para verificar a hipótese de interesse é o t de Student.
'),
    resposta_correta = 3
  ),
  
  #q26
  list(
    pergunta = '<b>Considere o seguinte problema para responder as questões abaixo:<b> Um surto
    de intoxicação alimentar por enterotoxina estafilocócica ocorreu após almoço em 
    um simpósio. O almoço, composto por salada de folhas verdes, tomate, pepino, medalhão
    ao molho madeira, macarrão espaguete sem molho, molho de queijo, molho ao sugo,
    salada de frutas, suco de laranja e água ocorreu por volta das 13:00h.  Os
    primeiros casos do surto apresentaram sintomas típicos de intoxicação alimentar,
    como náusea, vômito, cólica abdominal e diarreia uma hora após a refeição. No total,
    200 pessoas participaram do simpósio. Das 150 pessoas que almoçaram no local do evento,
    50 adoeceram. Foram considerados casos do surto as pessoas que apresentaram vômito
    e/ou diarreia após terem se alimentado com pelo menos um dos itens servidos no evento.
    Dado o caso, quais testes seriam adequados para comparar a contagem de leucócitos
    em muL entre doentes e não doentes e para verificar qual alimento está relacionado
    ao adoecimento dos participantes?
',
    respostas = c('Qui-quadrado de independência e t de Student, respectivamente.
',
                  'Teste de correlação de Pearson e Qui-quadrado, respectivamente.
',
                  ' t de Student e Qui-quadrado, de independência respectivamente.
',
                  'Qui-quadrado e Teste de correlação de Pearson, respectivamente.
'),
    resposta_correta = 3
  ),
  
  #q27
  list(
    pergunta = HTML(' Distribuição dos 1170 paciente de acordo com presença de cardiopatia
    e hábito de fumar. HSP, 2013.
    <br>
    <center><img src="images/questionario/q27_barplot.png"></center>

    
'),
    respostas = c('38% dos pacientes são cardiopatas e fumantes.
',
                  '94% dos pacientes não fumantes não tem cardiopatia.
',
                  '38% dos pacientes cardiopatas são fumantes.
',
                  '6% dos pacientes não fumantes são cardiopatas.
'),
    resposta_correta = 3
  ),
  
  #q28
  list(
    pergunta = HTML('Internações por doenças respiratórias em idosos e a intervenção vacinal contra influenza no Estado de São Paulo (Francisco, et al 2004).
    
<br>
    <center><img src="images/questionario/q28_serie.png"></center>
    
    <br>
    Da figura acima é correto afirmar que:


                    '),
    respostas = c('A proporção de internações por doenças respiratórias é maior nas mulheres idosas.
',
                  'Pode-se dizer que não houve efeito de intervenção da vacina pois os percentuais de internação mantiveram-se os mesmos.
',
                  'Não há sazonalidade na proporção de internações.
',
                  'Pode-se dizer que houve efeito da vacinação, pois os percentuais de internação diminuíram entre os idosos.
'),
    resposta_correta = 4
  ),
  
  #q29
  list(
    pergunta = HTML('Gráfico de dispersão entre o índice de massa corporal e os perímetros da cintura e do quadril em escolares, na faixa etária de 6 a 10 anos.
    <br>
        <center><img src="images/questionario/q29_scatter.png"></center>
        <br>
        Com relação aos gráficos acima, é correto afirmar:


'),
    respostas = c('Há indícios de correlação negativa entre IMC e perímetro da cintura das meninas.
',
                  'Há indícios de correlação negativa entre IMC e perímetro da cintura dos meninos.
',
                  'Não há indícios de correlação positiva entre IMC e perímetro da cintura das meninas.
',
                  'Há indícios de correlação positiva entre IMC e perímetro da cintura das meninas.
'),
    resposta_correta = 4
  ),
  
  #q30
  list(
    pergunta = 'Considerando que os valores séricos do sódio em uma população adulta e saudável segue distribuição normal com média igual a 141 mEq/l e com desvio padrão de 3mEq/l, é correto afirmar que:
',
    respostas = c('Um indivíduo com nível sérico de sódio igual a 144 mEq/l está entre os 5% superiores da distribuição.
',
                  'Aproximadamente 95% da população adulta e saudável tem nível sérico de sódio entre 135 mEq/l e147 mEq/l. 
',
                  'Há 95% de confiança de que um indivíduo desta população tenha nível sérico de sódio 137 mEq/l e144 mEq/l.
',
                  'Um nível sérico de sódio igual a 138 mEq/l coloca o indivíduo no 1% inferior da distribuição. 
'),
    resposta_correta = 2
  ),
  
  #q31
  list(
    pergunta = HTML(' A amplitude de um intervalo de confiança para mu, a média de uma característica de uma população, <b>não<b> depende de:
'),
    respostas = c('Tamanho amostral.
',
                  'Média da amostra.',
                  
                  'Coeficiente de confiança.
',
                  'Depende de todos os elementos citados acima.'),
    resposta_correta = 2
  ),
  
  #q32
  list(
    pergunta = HTML('Foram entrevistadas 1.091 pessoas residentes em uma área metropolitana
    da região Sul do Brasil. Cada pessoa foi classificada segundo duas variáveis:
    sexo (homem ou mulher) e tabagismo (tabagista ou não). Depois foram feitas as contagens:
    havia 600 homens, dos quais 177 disseram ser tabagistas, e 491 mulheres, das quais 
    204 afirmaram ser tabagistas. Considerando alpha igual a 5% e chiquadradocritico = 3,84, assinale
    a alternativa correta:
    
    <br>
    preciso arruamr a formatação!!!!!!!!
    
    <br>
    
            <center><img src="images/questionario/q32_tabela.png"></center>

    
    
    <br>
    <p>&chi;² observado = ((204 - 171)<sup>2</sup> / 171) + ((177 - 210)<sup>2</sup> / 210) + ((287 - 320)<sup>2</sup> / 320) + ((423 - 390)<sup>2</sup> / 390) = 17.7</p>
'),
    respostas = c(' Como observado2 > tabelado2, aceita-se a hipótese nula.
',
                  'Como observado2 > tabelado2, rejeita-se a hipótese nula.
',
                  'A hipótese nula é a de que existe relação entre o hábito de fumar e o sexo
',
                  'A hipótese alternativa é a de que não existe relação entre o hábito de fumar e o sexo.'),
    resposta_correta = 2
  ),
  #q33
  list(
    pergunta = HTML('Julgue os itens abaixo como FALSO (F) ou VERDADEIRO (V) e assinale a alternativa correta
    <br>
    <br>
 ( ) Amostragem corresponde à atividade de inspecionar todos os elementos de uma população e Censo corresponde à atividade de inspecionar parte dos elementos de uma população. 
 <br>
 ( ) Uma amostra formada por voluntários é considerada não probabilística. 
 <br>
( ) A variável “Número de curtidas em uma foto postada no Instagram” é do tipo quantitativa contínua. 
<br>
( ) Média, mediana e desvio-padrão são medidas resistentes.
<br>
( ) A Inferência Estatística está interessada na redução, análise e interpretação dos dados através de cálculos de medidas resumo e gráficos. 
'),
    respostas = c('F F V V F',
                  'F V F F F',
                  'F F F F V',
                  'F V F V F'),
    resposta_correta = 2
  ),
  
  #q34
  list(
    pergunta = HTML('Pedro tenta calcular algumas medidas-resumo para um conjunto de notas de exame. Seus resultados são os seguintes: mínimo = 30, máximo = 90, primeiro quartil = 50, terceiro quartil = 80 e mediana = 85. Há algo errado nas medidas-resumo calculadas por Pedro?
'),
    respostas = c('Sim, pois Pedro não calculou o intervalo interquartílico
',
                  'Sim, pois Pedro calculou o terceiro quartil como menor do que a mediana.
',
                  'Sim, pois o primeiro quartil deveria ser 30.
',
                  'Não, as medidas-resumo estão corretas.
'),
    resposta_correta = 2
  ),
  
  #q35
  list(
    pergunta = HTML('Os dados abaixo se referem ao salário mensal e à carga horária semanal (em horas) de 50 funcionários de um hospital:

                    <br>
                    
                                <center><img src="images/questionario/q32_tabela.png"></center>
<br>

A partir dos resultados obtidos, em termos relativos, assinale a alternativa cuja variável possui a <b>menor<b> variabilidade.

<br>
<br>
<b>Nota<b>:  Coeficiente de variação é igual ao desvio padrão dividido pela média, vezes 100.
                    '),
    respostas = c('O salário, pois tem maior desvio padrão.
',
                  ' O salário, pois o coeficiente de variação é menor.
',
                  'A carga horária semanal, pois o desvio padrão é menor.
',
                  ' A carga horária, pois o coeficiente de variação é menor.
'),
    resposta_correta = 2
  ),
  
  #q36
  list(
    pergunta = HTML('Para a temporada de 2013 a 2014, os salários, em dólar americano, de 450 jogadores da NBA variaram de pouco menos de $1 milhão para mais de $30 milhões, com 19 jogadores ganhando mais de $15 milhões e cerca de metade ganhando $2 milhões ou menos. Qual seria a melhor estatística para descrever o centro desta distribuição? 
'),
    respostas = c('Média',
                  'Mediana',
                  'Moda',
                  'Todas são igualmente robustas'),
    resposta_correta = 2
  ),
  
  #q37
  list(
    pergunta = HTML('Imagine que você é dono de uma farmácia ou um gestor de comunicação
    de alguma secretaria de Saúde e deseja saber em qual época do ano as pessoas costumam
    ficar mais doentes e/ou buscar por mais informações sobre a gripe, por exemplo.
    Que afirmações você pode fazer analisando o gráfico abaixo?
    <br>
    <center><img src="images/questionario/q32_tabela.png"></center>
    <br>
    <br>
    Fonte neste <a href="https://www.ibpad.com.br/blog/analise-de-dados/dados-de-saude-quando-mais-se-busca-sobre-gripe-no-ano/
">link

    
'),
    respostas = c('Como representado na série temporal, a procura por informações é mais comum no segundo semestre de cada ano do que no primeiro.
',
                  'A busca por informações não aparenta estar relacionada a quaisquer épocas dos anos.
',
                  ' Como representado na série temporal, a procura por informações é mais comum no primeiro semestre de cada ano do que no segundo.
',
                  'A busca por informações é sempre superior a 13 no período entre 2015 - 2017.
'),
    resposta_correta = 3
  )
  
  )
  
  
  