# --------------- Graficos baasicos --------------- 

# Histograma
hist(trees$Height)
hist(
  trees$Height, 
  main = "Arvore", 
  ylab = "Frequencia", 
  xlab = "Altura", 
  col="blue"
)

hist(
  trees$Height, 
  main = "Arvore", 
  ylab = "Frequencia", 
  xlab = "Altura", 
  col="blue", 
  density = 20, 
  breaks=20
)

# Densidade
densidade = density(trees$Height)
plot(densidade)

# Densidade sobre histograma, parametro par
hist(trees$Height, main=NULL, xlab = NULL, ylab = NULL)
par(new=T) # Informa que a proxima impressao sera sobreposta no grafico anterior
plot(densidade)

# Dispersao -> Comparar variaveis continuas
plot(trees$Girth, trees$Volume)
plot(trees$Girth, trees$Volume, main = "Arvores")
plot(trees$Girth, trees$Volume, main="Arvores", ylab = "Circuferencias", xlab="Volume", col="blue")

# pch muda a forma de renderizacao
plot(trees$Girth, trees$Volume, ylab="Circuferencias", xlab="Volume", col="blue", main="Arvores", pch=20, type="l")

# Tremulacao, siminui sobreposicao
plot(jitter(trees$Girth), trees$Volume, main="Arvores", ylab="Circuferencias", xlab="Volume", col="blue", pch=20, type="l")

# Legenda com dimensao categorica
CO2
plot(CO2$conc, CO2$uptake, pch=20, col=CO2$Treatment)
legend("bottomright", legend=c("chilled", "nonchilled"), cex=1, fill=c("black", "red"))

# Novos dados
plot(trees)

# Divisao de tela
split.screen(figs=c(2, 2))
screen(1)
plot(trees$Girth, trees$Volume)
screen(2)
plot(trees$Girth, trees$Height)
screen(3)
plot(trees$Height, trees$Volume)
screen(4)
hist(trees$Volume)
close.screen(all=T)

# Boxplot
boxplot(trees$Volume, main="Arvores", xlab="Volume")
boxplot(trees$Volume, main="Arvores", xlab="Volume", col="blue", horizontal=T)
boxplot(trees$Volume, main="Arvores", xlab="Volume", col="blue", outline=F)

# Notch
boxplot(trees$Volume, main="Arvores", xlab="Volume", col="blue", notch=TRUE)

# States
boxplot.stats(trees$Height)
boxplot.stats(trees$Height)$stats # Lendo apenas um parametro

# Varios graficos boxplot
boxplot(trees)

# Agregacao
InsectSprays
spray = aggregate(. ~ spray, data=InsectSprays, sum)
spray

# Grafico de barras
barplot(spray$count, col=gray.colors(6), xlab="Spray", ylab="Total", names.arg=spray$spray)
box()

# Grafico de setor - pizza
pie(spray$count, labels=spray$spray, main="Spray", col=c(1:6))

# Pizza com legenda
pie(spray$count, labels=NA, main="Spray", col=c(1:6))
legend("bottomright", legend=spray$spray, fill=c(1:6))

# --------------- Impressao de tabelas ---------------
#install.packages("stargazer")
library(stargazer)

# Formato latex
stargazer(iris)

# Html
stargazer(iris, type="html")

# Text
stargazer(iris, type="text")

# --------------- Pacote lattice ---------------
library(lattice)

# Boxplot
bwplot(trees$Volume)
bwplot(trees$Volume, main="Arvores", xlab="Volumes")

# Histograma
histogram(trees$Volume, main="Arvores", xlab="Volume", aspect=1, type="percent", nint=20)
