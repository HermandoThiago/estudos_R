# ------------------ Explorando dados ------------------

# Importando dados para exploracao
dados = read.csv(file.choose(), sep = ";", na.strings="", stringsAsFactors=T)
head(dados)
summary(dados)

# Dar nome correto para as colunas
colnames(dados) = c("Id", "Score", "Estado", "Genero", "idade", "Patrimonio", "Saldo", "Produtos", "TemCartCredito", "Ativo", "Salario", "Saiu")
head(dados)

# Analise exploratoria de dados
tableUf = table(dados$Estado)
barplot(counts, main="Estados", xlab="Estados") # Gerando grafico de barras

tableG = table(dados$Genero)
barplot(tableG, main="Generos", xlab="Generos")

# Explorando colunas numericas

# Score
summary(dados$Score)
boxplot(dados$Score)
hist(dados$Score) # Gerando histograma

# Idade
summary(dados$idade)
boxplot(dados$idade)
hist(dados$idade)

# Saldo
summary(dados$Saldo)
boxplot(dados$Saldo)
hist(dados$Saldo)

# Salarios por ano
summary(dados$Salario)
boxplot(dados$Salario)
boxplot(dados$Salario, outline = F)

# Trabalhando com valores faltantes
dados[!complete.cases(dados),] #Tras todas as linhas com dados imcompletos

# ------------------ Tratando dados ------------------

# Salario
# Ver mediana
median(dados$Salario, na.rm = T) # Realizando mediana sem valores Nas

# Atribuir mediana a NAs
dados[is.na(dados$Salario),]$Salario = median(dados$Salario, na.rm = T) # Trocando todos os valores NAs de salário para a mediana

# Buscar NAs em salários para checar
dados[!complete.cases(dados$Salario), ]

# Genero
# Resolvendo falta de padronizacao

# Ver valores
unique(dados$Genero)
summary(dados$Genero)

# Transformar F e Fem em feminino
# "",  M em Masculino (moda)
dados[is.na(dados$Genero) | dados$Genero == "M",]$Genero = "Masculino"
dados[dados$Genero == "F" | dados$Genero == "Fem", ]$Genero = "Feminino"

# Ver resultado
summary(dados$Genero)

# Remover levels nao utilizados
dados$Genero = factor(dados$Genero)
summary(dados$Genero)

# Idade
# Idades fora do dominio - idades anormais
dados[dados$idade<0 | dados$idade>110, ]

# Nao temos NAs nas idades
dados[is.na(dados$idade), ]$idade
median(dados$idade)
# Preencher com a mediana
# Substituindo
dados[dados$idade<0 | dados$idade>110, ]$idade = median(dados$idade)
summary(dados$idade)

# Removendo dados suplicados
# Buscar dados duplicados pelo ID
x = dados[duplicated(dados$Id), ]
x

# Excluir pelo ID nao pelo indice
dados = dados[!dados$Id %in% c(x$Id), ]

# Buscar linha que estava duplicada
dados[dados$Id == x$Id, ]

# Estado
unique(dados$Estado)
summary(dados$Estado)

# Preencher com a moda
dados[!dados$Estado %in% c("RS", "SC", "PR"), ]$Estado = "RS"

# Removendo fatores nao usados
dados$Estado = factor(dados$Estado)
barplot(summary(dados$Estado))

# Outliers, criando um parametro com desvio padrao
desv = sd(dados$Salario, na.rm=T)
dados[dados$Salario >= 2 * desv, ]$Salario

# Atualizamos para a mediana
median(dados$Salario)
dados[dados$Salario >= 2 * desv, ]$Salario = median(dados$Salario)

# Checamos se sairam os outliers
dados[dados$Salario >= 2 * desv, ]$Salario
