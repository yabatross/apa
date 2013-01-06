#
# Xarxa MLP
#

library(MASS)
library(nnet)

ds <- df
ds$X..50K <- factor(ds$X..50K)

# Fixem una llavor aleatoria per obtenir sempre el mateix resultat
set.seed(3)

# Part 1: Fem servir totes les variables d'entrada
neural.maxHiddenUnits <- 10

neural.netHitsAll <- seq(from=1, to=neural.maxHiddenUnits)

# Per accedir de forma segura a la taula quan només té una columna
neural.safeaccess <- function (x) { if (dim(x)[2] == 2) return (x[2, "1"]); return (0) }

# Esbrinem el millor nombre de unitats ocultes a la xarxa MLP
# 1 sola capa, funció d'error cross-entropy
for (i in 1:neural.maxHiddenUnits)
{
	neural.nnAll <- nnet(ds$X..50K
                ~ ds$X39 + ds$State.gov + ds$X77516 + ds$Bachelors + ds$X13 + ds$Never.married + ds$Adm.clerical
                + ds$Not.in.family + ds$White + ds$Male + ds$X2174 + ds$X0 + ds$X40 + ds$United.States
                , data=ds,size=i,decay=0,maxit=2000,trace=T)

	neural.resAll <- table(actual=ds$X..50K,predicted=predict(neural.nnAll,type='class'))
	
	# Calculem la proporcio d'encerts
	neural.netHitsAll[i] <- (neural.resAll[1] + neural.safeaccess(neural.resAll)) / dim(ds)[1]
}

# Resultat obtingut usant totes les variables d'entrada
neural.netHitsAll

# Part 2: Fem servir nomes les variables que millor expliquen el resultat
# Veure apartat de filtratge a l'arxiu glm.R
neural.netHitsRelevant <- seq(from=1, to=neural.maxHiddenUnits)

# Esbrinem el millor nombre de unitats ocultes a la xarxa MLP
# 1 sola capa, funció d'error cross-entropy
for (i in 1:neural.maxHiddenUnits)
{
	neural.nnRelevant <- nnet(ds$X..50K
                ~ ds$X39 + ds$State.gov + ds$X77516 + ds$Bachelors + ds$X13 + ds$Never.married + ds$Adm.clerical
                + ds$Not.in.family + ds$White + ds$Male + ds$X2174 + ds$X0 + ds$X40
                , data=ds,size=i,decay=0,maxit=2000,trace=T)

	neural.resRelevant <- table(actual=ds$X..50K,predicted=predict(neural.nnRelevant,type='class'))
	
	# Calculem la proporcio d'encerts
	neural.netHitsRelevant[i] <- (neural.resRelevant[1] + neural.safeaccess(neural.resRelevant)) / dim(ds)[1]
}

# Resultat obtingut usant nomes les variables que millor expliquen el resultat
neural.netHitsRelevant

