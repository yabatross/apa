#
# GLM
#

# Preproces
ds <- df
ds$X..50K <- factor (ds$X..50K)


# Construim el model amb totes les variables.

mylogit <- glm (df$X..50K
                ~ df$X39 + df$State.gov + df$Gender + df$Age + df$Studying + df$Contract + df$Firmtype
                + df$Accgrade + df$Grade + df$Startwork + df$Image + df$Exp.gene + df$Exp.spec + df$Qual.gen
                + df$Qual.spec + df$Value + df$Satisfaction, data=df, family="binomial")

# A continuacio fem una prediccio i mesurem l'error.
# En les nostres proves l'error es de 0.2284495, es a dir, que el model encerta un 77% dels cops.

pr <- predict(mylogit, df, type="response")
minus1 <- function(x) { return (x-1); }
sa <- sapply(as.numeric(df$Salary), minus1)
err <- mean(abs(pr-sa))

# Aqui fem el mateix, pero establim que si la prediccio es <0.5, la interpreten com un 0, i 1 d'altre manera.
# En les nostres proves l'error es de 0.1632653, es adir, que el model encerta un 83% dels cops.

tobin <- function (x) { if (x >= 0.5) return (1); return (0) }
pr2 <- sapply(pr, tobin)
err2 <- mean(abs(pr2-sa))



#
# Part 2
#
# Aqui veiem quines variables son les rellevants per estimar el salari, construim un model amb nomes aquestes
# variables i fem una prediccio.
#

require(FSelector)
require(mlbench)
require(MASS)

#Preparing data frame
x <- cbind(  df$Num, df$Career, df$Gender, df$Age, df$Studying, df$Contract,
             df$Firmtype, df$Accgrade, df$Grade, df$Startwork, df$Image, df$Exp.gene, 
             df$Exp.spec, df$Qual.gen, df$Qual.spec, df$Value, df$Satisfaction)

y <- cbind(df$Salary)

# The predictors are centered and scaled:
x <- as.data.frame(scale(x))

# Form final data frame
fullset.formula <- as.simple.formula(colnames(x), "Target")
dataforFSS <- cbind (x,y)
colnames(dataforFSS)[18] <- "Target"

# Linear Regression
LinearRegression <- glm (Target~., family = gaussian, data = dataforFSS)
subset.LinearRegression.formula <- step(LinearRegression)$formula

# Showing filtered formula to show most significant dependencies
# Target ~ V1 + V3 + V6 + V7 + V8 + V14 + V16
# Salary ~ Gender + Age + Contract + Image + Exp.spec + Qual.gen + Value + Satisfaction
print(subset.LinearRegression.formula)


# Construim un model amb les variables rellevants.

mylogit <- glm (df$Salary
                ~ df$Gender + df$Age + df$Contract + df$Image + df$Exp.spec + df$Qual.gen
                + df$Value + df$Satisfaction, data=df, family="binomial")

# A continuacio fem una prediccio i mesurem l'error.
# En les nostres proves l'error es de 0.254356, lleugerament diferent de err1.

pr3 <- predict(mylogit, df, type="response")
sa3 <- sapply(as.numeric(df$Salary), minus1)
err3 <- mean(abs(pr3-sa3))

# Aqui fem el mateix, pero establim que si la prediccio es <0.5, la interpreten com un 0, i 1 d'altre manera.
# En les nostres proves l'error es de 0.1632653, exactament el mateix que err2.

tobin <- function (x) { if (x >= 0.5) return (1); return (0) }
pr4 <- sapply(pr3, tobin)
err4 <- mean(abs(pr4-sa3))
