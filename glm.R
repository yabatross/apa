#
# GLM
#

ds <- df

tobin <- function (x) { if (x >= 0.5) return (1); return (0) }
minus1 <- function(x) { return (x-1); }

# Construim el model amb totes les variables.

mylogit <- glm (ds$X..50K
                ~ ds$X39 + ds$State.gov + ds$X77516 + ds$Bachelors + ds$X13 + ds$Never.married + ds$Adm.clerical
                + ds$Not.in.family + ds$White + ds$Male + ds$X2174 + ds$X0 + ds$X40 + ds$United.States
                , data=ds, family="binomial")

# A continuacio fem una prediccio i mesurem l'error.
# err = 0.2430621
pr <- predict(mylogit, ds, type="response")
sa <- sapply(as.numeric(ds$X..50K), minus1)
err <- mean(abs(pr-sa))

# Aqui fem el mateix, pero establim que si la prediccio es <0.5, la interpreten com un 0, i 1 d'altre manera.
# err2 = 0.1749079
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

# Preparing data frame
x <- cbind(ds$X39, ds$State.gov, ds$X77516, ds$Bachelors, ds$X13, ds$Never.married, ds$Adm.clerical,
           ds$Not.in.family, ds$White, ds$Male, ds$X2174, ds$X0, ds$X40, ds$United.States)

y <- cbind(ds$X..50K)

# The predictors are centered and scaled:
x <- as.data.frame(scale(x))

# Form final data frame
fullset.formula <- as.simple.formula(colnames(x), "Target")
dataforFSS <- cbind (x,y)
colnames(dataforFSS)[15] <- "Target"

# Linear Regression
LinearRegression <- glm (Target~., family = gaussian, data = dataforFSS)
subset.LinearRegression.formula <- step(LinearRegression)$formula

# Aqui obtenim la formula filtrada amb les entrades que millor expliquen les sortides
# Target ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13
print(subset.LinearRegression.formula)


# Construim un model amb les variables rellevants.

mylogit <- glm (ds$X..50K
                ~ ds$X39 + ds$State.gov + ds$X77516 + ds$Bachelors + ds$X13 + ds$Never.married + ds$Adm.clerical
                + ds$Not.in.family + ds$White + ds$Male + ds$X2174 + ds$X0 + ds$X40
                , data=ds, family="binomial")

# A continuacio fem una prediccio i mesurem l'error.
# err3 = 0.2431116
pr3 <- predict(mylogit, ds, type="response")
sa3 <- sapply(as.numeric(ds$X..50K), minus1)
err3 <- mean(abs(pr3-sa3))

# Aqui fem el mateix, pero establim que si la prediccio es <0.5, la interpreten com un 0, i 1 d'altre manera.
# err4 = 0.1753993
pr4 <- sapply(pr3, tobin)
err4 <- mean(abs(pr4-sa3))

# Calculem errors amb cross validation.
require (boot)

# Error amb LOOCV
# err_loocv <- cv.glm(ds, mylogit)

# Error amb k-fold CV.
#N <- 6
#err_kcv <- rep(0,N)
#for (i in 2:N)
#{
#err_kcv[i-1] <- cv.glm(ds, mylogit, K=i)
#}
