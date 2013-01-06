#
# LDA/QDA
#

library(MASS)

ds <- df
ds$X..50K <- factor(ds$X..50K)

# Splitting data for training and testing
ldaqda.learnIndex <- sample(1:dim(ds)[1], dim(ds)[1]/2)

ldaqda.learn <- ds[ldaqda.learnIndex, ]
ldaqda.test <- ds[-ldaqda.learnIndex, ]

# Aliasing for brevity on the formula
dl <- ldaqda.learn

############################################# 
#############################################  Part 1: LDA
############################################# 

# Modelling using only the most significant variables
ldaqda.model <- lda(dl$X..50K
                ~ dl$X39 + dl$State.gov + dl$X77516 + dl$Bachelors + dl$X13 + dl$Never.married + dl$Adm.clerical
                + dl$Not.in.family + dl$White + dl$Male + dl$X2174 + dl$X0 + dl$X40, 
                data=ldaqda.learn, prior = c(1,1)/2)

# A posteriori probabilities
predict(ldaqda.model, ldaqda.learn)$posterior

# Resubstitution error (not much reliable). Accuracy ~76%
ldaqda.learningPred <- predict(ldaqda.model, ldaqda.learn)$class
ldaqda.tab <- table(ds$X..50K[ldaqda.learnIndex], ldaqda.learningPred)  
sum(ldaqda.tab[row(ldaqda.tab)==col(ldaqda.tab)])/sum(ldaqda.tab)
                
# Estimation of true error with test data. Accuracy ~58%
ldaqda.testPred <- predict(ldaqda.model, ldaqda.test)$class
ldaqda.tab <- table(ds$X..50K[-ldaqda.learnIndex], ldaqda.testPred)  
sum(ldaqda.tab[row(ldaqda.tab)==col(ldaqda.tab)])/sum(ldaqda.tab)

# Assessment of predictive accuracy via LOOCV
ldaqda.model.cv <- lda(ds$X..50K
                ~ ds$X39 + ds$State.gov + ds$X77516 + ds$Bachelors + ds$X13 + ds$Never.married + ds$Adm.clerical
                + ds$Not.in.family + ds$White + ds$Male + ds$X2174 + ds$X0 + ds$X40, 
                data=ds, prior = c(1,1)/2, CV=TRUE) 
                
summary(ldaqda.model.cv$class)

# Regularized result. Accuracy ~77%
ldaqda.tab <- table(ds$X..50K, ldaqda.model.cv$class)  
sum(ldaqda.tab[row(ldaqda.tab)==col(ldaqda.tab)])/sum(ldaqda.tab)

############################################# 
############################################# Part 2: QDA
############################################# 

# Modelling using only the most significant variables
ldaqda.model <- qda(dl$X..50K
                ~ dl$X39 + dl$State.gov + dl$X77516 + dl$Bachelors + dl$X13 + dl$Never.married + dl$Adm.clerical
                + dl$Not.in.family + dl$White + dl$Male + dl$X2174 + dl$X0 + dl$X40, 
                data=ldaqda.learn, prior = c(1,1)/2)

# A posteriori probabilities
predict(ldaqda.model, ldaqda.learn)$posterior

# Resubstitution error (not much reliable). Accuracy ~81%
ldaqda.learningPred <- predict(ldaqda.model, ldaqda.learn)$class
ldaqda.tab <- table(ds$X..50K[ldaqda.learnIndex], ldaqda.learningPred)  
sum(ldaqda.tab[row(ldaqda.tab)==col(ldaqda.tab)])/sum(ldaqda.tab)
                
# Estimation of true error with test data. Accuracy ~68%
ldaqda.testPred <- predict(ldaqda.model, ldaqda.test)$class
ldaqda.tab <- table(ds$X..50K[-ldaqda.learnIndex], ldaqda.testPred)  
sum(ldaqda.tab[row(ldaqda.tab)==col(ldaqda.tab)])/sum(ldaqda.tab)

# Assessment of predictive accuracy via LOOCV
ldaqda.model.cv <- qda(ds$X..50K
                ~ ds$X39 + ds$State.gov + ds$X77516 + ds$Bachelors + ds$X13 + ds$Never.married + ds$Adm.clerical
                + ds$Not.in.family + ds$White + ds$Male + ds$X2174 + ds$X0 + ds$X40, 
                data=ds, prior = c(1,1)/2, CV=TRUE) 
                
summary(ldaqda.model.cv$class)

# Regularized result. Accuracy ~82%
ldaqda.tab <- table(ds$X..50K, ldaqda.model.cv$class)  
sum(ldaqda.tab[row(ldaqda.tab)==col(ldaqda.tab)])/sum(ldaqda.tab)
