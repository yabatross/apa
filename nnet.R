#
# Xarxa MLP
#

library(MASS)
library(nnet)

# 1 capa oculta, 3 unitats ocultes, sense regularitzar, funció d'error "cross-entropy"
# 
neural.nnAll <- nnet(ds$X..50K
                ~ ds$X39 + ds$State.gov + ds$X77516 + ds$Bachelors + ds$X13 + ds$Never.married + ds$Adm.clerical
                + ds$Not.in.family + ds$White + ds$Male + ds$X2174 + ds$X0 + ds$X40 + ds$United.States
                , data=ds,size=3,decay=0,maxit=2000,trace=T)

#yhat <- as.numeric(predict(nn1,type='class'))


#par(mfrow=c(1,2))
#plot(x,pch=19,col=c('red','blue')[y+1],main='actual labels',asp=1)
#plot(x,col=c('red','blue')[(yhat>0.5)+1],pch=19,main='predicted labels',asp=1)
#table(actual=y,predicted=predict(nn1,type='class'))

# Executem de nou amb seed=4 ...
#set.seed(4)
#nn1 <- nnet(y~x.1+x.2,data=mydata,entropy=T,size=3,decay=0,maxit=2000,trace=T)
#yhat <- as.numeric(predict(nn1,type='class'))
#par(mfrow=c(1,2))
#plot(x,pch=19,col=c('red','blue')[y+1],main='actual labels',asp=1)
#plot(x,col=c('red','blue')[(yhat>0.5)+1],pch=19,main='predicted labels',asp=1)
#table(actual=y,predicted=predict(nn1,type='class'))

# ja veiem que l'optimitzador no sempre troba una bona solució

# Quantes unitats ocultes necessitem?

#par(mfrow=c(2,2))
#for (i in 1:4)
#{
#  set.seed(3)
#  nn1 <- nnet(y~x.1+x.2,data=mydata,entropy=T,size=i,decay=0,maxit=2000,trace=T)
#  yhat <- as.numeric(predict(nn1,type='class'))
#  plot(x,pch=20,col=c('black','green')[yhat+1])
#  title(main=paste('nnet with',i,'hidden units'))
#}

# esbrinem quina és la funció que realment s'està calculant

#set.seed(3)
#nn1 <- nnet(y~x.1+x.2,data=mydata,entropy=T,size=3,decay=0,maxit=2000,trace=T)
#x1grid <- seq(-3,3,l=200)
#x2grid <- seq(-3,3,l=220)
#xg <- expand.grid(x1grid,x2grid)
#xg <- as.matrix(cbind(1,xg))
#h1 <- xg%*%matrix(coef(nn1)[1:3],ncol=1)
#h2 <- xg%*%matrix(coef(nn1)[4:6],ncol=1)
#h3 <- xg%*%matrix(coef(nn1)[7:9],ncol=1)

#par(mfrow=c(2,2))
#contour(x1grid,x2grid,matrix(h1,200,220),levels=0)
#contour(x1grid,x2grid,matrix(h2,200,220),levels=0,add=T)
#contour(x1grid,x2grid,matrix(h3,200,220),levels=0,add=T)
#title(main='boundaries from linear functions\n in hidden units')

#sigmoid <- function(x){exp(x)/(1+exp(x))}
#z <- coef(nn1)[10]+coef(nn1)[11]*sigmoid(h1)+coef(nn1)[12]*sigmoid(h2)+
#  coef(nn1)[13]*sigmoid(h3)
#contour(x1grid,x2grid,matrix(z,200,220))
#title('sum of sigmoids \n of linear functions')
#contour(x1grid,x2grid,matrix(sigmoid(z),200,220),levels=0.5)
#title('sigmoid of sum of sigmoids \n of linear functions')
#contour(x1grid,x2grid,matrix(sigmoid(z),200,220),levels=0.5)
#points(x,pch=20,col=c('black','green')[y+1])
#title('data values')
#
