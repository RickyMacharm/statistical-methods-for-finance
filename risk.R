#risk management
#import data
aapl = read.csv("AAPL.csv")
amzn = read.csv("AMZN.csv")
bac = read.csv("BAC.csv")
fb = read.csv("FB.csv")
goog = read.csv("GOOG.csv")
ibm = read.csv("IBM.csv")
jpm = read.csv("JPM.csv")
msft = read.csv("MSFT.csv")
tgt = read.csv("TGT.csv")
wmt = read.csv("WMT.csv")


#concatenate close prices
df <- cbind(aapl['Close'],amzn['Close'],bac['Close'],fb['Close'],
            goog['Close'],ibm['Close'],jpm['Close'],msft['Close'],
            tgt['Close'],wmt['Close'])
colnames(df) <- list('aapl','amzn','bac','fb','goog',
                     'ibm','jpm','msft','tgt','wmt')


#calculate stock returns
df <- data.matrix(df)
return <- matrix(nrow = 60,ncol = 10)
for(i in 1:10){
  return[,i] = exp(diff(log(df[,i]))) - 1
}
colnames(return) <- list('aapl','amzn','bac','fb','goog',
                         'ibm','jpm','msft','tgt','wmt')


#nonparametric method for value at risk
vaR.np <- function(l){
  m <- 100000
  q <- quantile(l,0.05)
  return(-m*q)
}

#nonparametric method for expected shortfall
es.np <- function(l){
  m <- 100000
  q <- quantile(l,0.05)
  i <- (l < q)
  return(-m*sum(l*i)/sum(i))
}

#bootstrap
set.seed(1234567)
B <- 1000
n <- nrow(return)
var.bootnp <- matrix(nrow=B,ncol=10)
es.bootnp <- matrix(nrow=B,ncol=10)
for(i in 1:B){
  for(j in 1:10){
    samp <- sample(return[,j],n,replace=T)
    var.bootnp[i,j] <- vaR.np(samp)
    es.bootnp[i,j] <- es.np(samp)
  }
}
#standard errors
name <- list('aapl','amzn','bac','fb','goog',
             'ibm','jpm','msft','tgt','wmt')
var.bootnp[is.na(var.bootnp)] <- 0
mean.var.bootnp <- apply(var.bootnp,2,mean)
se.var.bootnp <- apply(var.bootnp,2,sd)

es.bootnp[is.na(es.bootnp)] <- 0
mean.es.bootnp <- apply(es.bootnp,2,mean)
se.es.bootnp <- apply(es.bootnp,2,sd)
#95% ci
min.ci.var.bootnp <- mean.var.bootnp-se.var.bootnp
max.ci.var.bootnp <- mean.var.bootnp+se.var.bootnp

min.ci.es.bootnp <- mean.es.bootnp-se.es.bootnp
max.ci.es.bootnp <- mean.es.bootnp+se.es.bootnp


#parametric method for value at risk
vaR.p <- function(l){
  m <- 100000
  means <- mean(l)
  se <- sd(l)
  z <- qnorm(0.05)
  return(m*(means+z*se)*(-1))
}

#parametric method for expected shortfall
es.p <- function(l){
  m <- 100000
  means <- mean(l)
  se <- sd(l)
  z <- qnorm(0.05)
  return(m*((-1)*means+dnorm(z)*se/0.05))
}

#bootstrap
name <- list('aapl','amzn','bac','fb','goog',
             'ibm','jpm','msft','tgt','wmt')
set.seed(2345678)
B <- 1000
n <- nrow(return)
var.bootp <- matrix(nrow=B,ncol=10)
es.bootp <- matrix(nrow=B,ncol=10)
for(i in 1:B){
  for(j in 1:10){
    samp <- sample(return[,j],n,replace=T)
    var.bootp[i,j] <- vaR.p(samp)
    es.bootp[i,j] <- es.p(samp)
  }
}
#standard errors
var.bootp[is.na(var.bootp)] <- 0
mean.var.bootp <- apply(var.bootp,2,mean)
se.var.bootp <- apply(var.bootp,2,sd)

es.bootp[is.na(es.bootp)] <- 0
mean.es.bootp <- apply(es.bootp,2,mean)
se.es.bootp <- apply(es.bootp,2,sd)
#95% ci
min.ci.var.bootp <- mean.var.bootp-se.var.bootp
max.ci.var.bootp <- mean.var.bootp+se.var.bootp

min.ci.es.bootp <- mean.es.bootp-se.es.bootp
max.ci.es.bootp <- mean.es.bootp+se.es.bootp


#show list of results
df1 <- data.frame(mean.var.bootp,se.var.bootp,min.ci.var.bootp,max.ci.var.bootp,
                  mean.es.bootp,se.es.bootp,min.ci.es.bootp,max.ci.es.bootp)
colnames(df1) <- c("var_mean","var_se","var_min_ci","var_max_ci",
                   "es_mean","es_se","es_min_ci","es_max_ci")
rownames(df1) <- c("aapl","amzn","bac","fb","goog",
                   "ibm","jpm","msft","tgt","wmt")
df2 <- data.frame(mean.var.bootnp,se.var.bootnp,min.ci.var.bootnp,max.ci.var.bootnp,
                  mean.es.bootnp,se.es.bootnp,min.ci.es.bootnp,max.ci.es.bootnp)
colnames(df2) <- c("var_mean","var_se","var_min_ci","var_max_ci",
                   "es_mean","es_se","es_min_ci","es_max_ci")
rownames(df2) <- c("aapl","amzn","bac","fb","goog",
                   "ibm","jpm","msft","tgt","wmt")
result <- list(df1,df2)
names(result) <- c("parametric","nonparametric")
result






