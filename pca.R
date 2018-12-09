#pca
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


#sample correlation matrix of returns
corr <- cor(df)
#msft and amzn has max correlation 0.9831232
#fb and ibm has minimum correlation -0.5771376


#pca
pca <- princomp(df,cor=T)
pca
pca$loadings
summary(pca)




