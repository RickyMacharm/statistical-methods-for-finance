# Section 4. Copulas
#library&data
library(copula)
library(sn)
dat = return
n = 600
x1 = dat[,1]
x2 = dat[,2]
x3 = dat[,3]
x4 = dat[,4]
x5 = dat[,5]
x6 = dat[,6]
x7 = dat[,7]
x8 = dat[,8]
x9 = dat[,9]
x10 = dat[,10]

#nonparametric
edata= cbind(rank(x1)/(n + 1), rank(x2)/(n + 1), rank(x3)/(n + 1),
             rank(x4)/(n + 1),rank(x5)/(n + 1),rank(x6)/(n + 1),rank(x7)/(n + 1),rank(x8)/(n + 1),rank(x9)/(n + 1),rank(x10)/(n + 1))

#correlation
corM <- cor(return,method = "spearman")
cor45 <- c(corM[1,2:10],corM[2,3:10],corM[3,4:10],corM[4,5:10],corM[5,6:10],corM[6,7:10],corM[7,8:10],corM[8,9:10],corM[9,10])

#fit copulas
fn = fitCopula(copula=normalCopula(dim = 10), data=edata,
               method="ml", start=c(min(cor45)))
ft = fitCopula(copula=tCopula(dim = 10), data=edata,
               method="ml", start=c(min(cor45),10))
clcop=archmCopula(family="clayton", dim=10, param=2)
fclayton = fitCopula(data=edata,method="ml", copula=clcop)
gcop=archmCopula(family="gumbel", dim=10, param=2)
fgumbel = fitCopula(data=edata,method="ml", copula=gcop)

#results
fn
ft
fclayton
fgumbel

AIC(fn)
AIC(ft)
AIC(fclayton)
AIC(fgumbel)
