# Section 5. Portfolio Theory

## Minimum Variance Portfolio

#The mean of the MVP is 0.00918.
#The standard deviation of the MVP is 0.00084.
#The weights are 0.060 -0.080 -0.176  0.169  0.173  0.100  0.454 -0.011  0.047  0.265.
#Annualized mean and risk are 0.11 and 0.0029.
#compute mean and covariance matrix for the portfolio
mu <- sample_stats$Mean
omega <- cov(return)
vector_1 = rep(1, 10)
omega_inv <- solve(omega)

#compute MVP
w_mvp = as.numeric(omega_inv%*%vector_1)/as.numeric(t(vector_1)%*%omega_inv%*%vector_1)

#weights 
round(w_mvp, 3)

# mean
m <- sum(w_mvp*mu)
m

#annualized mean
12*m

# standard deviation
s <- t(w_mvp)%*%omega%*%w_mvp
s

#annualized standard deviation
s*sqrt(12)


## Value at risk and expect shortfall
#Assume that you have $100,000 to invest. For the MVP, determine the 5% value-at-risk of the
#$100,000 investment over a one month investment horizon. 
#VaR for MVP
loglik = function(par, data) {
  mu = par[1:10]
  scale_matrix = t(A) %*% A
  df = par[58]
  return(-sum(log(dmt(data, mean = mu, S = scale_matrix, df = df))))}
A = chol(cov(return))
start = as.vector(c(apply(return, 2, mean), A[1, 1], A[1, 2],A[1,3],A[1,4],A[1,5],A[1,6],A[1,7],A[1,8],A[1,9],A[1,10],A[2, 2],A[2,3],A[2,4],A[2,5],A[2,6],A[2,7],A[2,8],A[2,9],A[2,10],A[3,3],A[3,4],A[3,5],A[3,6],A[3,7],A[3,8],A[3,9],A[3,10],A[4,4],A[4,5],A[4,6],A[4,7],A[4,8],A[4,9],A[4,10],A[5,5],A[5,6],A[5,7],A[5,8],A[5,9],A[5,10],A[6,6],A[6,7],A[6,8],A[6,9],A[6,10],A[7,7],A[7,8],A[7,9],A[7,10],A[8,8],A[8,9],A[8,10],A[9,9],A[9,10],A[10,10],100))
fit_mvt = optim(start, loglik, data = return, method = "L-BFGS-B", hessian = T)

nu <- fit_mvt$par[58]
alpha <- 0.05
lambda =   s/sqrt( (nu)/(nu-2) )
qalpha = qt(alpha, df = nu)
Finv = m + lambda * qalpha
S <- 100000
VaR = -S * Finv
VaR

#VaR for each asset
var <- matrix(NA,ncol=1,nrow=10)
rownames(var) <- c("Apple","Amazon","Bank of America","Facebook","Google","IBM","JP Morgan","Microsoft","Target","Walmart")
colnames(var) <- "VaR(0.05) with $100,000 investment"
var[1,]<- 100000*VaR(return_a$aapl)
var[2,]<- 100000*VaR(return_az$amzn)
var[3,]<- 100000*VaR(return_ac$bac)
var[4,]<- 100000*VaR(return_fb$fb)
var[5,]<- 100000*VaR(return_gg$goog)
var[6,]<- 100000*VaR(return_ib$ibm)
var[7,]<- 100000*VaR(return_jp$jpm)
var[8,]<- 100000*VaR(return_ms$msft)
var[9,]<- 100000*VaR(return_tg$tgt)
var[10,]<- 100000*VaR(return_wmt$wmt)


## Sharpe Ratio for each asset
rf <- mean(riskfree$TB4WK/144)
sd <- sample_stats$`Standard deviation`

sharpe <- matrix(NA,nrow=10,ncol=1)
rownames(sharpe) <- c("Apple","Amazon","Bank of America","Facebook","Google","IBM","JP Morgan","Microsoft","Target","Walmart")
colnames(sharpe) <- "Sharpe Ratio"
for (i in 1:10){
  sharpe[i,] <- (mu[i]-rf)/sd[i]
}


## Tangency portfolio (no short sales allowed)
#The expected return on tangency portfolio is 0.0196.
#The standard deviation is 0.04.
#The variance is 0.04^2=0.016.
#The Sharpe ratio is 0.376.
#convert risk free to monthly
rf <- mean(riskfree$TB4WK/144)
t1 <- tangency.portfolio(er=mu,cov.mat=omega,risk.free = rf,shorts = F)
t1
#calculate Sharpe ratio
(t1$er-rf)/t1$sd
