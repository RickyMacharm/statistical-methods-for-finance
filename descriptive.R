# Section 2. Descriptive Statistics

## Sample statistics for returns

#Here is the table of mean, standard deviation, skewness coefficients, kurtosis coefficients and beta of each asset.
#report sample statistics
sample_stats <- data.frame(matrix(NA,ncol=5,nrow=10))
rownames(sample_stats) <- c("Apple","Amazon","Bank of America","Facebook","Google","IBM","JP Morgan","Microsoft","Target","Walmart")
colnames(sample_stats) <- c("Mean","Standard deviation","Skewness","Kurtosis","Beta")

for (i in 1:10){
  sample_stats[i,1] <- round(mean(return[,i]),3)
  sample_stats[i,2] <- round(sd(return[,i]),3)
  sample_stats[i,3] <- round(skewness(return[,i]),3)
  sample_stats[i,4] <- round(kurtosis(return[,i]),3)
  sample_stats[i,5] <- round(lm(return_b[,1]~return[,i])$coefficients[[2]],3)
}

datatable(sample_stats,caption = "Table 1: Sample statistics for returns",class = "cell-border stripe")


## Plot of monthly prices and returns

#plot monthly prices & returns
#Apple monthly price
df_a <- subset(df_d,select=c("Date","aapl"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_a$Date <- as.Date(df_d$Date)
gg_a <- ggplot(data = df_a, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=aapl))+scale_fill_gradient(low='white',high = 'dark green')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for Apple")+theme(axis.text.x = element_text(angle=330))

#Apple return
return_a <- subset(re_d,select=c("Date","aapl"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_a$Date <- as.Date(re_d$Date)
gg_a_d <- ggplot(data = return_a, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=aapl))+scale_fill_gradient(low='white',high = 'dark green')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for Apple")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_a,gg_a_d,ncol=2)

#Amazon monthly price
df_az <- subset(df_d,select=c("Date","amzn"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_az$Date <- as.Date(df_d$Date)
gg_az <- ggplot(data = df_az, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=amzn))+scale_fill_gradient(low='white',high = 'dark orange')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for Amazon")+theme(axis.text.x = element_text(angle=330))

#Amazon return
return_az <- subset(re_d,select=c("Date","amzn"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_az$Date <- as.Date(re_d$Date)
gg_az_d <- ggplot(data = return_az, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=amzn))+scale_fill_gradient(low='white',high = 'dark orange')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for Amazon")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_az,gg_az_d,ncol=2)

#Bank of America monthly price
df_ac <- subset(df_d,select=c("Date","bac"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_ac$Date <- as.Date(df_d$Date)
gg_ac <- ggplot(data = df_ac, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=bac))+scale_fill_gradient(low='white',high = 'dark red')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for Bank of America")+theme(axis.text.x = element_text(angle=330))

#Bank of America return
return_ac <- subset(re_d,select=c("Date","bac"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_ac$Date <- as.Date(re_d$Date)
gg_ac_d <- ggplot(data = return_ac, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=bac))+scale_fill_gradient(low='white',high = 'dark red')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for Bank of America")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_ac,gg_ac_d,ncol=2)

#Facebook monthly prices
df_fb <- subset(df_d,select=c("Date","fb"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_fb$Date <- as.Date(df_d$Date)
gg_fb <- ggplot(data = df_fb, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=fb))+scale_fill_gradient(low='white',high = 'steel blue')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for Facebook")+theme(axis.text.x = element_text(angle=330))

#Facebook return
return_fb <- subset(re_d,select=c("Date","fb"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_fb$Date <- as.Date(re_d$Date)
gg_fb_d <- ggplot(data = return_fb, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=fb))+scale_fill_gradient(low='white',high = 'steel blue')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for Facebook")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_fb,gg_fb_d,ncol=2)

#Google monthly prices
df_gg <- subset(df_d,select=c("Date","goog"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_gg$Date <- as.Date(df_d$Date)
gg_gg <- ggplot(data = df_gg, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=goog))+scale_fill_gradient(low='white',high = 'dark blue')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for Google")+theme(axis.text.x = element_text(angle=330))

#Google return
return_gg <- subset(re_d,select=c("Date","goog"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_gg$Date <- as.Date(re_d$Date)
gg_gg_d <- ggplot(data = return_gg, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=goog))+scale_fill_gradient(low='white',high = 'dark blue')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for Google")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_gg,gg_gg_d,ncol=2)

#IBM monthly prices
df_ib <- subset(df_d,select=c("Date","ibm"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_ib$Date <- as.Date(df_d$Date)
gg_ib <- ggplot(data = df_ib, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=ibm))+scale_fill_gradient(low='white',high = ' black')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for IBM")+theme(axis.text.x = element_text(angle=330))

#IBM return
return_ib <- subset(re_d,select=c("Date","ibm"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_ib$Date <- as.Date(re_d$Date)
gg_ib_d <- ggplot(data = return_ib, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=ibm))+scale_fill_gradient(low='white',high = 'black')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for IBM")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_ib,gg_ib_d,ncol=2)

#JP Morgan monthly prices
df_jp <- subset(df_d,select=c("Date","jpm"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_jp$Date <- as.Date(df_d$Date)
gg_jp <- ggplot(data = df_jp, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=jpm))+scale_fill_gradient(low='white',high ='dodgerblue4')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for JP Morgan")+theme(axis.text.x = element_text(angle=330))

#JP Morgan return
return_jp <- subset(re_d,select=c("Date","jpm"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_jp$Date <- as.Date(re_d$Date)
gg_jp_d <- ggplot(data = return_jp, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=jpm))+scale_fill_gradient(low='white',high = 'dodgerblue4')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for JP Morgan")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_jp,gg_jp_d,ncol=2)

#Microsoft monthly prices
df_ms <- subset(df_d,select=c("Date","msft"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_ms$Date <- as.Date(df_d$Date)
gg_ms <- ggplot(data = df_ms, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=msft))+scale_fill_gradient(low='white',high ='deepskyblue4')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for Microsoft")+theme(axis.text.x = element_text(angle=330))

#Microsoft return
return_ms <- subset(re_d,select=c("Date","msft"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_ms$Date <- as.Date(re_d$Date)
gg_ms_d <- ggplot(data = return_ms, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=msft))+scale_fill_gradient(low='white',high = 'deepskyblue4')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for Microsoft")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_ms,gg_ms_d,ncol=2)

#Target monthly prices
df_tg <- subset(df_d,select=c("Date","tgt"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_tg$Date <- as.Date(df_d$Date)
gg_tg <- ggplot(data = df_tg, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=tgt))+scale_fill_gradient(low='white',high ='firebrick3')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for Target")+theme(axis.text.x = element_text(angle=330))

#Target return
return_tg <- subset(re_d,select=c("Date","tgt"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_tg$Date <- as.Date(re_d$Date)
gg_tg_d <- ggplot(data = return_tg, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=tgt))+scale_fill_gradient(low='white',high = 'firebrick3')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for Target")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_tg,gg_tg_d,ncol=2)

#Target monthly prices
df_wmt <- subset(df_d,select=c("Date","wmt"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
df_wmt$Date <- as.Date(df_d$Date)
gg_wmt <- ggplot(data = df_wmt, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=wmt))+scale_fill_gradient(low='white',high ='cornflowerblue')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Monthly price for Walmart")+theme(axis.text.x = element_text(angle=330))

#Walmart return
return_wmt <- subset(re_d,select=c("Date","wmt"))%>%mutate(month=month(Date))%>%mutate(year=year(Date))
return_wmt$Date <- as.Date(re_d$Date)
gg_wmt_d <- ggplot(data = return_wmt, aes(x = as.factor(month), y = as.factor(year))) +
  geom_tile(aes(fill=wmt))+scale_fill_gradient(low='white',high = 'cornflowerblue')+scale_x_discrete(labels=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sep','Oct','Nov','Dec'))+scale_y_discrete(labels=c(2013,2014,2015,2016,2017,2018))+ylab("Year")+xlab("Month")+labs(title="Return for Walmart")+theme(axis.text.x = element_text(angle=330))

grid.arrange(gg_wmt,gg_wmt_d,ncol=2)


## Histograms
#Histograms for 10 assets
#histogram for Apple
h_a <- ggplot(data=return_a,aes(x=aapl))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='dark green',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='dark green',fill="chartreuse3")+xlab("Monthly return for Apple")
#histogram for Amazon
h_az <- ggplot(data=return_az,aes(x=amzn))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='dark orange',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='dark orange',fill="chocolate1")+xlab("Monthly return for Amazon")
#histogram for Bank of America
h_ba <- ggplot(data=return_ac,aes(x=bac))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='dark red',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='dark red',fill="darksalmon")+xlab("Monthly return for Bank of America")
#histogram for Facebook
h_fb <- ggplot(data=return_fb,aes(x=fb))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='steel blue',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='steel blue',fill="lightblue1")+xlab("Monthly return for Facebook")
#histogram for Google
h_gg <- ggplot(data=return_gg,aes(x=goog))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='dark blue',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='dark blue',fill="lightsteelblue1")+xlab("Monthly return for Google")
#histogram for IBM
h_ib <- ggplot(data=return_ib,aes(x=ibm))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='black',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='black',fill="grey")+xlab("Monthly return for IBM")
#histogram for JP Morgan
h_jp <- ggplot(data=return_jp,aes(x=jpm))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='dodgerblue4',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='dodgerblue4',fill="steelblue1")+xlab("Monthly return for JP Morgan")
#histogram for Microsoft
h_ms <- ggplot(data=return_ms,aes(x=msft))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='deepskyblue4',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='deepskyblue4',fill="powderblue")+xlab("Monthly return for Microsoft")
#histogram for Target
h_tgt <- ggplot(data=return_tg,aes(x=tgt))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='firebrick3',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='firebrick3',fill="indianred2")+xlab("Monthly return for Target")
#histogram for Walmart
h_wmt <- ggplot(data=return_wmt,aes(x=wmt))+geom_histogram(aes(y=..density..),binwidth = 0.01,color='cornflowerblue',fill='white',alpha=0.5)+
  geom_density(alpha=.2, color='cornflowerblue',fill="aliceblue")+xlab("Monthly return for Walmart")
grid.arrange(h_a,h_az,ncol=2)
grid.arrange(h_ba,h_fb,ncol=2)
grid.arrange(h_gg,h_ib,ncol=2)
grid.arrange(h_jp,h_ms,ncol=2)
grid.arrange(h_tgt,h_wmt,ncol=2)

## Boxplots
m1 <- melt(return)
ggplot(data=m1,aes(x=Var2,y=value,color=Var2,fill=Var2))+geom_boxplot(alpha=0.5,outlier.color='red',outlier.shape = 19)+xlab("Asset names")+scale_x_discrete(labels=c("Apple","Amazon","Bank of America","Facebook","Google","IBM","JP Morgan","Microsoft","Target","Walmart"))+ylab("Returns")


## QQ Plot
#qqplot for apple
q_a <- ggplot(data.frame(return), aes(sample=aapl))+stat_qq(color='dark green')+labs(title="QQ plot for Apple")+theme_minimal()
#qqplot for amazon
q_az <- ggplot(data.frame(return), aes(sample=amzn))+stat_qq(color='dark orange')+labs(title="QQ plot for Amazon")+theme_minimal()
#qqplot for BOA
q_ac <-ggplot(data.frame(return), aes(sample=bac))+stat_qq(color='dark red')+labs(title="QQ plot for Bank of America")+theme_minimal()
#qqplot for Facebook
q_fb <- ggplot(data.frame(return), aes(sample=fb))+stat_qq(color='steel blue')+labs(title="QQ plot for Facebook")+theme_minimal()
#qqplot for Google
q_gg <- ggplot(data.frame(return), aes(sample=goog))+stat_qq(color='dark blue')+labs(title="QQ plot for Google")+theme_minimal()
#qqplot for IBM
q_ib <- ggplot(data.frame(return), aes(sample=ibm))+stat_qq()+labs(title="QQ plot for IBM")+theme_minimal()
#qqplot for JP Morgan
q_jp <- ggplot(data.frame(return), aes(sample=jpm))+stat_qq(color='dodgerblue4')+labs(title="QQ plot for JP Morgan")+theme_minimal()
#qqplot for Microsoft
q_ms <- ggplot(data.frame(return), aes(sample=msft))+stat_qq(color='deepskyblue4')+labs(title="QQ plot for Microsoft")+theme_minimal()
#qqplot for Target
q_tg <- ggplot(data.frame(return), aes(sample=tgt))+stat_qq(color='firebrick3')+labs(title="QQ plot for Target")+theme_minimal()
#qqplot for Walmart
q_wmt <- ggplot(data.frame(return), aes(sample=wmt))+stat_qq(color='cornflowerblue')+labs(title="QQ plot for Walmart")+theme_minimal()
grid.arrange(q_a,q_az,ncol=2)
grid.arrange(q_ac,q_fb,ncol=2)
grid.arrange(q_gg,q_ib,ncol=2)
grid.arrange(q_jp,q_ms,ncol=2)
grid.arrange(q_tg,q_wmt,ncol=2)

## Pairwise scatter plots
#Construct pairwise scatter plots for 10 assets.
ggpairs(data=data.frame(return))


## Covariance matrix
datatable(round(cov(return),5),caption = "Table 2: Sample covariance matrix on the returns",class="cell-border stripe")
