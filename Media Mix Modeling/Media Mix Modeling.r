setwd("D:/2nd Qtr Study Material/Marketing Analytics/Lecture 3 Media Mix Modeling/Lecture 3 Media Mix Modeling")
#Install required packages
#install.packages("stargazer")
#install.packages("lmtest")
library(stargazer)
library(ggplot2)
library(graphics)
library(lmtest)

data = read.csv("sales_ads_use.csv", header = TRUE) #read data

stargazer(data, type="text", median=TRUE, iqr=TRUE, digits=2, title="Descriptive Statistics") #print summary statistics  
#can also use 'psych' package to describe(data) and determine if there is any skewness or not. Look at skew, if it is close to 0, its chill. If its greater than 0, try log normalizing it.
hist(data$sales)

#======================= Basic Regression ===========================
res1 = lm(sales ~ newspaper + magazine + radio + tv ,data = data)

stargazer(res1,
          title="Regression Results", type="text", 
          column.labels=c("Basic Model"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))

#======================= Lagged Variables ===========================

Lag_sales = data[-1, "sales"] #make dependent variable lag

X = data[-1 ,] #remove top row of independent variables. THIS IS NOT THE LAGGED X!
X = X[c('newspaper','magazine','radio','tv')]  #take only these columns

Lag_X = data[1:(nrow(data)-1), ] #make independent variables lag by removing last row
Lag_X = Lag_X[c('newspaper','magazine','radio','tv')] #take only these columns

#rename lagged X variables
names(Lag_X)
names(Lag_X)[1] = "lag_newspaper"
names(Lag_X)[2] = "lag_magazine"
names(Lag_X)[3] = "lag_radio"
names(Lag_X)[4] = "lag_tv"
names(Lag_X)


df1 = cbind(Lag_X, Lag_sales, X) #merge data and lagged data

res2 = lm(Lag_sales ~ newspaper + lag_newspaper + magazine + lag_magazine + radio +  lag_radio + tv + lag_tv , data = df1)

stargazer(res1, res2,
          title="Regression Results", type="text", 
          column.labels=c("Basic Model", "Lagged Model"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))

#======================= Ad Stock Variables =========================

#function to create Ad Stock Scale
AdStockScale <- function(lambda, n) {
  r <- lambda^(seq_len(n)-1) 
  m <- matrix(rep(r,n),nrow=n)
  z <- matrix(0,nrow=n,ncol=n) 
  z[lower.tri(z,diag=TRUE)] <- m[row(m) <= (n+1-col(m))] 
  z
} 

X = as.matrix(data[, 4:7]) #extract 'newspaper' 'magazine' 'radio' 'tv' only

#create ad stock variable with lambda = 0.1
AdStockScale1 = AdStockScale(0.1, nrow(X))
GX1 = AdStockScale1 %*% X

#create ad stock variable with lambda = 0.5
AdStockScale5 = AdStockScale(0.5, nrow(X))
GX5 = AdStockScale5 %*% X

#create ad stock variable with lambda = 0.9
AdStockScale9 = AdStockScale(0.9, nrow(X))
GX9 = AdStockScale9 %*% X



df2 = as.data.frame(cbind(data$sales, GX1)) #merge sales with ad stock variables where lambda = 0.1
colnames(df2)[1] = "sales" #rename 1st column to 'sales'
res3 = lm(sales ~ newspaper + magazine + radio + tv , data = df2)


df2 = as.data.frame(cbind(data$sales, GX5)) #merge sales with ad stock variables where lambda = 0.5
colnames(df2)[1] = "sales" #rename 1st column to 'sales'
res4 = lm(sales ~ newspaper + magazine + radio + tv , data = df2)


df2 = as.data.frame(cbind(data$sales, GX9)) #merge sales with ad stock variables where lambda = 0.9
colnames(df2)[1] = "sales" #rename 1st column to 'sales'
res5 = lm(sales ~ newspaper + magazine + radio + tv , data = df2)

stargazer(res1, res3, res4, res5,
          title="Regression Results", type="text", 
          column.labels=c("Basic Model", "AdStock1 Model","AdStock5 Model ", "AdStock9 Model"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))

a = cbind(X[,1],GX1[,1],GX5[,1], GX9[,1]) 
colorseq <- c("black","red","blue") 
matplot(a,type="l",col=colorseq,lwd=2) 
legend('top',c("original data","AdStock-0.1","AdStock-0.5", "AdStock-0.9"),lty=1:3,col=colorseq,cex=.75)


#======================== Dummy Variables ===============================

#-------------One Dummy Variable------------------

df3 = data

df3$isSummer = factor(df3$month,
                         levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                         labels = c(0,0,0,0,0,1,1,1,0,0,0,0))
#df3$isSummer = ifelse((df3$month == 6) |(df3$month == 7) | (df3$month == 8) , 1,0)

res6 = lm(sales ~ newspaper + magazine + radio + tv + isSummer, data = df3)

stargazer(res6,
          title="Regression Results", type="text", 
          column.labels=c("1 Dummy Variable Model"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))

#-------------Three Dummy Variables---------------
#df3$isJune = ifelse(df3$month == 6 ,1,0)
#df3$isJuly = ifelse(df3$month == 7 ,1,0)
#df3$isAugust = ifelse(df3$month == 8 ,1,0)

df3$summerMonth = factor(df3$month,
                      levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c(0,0,0,0,0,1,2,3,0,0,0,0))

#res7 = lm(sales ~ newspaper + magazine + radio + tv + isJune + isJuly + isAugust, data = df3)
res7 = lm(sales ~ newspaper + magazine + radio + tv + summerMonth, data = df3)
stargazer(res6, res7,
          title="Regression Results", type="text", 
          column.labels=c("One-Dummy", "Three-Dummy"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))

#================= Nonlinear Transformation ====================

df1 = df1[,1:5]
df1$newspaper2 = df1$lag_newspaper^2
df1$magazine2 = df1$lag_magazine^2 
df1$radio2 = df1$lag_radio^2
df1$tv2 = df1$lag_tv^2

names(df1)
names(df1)[1] = "newspaper"
names(df1)[2] = "magazine"
names(df1)[3] = "radio"
names(df1)[4] = "tv"
names(df1)



res8 =  lm(Lag_sales ~ newspaper + newspaper2 + magazine + magazine2 + radio + radio2 + tv + tv2, data = df1)
stargazer(res1, res8,
          title="Regression Results", type="text", 
          column.labels=c("Basic Model", "Sq Terms"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))



df1$newspaper = log(df1$newspaper + 1)
df1$magazine = log(df1$magazine + 1)
df1$radio = log(df1$radio + 1)
df1$tv = log(df1$tv + 1)

df1$newspaper2 = log(df1$newspaper2 + 1)
df1$magazine2 = log(df1$magazine2 + 1)
df1$radio2 = log(df1$radio2 + 1)
df1$tv2 = log(df1$tv2 + 1)

res9 =  lm(Lag_sales ~ newspaper2 + magazine2 + radio2 + tv2, data = df1)

stargazer(res1, res8, res9,
          title="Regression Results", type="text", 
          column.labels=c("Basic Model", "Sq Terms", "Log-Trans Sq Terms"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))
