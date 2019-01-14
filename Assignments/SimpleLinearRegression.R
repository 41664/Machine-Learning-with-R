# I don't have notebooks during live classes 
# IMPORT DATASET
# Creating a script to execute Simple Linear Regressin 
dataset <- read.csv("Advertising.csv")
# Function names () accesses all column names of the dataset 
# Fuction head() and tail() work likewise 
# str- gives a structure of the dataset 
# Length() gives length
# INITIALIZATION
x<- dataset$TV
y<- dataset$Sales
# FINDING MEAN 
xmean<- mean(x)
ymean<- mean(y)
# FINDING COEFFICIENTS OF REGRESSION
nsum<-0
dsum<- 0
n<- length(x)
for(i in 1:n){
  nsum <- nsum + ((x[i]- xmean)*(y[i]- ymean))
  dsum <- dsum + (x[i] - xmean)^2 
}
b0<- nsum /dsum
b1 <- ymean - b0*xmean
# Now find y cap
n <- length(y)
ycap = list()
for( i in 1:n){
  ycap[i] = b0* x[i] + b1
}
# Plotiing
plot(x,y, col="blue", xlab="TV", ylab="Sales")
lines(x,ycap, col="red", lwd=2)
# Finding error terms 
# RSS
rss <- 0
for (i in 1:n){
rss=rss+ (y[i]-ycap[[i]])^2}
# RSE 
rse <- sqrt(rss / (n-2))
# Standard error for b0
seb0<-rse*rse * (1/n)
temp <- rse*rse * ( ( xmean * xmean) / dsum)
seb0 = seb0+ temp
rm(temp)
#Finding tss 
tss<- dsum
# Standard error for b1
seb1<- rse*rse
seb1 = seb1 / dsum
# Confidence interval 
print(b1 + 2* seb1)
print(b1 - 2*seb1)
print(b0 + 2*seb0)
print(b1 - 2*seb1)
#t statistic 
tb0 <- b0 / seb0
tb1 <- b1 / seb1
#R^2 statistic 
rsquare <- 1-(rss/tss)
# Summary table 
summaryfinal<- data.frame(c(b0,seb0,tb0),c(b1,seb1,tb1),row.names = c("Coefficients", "standard Error", "t-statistic"), axis(0))