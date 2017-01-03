sns<-read.csv("snsdata.csv")
length(sns[is.na(sns$age)])
table(sns$gradyear)
n2006<-sns[sns$gradyear==2006,]
n2007<-sns[sns$gradyear==2007,]
n2008<-sns[sns$gradyear==2008,]
n2009<-sns[sns$gradyear==2009,]

nrow(n2006[is.na(n2006$age),])
nrow(n2007[is.na(n2007$age),])
nrow(n2008[is.na(n2008$age),])
nrow(n2009[is.na(n2009$age),])

n1<-nrow(sns[sns$gender=="M",])
n2<-nrow(sns[sns$gender=="F",])
n3<-nrow(sns[is.na(sns$gender),])
n<-nrow(sns)
n1/n
n2/n
n3/n

m2006<-mean(n2006[!is.na(n2006$age),]$age)
m2007<-mean(n2007[!is.na(n2007$age),]$age)
m2008<-mean(n2008[!is.na(n2008$age),]$age)
m2009<-mean(n2009[!is.na(n2009$age),]$age)
sns[is.na(sns$age)&(sns$gradyear==2006),]$age<-m2006
sns[is.na(sns$age)&(sns$gradyear==2007),]$age<-m2007
sns[is.na(sns$age)&(sns$gradyear==2008),]$age<-m2008
sns[is.na(sns$age)&(sns$gradyear==2009),]$age<-m2009
nrow(sns[is.na(sns$age),])

#重新加载
sns<-read.csv("snsdata.csv")
f<-function(x){
x<-x[!is.na(x)]
res<-c(min(x),median(x),max(x),quantile(x,probs=0.2))
names(res)<-c("min","median","max","20%")
res
}
f(sns$age)

sns<-read.csv("snsdata.csv")
boxplot(sns$age,outline=T)
res<-boxplot(sns$age,outline=F)
res$out
sns.new<-sns[!sns$age%in%res$out,]
f(sns.new$age)



#取数据
sns<-read.csv("snsdata.csv")
data<-sns[1:8,]
data<-data[,-c(1,2,3,4)]
#计算
d<-dist(data,method="binary")

#取数据
sns<-read.csv("snsdata.csv")
sns.new<-sns[!(is.na(sns$gender)|is.null(sns$gender)),]
sns.new<-sns.new[!(is.na(sns$age)|is.null(sns$age)),]
sns.new<-sns.new[1:1000,]
sns.new<-sns.new[,-c(1,2,3,4)]
#计算系数矩阵
d<-dist(sns.new,method="binary")


hc<-hclust(d,"single")
table(cutree(hc,k=2))



smarket<-read.csv("Smarket.csv")
smarket$Direction<-ifelse(smarket$Today>0,"up","down")
smarket$Direction<-as.factor(smarket$Direction)


sub<-smarket[smarket$Year!=2001,]
table(sub$Year)
attach(sub)
sub.g<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family=binomial)
summary(sub.g)
coef(sub.g)


sub.step<-step(sub.g)
summary(sub.step)

sub2<-smarket[smarket$Year==2001,]
pre<-predict(sub.g,newdata=sub2)
p<-exp(pre)/(1+exp(pre))
pred<-ifelse(p>0.5,"up","down")
pred<-as.factor(pred)
pred

sub2.d<-sub2$Direction
con<-table(pred,sub2.d)
error<-(con[2]+con[3])/sum(con);error

#取数据
se<-iris[iris$Species=="setosa",]
vir<-iris[iris$Species=="virginica",]
iris.new<-rbind(se,vir)
#判别
attach(iris.new)
library(MASS)
iris.lda<-lda(Species~Sepal.Length+Sepal.Width)
iris.lda


#确定图的横纵轴区间
min(se$Sepal.Length)
min(vir$Sepal.Length)
max(se$Sepal.Length)
max(vir$Sepal.Length)

min(se$Sepal.Width)
min(vir$Sepal.Width)
max(se$Sepal.Width)
max(vir$Sepal.Width)
#画图
plot(se$Sepal.Length~se$Sepal.Width,pch=16,xlab="Sepal.Width",ylab=c("Sepal.Length"),col="black",xlim=c(2,5),ylim=c(4,8),main=("setosa(black) vs. virginica(red)"))
points(vir$Sepal.Length~vir$Sepal.Width,pch=16,col="red")

iris.c<-coefficients(iris.lda)
slope<--iris.c[1]/iris.c[2];slope
abline(2.8,slope,col=3,lw=2,lty=2)
abline(2.8,slope,lty=2)








