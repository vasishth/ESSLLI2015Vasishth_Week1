## ----setup,include=FALSE,cache=FALSE-------------------------------------
library(knitr)
library(coda)

# set global chunk options, put figures into folder
options(replace.assign=TRUE,show.signif.stars=FALSE)
opts_chunk$set(fig.path='figures/figure-', fig.align='center', fig.show='hold')
options(replace.assign=TRUE,width=75)
opts_chunk$set(dev='postscript')
#library(rstan)
#set.seed(9991)
# save workspace image, if you want
#the.date <- format(Sys.time(), "%b%d%Y")
#save.image(file=paste0("homework01-",the.date,".RData")

## ------------------------------------------------------------------------
sample <- rnorm(11,mean=12,sd=10)
(x_bar<-mean(sample))
(SE<-sd(sample)/sqrt(11))

## ----nullhyp,echo=FALSE,fig.height=4-------------------------------------
x<-seq(-20,20,by=0.1)
plot(x,dt(x,df=10),type="l",main="",
     ylab="density")
points(x_bar/SE,0,col="red",pch=20)
text(x=x_bar/SE,y=0.05,label="sample mean",col="red")

## ----tversusnorm,echo=FALSE,fig.height=4---------------------------------
range <- seq(-4,4,.01)  

op<-par(mfrow=c(2,2),pty="s")

op<-par(mar=c(2,2,3,2),pty="s")

 for(i in c(2,5,15,20)){
   plot(range,dnorm(range),type="l",lty=1,
        xlab="",ylab="",
        cex.axis=1,cex.axis=0.8)
   lines(range,dt(range,df=i),lty=2,lwd=1)
   mtext(paste("df=",i),cex=1.2)
 }

## ----nullhyprepeat,echo=FALSE,fig.height=4-------------------------------
x<-seq(-20,20,by=0.1)
plot(x,dt(x,df=10),type="l",main="t(n-1)",
     ylab="density")
points(x_bar/SE,0,col="red",pch=20)
text(x=x_bar/SE,y=0.01,label="sample mean",col="red")

lower<-qt(0.025,df=10)
upper<-qt(0.975,df=10)
abline(v=lower)
abline(v=upper)

x1 <- seq(upper,20,abs(0.975)/5)
y1 <- dt(x1,df=10)
polygon(c(x1, rev(x1)), 
        c(rep(0, length(x1)), rev(y1)), 
        col = gray(0.3))

x1 <- seq(-20,lower,abs(0.975)/5)
y1 <- dt(x1,df=10)
polygon(c(x1, rev(x1)), 
        c(rep(0, length(x1)), rev(y1)), 
        col = gray(0.3))


## ------------------------------------------------------------------------
abs(qt(0.025,df=15))
abs(qt(0.025,df=50))

## ------------------------------------------------------------------------
## null hypothesis mean:
mu<-0
(t_value<-(x_bar-mu)/SE)

## ----probttest-----------------------------------------------------------
pt(-2,df=10)
pt(-2,df=20)
pt(-2,df=50)

## ------------------------------------------------------------------------
## from t-test function:
## t-value
t.test(sample)$statistic

## ----echo=FALSE,fig.height=4---------------------------------------------
## function for plotting CDF:
plot.prob<-function(x,
                           x.min,
                           x.max,
                           prob,
                           mean,
                           sd,
                           gray.level,main){

        plot(x,dnorm(x,mean,sd), 
                     type = "l",xlab="",
             ylab="",main=main)
        abline(h = 0)

## shade X<x    
    x1 = seq(x.min, qnorm(prob), abs(prob)/5)
    y1 = dnorm(x1, mean, sd)

    polygon(c(x1, rev(x1)), 
            c(rep(0, length(x1)), rev(y1)), 
            col = gray.level)
  }

shadenormal<- 
function (prob=0.5,
          gray1=gray(0.3),
          x.min=-6,
          x.max=abs(x.min),
          x = seq(x.min, x.max, 0.01),
          mean=0,
          sd=1,main="P(X<0)") 
{

     plot.prob(x=x,x.min=x.min,x.max=x.max,
               prob=prob,
                      mean=mean,sd=sd,
     gray.level=gray1,main=main)     
}
shadenormal(prob=0.025,main="Type I, II error")

x1 <- seq(qnorm(0.975),6,abs(0.975)/5)
y1 <- dnorm(x1)

polygon(c(x1, rev(x1)), 
        c(rep(0, length(x1)), rev(y1)), 
        col = gray(0.3))

x<-seq(-6,6,by=0.1)
lines(x,dnorm(x,mean=2),col="red",lwd=2)
abline(v=2)
abline(v=-2)

x1 <- seq(-2,2,0.01)
y1 <- dnorm(x1,mean=2)

polygon(c(x1, rev(x1)), 
        c(rep(0, length(x1)), rev(y1)), 
        col = "orange")


## ------------------------------------------------------------------------
## Sampling from Normal(0,1)
(sample<-rnorm(10))

## ------------------------------------------------------------------------
n<-length(sample)
x_bar<-mean(sample)
stddev<-sd(sample)
(t_value<- (x_bar - 0)/(stddev/sqrt(n)))

## ------------------------------------------------------------------------
pt(-abs(t_value),df=n-1)

## ------------------------------------------------------------------------
pt(abs(t_value),df=n-1,lower.tail=FALSE)

## ------------------------------------------------------------------------
2*pt(-abs(t_value),df=n-1)

## ------------------------------------------------------------------------
2*pt(-abs(t_value),df=n-1)

## ------------------------------------------------------------------------
t.test(sample)

## ----cache=TRUE----------------------------------------------------------
nsim<-10000
n<-10
pvals<-rep(NA,nsim)
for(i in 1:nsim){
  x<-rnorm(n)
  pvals[i]<-t.test(x)$p.value
}
mean(pvals<0.05)

## ----typesandm,cache=TRUE,echo=TRUE--------------------------------------
## probable effect size derived from past studies:
D<-15
## SE from the study of interest:
se<-46
stddev<-se*sqrt(37)
nsim<-10000
drep<-rep(NA,nsim)
for(i in 1:nsim){
drep[i]<-mean(rnorm(37,mean=D,sd=stddev))
}

## ----typesandm2,cache=TRUE,echo=TRUE-------------------------------------
##power:
(pow<-mean(ifelse(abs(drep/se)>2,1,0)))

## ----typesandm3,cache=TRUE,echo=TRUE-------------------------------------
## which cells in drep are significant at alpha=0.05?
signif<-which(abs(drep/se)>2)

## Type S error rate | signif:
(types_sig<-mean(drep[signif]<0))
## Type S error rate | non-signif: 
(types_nonsig<-mean(drep[-signif]<0))


## ----typesandm4,cache=TRUE,echo=TRUE-------------------------------------
## Type M error rate | signif: 
(typem_sig<-mean(abs(drep[signif])/D))
## Type M error rate | not-signif: 
(typem_nonsig<-mean(abs(drep[-signif])/D))

## ----funnelplot,echo=FALSE,fig.height=4,cache=TRUE-----------------------
## funnel plot:
truemu<-15
se<-46
sampsize<-seq(10,10000,by=1)
n_expts<-length(sampsize)
means<-power<-rep(NA,n_expts)
SE<-46
for(i in 1:n_expts){
  x<-rnorm(sampsize[i],mean=truemu,
                       sd=se)
  means[i]<-mean(x)
  power[i]<-power.t.test(d=truemu,sd=250,
                      n=sampsize[i])$power
}

plot(means,power,
     main="Funnel plot",
     xlim=range(c(min(means),max(means))),
     xlab="effect",ylab="power",
     #ylim=c(0,0.003),
     cex.axis=0.8,cex.main=0.8)
abline(v=15)
abline(h=0.20,col="red")

## ----stoppingrules-------------------------------------------------------
##Standard:
pvals<-NULL
tstat_standard<-NULL
n<-10
nsim<-10000
## assume a standard dev of 1:
stddev<-1
mn<-0
for(i in 1:nsim){
  samp<-rnorm(n,mean=mn,sd=stddev)
  pvals[i]<-t.test(samp)$p.value
  tstat_standard[i]<-t.test(samp)$statistic
}


## ------------------------------------------------------------------------
## Type I error rate: about 5% as theory says:
table(pvals<0.05)[2]/nsim

## ----stoppingrules2------------------------------------------------------
pvals<-NULL
tstat<-NULL
## how many subjects can I run?
upper_bound<-n*6

## ------------------------------------------------------------------------
for(i in 1:nsim){
  significant<-FALSE 
  x<-rnorm(n,mean=mn,sd=stddev) ## take sample
while(!significant & length(x)<upper_bound){
  ## if not significant:
if(t.test(x)$p.value>0.05){
  x<-append(x,rnorm(n,mean=mn,sd=stddev)) ## get more data
} else {significant<-TRUE}   ## otherwise stop:
}
pvals[i]<-t.test(x)$p.value
tstat[i]<-t.test(x)$statistic
}

## ------------------------------------------------------------------------
## Type I error rate: much higher than 5%:
table(pvals<0.05)[2]/nsim

## ----stoppingrules3,echo=FALSE,fig.height=4------------------------------
plot(density(tstat_standard),main="",xlab="")
lines(density(tstat),col="red")
#hist(tstat_standard,
#main="The t-distributions for the standard case (white) \n
#     vs the stopping rule (gray)",freq=F)
#hist(tstat,add=T,col="#0000ff22",freq=F)

## ------------------------------------------------------------------------
heights <- c(173,174,160,157,158,170,172,170,
            175,168,165,170,173,180,168,162,
            180,160,155,163,173,175,176,172,
            160,161,150,170,165,184,165)

## ------------------------------------------------------------------------
t.test(heights,mu=170)

## ----echo=FALSE,fig.width=4----------------------------------------------
range <- seq(-4,4,.01)  
 
op<-par(mfrow=c(2,2),pty="s")

 for(i in c(2,5,15,20)){
   plot(range,dnorm(range),type="l",lty=1,
        xlab="",ylab="",
        cex.axis=1)
   lines(range,dt(range,df=i),lty=2,lwd=1)
   mtext(paste("df=",i),cex=1.2)
 }

## ------------------------------------------------------------------------
(observed_t<-(mean(heights)-170)/(sd(heights)/sqrt(31)))

## ------------------------------------------------------------------------
2*pt(observed_t,df=30)

## ------------------------------------------------------------------------
2*(pnorm(mean(heights),mean=170,sd=sd(heights)/sqrt(30)))

## ------------------------------------------------------------------------
F1data<-read.table("data/F1_data.txt",header=TRUE)
head(F1data)

## ------------------------------------------------------------------------
t.test(F1data$female,F1data$male,var.equal=TRUE)

## ------------------------------------------------------------------------
d<-mean(F1data$female)-mean(F1data$male)
(SE<-sqrt(var(F1data$male)/19+var(F1data$female)/19))
observed_t <- (d-0)/SE
2*(1-pt(observed_t,df=36))

## ------------------------------------------------------------------------
diff<-F1data$female-F1data$male
t.test(diff)

## ------------------------------------------------------------------------
(SE<-sqrt(var(diff)/19))
(observed_t <- (mean(d)-0)/SE)
2*(1-pt(observed_t,df=18))

## ------------------------------------------------------------------------
dataN2<-read.table("data/dataN2.txt",header=T)
head(dataN2)

## ------------------------------------------------------------------------
## significant effect:
with(dataN2,
t.test(N2_dur.2,N2_dur.1,paired=TRUE))

## ------------------------------------------------------------------------
N2dur1data<-data.frame(item=dataN2$Sentence,
                          subj=dataN2$Speaker_id,
                          cond="a",
                          dur=dataN2$N2_dur.1)
N2dur2data<-data.frame(item=dataN2$Sentence,
                          subj=dataN2$Speaker_id,
                          cond="b",
                          dur=dataN2$N2_dur.2)

N2data<-rbind(N2dur1data,N2dur2data)

## ------------------------------------------------------------------------
head(N2data)

## ------------------------------------------------------------------------
N2data_bysubj<-aggregate(dur~subj+cond,mean,
               data=N2data)

## ------------------------------------------------------------------------
conda<-subset(N2data_bysubj,cond=="a")
condb<-subset(N2data_bysubj,cond=="b")

## ------------------------------------------------------------------------
## not significant:
t.test(condb$dur,conda$dur,paired=TRUE)

## ------------------------------------------------------------------------
## alternative syntax:
t.test(dur~cond,paired=TRUE,N2data_bysubj)

## ------------------------------------------------------------------------
head(N2data)

## ------------------------------------------------------------------------
## STEP 1: Aggregate over items:
#N2data_byitem<-aggregate(dur~item+cond,mean,
#               data=N2data)

## ------------------------------------------------------------------------
## STEP 2: Create a vector for condition a and b:
#conda<-subset(N2data_byitem,cond=="a")
#condb<-subset(N2data_byitem,cond=="b")
#conda<-...
#condb<-...
## Do a by subject paired t-test:
#t.test(condb$dur,conda$dur,paired=TRUE)

