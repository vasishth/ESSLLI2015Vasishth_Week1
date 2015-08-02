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

## ----forloops,cache=TRUE-------------------------------------------------
## number of iterations:
nsim<-10
## vector for storing results:
results<-rep(NA,10)
for(i in 1:nsim){
  results[i]<-1+2*i
}
results

## ----calculatecdfbinomial,include=TRUE,echo=TRUE,cache=TRUE--------------
## sample size
n<-10
## prob of success
p<-0.5
probs<-rep(NA,11)
for(x in 0:10){
  ## Cumulative Distribution Function:
probs[x+1]<-round(pbinom(x,size=n,prob=p),digits=2)
}

## ----echo=FALSE----------------------------------------------------------
xtabl.probs<-data.frame(x=0:10,prob=probs)

## ----cdfbinomial,include=TRUE,echo=TRUE,cache=TRUE,fig.width=4,fig.height=3,out.width='0.75\\textwidth'----
## Plot the CDF:
plot(1:11,probs,xaxt="n",xlab="x",
     ylab=expression(P(X<=x)),main="CDF")
axis(1,at=1:11,labels=0:10)

## ------------------------------------------------------------------------
## using cdf:
pbinom(1,size=10,prob=0.5)-pbinom(0,size=10,prob=0.5)
## using pmf:
choose(10,1) * 0.5 * (1-0.5)^9

## ----binomcomputePequalszero---------------------------------------------
## P(X=1)
choose(10,1) * 0.5 * (1-0.5)^9
## using the built-in function:
dbinom(1,size=10,prob=0.5)

## ----pdfbinomial,include=TRUE,echo=TRUE,cache=TRUE,fig.width=4,fig.height=3,out.width='0.75\\textwidth'----
## Plot the pmf:
plot(1:11,dbinom(0:10,size=10,prob=0.5),main="PMF",
     xaxt="n",ylab="P(X=x)",xlab="x")
axis(1,at=1:11,labels=0:10)

## ----pdfnormal,include=TRUE,echo=TRUE,cache=TRUE,fig.width=4,fig.height=3,out.width='0.75\\textwidth'----
plot(function(x) dnorm(x), -3, 3,
      main = "Normal density",ylim=c(0,.4),
              ylab="density",xlab="X")

## ----shadenormal,echo=FALSE----------------------------------------------
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

## ----plotshadenormal,include=TRUE,echo=FALSE,cache=TRUE,fig.width=4,fig.height=3,out.width='0.75\\textwidth'----
shadenormal(prob=0.975,main="P(X<1.96)")

## ----computingprobsnormal------------------------------------------------
## The area under curve between +infty and -infty:
pnorm(Inf)-pnorm(-Inf)
## The area under curve between 2 and -2:
pnorm(2)-pnorm(-2)
## The area under curve between 1 and -1:
pnorm(1)-pnorm(-1)

## ------------------------------------------------------------------------
qnorm(0.975,mean=500,sd=100)

## ----pnormtwo------------------------------------------------------------
pnorm(2)

## ----negativexpnorm------------------------------------------------------
1-pnorm(2)
## alternatively:
pnorm(2,lower.tail=F)

## ------------------------------------------------------------------------
(x<-rbinom(5,size=10,prob=0.5))

## ----likfun0,echo=TRUE,fig.width=6---------------------------------------
## probability parameter fixed at 0.5
theta<-0.5
prod(dbinom(x,size=10,prob=theta))
## probability parameter fixed at 0.1
theta<-0.1
prod(dbinom(x,size=10,prob=theta))

## ----likfun,echo=TRUE----------------------------------------------------
theta<-seq(0,1,by=0.01)
store<-rep(NA,length(theta))
for(i in 1:length(theta)){
store[i]<-prod(dbinom(x,size=10,prob=theta[i]))
}

## ----likfunplot,echo=FALSE,fig.height=4----------------------------------
plot(1:length(store),store,xaxt="n",xlab="theta",
     ylab="f(x1,...,xn|theta")
axis(1,at=1:length(theta),labels=theta)

## ------------------------------------------------------------------------
x<-rnorm(100,mean=500,sd=50)
mean(x)
x<-rnorm(100,mean=500,sd=50)
mean(x)

## ------------------------------------------------------------------------
nsim<-1000
n<-100
mu<-500
sigma<-100
samp_distrn_means<-rep(NA,nsim)
samp_distrn_sd<-rep(NA,nsim)
for(i in 1:nsim){
  x<-rnorm(n,mean=mu,sd=sigma)
  samp_distrn_means[i]<-mean(x)
  samp_distrn_sd[i]<-sd(x)
}

## ----samplingdistrnmeans,echo=FALSE,fig.height=4-------------------------
hist(samp_distrn_means,main="Samp. distrn. means",
     freq=F,xlab=expression(hat(mu)),ylab="density")

## ------------------------------------------------------------------------
for(i in 1:nsim){
  x<-rexp(n)
  samp_distrn_means[i]<-mean(x)
  samp_distrn_sd[i]<-sd(x)
}

## ----samplingdistrnmeansexp,echo=FALSE,fig.height=4----------------------
hist(samp_distrn_means,main="Samp. distrn. means",
     freq=F,xlab=expression(hat(mu)),ylab="density")

## ------------------------------------------------------------------------
## estimate from simulation:
sd(samp_distrn_means)

## ------------------------------------------------------------------------
x<-rnorm(100,mean=500,sd=100)
hat_sigma<-sd(x)
hat_sigma/sqrt(n)

## ----confint1------------------------------------------------------------
## lower bound:
mu-(2*hat_sigma/sqrt(n))
## upper bound:
mu+(2*hat_sigma/sqrt(n))

## ----confint2------------------------------------------------------------
lower<-rep(NA,nsim)
upper<-rep(NA,nsim)
for(i in 1:nsim){
  x<-rnorm(n,mean=mu,sd=sigma)
  lower[i]<-mean(x) - 2 * sd(x)/sqrt(n)
  upper[i]<-mean(x) + 2 * sd(x)/sqrt(n)
}

## ----confint3------------------------------------------------------------
## check how many CIs contain mu:
CIs<-ifelse(lower<mu & upper>mu,1,0)
table(CIs)
## approx. 95% of the CIs contain true mean:
table(CIs)[2]/sum(table(CIs))

## ----repeatedCIsplot,echo=FALSE,fig.height=4-----------------------------
se <- function(x)
      {
        y <- x[!is.na(x)] # remove the missing values, if any
        sqrt(var(as.vector(y))/length(y))
}
ci <- function (scores){
m <- mean(scores,na.rm=TRUE)
stderr <- se(scores)
len <- length(scores)
upper <- m + qt(.975, df=len-1) * stderr 
lower <- m + qt(.025, df=len-1) * stderr 
return(data.frame(lower=lower,upper=upper))
}
lower <- rep(NA,100)
upper <- rep(NA,100)

for(i in 1:100){ 
  sample <- rnorm(100,mean=60,sd=4)
  lower[i] <- ci(sample)$lower
  upper[i] <- ci(sample)$upper
}
  
cis <- cbind(lower,upper)

store <- rep(NA,100)

pop.mean<-60
pop.sd<-4

for(i in 1:100){ 
  sample <- rnorm(100,mean=pop.mean,sd=pop.sd)
  lower[i] <- ci(sample)$lower
  upper[i] <- ci(sample)$upper
  if(lower[i]<pop.mean & upper[i]>pop.mean){
    store[i] <- TRUE} else {
      store[i] <- FALSE}
}

## need this for the plot below:
cis <- cbind(lower,upper)

main.title<-"95% CIs in 100 repeated samples"

line.width<-ifelse(store==FALSE,2,1)
cis<-cbind(cis,line.width)
x<-0:100
y<-seq(55,65,by=1/10)
plot(x,y,type="n",xlab="i-th repeated sample",
     ylab="Scores",main=main.title)
abline(60,0,lwd=2)
x0<-x
x1<-x
arrows(x0,y0=cis[,1],
       x1,y1=cis[,2],length=0,lwd=cis[,3])

