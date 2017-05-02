chal_func<-function(x,conf.lev){
  z<-qnorm((1-conf.lev)/2,lower.tail=FALSE)
  mean<-mean(x)
  sd<-sd(x)
  conf.low<-mean-z*(sd/sqrt(length(x)))
  conf.up<-mean+z*(sd/sqrt(length(x)))
  res<-c(mean,sd,conf.low,conf.up)
  name<-c("Mean","Std. Deviation",paste(as.character((conf.lev*100)),"% conf. int. lower limit"),paste(as.character((conf.lev*100)),"% conf. int. upper limit"))
  names(res)<-name
  return(res)
}

set.seed(123)
vec <- runif(8999, 0.0, 1.0)
chal_func(vec,0.95)
chal_func(vec,0.90)