##Inputs

#Rescale
reScale<-10
#This increases and decreases the residual.
#Will not change the coefficients
#Increasing will reduce r-sq, increase SSres, decrease MSres, decrease F-ratio, increase p


#SampleSize
sampleSize<-50
#Changes error degrees of freedom
#increase will increase DFerr, decrease SSres and MSres, increase F-ratio, decrease p


#RelStrength
gradient<-10
#changes strength of the relationship so will change the coefficient of X
#increase will increase SSmod and MSmod, increase F-ratio, decrease p

##Plots

par(mfrow=c(1,2))

#SetResiduals
set.seed(1212)
yRes<-rnorm(sampleSize,0,0.5)
yResDyn<-yRes*reScale

#Create a sequence of x values of required length
xVal<-seq(1,10,length.out=sampleSize)

#Generate the list of fitted y values given a known intercept and B1
yfit<-c()
for(i in xVal) {
  yi<-100+(gradient*i)
  yfit<-append(yfit,yi)
}

#For each x get the y that is removed by the equivalent residual
yVal<-c()
for (i in xVal) {
  yi<-100+(gradient*(i))+yResDyn[which(xVal==i)]
  yVal<-append(yVal,yi)
}

#run lm and anova
reg.lm<-lm(yVal~xVal)
reg.anova<-anova(reg.lm)
reg.anova

#Plot SSresidual
plot(xVal,reg.lm$fitted,type="l",ylim=c(25,200),xlab="X Value",ylab="Y Value",col="red")
points(xVal,yVal,pch=19,col="orange")
for (i in xVal) {
  lines(c(xVal[which(xVal==i)],xVal[which(xVal==i)]),c(yVal[which(xVal==i)],reg.lm$fitted[which(xVal==i)]))
}
text(3,30,paste("SS Res =",round(reg.anova$Sum[2],0)))
text(7,30,paste("df Res = ",(reg.anova$Df[2])))

#Plot SSregression
plot(xVal,reg.lm$fitted,type="l",ylim=c(25,200),xlab="X Value",ylab="Y Value",col="red")
abline(h=mean(yVal),lty="dotted",col="blue")
points(xVal,yVal,col="orange",pch=19)
for (i in xVal) {
  lines(c(xVal[which(xVal==i)],xVal[which(xVal==i)]),c(mean(yVal),reg.lm$fitted[which(xVal==i)]))
}
text(3,30,paste("SS Reg =",round(reg.anova$Sum[1],0)))
text(7,30,paste("df Res = ",(reg.anova$Df[1])))