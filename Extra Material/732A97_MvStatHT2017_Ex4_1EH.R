library(calibrate) ## for textxy
fsimdist<-function(i,j){
    absij<-abs(i-j)
    res<-NA
    if (absij==0){res<-9}
    if ((absij>=1)&&(absij<=3)){res<-8}
    if ((absij>=4)&&(absij<=6)){res<-7}
    if ((absij>=7)&&(absij<=9)){res<-6}
    if ((absij>=10)&&(absij<=12)){res<-5}
    if ((absij>=13)&&(absij<=15)){res<-4}
    if ((absij>=16)&&(absij<=18)){res<-3}
    if ((absij>=19)&&(absij<=21)){res<-2}
    if ((absij>=22)&&(absij<=24)){res<-1}
    if (absij >=25){res<-0}
    res
}

vOdata<-1:51

mD<-matrix(0,ncol=length(vOdata),nrow=length(vOdata))
for(i in vOdata){
    for (j in vOdata){
	mD[i,j]<-sqrt(fsimdist(i,i)+fsimdist(j,j)-2*fsimdist(i,j))
    }
}

fit2<-cmdscale(mD,k=2,eig=TRUE)
cumsum(fit2$eig^2)/(sum(fit2$eig^2))

## we get a horshoe effect
## http://www1.maths.leeds.ac.uk/~john/3772/5772-notes.pdf p25 bottom
## Sometimes a 2D solution can be a 1D solution in disguise. This is called the horseshoe
## effect. In our example, note that is is fairly easy for two objects to have a similarity of
## zero and hence a maximum distance of sqrt(2p). So in a large sample of objects, we can get
## a lot of “maximum distance” pairs, all tying. This means that once you hit a "threshold"
## can't get any more distant. This can result in our not being able to distinguish between
## "moderate" and "large" distances.
## The classic example of the horseshoe effect is due to Kendall (1971). This exampe is considered in this exercise
##
## Here, there is a very clear seriation. Any object can be well-ordered with respect to the
## objects "near" to it in the ordering. However, objects "further away" do not appear to
## be as far as they actually are, causing the plot to wrap round.

plot(fit2$points,pch=19,xlab="",ylab="")
textxy(fit2$points[,1],fit2$points[,2],1:51,cex=1)
