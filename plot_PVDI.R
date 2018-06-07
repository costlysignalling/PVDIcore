#Just set the simulation run values

gens<-100 		#Number of generations
optimum<-120	#Optimum trait value
pop.size<-800	#Population size
selecta<-0.2	#Ratio of surviving individuals
ni<-1			#Relative offspring variability

runs<-1

for(play in 1:runs){

###This will caculate the vales, taht enter the model from the set of parameters
(pick<-selecta*pop.size)
(pairs<-1:(pick/2))
(pair.generate<-pop.size/(pick/2))
(pair.floor<-floor(pair.generate))
(done<-pair.floor*length(pairs))
(extra<-(pop.size-pair.floor*length(pairs)))


###Starting population (now homogeneous pop with the mean trait value = 10, and one exceptional individual with trait value 11)
population<-c(11,rep(10,(pop.size-1)))

generation<-population

means<-NA
sds<-NA
first.opt<-NA
is.it<-0

means[1]<-mean(generation)
sds[1]<-sd.dist(generation)

if(round(pair.generate)==pair.generate){
for(i in 2:gens){
generation<-new.generation.straight(generation,ni,optimum,pick,pairs,pair.generate,pair.floor,done,extr)
average<-mean(generation)
deviation<-sd.dist(generation)
means[i]<-average
sds[i]<-deviation
if(is.it==0){
if(sum(generation>=optimum)>0){
is.it<-is.it+1
first.opt<-i
}
}
}
}else{
for(i in 2:gens){
generation<-new.generation.fill(generation,ni,optimum,pick,pairs,pair.generate,pair.floor,done,extra)
average<-mean(generation)
deviation<-sd.dist(generation)
means[i]<-average
sds[i]<-deviation
if(is.it==0){
if(sum(generation>=optimum)>0){
is.it<-is.it+1
first.opt<-i
}
}
}
}


relative.means<-means/optimum
relative.sds<-sds/optimum
CV<-sds/means

#Just make the plot

plotni<-function(){
plot(1:gens,relative.means, mgp=c(2,0.8,0),type="n", ylim=c(0,1.2), xaxs="i", yaxs="i", xlab="Generation",ylab="Trait value relative to the optimum", )
title(main="a.", adj=0,)
polygon(c(1:length(means), rev(1:length(means))),c(relative.means+2*relative.sds, rev(relative.means-2*relative.sds)),col = "#FF9999", border = NA)
lines(relative.means+2*relative.sds, type="l", lwd=2, col="#FF6666")
lines(relative.means-2*relative.sds, type="l", lwd=2, col="#FF6666")
lines(relative.means, type="l", lwd=2, col="#FF0000")
lines(CV, lwd=2, col="#0000FF")
abline(v=first.opt, lwd=2, col="#00CC00")
text(x=gens/30, y=c(1.1,1,0.9),adj=0, c(expression(paste(nu)),expression(paste(s)),expression(paste(N))))
text(x=gens/30+4, y=c(1.1,1,0.9),adj=0, c(paste("=",ni),paste("=",selecta),paste("=",pop.size)))

#If you want to plot a legent, just destroy the hashtags

#legend(x=gens/2.8,y=0.4,
#	c("Mean trait value","±2 standard deviations","Coefficient of variation","First individual reaches the optimum"),
#	lwd=2, col=c("#FF0000","#FF6666","#0000FF","#00CC00"))
}

#Plot it for immediate inspection
dev.new(width=7, height=4)
plotni()

#Save the picture to the working directory
png(filename=paste(play,".png",sep=""), 
    units="in", 
    width=7, 
    height=4, 
    res=200)
plotni()
dev.off()

}


