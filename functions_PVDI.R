###Just run the code ctrl+a, ctrl+r

###multi-parental funcion (nice SD function, not just the estimation)
sd.dist<-function(vector){
prumer<-mean(vector)
delka<-length(vector)
vysledek<-sqrt(sum((vector-prumer)^2)/(delka))
return(vysledek)
}


###function that will create a new generation from the old one
new.generation.fill<-function(population,ni,optimum,pick,pairs,pair.generate,pair.floor,done,extra){
	hand<-(population-optimum)^2
	sortpop<-population[order(hand)]
	survival<-sortpop[1:pick]
	pairing<-sample(rep(pairs,2))
	new.population<-1
	for(i in pairs){
	pair<-survival[pairing==i]
	new.population[((i-1)*pair.floor+1):((i-1)*pair.floor+pair.floor)]<-rnorm(pair.floor,mean(pair),ni*(abs(pair[1]-pair[2])/2)) ###tady by se mela pak ta vzdalenost zmenit na tu SD dist, ale bude to pomalejsi, jeste to testnu 
	}
	for(i in 1:extra){
	pair<-survival[pairing==i]
	new.population[done+i]<-rnorm(1,mean(pair),ni*(abs(pair[1]-pair[2])/2)) ###tady by se mela pak ta vzdalenost zmenit na tu SD dist, ale bude to pomalejsi, jeste to testnu 
	}
	return(new.population)
	}

new.generation.straight<-function(population,ni,optimum,pick,pairs,pair.generate,pair.floor,done,extra){
	hand<-(population-optimum)^2
	sortpop<-population[order(hand)]
	survival<-sortpop[1:pick]
	pairing<-sample(rep(pairs,2))
	new.population<-1
	for(i in pairs){
	pair<-survival[pairing==i]
	new.population[((i-1)*pair.floor+1):((i-1)*pair.floor+pair.floor)]<-rnorm(pair.floor,mean(pair),ni*(abs(pair[1]-pair[2])/2)) ###tady by se mela pak ta vzdalenost zmenit na tu SD dist, ale bude to pomalejsi, jeste to testnu 
	}
	return(new.population)
	}

###Simulation function (basicly iteration of the previous function)
simulation<-function(runs,pop.size, selecta, ni, population=c(11,rep(10,(pop.size-1))), optimum=120){

success<-0
fail<-0
chaos<-0

pick<-selecta*pop.size
pairs<-1:(pick/2)
pair.generate<-pop.size/(pick/2)
pair.floor<-floor(pair.generate)
done<-pair.floor*length(pairs)
extra<-(pop.size-pair.floor*length(pairs))

###set the starting population
for(run in 1:runs){
generation<-population


#loop it for instant simulation run
if(round(pair.generate)==pair.generate){
for(i in 1:10000){
generation<-new.generation.straight(generation,ni,optimum,pick,pairs,pair.generate,pair.floor,done,extra)
average<-mean(generation)
deviation<-sd.dist(generation)
if(average<0){
chaos<-chaos+1
break
}
if(deviation<0.0001){
if(round(average)==optimum){
success<-success+1
}else{
fail<-fail+1
}
break
}
}
}else{
for(i in 1:10000){
generation<-new.generation.fill(generation,ni,optimum,pick,pairs,pair.generate,pair.floor,done,extra)
average<-mean(generation)
deviation<-sd.dist(generation)
if(average<0){
chaos<-chaos+1
break
}
if(deviation<0.0001){
if(round(average)==optimum){
success<-success+1
}else{
fail<-fail+1
}
break
}
}
}

}

result<-c(success,fail,chaos)

return(result)
}


#############
###Next function - returns also generational values, good for big simulation sets
#############

###it is very similar
simulation.descript<-function(pop.size, selecta, ni, population=c(11,rep(10,(pop.size-1))), optimum=120){

means<-NA
sds<-NA
first.opt<-NA
is.it<-0


pick<-selecta*pop.size
pairs<-1:(pick/2)
pair.generate<-pop.size/(pick/2)
pair.floor<-floor(pair.generate)
done<-pair.floor*length(pairs)
extra<-(pop.size-pair.floor*length(pairs))

###set starting population
generation<-population

means[1]<-mean(generation)
sds[1]<-sd.dist(generation)

if(round(pair.generate)==pair.generate){
for(i in 1:10000){
generation<-new.generation.straight(generation,ni,optimum,pick,pairs,pair.generate,pair.floor,done,extra)
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
if(average<0){
end<-"chaos"
break
}
if(deviation<0.0001){
if(round(average)==optimum){
end<-"success"
}else{
end<-"fail"
}
break
}
end<-"keep"
}
}else{
for(i in 1:10000){
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
if(average<0){
end<-"chaos"
break
}
if(deviation<0.0001){
if(round(average)==optimum){
end<-"success"
}else{
end<-"fail"
}
break
}
end<-"keep"
}
}


return(list(end,means,sds,first.opt))
}


###simulation returning all individual values

#You will need these libraries
library(MASS)
library(RColorBrewer)

###ramp pallete alpha - transparent colours in matrix images, i found these two function online. Thank you!
###https://www.r-bloggers.com/colorramppalettealpha-and-addalpha-helper-functions-for-adding-transparency-to-colors-in-r/

# addalpha()
addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}

# colorRampPaletteAlpha()
colorRampPaletteAlpha <- function(colors, n=32, interpolate='linear') {
  # Create the color ramp normally
  cr <- colorRampPalette(colors, interpolate=interpolate)(n)
  # Find the alpha channel
  a <- col2rgb(colors, alpha=T)[4,]
  # Interpolate
	if (interpolate=='linear') {
		l <- approx(a, n=n)
	} else {
		l <- spline(a, n=n)
	}
	l$y[l$y > 255] <- 255 # Clamp if spline is > 255
	cr <- addalpha(cr, l$y/255.0)
	return(cr)
}



