rm(list = ls())

infectdist<-10
n<-100
#initial state
x1<-runif(n,0,100)
y1<-runif(n,0,100)
plot(x1,y1,xlim = c(0,100),ylim = c(0,100),col=rainbow(n))
# text(x1,y1,1:10)
#compute distance matrix
d1<-as.matrix(dist(cbind(x1,y1)))
d1
#label one point as poison
# poisonlabel<-c(1,rep(0,n-1))

# seed the virus among a hnumber of peopel
poisonlabel<-rbinom(100,1,0.1)

# if dist between poison point is lower than 10, 
#then that other point becomes infected
wheninfected = rep(0,n)
numinf=NULL
# for (i in 1:n){
# thispersoninfected<-ifelse(poisonlabel[i]==1,1,0)
# infectednearby<-ifelse(d1[i,]<infectdist,1,0)
# poisonlabel<-poisonlabel+thispersoninfected*infectednearby
# }
# poisonlabel<-ifelse(poisonlabel>0,1,0)
# poisonlabel

plot(x1,y1,xlim = c(0,100),ylim = c(0,100),col=poisonlabel+1)
#text(x1,y1,1:10)


# ***** run the simulation *****
for (i in 1:100){

x2<-(x1+(rbinom(n,2,0.5)-1)*1) %% 100
y2<-(y1+(rbinom(n,2,0.5)-1)*1) %% 100
plot(x1,y1,xlim = c(0,100),ylim = c(0,100),col = poisonlabel+1)
# segments(x1,y1,x2,y2,col = poisonlabel+1)
#points(x1,y1,col = poisonlabel+1)
# text(x1,y1,1:n,col=poisonlabel+1)

x1<-x2
y1<-y2
Sys.sleep(0.01)
d1<-as.matrix(dist(cbind(x1,y1)))
for (j in 1:n){
  thispersoninfected<-ifelse(poisonlabel[j]==1,1,0)
  infectednearby<-ifelse(d1[j,]<infectdist,1,0)
  poisonlabel<-poisonlabel+thispersoninfected*infectednearby
}
poisonlabel<-ifelse(poisonlabel>0,1,0)
# print(poisonlabel)
wheninfected<-poisonlabel+wheninfected

#now modify the poison label so that once it's been infected for x rounds, it is immune
immune<-ifelse(wheninfected>30,0,1)

#update poison label
poisonlabel<-poisonlabel*immune

numinf[i]<-sum(poisonlabel)

}
plot(numinf)
