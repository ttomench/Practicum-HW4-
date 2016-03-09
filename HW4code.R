#######################################################
################### Practicum HW4 #####################
#######################################################


# dates:        all dates for which data are available
# stations:     names of all stations for which data are available
# lat, lon:     longitude and latitude coordinate for each of these stations
# elev:         station elevation (m)
# ptype:        all cases where one of the four precipitation types of interest was reported
# Twb.prof:     the corresponding vertical profiles of wetbulb temperature (0m to 3000m above the surface in steps of 100m)
# station.ind:  for each case, the index of the station where it was reported
# date.ind:     for each case, the index of the date on which it was reported



#################################
########## Problem 1 ############
#################################
#setwd("Z:/Practicum")
setwd("~/Documents/Practicum")
load("predictors.Rdata")
library(fields)
library(maps)
library(mvtnorm)
<<<<<<< HEAD
library(base)
=======
>>>>>>> origin/master

long = lon-360

years = as.numeric(substring(dates,1,4))
months = as.numeric(substring(dates,5,6))
<<<<<<< HEAD

=======
#years = floor(dates/1000000)
#years = dates%/%1000000
>>>>>>> origin/master

#graphhist <- function(){
for(i in 1:16) {
  
  pdf(file=paste("HW4_Figures/Hist_level_",i,".pdf",sep=""),width = 18, height=5)
  par(mfrow=c(1,4))
  #if(ptypes=='rain'){
  prof.rain = Twb.prof[ptype=='RA',col=i]
  mean.rain = mean(prof.rain)
  std.rain = sd(prof.rain)
  
  hist(prof.rain, breaks ="FD",xlab="Temperature", main= paste("Rain at Level",i),prob=T, ylim=c(0,0.08))#;axis(4,at=mean.rain,label=round(mean.rain,4))
  curve(dnorm(x, mean=mean.rain, sd=std.rain), col="blue", lwd=2, add=TRUE, yaxt="n")
  abline(v=mean.rain,col="blue", lwd=3)
  #}  
  
  #if(ptypes=='snow'){
  prof.snow = Twb.prof[ptype=='SN',col=i]
  mean.snow = mean(prof.snow)
  std.snow = sd(prof.snow)
  
  hist(prof.snow, breaks ="FD",xlab="Temperature", main= paste("Snow at Level",i),prob=T)
  curve(dnorm(x, mean=mean.snow, sd=std.snow), col="darkgreen", lwd=2, add=TRUE, yaxt="n")
  abline(v=mean.snow, col="darkgreen", lwd=3)
  #}
  
  #if(ptypes=='freeze'){
  prof.frzrain = Twb.prof[ptype=='FZRA',col=i]
  mean.frzrain = mean(prof.frzrain)
  std.frzrain = sd(prof.frzrain)
  
  hist(prof.frzrain, breaks ="FD",xlab="Temperature", main= paste("Freezing Rain at Level",i),prob=T)
  curve(dnorm(x, mean=mean.frzrain, sd=std.frzrain), col="purple", lwd=2, add=TRUE, yaxt="n")
  abline(v=mean.frzrain, col="purple", lwd=3)
  #}
  
  #if(ptypes=='ice'){
  prof.ip = Twb.prof[ptype=='IP',col=i]
  mean.ip = mean(prof.ip)
  std.ip = sd(prof.ip)
  
  hist(prof.ip, breaks ="FD",xlab="Temperature", main= paste("Ice Pellets at Level",i),prob=T)
  curve(dnorm(x, mean=mean.ip, sd=std.ip), col="red", lwd=2, add=TRUE, yaxt="n")
  abline(v=mean.ip, col="red", lwd=3)
  #}
  dev.off()
  
  pdf(file=paste("HW4_Figures/QQ_Plot_",i,".pdf",sep=""),width = 18, height=5)
  par(mfrow=c(1,4))
  qqnorm(prof.rain, main = paste("Rain at Level ", i),col = "blue"); qqline(prof.rain)
  qqnorm(prof.snow, main = paste("Snow at Level ", i), col = "darkgreen"); qqline(prof.snow)
  qqnorm(prof.frzrain, main = paste("Freezing Rain at Level ", i), col = "purple"); qqline(prof.frzrain)
  qqnorm(prof.ip, main = paste("Ice Pellets at Level ", i), col = "red"); qqline(prof.ip)
  dev.off()
  
  
  # print(paste("SW Tests Level ", i ))
  #   print("Rain")
  #   shapiro.test(prof.rain)
  #   print("Snow")
  #   shapiro.test(prof.snow)
  #   print("Freezing Rain")
  #   shapiro.test(prof.frzrain)
  #   print("Ice Pellets")
  #   shapiro.test(prof.ip)
  
  
}

#}
graphhist
#graphhist('rain')
#graphhist('snow')
#graphhist('freeze')
#graphhist('ice')

#################################
########## Problem 4 ############
#################################

<<<<<<< HEAD
=======


prior.prob = list()
for(i in 1:length(stations)){
  prior.prob[[i]] = matrix(0, nrow = 4, ncol = length(unique(months)))
  
}

for(i in 1:length(stations)){
  for(j in 1:length(sort(unique(months)))) {
    station.rows = which(station.ind==i)
    mon = sort(unique(months))[j]
    
    month.labels= which(months==mon)
    month.rows = which(date.ind%in%month.labels)
    row.need = intersect(month.rows,station.rows)
    
    rain.rows = which(ptype[row.need]=="RA")
    snow.rows = which(ptype[row.need]=="SN")
    fzrain.rows = which(ptype[row.need]=="FZRA")
    ip.rows = which(ptype[row.need]=="IP")
    
    prior.prob[[i]][1,j]= length(rain.rows)/length(row.need)
    prior.prob[[i]][2,j]= length(snow.rows)/length(row.need)
    prior.prob[[i]][3,j]= length(fzrain.rows)/length(row.need)
    prior.prob[[i]][4,j]= length(ip.rows)/length(row.need)
    
  }
  
}


>>>>>>> origin/master
plot.prior = list()
for(i in 1:length(unique(ptype))){
  plot.prior[[i]] = matrix(0, nrow = length(stations), ncol = length(unique(months)))
  
}

for(i in 1:length(stations)){
  for(j in 1:length(sort(unique(months)))) {
    station.rows = which(station.ind==i)
    mon = sort(unique(months))[j]
    
    month.labels= which(months==mon)
    month.rows = which(date.ind%in%month.labels)
    row.need = intersect(month.rows,station.rows)
    
    rain.rows = which(ptype[row.need]=="RA")
    snow.rows = which(ptype[row.need]=="SN")
    fzrain.rows = which(ptype[row.need]=="FZRA")
    ip.rows = which(ptype[row.need]=="IP")
    
    plot.prior[[1]][i,j]= length(rain.rows)/length(row.need)
    plot.prior[[2]][i,j]= length(snow.rows)/length(row.need)
    plot.prior[[3]][i,j]= length(fzrain.rows)/length(row.need)
    plot.prior[[4]][i,j]= length(ip.rows)/length(row.need)
    
  }
  
}

months.names = c("January", "February", "March", "April", "May", "September", "October", "November","December")

for(j in 1:length(unique(months))) {
  
  #mon = sort(unique(months))[j]
  mon = months.names[j]
  pdf(file=paste("HW4_Figures/Prior_Probs_", j, sep = ""),width = 20, height=18)
  
  par(mfrow=c(2,2))
  quilt.plot(long,lat,plot.prior[[1]][,j], xlab= "Longitude", ylab="Latitude", cex.axis=1.5, cex.lab=1.5)
  title(paste("Rain in ", mon, sep = ""), cex.main=3)
  map('world', add=T)
  map("state",add=T)
  
  quilt.plot(long,lat,plot.prior[[2]][,j], xlab= "Longitude", ylab="Latitude", cex.axis=1.5, cex.lab=1.5)
  title(paste("Snow in ", mon, sep = ""), cex.main=3)
  map('world', add=T)
  map("state",add=T)
  
  quilt.plot(long,lat,plot.prior[[3]][,j], xlab= "Longitude", ylab="Latitude", cex.axis=1.5, cex.lab=1.5)
  title(paste("Freezing Rain in ", mon, sep = ""), cex.main=3)
  map('world', add=T)
  map("state",add=T)
  
  quilt.plot(long,lat,plot.prior[[4]][,j], xlab= "Longitude", ylab="Latitude", cex.axis=1.5, cex.lab=1.5)
  title(paste("Ice Pellets in ", mon, sep = ""), cex.main=3)
  map('world', add=T)
  map("state",add=T)
  dev.off()
  
}


#######################################
########## Problem 2 and 3 ############
#######################################
#Training         n
#1996 - 2000    57,328
#1997 - 2001    66,604
#Testing
#2001           12,673
#2002           13,202
test.ind = array()

<<<<<<< HEAD

=======
# mean.train=list()
# mean.train[[1]] = matrix(0, nrow=16, ncol=12)
# mean.train[[2]] = matrix(0, nrow=16, ncol=12)
# mean.train[[3]] = matrix(0, nrow=16, ncol=12)
# mean.train[[4]] = matrix(0, nrow=16, ncol=12)
# names(mean.train)= c("rain","snow", "freeze", "ice pellets")
# 
# cov.train=list()
# cov.train[[1]]=list()
# cov.train[[2]]=list()
# cov.train[[3]]=list()
# cov.train[[4]]=list()
# names(cov.train)= c("rain","snow", "freeze", "ice pellets")
>>>>>>> origin/master


mean.rain = list()
mean.snow = list()
mean.frzrain = list()
mean.ip = list()

cov.rain = list()
cov.snow = list()
cov.frzrain = list()
cov.ip = list()

<<<<<<< HEAD
#prob.test = list()

#b.s = list()
#b.s.new= array()

#test.n = array()

prob.hats = data.frame(matrix(0,nrow = sum(test.nn),ncol = 5))
ind = 0
=======
prob.test = list()

b.s = list()
b.s.new= array()

test.n = array()

prob.hats = data.frame(matrix(o,nrow = sum(test.nn),ncol = 5))
>>>>>>> origin/master

for(i in 1:12) {
  print(i)
  years.ind.start = min(which(years==(i+1995)))
  years.ind.end = max(which(years==(i+1999)))
  test.years.start = min(which(years==(i+2000)))
  test.years.end = max(which(years==(i+2000)))
  
  train.ind.start = min(which(date.ind==years.ind.start)) 
  train.ind.end = max(which(date.ind==years.ind.end))
  
  test.ind.start = min(which(date.ind==test.years.start)) 
  test.ind.end = max(which(date.ind==test.years.end))
  
  #print(train.ind[2])
  #print(test.ind[2])
  training.set = Twb.prof[train.ind.start:train.ind.end,1:16]
  testing.set = Twb.prof[test.ind.start:test.ind.end,1:16]
  
  ptype.train = ptype[train.ind.start:train.ind.end]
  ptype.test=ptype[test.ind.start:test.ind.end]
  
  station.rows = station.ind[test.ind.start:test.ind.end]
  date.rows = date.ind[test.ind.start:test.ind.end]
  
  
  rain.train = training.set[ptype.train=="RA",]
  snow.train = training.set[ptype.train=="SN",]
  frzrain.train = training.set[ptype.train=="FZRA",]
  ip.train = training.set[ptype.train=="IP",]
  
  #print(dim(rain.train))
  #print(dim(snow.train))
  #print(dim(frzrain.train))
  #print(dim(ip.train))
  
<<<<<<< HEAD
  mean.rain[[i]] <- apply(rain.train,2,mean)
  mean.snow[[i]] <- apply(snow.train,2,mean)
  mean.frzrain[[i]] <- apply(frzrain.train,2,mean)
  mean.ip[[i]] <- apply(ip.train,2,mean)
  
  cov.rain[[i]] <- cov(rain.train)
  cov.snow[[i]] <- cov(snow.train)
  cov.frzrain[[i]] <- cov(frzrain.train)
  cov.ip[[i]] <- cov(ip.train)
=======
  # mean.rain[[i]] <- apply(rain.train,2,mean)
  # mean.snow[[i]] <- apply(snow.train,2,mean)
  # mean.frzrain[[i]] <- apply(frzrain.train,2,mean)
  # mean.ip[[i]] <- apply(ip.train,2,mean)
  # 
  # cov.rain[[i]] <- cov(rain.train)
  # cov.snow[[i]] <- cov(snow.train)
  # cov.frzrain[[i]] <- cov(frzrain.train)
  # cov.ip[[i]] <- cov(ip.train)
>>>>>>> origin/master
  
  #image.plot(1:16,1:16,t(X[,16:1]))
  #print(paste("Training set:",length(training.set)/16))
  #print(paste("Testing set:",length(testing.set)/16))
<<<<<<< HEAD
  #test.n[i]=length(testing.set)/16
  
  ##### Finding the Probability that a x_i is in a population  #####
  #prob.test[[i]] = matrix(0, length(testing.set),4)
  #b.s[[i]] = array()
  #b.s[i] = 0
  
  for(j in 1:length(testing.set[,1])) {
    ind = ind + 1
    mons = months[date.rows[j]]
    mon = which(sort(unique(months))==mons)
    sta = station.rows[j] 
    
    rain.pi = plot.prior[[1]][sta,mon]
    snow.pi = plot.prior[[2]][sta,mon]
    frzrain.pi = plot.prior[[3]][sta,mon]
    ip.pi = plot.prior[[4]][sta,mon]
    
    phi.rain = dmvnorm(testing.set[j,], mean.rain[[i]], cov.rain[[i]])
    phi.snow = dmvnorm(testing.set[j,], mean.snow[[i]], cov.snow[[i]])
    phi.frzrain = dmvnorm(testing.set[j,], mean.frzrain[[i]], cov.frzrain[[i]])
    phi.ip = dmvnorm(testing.set[j,], mean.ip[[i]], cov.ip[[i]])
    
    denom = rain.pi*phi.rain + snow.pi*phi.snow + frzrain.pi*phi.frzrain + ip.pi*phi.ip
    
    prob.hats[ind,1] = round((rain.pi*phi.rain)/denom,5)
    prob.hats[ind,2] = round((snow.pi*phi.snow)/denom, 5)
    prob.hats[ind,3] = round((frzrain.pi*phi.frzrain)/denom, 5)
    prob.hats[ind,4] = round((ip.pi*phi.ip)/denom, 5)
    prob.hats[ind,5] = ptype.test[j]
    colnames(prob.hats) = c("rain", "snow", "freeze", "ip", "obs")
    
#     prob.test[[i]][j,1] = round((rain.pi*phi.rain)/denom,5)
#     prob.test[[i]][j,2] = round((snow.pi*phi.snow)/denom, 5)
#     prob.test[[i]][j,3] = round((frzrain.pi*phi.frzrain)/denom, 5)
#     prob.test[[i]][j,4] = round((ip.pi*phi.ip)/denom, 5)
#     colnames(prob.test[[i]]) = c("rain", "snow", "freezing", "ice pellets")
#     
#     o.i = c(0,0,0,0)
#     
#     if(ptype.test[j]=="RA"){
#       o.i[1]= 1
#     }
#     if(ptype.test[j]=="SN"){
#       o.i[2]= 1
#     }
#     if(ptype.test[j]=="FZRA"){
#       o.i[3]= 1
#     }
#     if(ptype.test[j]=="IP"){
#       o.i[4]= 1
#     }
#     
#     b.s[[i]][j]<-sum((prob.test[[i]][j,]-o.i)^2, na.rm = TRUE)
    #b.s[i] <- b.s[i] + sum((prob.test[[i]][j,]-o.i)^2)
    # if(is.nan(b.s[i])==TRUE){
    #   print(j)
    #   break
    # }
    
    
  }
  
  #b.s.new[i] <- sum(b.s[[i]], na.rm = TRUE)
=======
  test.n[i]=length(testing.set)/16
  
  # ##### Finding the Probability that a x_i is in a population  #####
  # prob.test[[i]] = matrix(0, length(testing.set),4)
  # b.s[[i]] = array()
  # #b.s[i] = 0
  # for(j in 1:length(testing.set[,1])) {
  #   
  #   mons = months[date.rows[j]]
  #   mon = which(sort(unique(months))==mons)
  #   sta = station.rows[j] 
  #   
  #   rain.pi = plot.prior[[1]][sta,mon]
  #   snow.pi = plot.prior[[2]][sta,mon]
  #   frzrain.pi = plot.prior[[3]][sta,mon]
  #   ip.pi = plot.prior[[4]][sta,mon]
  #   
  #   phi.rain = dmvnorm(testing.set[j,],mean.rain[[i]], cov.rain[[i]])
  #   phi.snow = dmvnorm(testing.set[j,],mean.snow[[i]], cov.snow[[i]])
  #   phi.frzrain = dmvnorm(testing.set[j,],mean.frzrain[[i]], cov.frzrain[[i]])
  #   phi.ip = dmvnorm(testing.set[j,],mean.ip[[i]], cov.ip[[i]])
  #   
  #   denom = rain.pi*phi.rain + snow.pi*phi.snow + frzrain.pi*phi.frzrain + ip.pi*phi.ip
  #   
  #   prob.test[[i]][j,1] = round((rain.pi*phi.rain)/denom,5)
  #   prob.test[[i]][j,2] = round((snow.pi*phi.snow)/denom, 5)
  #   prob.test[[i]][j,3] = round((frzrain.pi*phi.frzrain)/denom, 5)
  #   prob.test[[i]][j,4] = round(ip.pi*phi.ip)/denom, 5)
  #   colnames(prob.test[[i]],c("rain", "snow", "freezing", "ice pellets"))
  #   
  #   o.i = c(0,0,0,0)
  #   
  #   if(ptype.test[j]=="RA"){
  #     o.i[1]= 1
  #   }
  #   if(ptype.test[j]=="SN"){
  #     o.i[2]= 1
  #   }
  #   if(ptype.test[j]=="FZRA"){
  #     o.i[3]= 1
  #   }
  #   if(ptype.test[j]=="IP"){
  #     o.i[4]= 1
  #   }
  #   
  #   b.s[[i]][j]<-sum((prob.test[[i]][j,]-o.i)^2, na.rm = TRUE)
  #   #b.s[i] <- b.s[i] + sum((prob.test[[i]][j,]-o.i)^2)
  #   # if(is.nan(b.s[i])==TRUE){
  #   #   print(j)
  #   #   break
  #   # }
  #   
  #   
  # }
  # 
  # b.s.new[i] <- sum(b.s[[i]], na.rm = TRUE)
>>>>>>> origin/master
  
  
  #   pdf(file=paste("HW4_Figures/covariance_training_",i,".pdf",sep=""),width = 18, height=5)
  #   par(mfrow=c(1,4))
  #   image.plot(1:16,1:16,cov.rain[[i]][,16:1],xlab="",ylab="")
  #   title(paste("Rain Cov Training ",i,sep=""),cex.main=2)
  #   image.plot(1:16,1:16,cov.snow[[i]][,16:1],xlab="",ylab="")
  #   title(paste("Snow Cov Training ",i,sep=""),cex.main=2)
  #   image.plot(1:16,1:16,cov.frzrain[[i]][,16:1],xlab="",ylab="")
  #   title(paste("Freezing Cov Training ",i,sep=""),cex.main=2)
  #   image.plot(1:16,1:16,cov.ip[[i]][,16:1],xlab="",ylab="")
  #   title(paste("Pellets Cov Training ",i,sep=""),cex.main=2)
  #   dev.off()
  #   
  #   pdf(file=paste("HW4_Figures/mean_training_",i,".pdf",sep=""),width = 10, height=10)
  #   plot(mean.rain[[i]],1:16,type='b',lwd=2,col='blue',xlab="Mean Temperature",ylab="Level",xlim = c(260,284))
  #   title(paste("Ptype Means Training ",i,sep=""),cex.main=2)
  #   abline(v=273.15,lwd=2)
  #   lines(mean.snow[[i]],1:16,type='b',lwd=2,col='darkgreen')
  #   lines(mean.frzrain[[i]],1:16,type='b',lwd=2,col='purple')
  #   lines(mean.ip[[i]],1:16,type='b',lwd=2,col='red')
  #   legend(278,16,c("rain","snow","freezing rain","ice pellets"),lwd=c(2,2,2,2), col=c('blue','darkgreen','purple','red'),bty="n",cex=1.5)
  #   dev.off()
  
}


test.nn = sum(test.n, na.rm= T)


<<<<<<< HEAD

=======
# mean.rain[[6]]
# mean.snow[[6]]
# mean.frzrain[[6]]
# mean.ip[[6]]
# 
# cov.rain[[6]]
# cov.snow[[6]]
# cov.frzrain[[6]]
# cov.ip[[6]]
# 
>>>>>>> origin/master
pdf(file="HW4_Figures/Mean_ptypes",width = 18, height=5)
par(mfrow=c(1,4))

plot(mean.rain[[1]],1:16,lty=3,lwd=3,col='darkblue',main="Rain",xlab="Mean Temperature",ylab="level", xlim=c(276,283))
for(i in 2:11){
  lines(mean.rain[[i]],1:16,type='l',lwd=1,col='blue')
}
lines(mean.rain[[12]],1:16,lty=5,lwd=3,col='darkblue')
legend(279,16,c("1996-2000","2008-2013"),lwd=c(3,3),lty=c(3,5), col=c('darkblue','darkblue'),bty="n",cex=1.5)

plot(mean.snow[[1]],1:16,lty=3,lwd=3,col='darkgreen',main="Snow",xlab="Mean Temperature",ylab="level",xlim = c(260,268))
for(i in 2:11){
  lines(mean.snow[[i]],1:16,type='l',lwd=1,col='green')
}
lines(mean.snow[[12]],1:16,lty=5,lwd=3,col='darkgreen')
legend(264,16,c("1996-2000","2008-2013"),lwd=c(3,3),lty=c(3,5), col=c('darkgreen','darkgreen'),bty="n",cex=1.5)

plot(mean.frzrain[[1]],1:16,lty=3,lwd=3,col='black',main="Freezing Rain",xlab="Mean Temperature",ylab="level",xlim = c(270,273))
for(i in 2:11){
  lines(mean.frzrain[[i]],1:16,type='l',lwd=1,col='purple')
}
lines(mean.frzrain[[12]],1:16,lty=5,lwd=3,col='black')
legend(271.5,5,c("1996-2000","2008-2013"),lwd=c(3,3),lty=c(3,5), col=c('black','black'),bty="n",cex=1.5)

plot(mean.ip[[1]],1:16,lty=3,lwd=3,col='darkred',main="Ice Pellets",xlab="Mean Temperature",ylab="level",xlim = c(269,272))
for(i in 2:11){
  lines(mean.ip[[i]],1:16,type='l',lwd=1,col='red')
}
lines(mean.ip[[12]],1:16,lty=5,lwd=3,col='darkred')
legend(270.5,7,c("1996-2000","2008-2013"),lwd=c(3,3),lty=c(3,5), col=c('darkred','darkred'),bty="n",cex=1.5)

dev.off()


<<<<<<< HEAD
###########################################
################ Problem 6 ################ 
###########################################

BS = sum(b.s.new)
BS/test.nn

if(prob.hats[,5]=='RA'){prob.hats[,5]=1}
if(prob.hats[,5]=='SN'){prob.hats[,5]=2}
if(prob.hats[,5]=='FZRA'){prob.hats[,5]=3}
if(prob.hats[,5]=='IP'){prob.hats[,5]=4}

# obs.rain = which(prob.hats[,5]=='RA')
# obs.snow = which(prob.hats[,5]=='SN')
# obs.frzrain = which(prob.hats[,5]=='FZRA')
# obs.ip = which(prob.hats[,5]=='IP')

which.ptype = apply(prob.hats[,1:4], 1, which.max)

obs.rain = which(which(which.ptype==1)==which(prob.hat[,5]=='RA'))
obs.snow = which(which(which.ptype==2)==which(prob.hat[,5]=='SN'))
obs.frzrain = which(which(which.ptype==3)==which(prob.hat[,5]=='FZRA'))
obs.ip = which(which(which.ptype==4)==which(prob.hat[,5]=='IP'))
observed = rbind(obs.rain,obs.snow,obs.frzrain,obs.ip)

correct.class = length(observed)/length(prob.hats[,5])
correct.class


############################################
################ Extra Code ################ 
############################################

#years = floor(dates/1000000)
#years = dates%/%1000000

# mean.train=list()
# mean.train[[1]] = matrix(0, nrow=16, ncol=12)
# mean.train[[2]] = matrix(0, nrow=16, ncol=12)
# mean.train[[3]] = matrix(0, nrow=16, ncol=12)
# mean.train[[4]] = matrix(0, nrow=16, ncol=12)
# names(mean.train)= c("rain","snow", "freeze", "ice pellets")
# 
# cov.train=list()
# cov.train[[1]]=list()
# cov.train[[2]]=list()
# cov.train[[3]]=list()
# cov.train[[4]]=list()
# names(cov.train)= c("rain","snow", "freeze", "ice pellets")


# prior.prob = list()
# for(i in 1:length(stations)){
#   prior.prob[[i]] = matrix(0, nrow = 4, ncol = length(unique(months)))
#   
# }
# 
# for(i in 1:length(stations)){
#   for(j in 1:length(sort(unique(months)))) {
#     station.rows = which(station.ind==i)
#     mon = sort(unique(months))[j]
#     
#     month.labels= which(months==mon)
#     month.rows = which(date.ind%in%month.labels)
#     row.need = intersect(month.rows,station.rows)
#     
#     rain.rows = which(ptype[row.need]=="RA")
#     snow.rows = which(ptype[row.need]=="SN")
#     fzrain.rows = which(ptype[row.need]=="FZRA")
#     ip.rows = which(ptype[row.need]=="IP")
#     
#     prior.prob[[i]][1,j]= length(rain.rows)/length(row.need)
#     prior.prob[[i]][2,j]= length(snow.rows)/length(row.need)
#     prior.prob[[i]][3,j]= length(fzrain.rows)/length(row.need)
#     prior.prob[[i]][4,j]= length(ip.rows)/length(row.need)
#     
#   }
#   
# }



=======
>>>>>>> origin/master
# par(mfrow=c(1,4))
# image.plot(1:16,1:16,cov.rain[[1]][,16:1],main="Rain",xlab="",ylab="")
# image.plot(1:16,1:16,cov.snow[[1]][,16:1],main="Snow",xlab="",ylab="")
# image.plot(1:16,1:16,cov.frzrain[[1]][,16:1],main="Freezing Rain",xlab="",ylab="")
# image.plot(1:16,1:16,cov.ip[[1]][,16:1],main="Ice Pellets",xlab="",ylab="")
<<<<<<< HEAD


# mean.rain[[6]]
# mean.snow[[6]]
# mean.frzrain[[6]]
# mean.ip[[6]]
# 
# cov.rain[[6]]
# cov.snow[[6]]
# cov.frzrain[[6]]
# cov.ip[[6]]
# 
=======
# 





###########################################
################ Problem 6 ################ 
###########################################

BS = sum(b.s.new)
BS
>>>>>>> origin/master
