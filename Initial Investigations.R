load('synthetic_ESSCLC_data.RData')

#convert to grid of observations
t = seq(0,1,length = 100)
t = rev(rev(t)[-1])
foo = matrix(0,nrow=nrow(episodes),ncol = length(t))

indx = 1
for(i in t){
 foo[,indx] = ifelse(i>=episodes$FromDateRel&i<episodes$ThruDateRel,1,0)
 indx = indx + 1
}

dat = data.frame(episodes[,c('ID','setting')],foo)

require(dplyr)
require(plyr)
foo2 = ddply(dat,.(ID,setting),function(d) colSums(d[,-(1:2)]) )

foo21 = expand.grid(unique(dat$ID),unique(dat$setting))
names(foo21) = c('ID','setting')
foo3 = left_join(foo21,foo2) 
foo3[is.na(foo3)] = 0



foo4 = reshape(foo3,direction='wide',idvar='ID',timevar = 'setting')




require(ade4)
D = dist.binary(foo4[,-1],method=2)

require(cluster)
clust = pam(D,5)
clust$silinfo$avg.width

table(clust$clustering,summary.stats$class)
boxplot(summary.stats$Survival.Days~clust$clustering)
require(mosaic)
mean(summary.stats$Survival.Days~clust$clustering)



image(t(as.matrix(foo4[clust$clustering == 1,-1])))
abline(v=seq(.25,.75,by=.25),col='black',lwd=3)
mtext(c('None','Hospital','Hospice','SNF'),3,line=1,at=seq(.12,.88,by=.25))

image(t(as.matrix(foo4[clust$clustering == 2,-1])))
abline(v=seq(.25,.75,by=.25),col='black',lwd=3)
mtext(c('None','Hospital','Hospice','SNF'),3,line=1,at=seq(.12,.88,by=.25))

image(t(as.matrix(foo4[clust$clustering == 3,-1])))
abline(v=seq(.25,.75,by=.25),col='black',lwd=3)
mtext(c('None','Hospital','Hospice','SNF'),3,line=1,at=seq(.12,.88,by=.25))

image(t(as.matrix(foo4[clust$clustering == 4,-1])))
abline(v=seq(.25,.75,by=.25),col='black',lwd=3)
mtext(c('None','Hospital','Hospice','SNF'),3,line=1,at=seq(.12,.88,by=.25))

image(t(as.matrix(foo4[clust$clustering == 5,-1])))
abline(v=seq(.25,.75,by=.25),col='black',lwd=3)
mtext(c('None','Hospital','Hospice','SNF'),3,line=1,at=seq(.12,.88,by=.25))




P = prop.table(table(clust$clustering))

par(mfrow=c(2,3))
plot(seq(0,1,length=396),colMeans(foo4[clust$clustering == 1,-1]),type='l',main=paste('Cluster 1:',round(P[1],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.25,.75,by=.25),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.0625),label=rep(c(0,0.25,0.5,0.75),4))
mtext(c('None','Hospital','Hospice','SNF'),3,line=.75,at=seq(.12,.88,by=.25),cex=.8)


plot(seq(0,1,length=396),colMeans(foo4[clust$clustering == 2,-1]),type='l',main=paste('Cluster 2:',round(P[2],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.25,.75,by=.25),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.0625),label=rep(c(0,0.25,0.5,0.75),4))
mtext(c('None','Hospital','Hospice','SNF'),3,line=.75,at=seq(.12,.88,by=.25),cex=.8)


plot(seq(0,1,length=396),colMeans(foo4[clust$clustering == 3,-1]),type='l',main=paste('Cluster 3:',round(P[3],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.25,.75,by=.25),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.0625),label=rep(c(0,0.25,0.5,0.75),4))
mtext(c('None','Hospital','Hospice','SNF'),3,line=.75,at=seq(.12,.88,by=.25),cex=.8)


plot(seq(0,1,length=396),colMeans(foo4[clust$clustering == 4,-1]),type='l',main=paste('Cluster 4:',round(P[4],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.25,.75,by=.25),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.0625),label=rep(c(0,0.25,0.5,0.75),4))
mtext(c('None','Hospital','Hospice','SNF'),3,line=.75,at=seq(.12,.88,by=.25),cex=.8)


plot(seq(0,1,length=396),colMeans(foo4[clust$clustering == 5,-1]),type='l',main=paste('Cluster 5:',round(P[4],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.25,.75,by=.25),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.0625),label=rep(c(0,0.25,0.5,0.75),4))
mtext(c('None','Hospital','Hospice','SNF'),3,line=.75,at=seq(.12,.88,by=.25),cex=.8)

colMeans(summary.stats[clust$clustering == 1,2:9])
colMeans(summary.stats[clust$clustering == 2,2:9])
colMeans(summary.stats[clust$clustering == 3,2:9])
colMeans(summary.stats[clust$clustering == 4,2:9])
colMeans(summary.stats[clust$clustering == 5,2:9])


#Compare to using summary stats (roughly the same shapes....)
clust2 = pam(summary.stats[,2:9],3)
clust2$silinfo$avg.width

colMeans(summary.stats[clust2$clustering == 1,2:9])
colMeans(summary.stats[clust2$clustering == 2,2:9])
colMeans(summary.stats[clust2$clustering == 3,2:9])

P2 = prop.table(table(clust2$clustering))

par(mfrow=c(1,3))
plot(seq(0,1,length=400),colMeans(foo4[clust2$clustering == 1,-1]),type='l',main=paste('Cluster 1:',round(P2[1],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.25,.75,by=.25),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.0625),label=rep(c(0,0.25,0.5,0.75),4))
mtext(c('None','Hospital','Hospice','SNF'),3,line=.75,at=seq(.12,.88,by=.25),cex=.8)

plot(seq(0,1,length=400),colMeans(foo4[clust2$clustering == 2,-1]),type='l',main=paste('Cluster 2:',round(P2[2],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.25,.75,by=.25),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.0625),label=rep(c(0,0.25,0.5,0.75),4))
mtext(c('None','Hospital','Hospice','SNF'),3,line=.75,at=seq(.12,.88,by=.25),cex=.8)

plot(seq(0,1,length=400),colMeans(foo4[clust2$clustering == 3,-1]),type='l',main=paste('Cluster 3:',round(P2[3],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.25,.75,by=.25),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.0625),label=rep(c(0,0.25,0.5,0.75),4))
mtext(c('None','Hospital','Hospice','SNF'),3,line=.75,at=seq(.12,.88,by=.25),cex=.8)



###########################################################################################################
#Include Death as Category

M = max(episodes$ThruDate)

#convert to grid of observations
t = seq(0,M,length = 101)
t = rev(rev(t)[-1])
foo = matrix(0,nrow=nrow(episodes),ncol = length(t))

indx = 1
for(i in t){
        foo[,indx] = ifelse(i>=episodes$FromDate&i<episodes$ThruDate,1,0)
        indx = indx + 1
}

dat = data.frame(episodes[,c('ID','setting')],foo)
require(dplyr)
require(plyr)
foo2 = ddply(dat,.(ID,setting),function(d) colSums(d[,-(1:2)]) )

foo20 = ddply(dat,.(ID),function(d) colSums(d[,-(1:2)]) )
foo20[foo20 == 1] = NA
foo20[foo20 == 0] = 1
foo20[is.na(foo20)] = 0
foo20$ID[1]  = 1
foo20$setting = 'Death'
foo20 = foo20[,c(1,102,2:101)]

foo21 = rbind(foo2,foo20)

foo22 = expand.grid(unique(foo21$ID),unique(foo21$setting))
names(foo22) = c('ID','setting')
foo3 = left_join(foo22,foo21) 
foo3[is.na(foo3)] = 0



foo4 = reshape(foo3,direction='wide',idvar='ID',timevar = 'setting')


#This is dominated by when you die rather than the utility of services during your life......
#Need to check to see how fine the grid would need to be (check min length of stay)

require(ade4)
D = dist.binary(foo4[,2:401],method=2)

require(cluster)
clust = pam(D,5)
clust$silinfo$avg.width

P = prop.table(table(clust$clustering))

par(mfrow=c(2,3))
plot(seq(0,1,length=500),colMeans(foo4[clust$clustering == 1,-1]),type='l',main=paste('Cluster 1:',round(P[1],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.2,.8,by=.2),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.05),label=rep(c(0,0.25,0.5,0.75),5))
mtext(c('None','Hospital','Hospice','SNF','Death'),3,line=.75,at=seq(.1,.95,by=.2),cex=.8)


plot(seq(0,1,length=500),colMeans(foo4[clust$clustering == 2,-1]),type='l',main=paste('Cluster 2:',round(P[2],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.2,.8,by=.2),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.05),label=rep(c(0,0.25,0.5,0.75),5))
mtext(c('None','Hospital','Hospice','SNF','Death'),3,line=.75,at=seq(.1,.95,by=.2),cex=.8)


plot(seq(0,1,length=500),colMeans(foo4[clust$clustering == 3,-1]),type='l',main=paste('Cluster 3:',round(P[3],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.2,.8,by=.2),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.05),label=rep(c(0,0.25,0.5,0.75),5))
mtext(c('None','Hospital','Hospice','SNF','Death'),3,line=.75,at=seq(.1,.95,by=.2),cex=.8)


plot(seq(0,1,length=500),colMeans(foo4[clust$clustering == 4,-1]),type='l',main=paste('Cluster 4:',round(P[4],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.2,.8,by=.2),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.05),label=rep(c(0,0.25,0.5,0.75),5))
mtext(c('None','Hospital','Hospice','SNF','Death'),3,line=.75,at=seq(.1,.95,by=.2),cex=.8)


plot(seq(0,1,length=500),colMeans(foo4[clust$clustering == 5,-1]),type='l',main=paste('Cluster 4:',round(P[4],2)),ylab='Proportion',xlab='',xaxt='n')
abline(v=seq(.2,.8,by=.2),col='black',lwd=1)
axis(1,at=seq(0,.95,by=0.05),label=rep(c(0,0.25,0.5,0.75),5))
mtext(c('None','Hospital','Hospice','SNF','Death'),3,line=.75,at=seq(.1,.95,by=.2),cex=.8)
