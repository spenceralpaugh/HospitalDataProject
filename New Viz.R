load('synthetic_ESSCLC_data.RData')

foo = matrix(0,nrow=nrow(episodes),ncol = length(t))
dat = data.frame(episodes[,c('ID','setting')],foo)
IDS = sample(unique(dat$ID),500) #this does not work with the current changes to the code, dat is not found and I can't remember where it was supposed to come from
episodes = episodes[episodes$ID %in% IDS,]
summary.stats = summary.stats[summary.stats$ID %in% IDS,]

#IDS <- sample(unique(episodes$ID),500)    #took from Initial Investigations 
#episodes <- episodes[episodes$ID %in% IDS,]
#summary.stats <- summary.stats[summary.stats$ID %in% IDS,]



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


#IDS = sample(unique(dat$ID),500)
foo3 = foo3[foo3$ID %in% IDS,]
foo4 = reshape(foo3,direction='wide',idvar='ID',timevar = 'setting')



#####REDO Code###########
t = seq(0,365*2,length = 100)
t = rev(rev(t)[-1])
snap = matrix(0,nrow=nrow(episodes),ncol = length(t))


indx = 1
for(i in t){
  snap[,indx] = ifelse(i>=episodes$FromDate&i<episodes$ThruDate,1,0)
  indx = indx + 1
}

dat2 = data.frame(episodes[,c('ID','setting')],snap)
require(dplyr)
require(plyr)
snap2 = ddply(dat2,.(ID,setting),function(d) colSums(d[,-(1:2)]) )

snap21 = expand.grid(unique(dat2$ID),c(unique(dat2$setting),"Death"))
names(snap21)=c('ID', 'setting')
snap3 = left_join(snap21,snap2) 
snap3[is.na(snap3)] = 0

for(i in unique(snap3$ID)){
  tmp=c(rep(1,2),colSums(snap3[snap3$ID==i,-(1:2)]))
  snap3[snap3$ID==i & snap3$setting=='Death', tmp==0]=1
}



snap4 = snap3[snap3$ID %in% IDS,]
snap5 = reshape(snap4,direction='wide',idvar='ID',timevar = 'setting')
######################

require(cluster)
require(ade4)
require(dplyr)
D1 = dist.binary(foo4[,-1],method=2) 

D2 = dist(summary.stats[summary.stats$ID %in% IDS,c(2:5)])
D2 = D2/max(D2)

D3 = dist(summary.stats[summary.stats$ID %in% IDS,c(6:9)])
D3 = D3/max(D3)

D4 = dist(summary.stats[summary.stats$ID %in% IDS,c(13)])
D4 = D4/max(D4)

D5 = dist.binary(snap5[,-1],method=2)

clust = pam(D1, 5) 
clustlab = data.frame(cbind(clust$clustering, IDS))
names(clustlab) = c("cluster","ID")
sorted.traj = episodes %>% ungroup() %>% left_join(clustlab, by="ID") %>% mutate(ID = factor(ID,levels=order(cluster,ID)))


require(ggplot2)
require(gridExtra)
max.days = 365

## Function to make the plots
## takes sorted trajectory data
plot.sorted.trajectories <- function(traj.dat){
  ## Only plot 1 thru 100
  plot.scaled.traj <- ggplot(filter(traj.dat,ID%in%IDS[1:100]),
                             aes(y=ID,yend=ID,x=FromDateRel,xend=ThruDateRel,group=ID)) +
    geom_segment(aes(col=setting),size=3) +
    xlab("Proportion") + ylab("") +
    scale_color_brewer(type="qual",palette=3,guide="none")+
    theme(legend.position="bottom",legend.title=element_blank(),panel.grid=element_blank(),axis.text.x=element_text(angle=90)) + coord_flip()
  plot.traj <- ggplot(filter(traj.dat,ID%in%IDS[1:100]),
                      aes(y=ID,yend=ID,x=FromDate,xend=ThruDate,group=ID)) +
    geom_segment(aes(col=setting),size=3) +
    scale_x_continuous("Days",limits=c(0,max.days)) + ylab("") +
    scale_color_brewer(type="qual",palette=3,guide="none")+
    theme(legend.position="bottom",legend.title=element_blank(),panel.grid=element_blank(),axis.text.x=element_text(angle=90)) + coord_flip()
  grid.arrange(plot.scaled.traj,plot.traj)
}

plot.sorted.trajectories(sorted.traj)
