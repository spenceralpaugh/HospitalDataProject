load('synthetic_ESSCLC_data.RData')
require(ggplot2)
require(tidyverse)

foo = matrix(0,nrow=nrow(episodes),ncol = length(t))
dat = data.frame(episodes[,c('ID','setting')],foo)
IDS = sample(unique(dat$ID),500) #this does not work with the current changes to the code, dat is not found and I can't remember where it was supposed to come from
episodes = episodes[episodes$ID %in% IDS,]
summary.stats = summary.stats[summary.stats$ID %in% IDS,]

IDSplot = sample(unique(episodes$ID),100) 
episodes1 = episodes[episodes$ID %in% IDSplot,]
summary.stats1 = summary.stats[summary.stats$ID %in% IDSplot,]

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


require(ade4)
D1 = dist.binary(foo4[,-1],method=2) #needs fixing for new code

D2 = dist(summary.stats[summary.stats$ID %in% IDS,c(2:5)])
D2 = D2/max(D2)

D3 = dist(summary.stats[summary.stats$ID %in% IDS,c(6:9)])
D3 = D3/max(D3)

D4 = dist(summary.stats[summary.stats$ID %in% IDS,c(13)])
D4 = D4/max(D4)

D5 = dist.binary(snap5[,-1],method=2)


Viz = function(D,widedat,stats,DLabels){
  require(shiny)
  require(cluster)
  Dm = length(D)
  shinyApp(
    ui = fluidPage(
      
      titlePanel("Hospitalization in Cancer Patients"),
      sidebarLayout(position="right",
                    sidebarPanel(selectInput('K',label = 'Select Number of Clusters',choices = 2:10,selected = 2),"Select Weighting For Clusters",
                                 
                                 uiOutput('weights')),
                    #mainPanel(plotOutput("Viz",height='500px'))
                    mainPanel(uiOutput('tabs'))
      )
    ),
    server = function(input, output){
      output$weights = renderUI({
        lapply(1:(Dm), function(i) {
          sliderInput(paste0('w', i), DLabels[i],
                      value = 1/Dm,0,1)
        })})
      
      output$tabs = renderUI({
        myTabs = lapply(0:(as.numeric(input$K)), function(i) {
          if(i>=1){
          tabPanel(paste0('Cluster ', i), plotOutput(paste0('plot',i)))
          }else{tabPanel('Overall', plotOutput('blockviz'))}
        })
        do.call(tabsetPanel, myTabs)
      })
      #cluster based on new distance
      
      
      
      observe({
        if(!is.null(input$w1)){
          
          #w=c(.1,.1,.2,.3,.3)
          Dtmp = Reduce('+',lapply(1:(Dm), function(i) eval(parse(text=paste0('input$w',i,'*D[[',i,']]'))))) #not finding weights yet here...
          #w[i]*D[[i]]))
          K = as.numeric(input$K)
          
          clusters = pam(Dtmp,k=K)$clustering
          #clusters
          
          output$blockviz = renderPlot({ #issue here with data
            clust = data.frame(ID=IDSplot,clust = clusters[IDS %in% IDSplot])
            episodes1 = episodes1 %>% left_join(clust) %>%  arrange(clust, ID,order)
            
            IDS1 = unique(episodes1$ID)
            episodes$FID = unlist(sapply(1:length(IDS1),function(i){
              m = nrow(episodes1[episodes1$ID == IDS1[i],])
              rep(i,m)
            }))
            
            L = episodes1 %>% group_by(clust) %>% summarize(m=max(FID)+1) %>% select(m) %>% slice(1:(K-1))
            
            plot1 = ggplot() +
              geom_rect(data=episodes, aes(xmin=FromDateRel, xmax=ThruDateRel, ymin=FID, ymax=FID+1, fill=setting)) +
              scale_fill_brewer(palette="Set1") +
              theme_minimal() +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
              xlab("Relative Time") +
              geom_hline(data = L,aes(yintercept = m), linetype="dotted") + 
              theme(legend.position = 'none')
            
            
            plot2 = ggplot() +
              geom_rect(data=episodes, aes(xmin=FromDate, xmax=ThruDate, ymin=FID, ymax=FID+1, fill=setting)) +
              scale_fill_brewer(palette="Set1") +
              theme_minimal() +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
              xlab("Days Since Diagnosis") +
              theme(legend.title=element_blank()) +
              geom_hline(data = L,aes(yintercept = m),linetype="dotted")
            
            require(cowplot)
            require(gridExtra)
            p <- plot_grid(plot1,plot2,ncol=2, rel_widths = c(1,1.5))
            title <- ggdraw() + draw_label("Sample Hospitalization in Cancer Patients", fontface='bold')
            plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
          })
            
          lapply(1:K, function(k){
            #cat(k,' BLAH')
            output[[paste0('plot',k)]] = renderPlot({
              foo = colMeans(widedat[clusters == k,-1])
              plot(foo[1:99],type='n',main=paste0('Cluster ',k),ylab='Proportion',xlab='',xaxt='n',ylim=c(0,1))
              for(j in 1:4){
                lines(foo[(1+99*(j-1)):(99 + 99*(j-1))],col=j)
              }
            })
          })
        }
        
      })
      # Add a column of summary info for each cluster....
    }
  )
  
}


Viz(list(D1,D2,D3,D4,D5),widedat=foo4,stats=summary.stats[c(2:9,13)],DLabels = paste0('Weight ',1:5))

#image(as.matrix(D1-D2),breaks=seq(-1,1,length=13))  #Why are these so similar?
#image(as.matrix(D1-D3),breaks=seq(-1,1,length=13)) 
#image(as.matrix(D1-D4),breaks=seq(-1,1,length=13)) 
#image(as.matrix(D1-D5),breaks=seq(-1,1,length=13)) 
#image(as.matrix(D4-D5),breaks=seq(-1,1,length=13)) 
