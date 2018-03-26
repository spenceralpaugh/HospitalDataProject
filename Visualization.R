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
        myTabs = lapply(1:(as.numeric(input$K)), function(i) {
          tabPanel(paste0('Cluster ', i), plotOutput(paste0('plot',i)))
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
