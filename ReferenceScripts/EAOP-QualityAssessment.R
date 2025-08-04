################################################################################$
## Assessing the Quality of Emergent and Existing AOPs Using Semantic Analysis ##
##                         ~~~~~~~~~~~~~~~~~~~~~~~                             ##
## Code written by N. Pollesch with statistical support by K. Vitense          ##
##        contact pollesch.nathan@epa.gov with any questions                   ##
################################################################################$

## Packages needed ##
library(Hmisc)
library(pheatmap)
library(stats)
library(dendextend)
library(gdata)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
library(grid)
library(gtable)
library(cowplot)
library(lemon)
library(RColorBrewer)
library(DescTools)
library(fields)
library(NSM3)
library(boot)
library(gtools)
library(stringr)
library(gridBase)
library(graph4lg)
library(readxl)
library(gplots)
library(plot.matrix)
library(rlang)


## DATA FILES ##
## & SYMBOLS  ##

## Load Data Files List
load("AOPs_scored.RDA") ## data.frame of scored AOPs based on multiple metrics
load("EAOPs_scored.RDA") ## data.frame of scored EAOPs based on multiple metrics
load("KER_weights.RDA") ## data.frame of KER weights (WOE & QU) for KERs
load("EAOPS.RDA") ## data.frame of EAOPS including all KEs
load("KE2KE_SS.RDA") ## data.frame of semantic similiarity scores for KE pairs

## R Symbol assignments 
`%!in%`<-Negate(`%in%`) # Creates a custom command for 'not in' that is used to help identify if a KE ID is not in a particular data.frame()

###########################################################$
#### Compute and visualize Rank Correlations of Metrics ####
###########################################################$

#############$
#### AOPs ####
#############$

## Calculate rank correlation
## This is being done to assess how evenly the methods rank the AOPs based on the different quality metrics
## Starting with the AOPs_scored data Object that stores the scores for AOPS based on the different metrics

## View data set needed
head(AOPs_scored)

## Calculate rank correlations
AOP_rankcorrs<-cor(as.matrix(AOPs_scored[,2:8]),method="kendall",use="pairwise.complete.obs")

## Remove diagonals for visualization in heatmap and clustering
## Plot heatmap
AOP_rankcorrs_nodiag<-AOP_rankcorrs
diag(AOP_rankcorrs_nodiag)<-NA
pheatmap(AOP_rankcorrs_nodiag,display_numbers=T,rownames=c("SSS_mean","SSS_min","PSS_mean","SQWOE_mean","SQWOE_min","SQQU_mean","SQQU_min"),main="Rank correlations between metrics for AOPs")

##############################################$
## Add CIs for EAOP Kendall Tau-b statistics ##
##############################################$

## Remove any EAOPs that have any NA values in for any metric
AOP_scored_nona<-drop_na(AOPs_scored[,2:8])
AOP_scored_nona

## Names of metrics to combine and run (7 items in combinations of 2)
AOP_metric_combn<-combinations(7,2,colnames(AOP_scored_nona))

## Build df to hold data
AOP_RC_ALL<-data.frame(names=rep(NA,dim(AOP_metric_combn)[1]))

####################################################$
## Bootstrapping for AOPs Metric Rank Correlations ##
####################################################$

## Define function 'fc' specifically for use in the boot function
fc <- function(d, i, X, Y){
  d2 <- d[i,]
  return(cor(d2[X], d2[Y],method="kendall",use="pairwise.complete.obs"))
}
## Loop for building AOP_RC_ALL data.frame contents, including names, Tau-b values, and bootstrapped CIs
for(i in 1:dim(AOP_metric_combn)[1]){
  names<-AOP_metric_combn[i,]
  flatname<-str_flatten(AOP_metric_combn[i,],collapse="::")
  AOP_RC_ALL[i,"names"]<-flatname
  ##Get tau stat
  AOP_RC_ALL[i,"tau"]<-cor(AOP_scored_nona[,names[1]],AOP_scored_nona[,names[2]],method="kendall",use="pairwise.complete.obs")
  ##Bootstrap
  boot_out<-boot(AOP_scored_nona,fc,R=10000,X=names[1],Y=names[2])
  boot_ci_out<-boot.ci(boot.out=boot_out,type="bca")
  AOP_RC_ALL[i,"LB"]<-boot_ci_out$bca[4]
  AOP_RC_ALL[i,"UB"]<-boot_ci_out$bca[5]
  }
## View once complete
AOP_RC_ALL
## Add type for combining
AOP_RC_ALL$type<-"AOP"
## Sort by tau stat
AOP_RC_ALL<-AOP_RC_ALL %>% arrange(desc(tau))


##############$
#### EAOPs ####
##############$

## View data set needed
head(EAOPs_scored)

#Heatmap visualizations
EAOP_rankcorrs<-cor(as.matrix(EAOPs_scored[,2:8]),method="kendall",use="pairwise.complete.obs")
EAOP_rankcorrs_nodiag<-EAOP_rankcorrs
diag(EAOP_rankcorrs_nodiag)<-NA
pheatmap(EAOP_rankcorrs_nodiag,display_numbers=T, main="Rank correlations between metrics for EAOPs")

##############################################$
## Add CIs for EAOP Kendall tau-b statistics ##
##############################################$

## Remove any EAOPs that have any NA values in for any metric
EAOP_scored_nona<-drop_na(EAOPs_scored[,2:8])
EAOP_metric_combn<-combinations(7,2,colnames(EAOP_scored_nona))

## Build df to hold data
EAOP_RC_ALL<-data.frame(names=rep(NA,dim(EAOP_metric_combn)[1]))

####################################################$
## Bootstrapping for AOPs Metric Rank Correlations ##
####################################################$

## Define function 'fc' specifically for use in the boot function
fc <- function(d, i, X, Y){
  d2 <- d[i,]
  return(cor(d2[X], d2[Y],method="kendall",use="pairwise.complete.obs"))
}

## Loop for building EAOP_RC_ALL data.frame contents, including names, Tau-b values, and bootstrapped CIs
for(i in 1:11){
  names<-EAOP_metric_combn[i,]
  flatname<-str_flatten(EAOP_metric_combn[i,],collapse="::")
  EAOP_RC_ALL[i,"names"]<-flatname
  ##Get tau stat
  EAOP_RC_ALL[i,"tau"]<-cor(EAOP_scored_nona[,names[1]],EAOP_scored_nona[,names[2]],method="kendall",use="pairwise.complete.obs")
  ##Bootstrap
  boot_out<-boot(EAOP_scored_nona,fc,R=10000,X=names[1],Y=names[2])
  boot_ci_out<-boot.ci(boot.out=boot_out)
  EAOP_RC_ALL[i,"LB"]<-boot_ci_out$bca[4]
  EAOP_RC_ALL[i,"UB"]<-boot_ci_out$bca[5]
}
## Note: Loop broken into two sections to avoid bootstrapping error at i=12 due to SQQU_min vector containing so many 0 values
for(i in 13:dim(EAOP_metric_combn)[1]){
  names<-EAOP_metric_combn[i,]
  flatname<-str_flatten(EAOP_metric_combn[i,],collapse="::")
  EAOP_RC_ALL[i,"names"]<-flatname
  ##Get tau stat
  EAOP_RC_ALL[i,"tau"]<-cor(EAOP_scored_nona[,names[1]],EAOP_scored_nona[,names[2]],method="kendall",use="pairwise.complete.obs")
  ##Bootstrap
  boot_out<-boot(EAOP_scored_nona,fc,R=10000,X=names[1],Y=names[2])
  boot_ci_out<-boot.ci(boot.out=boot_out)
  EAOP_RC_ALL[i,"LB"]<-boot_ci_out$bca[4]
  EAOP_RC_ALL[i,"UB"]<-boot_ci_out$bca[5]
}

## View output
head(EAOP_RC_ALL)

## Add type for combining
EAOP_RC_ALL$type<-"EAOP"

## Sort by tau stat
EAOP_RC_ALL<-EAOP_RC_ALL %>% arrange(desc(tau))
head(EAOP_RC_ALL)

## COMBINE FOR NEW PLOT FOR NEW CI METHOD USING BOOT ##
all_RC<-rbind(AOP_RC_ALL,EAOP_RC_ALL)
head(all_RC)

#################################################$
## PAIRWISE AOP/EAOP RCs WITH CI FOR MANUSCRIPT ##
#################################################$

## Reorder to get plot to be in mostly descending order from Tau-b values
all_RC_sort<- all_RC %>% group_by(names) %>%  # group by Name
  mutate(NameMax = max(tau)) %>%  # create a temp variable holding the max of each Name
  arrange(desc(NameMax),type)
## Generate plot order to pass to ggplot in 'limits' of scale_x_discrete()
plot_order<-unique(all_RC_sort$names)


## Plot
dev.off()
ggplot(all_RC_sort,aes(names,tau,color = type, shape=type)) + 
  geom_hline(yintercept=0,linetype="dotted")+
  geom_errorbar(aes(ymin=LB,ymax=UB),na.rm=T,position = position_dodge(width=.25))+
  geom_point(size=3, position = position_dodge2(width=.25,reverse=F), show.legend=T) +
  theme_classic() +
  labs(title="Pairwise Comparison of Metric Rank Correlations by AOP Type",shape="Type",color="Type") +
  theme(axis.text.x = element_text(angle = 90),
        axis.title  = element_blank(),
        plot.title  = element_text(hjust = 0.5)) +
  scale_x_discrete(limits=plot_order)+
  scale_color_manual(values=c("AOP"="goldenrod","EAOP"="steelblue"))+
  guides(fill=guide_legend(reverse=F))

#################################$
## HEATMAP PLOTS FOR MANUSCRIPT ##
#################################$

## Note: These create files and don't print to R display window
aophm<-pheatmap(AOP_rankcorrs_nodiag,display_numbers=T, color=colorRampPalette(c("goldenrod","white","cornsilk2"))(100), na_col="white",border_color="black", number_color="black", treeheight_row=20,treeheight_col=20,cex.main=1.5,main=print("Rank Correlations Between Metrics for AOPs \n"),filename="Rank_Correlations_Between_Metrics_for_AOPs.png",width=8,height=6)
eaophm<-pheatmap(EAOP_rankcorrs_nodiag,display_numbers=T, color=colorRampPalette(c("steelblue","white","grey"))(100), na_col="white",border_color="black", number_color="Black",treeheight_row=20,treeheight_col=20,cex.main=1.5,main=print("Rank Correlations Between Metrics for EAOPs \n"),filename="Rank_Correlations_Between_Metrics_for_EAOPs.png",width=8,height=6)

#####################################$
## DENDROGRAMS PLOTS FOR MANUSCRIPT ##
#####################################$
dev.off()
par(mfrow=c(1,2))
plot(aophm[[1]],lwd=2,axes=F,xlab='',ylab='',main="Complete Cluster Dendrogram of \n AOP Metric Rank Correlations",sub='')
rect.hclust(aophm[[1]],h=60,border=c("darksalmon","skyblue2"))
text(x=2.55,y=1.47,labels="Semantic",col="black",cex=.9)
text(x=4.55,y=1.47,labels="Expert",col="black",cex=.9)
plot(eaophm[[1]],lwd=2,axes=F,xlab='',ylab='',main="Complete Cluster Dendrogram of \n EAOP Metric Rank Correlations",sub='',horizontal=T)
text(x=2.55,y=1.36,labels="Semantic",col="black",cex=.9)
text(x=4.55,y=1.35,labels="Expert",col="black",cex=.9)
rect.hclust(eaophm[[1]],h=50,border=c("darksalmon","skyblue2"))

## Black and white version used in manuscript
par(mfrow=c(1,2))
plot(aophm[[1]],lwd=2,axes=F,xlab='',ylab='',main="Complete Cluster Dendrogram of \n AOP Metric Rank Correlations",sub='')
rect.hclust(aophm[[1]],h=60,border=c("black","black"))
text(x=2.55,y=1.47,labels="Semantic",col="black",cex=.9)
text(x=4.55,y=1.47,labels="Expert",col="black",cex=.9)
plot(eaophm[[1]],lwd=2,axes=F,xlab='',ylab='',main="Complete Cluster Dendrogram of \n EAOP Metric Rank Correlations",sub='',horizontal=T)
text(x=2.55,y=1.36,labels="Semantic",col="black",cex=.9)
text(x=4.55,y=1.35,labels="Expert",col="black",cex=.9)
rect.hclust(eaophm[[1]],h=50,border=c("black","black"))

##################################################$
## PLOT AOP/EAOP SCORES BY METRIC FOR MANUSCRIPT ##
##################################################$

names(AOPs_scored)[1]<-"ID"
names(EAOPs_scored)[1]<-"ID"

AOPs_scored$AOP_Type<-"AOP"
EAOPs_scored$AOP_Type<-"EAOP"

All_scored<-rbind(AOPs_scored,EAOPs_scored)
all_long <- melt(All_scored, id.vars = c("ID","AOP_Type"), variable.name = "Metric")
View(all_long)

## Add new category, metric type
all_long$Metric_Type<-NA
all_long[which(all_long$Metric %in% c("SSS_mean","SSS_min","PSS_mean")),5]<-"Semantic"
all_long[which(all_long$Metric %!in% c("SSS_mean","SSS_min","PSS_mean")),5]<-"Expert"
View(all_long)

## Use custom function to help organize visualization of legend in Facet ggplot
shift_legend2 <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  reposition_legend(p, 'center', panel=names)
}


p2<- ggplot(all_long, aes(x=Metric,y=value,fill=AOP_Type)) + 
  geom_rect(aes(fill=Metric_Type),data=all_long[!duplicated(all_long$Metric),],inherit.aes=FALSE,xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.2)+
  geom_boxplot(aes(fill=AOP_Type,col=AOP_Type))+ 
  facet_wrap(~factor(Metric, c("SSS_mean","SQWOE_mean","SQQU_mean","SSS_min","SQWOE_min","SQQU_min","PSS_mean")), nrow=3,ncol=3,dir="v",scale="free_x")+
  scale_fill_manual(values = c("Semantic" = "brown","Expert" = 4,
                               "AOP"="goldenrod","EAOP"="steelblue"), breaks=c("Semantic","Expert"))+
  labs(title="Metric Scores by AOP Type and Metric Class",color="AOP Type:",fill="Metric Class:",x="",y="Metric Scores")+
  #scale_fill_manual(values=c("black","steelblue"))+
  scale_color_manual(values=c("black","black"),
                     guide = guide_legend(override.aes = list(fill=c("goldenrod","steelblue"), color = c("black","black"))))+
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        plot.title = element_text(hjust=0.5))
grid.draw(shift_legend2(p2))



#################$
## KER ANALYSIS ##
#################$

#### Analysis of KER to SS ####
## View KER weights dataset
head(KER_weights)

## View boxplots to see how these SS scores compare across levels of QQU and QWOE
dev.off()
par(mfrow=c(2,2),oma=c(0,0,2,0))
bp1<-boxplot(SS~QWOE,data=KER_weights,na.rm=T,main="KER Metrics: SS by QWOE")
bp2<-boxplot(SS~QQU,data=KER_weights,na.rm=T,main="KER Metrics: SS by QQU")
bp3<-boxplot(QQU~QWOE,data=KER_weights,na.rm=T,main="KER Metrics: QQU by QWOE")
mtext("Relationships between KER Quality/Similiarity Metrics",line=0, side=3, outer=TRUE, cex=1.5)

## Rank correlations for KERs
KER_rankcorrs<-cor(as.matrix(KER_weights[,c(5,6,8)]),method="kendall",use="pairwise.complete.obs")
KER_rankcorrs_nodiag<-KER_rankcorrs
diag(KER_rankcorrs_nodiag)<-NA
## heatmap viz
pheatmap(KER_rankcorrs_nodiag,display_numbers=T,main="Rank Correlations among KER quality measures",cluster_cols=T)

## 95% CIs for Rank Correlations (Kendall tau-b)
head(KER_weights)
## Drop NAs
KER_scored_nona<-drop_na(KER_weights[,c(5,6,8)])
KER_metric_combn<-combinations(3,2,colnames(KER_scored_nona[]))

## Build dataframe to hold data
KER_RC_ALL<-data.frame(names=rep(NA,dim(KER_metric_combn)[1]))

for(i in 1:dim(KER_metric_combn)[1]){
  names<-KER_metric_combn[i,]
  flatname<-str_flatten(KER_metric_combn[i,],collapse="::")
  KER_RC_ALL[i,"names"]<-flatname
  ##Get tau stat
  KER_RC_ALL[i,"tau"]<-cor(KER_scored_nona[,names[1]],KER_scored_nona[,names[2]],method="kendall",use="pairwise.complete.obs")
  ##Bootstrap
  boot_out<-boot(KER_scored_nona,fc,R=10000,X=names[1],Y=names[2])
  boot_ci_out<-boot.ci(boot.out=boot_out)
  KER_RC_ALL[i,"LB"]<-boot_ci_out$bca[4]
  KER_RC_ALL[i,"UB"]<-boot_ci_out$bca[5]
}

head(KER_RC_ALL)

## VISUALIZE with 95% CI
ggplot(KER_RC_ALL,aes(names, tau)) + 
  geom_point(size=3, position = position_dodge2(width=.25,reverse=F), show.legend=T) +
  #theme_classic() +
  geom_errorbar(aes(ymin=LB,ymax=UB),na.rm=T,position = position_dodge(width=.25))+
  labs(title="KER Metric Rank Correlations") +
  theme(axis.text.x = element_text(angle = 0),
        axis.title.x  = element_blank(),
        plot.title  = element_text(hjust = 0.5)) +
  ylab("Tau-b")+
  geom_hline(yintercept=0,linetype="dashed")+
  guides(fill=guide_legend(reverse=F))


############################$
## KER PLOT FOR MANUSCRIPT ##
############################$

## Combine boxplots and heatmap in a single 4 panel diagram
dev.off()
par(mfrow=c(2,2),oma=c(0,0,2,0))
bp1<-boxplot(SS~QWOE,data=KER_weights,na.rm=T,main="(a) KER Metrics: SS by QWOE",col="darkseagreen")
bp2<-boxplot(SS~QQU,data=KER_weights,na.rm=T,main="(b) KER Metrics: SS by QQU",col="darkseagreen2")
bp3<-boxplot(QQU~QWOE,data=KER_weights,na.rm=T,main="(c) KER Metrics: QQU by QWOE",col="seagreen3")
mtext("Relationships Between KER Metrics",line=0, side=3, outer=TRUE, cex=1.5)
plot.new()              ## suggested by @Josh
vps <- baseViewports()
pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values 
p<-ggplot(KER_RC_ALL,aes(names, tau,col=names)) + 
  geom_hline(yintercept=0,linetype="dotted")+
  geom_errorbar(aes(ymin=LB,ymax=UB),na.rm=T,position = position_dodge(width=.25),col="black")+
  geom_point(size=3, position = position_dodge2(width=.25,reverse=F), show.legend=T,col="black") +
  theme_classic() +
  labs(title="(d) KER Metric Rank Correlations")+
  ylab("Tau-b")+
  theme(axis.text.x = element_text(angle = 0),
        axis.title.x  = element_blank(),
        plot.title  = element_text(hjust = 0.5)) +
  #scale_fill_manual("AOP Type",values=c("sort.EAOPs_RC_s_u."="black","sort.AOPs_RC_s_u."="steelblue"),labels=c("AOP","eAOP"))+
  guides(fill=guide_legend(reverse=F))
print(p,vp=vp1)


###########################$
## EAOP SS Matrix Heatmap ##
###########################$

## Visualize the scores of the eAOP by organizing in a lower-diagonal matrix and visualizing
head(EAOPs)

## Define diags() function to quickly extract super- and sub-diagonals (https://stackoverflow.com/questions/15672585/looping-through-diagonal1-of-a-matrix)
diags<- function(m, type = c("sub", "super"), offset = 1) {
  type <- match.arg(type)
  FUN <-
    if(isTRUE(all.equal(type, "sub")))
      `+`
  else
    `-`
  m[row(m) == FUN(col(m), offset)] 
}


############################################$
## A function to score and visualize EAOPs ##
############################################$

## Note that is function is for use with linear EAOPs, generalization to functions for all AOPs,
## linear, or not, will require modification, specifically for non-lower triangular score matrices (i.e. full score matrices)
## and aggregation techniques that may include cycles within AOPs that are not present in linear AOPs

EAOPScoreMap<-function(EAOPnumber,scores,EAOPList=EAOPs,nonSS=F,meanDups=F,showMetrics=T,showScoreFile=T,scoreFileAsSub=F,textCex=1,retImage=T,retScores=F,retMat=F,retMet=F,customEAOPTitle=F,customTitleText=NA){
  
  ## Extract eaop KEs and remove NAs
  eaop<-EAOPList[EAOPnumber,]
  eaopvec<-as.vector(unlist(eaop))[-1]
  eaopvec_nona<-eaopvec[!is.na(eaopvec)]
  
  ## Create all combinations of KEs for score gathering
  seaop<-expand.grid(eaopvec_nona,eaopvec_nona)
  
  ## ensure label names match and data types
  names(seaop)<-c("KEX","KEY")
  seaop[,1]<-as.character(seaop[,1])
  seaop[,2]<-as.character(seaop[,2])
  seaop[,1]<-as.numeric(seaop[,1])
  seaop[,2]<-as.numeric(seaop[,2])
  ## import and ensure labels for scores df input
  scoredfpre<-as.data.frame(scores)
  scoredfpre[,1]<-as.character(scoredfpre[,1])
  scoredfpre[,2]<-as.character(scoredfpre[,2])
  scoredfpre[,1]<-as.numeric(scoredfpre[,1])
  scoredfpre[,2]<-as.numeric(scoredfpre[,2])
  
  ## Deal with duplicates in scores for nonSS scores
  if(nonSS){
  if(meanDups){
    #If you want to aggregate duplicates in nonSS scores by using their mean
  scoredf <- scoredfpre %>%
    group_by(KEX, KEY) %>%
    summarize(score = if (all(is.na(score))) NA else mean(score, na.rm = TRUE))}
  else{
    #Else duplicates are aggregated in nonSS scores by using their max
    scoredf <- scoredfpre %>%
      group_by(KEX, KEY) %>%
      filter(if (all(is.na(score))) TRUE else !is.na(score)) %>%
      slice_max(order_by = score, n = 1, with_ties = FALSE)}}
  if(!nonSS){scoredf<-scoredfpre} 
  
  ## Apply scores to eaop dataframes
  seaop_lj<-suppressMessages(left_join(seaop,scoredf))

  ## Assign 1 to diagonals or self score by default for reflexive (semantic similiarity) type scores
  ## Ensure reflexive scores, i.e. SS(X,Y)=SS(Y,X)
  ## This only happens if the score type is semantic (or a reflexive type).  KER weights are not.
  if(!nonSS){
    for(i in 1:dim(seaop_lj)[1]){
      if(seaop_lj[i,1]==seaop_lj[i,2]){
        seaop_lj[i,3]<-1
      }
    }
    
  rseaop<-data.frame(KEX=seaop_lj$KEY,KEY=seaop_lj$KEX,score=seaop_lj$score)
  aseaop<-suppressMessages(left_join(seaop_lj,rseaop,by=c("KEX","KEY")))
  
  for(i in 1:dim(aseaop)[1]){
    if(is.na(aseaop$score.x[i])){
      aseaop$score.x[i]<-aseaop$score.y[i]}
    else if(is.na(aseaop$score.y[i])){
      aseaop$score.y[i]<-aseaop$score.x[i]}
    else if(aseaop$score.x[i]!=aseaop$score.y[i]){
      aseaop$score.x[i]<-mean(c(aseaop$score.x[i],aseaop$score.y[i]))
      aseaop$score.y[i]<-aseaop$score.x[i]
    }
  }
  dseaop<-data.frame(KEX=aseaop$KEX,KEY=aseaop$KEY,score=aseaop$score.x)
  }
  
  if(nonSS){dseaop<-seaop_lj}
  
  ## Create a score matrix
  eaopMat<-xtabs(score~.,dseaop)
  eaopMat<-as.matrix(eaopMat)
  
  ## Change dimension names from KEX KEY to KE KE
  names(dimnames(eaopMat))<-c("KE","KE")
  
  ## Ensure proper order of score metrics for interpretation (Self score on diagonals)
  if(!nonSS){
  eaopMat<-reorder_mat(mat=eaopMat,order=eaopvec_nona)}
    else if(nonSS){
    eaopMat_reordered_rows<-eaopMat[eaopvec_nona,]
    eaopMat_reorder_cols<-eaopMat_reordered_rows[,eaopvec_nona]
    eaopMat<-t(eaopMat_reorder_cols)
    }
  
  ## Ensure only lower tridiagonal is considered
  eaopMat[upper.tri(eaopMat,diag=T)]=NA
  eaopMatLT<-round(eaopMat,6)
  
  ## Calculate metrics for SS Scores
  smean<-round(mean(diags(eaopMatLT,"sub",1)),3)
  smin<-round(min(diags(eaopMatLT,"sub",1)),3)
  pmean<-round(mean(eaopMatLT[lower.tri(eaopMatLT)]),3)
  ## Add to metric df
  metrics<-data.frame("Seq-mean"=smean,"Seq-min"=smin,"Pw-mean"=pmean)
  call <- match.call()
  score_name <- if (!is.null(call[["scores"]])) deparse(call[["scores"]]) else "KE2KE_SS"
  subtitle<-paste0("Scores from '",score_name,"'")
  if(retScores){
   return(dseaop)}
  else if(retMat){
      return(eaopMatLT)}
  else if(retMet){
      return(metrics)}
  else if(retImage){# & KElabels){
    plotmat<-eaopMatLT
    kenames<-colnames(eaopMatLT)
    res<-plot(plotmat,na.cell=F,fmt.cell='%.4f',col=viridis,xlab="KE Down",ylab="KE UP",key=NULL, axes=F,axis.row = list(side=2, las=1,cex=textCex),axis.col = list(side=1, las=1,cex=textCex),
              col.ticks=NA,outer=NA,cex=textCex,main=paste0("KE to KE Score Matrix for \n EAOP: ",ifelse(customEAOPTitle,customTitleText,EAOPnumber)),sub=if(scoreFileAsSub) subtitle else "")
    
    if(showMetrics){
      matdim<-dim(eaopMatLT)[1]
      metlen<-length(metrics)
      metText<-c()
      for(i in 1:metlen){metText[i]<-paste0(names(metrics)[i],": ",metrics[1,i])}
      allMetText<-paste(c(" ",metText),collapse="\n")
      text(matdim,matdim,labels=allMetText,cex=textCex,adj=c(1,1))
      text(matdim,matdim,labels=expression(bold("Metric: value")),adj=c(1,1),cex=textCex)
      if(showScoreFile){
      subtitle_space<-paste(c(rep(" ",times=metlen+1),subtitle),collapse="\n")
      if(!scoreFileAsSub) text(matdim,matdim,labels=subtitle_space,adj=c(1,1),cex=textCex)}
      }
  }
}

#############################################################$
#### Visualization for Manuscript compare EAOP score MAPS ####
#############################################################$

dev.off()
par(mfrow=c(1,2))
EAOPScoreMap(1606,scores=KE2KE_SS,nonSS=F,meanDups=F,retMet=F,textCex=.8,showScoreFile=F)
## Note currently in use, but may be useful to highlight certain cells for visualizations created by the EAOPScoreMap() function
bold_cells <- list(c(1, 1), c(2, 1), c(3, 1),c(4,1),c(5,1))  # Example: cells at (2,2), (3,3), (4,4)
# Iterate over the cells and draw thicker borders
for (cell in bold_cells) {
  x <- cell[1]
  y <- cell[2]
  
  # Draw a rectangle with a thicker border around the specified cell
  rect(xleft = x - 0.5, ybottom = y - 0.5, xright = x + 0.5, ytop = y + 0.5,
       border = "red", lwd = 3)  # lwd controls the line width
}

EAOPScoreMap(1698,scores=KE2KE_SS,nonSS=F,meanDups=F,retMet=F,textCex=.8,showScoreFile=F)
bold_cells <- list(c(1, 1), c(2, 1), c(3, 1),c(4,1),c(5,1))  # Example: cells at (2,2), (3,3), (4,4)
# Iterate over the cells and draw thicker borders
for (cell in bold_cells) {
  x <- cell[1]
  y <- cell[2]
  
  # Draw a rectangle with a thicker border around the specified cell
  rect(xleft = x - 0.5, ybottom = y - 0.5, xright = x + 0.5, ytop = y + 0.5,
       border = "red", lwd = 3)  # lwd controls the line width
}

##########################################################################$
####    Create a generic visualization with new EAOP list and scores   ####
##########################################################################$
#EAOPScoreMap<-function(EAOPnumber,scores,EAOPList=EAOPs,nonSS=F,meanDups=F,showMetrics=T,showScoreFile=T,scoreFileAsSub=F,textCex=1,retImage=T,retScores=F,retMat=F,retMet=F)
EAOPNum<-1

#Manually create scores for the generic EAOP
xcomb<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
ycomb<-rep(c(1,2,3,4),4)
generic_scores<-data.frame(KEX=xcomb,KEY=ycomb,score=c(NA,NA,NA,NA))
generic_scores
generic_scores[2,"score"]<-0.75
generic_scores[3,"score"]<-0.25
generic_scores[4,"score"]<-0.1
generic_scores[7,"score"]<-0
generic_scores[8,"score"]<-0.5
generic_scores[12,"score"]<-0.95
generic_scores

#manually create EAOPlist
names(EAOPs)
EAOPli<-EAOPs[1,]
EAOPli[1,2]<-1
EAOPli[1,3]<-2
EAOPli[1,4]<-3
EAOPli[1,5]<-4
EAOPli[1,6]<-NA

#Attempt to visualize
EAOPScoreMap(1,scores=generic_scores,EAOPList=EAOPli,showScoreFile=F,customEAOPTitle=T,customTitleText="Example")

#Visualize another EAOP for presentation
EAOPScoreMap(8441,scores=KE2KE_SS,showScoreFile=F)

##########################################################################$
#### Explore use of EAOPScoreMap() with non-semantic similarity scores ####
##########################################################################$

## Create new KE to KE scores using KER weights
KER_QWOE<-data.frame(KEX=KER_weights$KEX,KEY=KER_weights$KEY,score=KER_weights$QWOE)
KER_QQU<-data.frame(KEX=KER_weights$KEX,KEY=KER_weights$KEY,score=KER_weights$QQU)


## 
EAOPScoreMap(52,scores=KER_QWOE,nonSS=T,retMet=F)


## Visualize scores using SS, QWOE, and QQU Scores
dev.off()
par(mfrow=c(2,2))
EAOPScoreMap(52,scores=KE2KE_SS,nonSS=F,meanDups=F,retMet=F,scoreFileAsSub=T, textCex=.8)
EAOPScoreMap(52,scores=KER_QWOE,nonSS=T,scoreFileAsSub = T, textCex=.8)
EAOPScoreMap(52,scores=KER_QQU,nonSS=T,scoreFileAsSub = T, textCex=.8)


############################ NOT IN USE #########################$
#### Alternate way to score all EAOPs by using EAOPScoreMap() ####
####      from method used in AOP-SemanticAnalysis.R          ###$
############################ NOT IN USE #########################$
# 
# ssscores<-data.frame(SSSMean=c(),SSSMin=c(),PSSMean=c())
# ssscores[1,1:3]<-EAOPScoreMap(1,scores=KE2KE_SS,retMet=T)
# 
# for(i in 1:dim(EAOPs)[1]){
#   ssscores[i,1:3]<-EAOPScoreMap(i,scores=KE2KE_SS,retMet=T)
# }
# 
# write.csv(ssscores,file="EAOPScores_KE2KE_SS.csv")
# 
# ## Repeat for QWOE
# QWOEscores<-data.frame(SWOEMean=c(),SWOEMin=c(),PWOEMean=c())
# QWOEscores[1,1:3]<-EAOPScoreMap(1,scores=KER_QWOE,retMet=T,nonSS=T)
# 
# for(i in 1:dim(EAOPs)[1]){
#   QWOEscores[i,1:3]<-EAOPScoreMap(i,scores=KER_QWOE,retMet=T,nonSS=T)
# }
# 
# write.csv(QWOEscores,file="EAOPScores_KER_QWOE.csv")
# 
# ## Repeat for QQU
# QQUscores<-data.frame(SWOEMean=c(),SWOEMin=c(),PWOEMean=c())
# QQUscores[1,1:3]<-EAOPScoreMap(1,scores=KER_QQU,retMet=T,nonSS=T)
# 
# for(i in 1:dim(EAOPs)[1]){
#   QQUscores[i,1:3]<-EAOPScoreMap(i,scores=KER_QQU,retMet=T,nonSS=T)
# }
# 
# write.csv(QQUscores,file="EAOPScores_KER_QWOE.csv")
