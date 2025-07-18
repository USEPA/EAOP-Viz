################################################################################$
## Assessing the Quality of Emergent and Existing AOPs Using Semantic Analysis ##
##                         ~~~~~~~~~~~~~~~~~~~~~~~                             ##
## Code written by N. Pollesch with code co-developed by J. O'Brien            ##
##        contact pollesch.nathan@epa.gov with any questions                   ##
################################################################################$

## This file contains 2 major components
## 1) AOPWiki and Emergent AOPs: Imports and formats a specified AOPwiki XML file to create the AOPwiki network and extract emergent AOPs and their associated information (based on Pollesch et al. 2019)
## 2) Semantic Analysis of AOPs and EAOPs: Defines custom algorithms for the analysis of AOPs and eAOPs using the imported semantic similiarity scores from Wang 2020

## The outputs of this file inculde:
## 1) The lists of all scored AOPs and eAOP as csv files
##    'aopkess_**.xlsx', 
## 2) The lists of the top 10 of each eAOP based on three different metrics
##    "eAOPTop10-PSSMean.xlsx", "eAOPTop10-SSSMin.xlsx", "eAOPTop10-SSSMean.xlsx"

## Packages needed
library(xml2)
library(igraph)
library(prodlim)
library(RColorBrewer)
library(plotrix)
library(autoimage)
library(dplyr)
library(openxlsx)
library(readxl)
library(data.table)
library(ggplot2)
library(latex2exp)
library(ggrepel)

## R Symbol assignments
`%!in%`<-Negate(`%in%`) # Creates a custom command for 'not in' that is used to help identify if a KE ID is not in a particular data.frame()

## File list:
## 'AOP-Net-Functions.R' # The custom functions developed for AOP Network analyses included
## 'AOP-wiki-data.xml' # XML file from the AOPWiki containing the data for analysis
## 'eLaops.named.xlsx' # Imports a list of all emergent AOPs (Originially identified in Pollesch et al., 2019)
## 'KE2KE_SS_Dups.xlsx' # An excel file with semantic similiarity scores between all KEs, 'Dups' indicates here that some KEs have multiple SS scores, based on multiple logical definitions
## 'PSSEAOP-IndiciesfromeLAOPsNamed.csv' # Contains proper subset eAOPs so that they can be easily removed from the eLAOP list 

#### REMOVE BEFORE SUBMISSION/GENERALIZE TO NOT YOUR COMPUTER ####
## Set source directory
sourceDir<-paste("~/Projects/GitHub/Emergent-AOP") ## Sets sourceDir variable to current working directory subfolder 'R'
####

######################################$
#### 1 - AOPWiki and Emergent AOPs ####
######################################$

## Load AOP Network Analysis function from Pollesch et. al 2019 
source("AOP-Net-Functions.R") ## Imports custom functions from location set by sourceDir variable


## Provide AOP-Wiki XML file location
fName<-paste(sourceDir,"/Data/AOP-wiki-data.xml",sep="") ##This assumes that there is a subfolder 'Data/AOPWikiSnapshots' within the working directory where the XML file is located

xData<-read_xml(fName)
xData<-xml_ns_strip(xData)

## Ref ID to AOPwiki ID

keID<-data.frame(
  ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"),"id"),
  ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"),"aop-wiki-id"),
  stringsAsFactors=FALSE
)

kerID<-data.frame(
  ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"),"id"),
  ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"),"aop-wiki-id"),
  stringsAsFactors=FALSE
)

aopID<-data.frame(
  ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"),"id"),
  ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"),"aop-wiki-id"),
  stringsAsFactors=FALSE
)


## Key event (KE) Data

keData<-data.frame(
  ID=keID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event"), "id"),keID$ref)],
  title=xml_text(xml_find_all(xData, "/data/key-event/title")),
  LOBO=xml_text(xml_find_all(xData, "/data/key-event/biological-organization-level")),
  stringsAsFactors=FALSE
)


## Key event relationship (KER) Data

kerData<-data.frame(
  ID=kerID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event-relationship"), "id"),kerID$ref)],
  KEup=keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/upstream-id")),keID$ref)],
  KEdown=keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/downstream-id")),keID$ref)],
  stringsAsFactors=FALSE
)


## AOP data

## OECD status: not all aops have an "oecd-status" xml tag, so must us "if" to return NA when missing
oecdStatus<-sapply(xml_find_all(xData, "/data/aop/status"),FUN=function(x){
  if("oecd-status"%in%xml_name(xml_children(x))){
    return(xml_text(xml_find_all(x,"oecd-status")))
  }else{
    return("not specified")
  }
})

## SAAOP status: not all aops have an "saaop-status" xml tag, so must us "if" to return NA when missing
saaopStatus<-sapply(xml_find_all(xData, "/data/aop/status"),FUN=function(x){
  if("saaop-status"%in%xml_name(xml_children(x))){
    return(xml_text(xml_find_all(x,"saaop-status")))
  }else{
    return("not specified")
  }
})

## MIEs: more than one MIE possible per aop, so must return list
mies<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
  if("molecular-initiating-event"%in%xml_name(xml_children(x))){
    return(keID$ID[match(xml_attr(xml_find_all(x, "molecular-initiating-event"),"key-event-id"),keID$ref)])
  }else{
    return(NULL)
  }
})

## AOs: more than one AO possible per aop, so must return list
aos<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
  if("adverse-outcome"%in%xml_name(xml_children(x))){
    return(keID$ID[match(xml_attr(xml_find_all(x, "adverse-outcome"),"key-event-id"),keID$ref)])
  }else{
    return(NULL)
  }
})


## KEs: more than one KE possible per aop, so must return list
kes<-lapply(xml_find_all(xData, "/data/aop/key-events"),FUN=function(x){
  if("key-event"%in%xml_name(xml_children(x))){
    return(keID$ID[match(xml_attr(xml_find_all(x, "key-event"),"id"),keID$ref)])
  }else{
    return(NULL)
  }
})

## KERs: more than one KER per aop, each with aop-specific "adjaceny", "quantitative understanding", and "WoE"
## So must return data frame of KERs
kers<-lapply(xml_find_all(xData, "/data/aop/key-event-relationships"),FUN=function(x){
  if("relationship"%in%xml_name(xml_children(x))){
    return(data.frame(
      ID=kerID$ID[match(xml_attr(xml_find_all(x, "relationship"),"id"),kerID$ref)],
      adjacency=xml_text(xml_find_all(x, "relationship/adjacency")),
      quant=xml_text(xml_find_all(x, "relationship/quantitative-understanding-value")),
      woe=xml_text(xml_find_all(x, "relationship/evidence")),
      stringsAsFactors=FALSE
    ))
  }else{
    return(NULL)
  }
})

## add kes and MIE/AO designation (which is AOP-specific) for each KER in kers data.frame
for(i in 1:length(kers)){
  if(length(kers[[i]])>0){
    KEup<-kerData$KEup[match(kers[[i]]$ID,kerData$ID)]
    KEDup<-sapply(KEup, FUN=function(x){
      if(x%in%mies[[i]]){
        return("MIE")
      }else{
        if(x%in%aos[[i]]){
          return("AO")
        }else{
          return("KE")
        }
      }
    })
    
    KEdown<-kerData$KEdown[match(kers[[i]]$ID,kerData$ID)]
    KEDdown<-sapply(KEdown, FUN=function(x){
      if(x%in%mies[[i]]){
        return("MIE")
      }else{
        if(x%in%aos[[i]]){
          return("AO")
        }else{
          return("KE")
        }
      }
    })
    
    kers[[i]]<-data.frame(
      ID=kers[[i]]$ID,
      KEup=KEup,
      KEDup=KEDup,
      KEdown=KEdown,
      KEDdown=KEDdown,
      adjacency=kers[[i]]$adjacency,
      quant=kers[[i]]$quant,
      woe=kers[[i]]$woe,
      row.names=NULL,
      stringsAsFactors = FALSE
    )
  }
}


aopData<-data.frame(
  ID=aopID$ID[match(xml_attr(xml_find_all(xData, "/data/aop"), "id"),aopID$ref)],
  oecdStatus=oecdStatus,
  saaopStatus=saaopStatus,
  mies=I(mies),
  aos=I(aos),
  kes=I(kes),
  kers=I(kers),
  stringsAsFactors=FALSE
)



################################################$
#### 2 - Semantic Analysis of AOPs and EAOPs ####
################################################$

## Load and process files
## Load eLAOPs
eLAOPs<- read_excel(paste(sourceDir,"/Data/eLaops.named.xlsx",sep=""), col_names = FALSE)
eLAOPs<-as.data.frame(eLAOPs)

## Load proper subset eAOPs so they can be removed from eLAOP list
PSSEAOPs<- read.csv(paste(sourceDir,"/Data/PSSEAOP-IndiciesfromeLAOPsNamed.csv",sep=""))
PSSEAOPs<-PSSEAOPs[,2]

## Load KE to KE semantic similarity scores - Note: If the file has duplicated KE to KE scores (such as the current one does '..._Dups.xlsx') processing is needed to remove duplicates before the analyses are run
KE2KEpre<- read_excel(paste(sourceDir,"/Data/KE2KE_SS_Dups.xlsx",sep=""), col_names = TRUE)
KE2KEpre<-as.data.frame(KE2KEpre)

KE2KEpre<-data.table(KE2KEpre)
head(KE2KEpre)

## Identify and aggregate any duplicated KE to KE semantic similarity scores, values are aggregated by using the arithmetic mean 'mean' in this instance
group_by(KE2KEpre, KEX, KEY) %>% summarise(across(everything(), ~mean(.))) %>% data.frame() ->KE2KE

head(KE2KE) # Inspect the new data.frame
dim(KE2KEpre)[1]-dim(KE2KE)[1] # Shows number of duplicate entries removed # 89,427

## NOTE: KE2KE here is same as KE2KE_SS (Need to verify?)

## Create a list that has quantitative and weight of evidence ratings for KERs based on their KEs  
kerWeights<-data.frame("KEX"=c(),"KEY"=c(),"woe"=c(),"quant"=c())
for(i in 1:length(kers)){
kerWs<-as.data.frame(cbind(kers[[i]]$KEup,kers[[i]]$KEdown,kers[[i]]$woe,kers[[i]]$quant))
kerWeights<-rbind(kerWeights,kerWs)
}
names(kerWeights)<-c("KEX","KEY","woe","quant")
kerWeights
kerWeights<-cbind(kerWeights,NA,NA)
names(kerWeights)<-c("KEX","KEY","woe","quant","qwoe","qquant")
kerWeights[which(kerWeights$woe=="Low"),]$qwoe<-0
kerWeights[which(kerWeights$woe=="Moderate"),]$qwoe<-.5
kerWeights[which(kerWeights$woe=="High"),]$qwoe<-1

kerWeights[which(kerWeights$quant=="Low"),]$qquant<-0
kerWeights[which(kerWeights$quant=="Moderate"),]$qquant<-.5
kerWeights[which(kerWeights$quant=="High"),]$qquant<-1

kerWeights<-distinct(kerWeights)

group_by(kerWeights, KEX, KEY) %>% summarise(across(everything(), ~mean(.))) %>% data.frame() ->kerWQ

## Use left_join to inspect differences between kerWQ and kerWeights, specifically to find any points where they don't match for the q* metrics
kerTest<-left_join(kerWQ,kerWeights[,c(1,2,5,6)],by=c("KEX","KEY"))
## Finds places where qwoe values don't match
kerTest[which(kerTest$qwoe.x!=kerTest$qwoe.y),]
## Finds places where qquant values don't match
kerTest[which(kerTest$qquant.x!=kerTest$qquant.y),]
## In all cases, the values in kerWQ represent the means of values that are repeated in kerWeights
## Can continue on with kerWQ weights

# Create a function that returns the semantic similarity (SS) for a pair of KEs. Since SS is symmetric, it will scan for SS(KE X, KE Y) or SS(KE Y, KE X) and return which ever it finds
KESS<-function(X,Y,KESSDF=KE2KE_SS){
  #If statement tests to make sure that the KEs supplied are in the KE
  if(( X %in% KESSDF$KEX || X %in% KESSDF$KEY) && ( Y %in% KESSDF$KEX || Y %in% KESSDF$KEY)){
    
    return(max(KESSDF[which(KESSDF$KEX==X & KESSDF$KEY==Y),]$score,KESSDF[which(KESSDF$KEX==Y & KESSDF$KEY==X),]$score))}
  else {return(NA)}
}


# Create a function that returns the chosen KER Weight for a pair of KEs.  KER weights are not symmetric, so only X=KEX and Y=KEY results are returned for the specified metric
KEKERW<-function(X,Y,met,KERDF=kerWQ){
  #If statement tests to make sure that the KEs supplied are in the KE
  if(any(KERDF$KEX==X & KERDF$KEY==Y)){
    if(length(KERDF[which(KERDF$KEX==X & KERDF$KEY==Y),met])>1 && !all(is.na(KERDF[which(KERDF$KEX==X & KERDF$KEY==Y),met]))){
      return(max(KERDF[which(KERDF$KEX==X & KERDF$KEY==Y),met], na.rm=T))}
    else if(length(KERDF[which(KERDF$KEX==X & KERDF$KEY==Y),met])>1 && all(is.na(KERDF[which(KERDF$KEX==X & KERDF$KEY==Y),met]))){
      return(NA)} 
    else return(KERDF[which(KERDF$KEX==X & KERDF$KEY==Y),met])
  } 
  else {
    return(NA)}
}


#####################################################################$
#### 2.1 - Calculate the semantic similarity metrics for all AOPs ####
#####################################################################$
AOPKESSINFO<-list() # Create a list to store the metric scores
AOPIDs<-aopID$ID # Assign the IDs that you want to run in the loop, in this case it is all AOPs 
AOPKESSSData<-data.frame(AOP=aopID$ID,SSSMean=NA,SSSMin=NA,PSSMean=NA,SQWMean=NA,SQWMin=NA,SQQMean=NA,SQQMin=NA) #Create a data.frame structure to store metric values for each AOP that added to the ...INFO list

for(AOPID in AOPIDs){
  
  # Gets KE information based on the AOPID specified in the loop
  AOPKEs<-as.vector(unlist(c(aopData[aopData$ID==AOPID,]$mies, aopData[aopData$ID==AOPID,]$kes, aopData[aopData$ID==AOPID,]$aos)))
  # KE Targets to get names (titles) for 
  KETars<-as.numeric(AOPKEs)
  # Catches any AOPs with only a single KE and assigns their scores as 'NA' for all metrics
  if(length(KETars)<2)
    {
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SSSMean<-NA
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SSSMin<-NA
    #AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SSSSum<-NA # Removed since SUM metrics are highly dependent on AOP length
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SQWMean<-NA
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SQWMin<-NA
    #AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SWQSum<-NA # Removed since SUM metrics are highly dependent on AOP length
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SQQMean<-NA
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SQQMin<-NA
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$PSSMean<-NA
    }
    else{
    # Check that all KEs from AOPs have data in the keData data.frame()
      if(!length(which(keData$ID %in% KETars))==length(KETars)){
      missingKEindices<-which(KETars %!in% keData$ID)
      for(missingKEindex in missingKEindices){
        missingKEdf<-data.frame(ID = KETars[missingKEindex], title="NO INFO IN WIKI SNAPSHOT",LOBO="UNKNOWN")
        keData<-rbind(keData,missingKEdf)
        }
        }
    
    # Creates all combinations of all KE pairs in the AOP for the pairwise semantic similarity scores 
    allpairs<-combn(KETars,2)
    apdf<-as.data.frame(t(allpairs))
    apdf<-data.frame(apdf,SS=NA,qwoe=NA,qquant=NA)
    for(i in 1:dim(apdf)[1]){
      apdf[i,"SS"]<-KESS(apdf[i,1],apdf[i,2])
      #print(paste(apdf[i,1],apdf[i,2]))
      apdf[i,"qwoe"]<-KEKERW(apdf[i,1],apdf[i,2],met="qwoe")
      apdf[i,"qquant"]<-KEKERW(apdf[i,1],apdf[i,2],met="qquant")}
    apdf$isadj=FALSE    
    KEPairs<-cbind(KETars, c(KETars[-1], KETars[1]))
    KEPairs<-head(KEPairs,-1)
    
    # Need to know if a KE pair is adjacent in the AOP, if so, then it is marked as adjacent and it can be used in the metric
    for(i in 1:dim(apdf)[1]){
      for(j in 1:dim(KEPairs)[1]){
        if(all(apdf[i,1:2]==KEPairs[j,])){
          apdf$isadj[i]=T
        }
      }
    }
    
    apdf$CUMMEANSS[which(apdf$isadj==T)]=cummean(apdf$SS[which(apdf$isadj==T)])
    apdf$CUMMINSS[which(apdf$isadj==T)]=cummin(apdf$SS[which(apdf$isadj==T)])
    #apdf$CUMSUMSS[which(apdf$isadj==T)]=cumsum(apdf$SS[which(apdf$isadj==T)]) # Removed since SUM metrics are highly dependent on AOP length
    apdf$CUMMEANQW[which(apdf$isadj==T)]=cummean(apdf$qwoe[which(apdf$isadj==T)])
    apdf$CUMMINQW[which(apdf$isadj==T)]=cummin(apdf$qwoe[which(apdf$isadj==T)])
    #apdf$CUMSUMQW[which(apdf$isadj==T)]=cumsum(apdf$qwoe[which(apdf$isadj==T)]) # Removed since SUM metrics are highly dependent on AOP length
    apdf$CUMMEANQQ[which(apdf$isadj==T)]=cummean(apdf$qquant[which(apdf$isadj==T)])
    apdf$CUMMINQQ[which(apdf$isadj==T)]=cummin(apdf$qquant[which(apdf$isadj==T)])
    
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SSSMean<-tail(apdf$CUMMEANSS,1)
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SSSMin<-tail(apdf$CUMMINSS,1)
    #AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SSSSum<-tail(apdf$CUMSUMSS,1) # Removed since SUM metrics are highly dependent on AOP length
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SQWMean<-tail(apdf$CUMMEANQW,1)
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SQWMin<-tail(apdf$CUMMINQW,1)
    #AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SWQSum<-tail(apdf$CUMSUMWQ,1) # Removed since SUM metrics are highly dependent on AOP length
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$PSSMean<-mean(apdf$SS)
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SQQMean<-tail(apdf$CUMMEANQQ,1)
    AOPKESSSData[which(AOPKESSSData$AOP==AOPID),]$SQQMin<-tail(apdf$CUMMINQQ,1)
    
    AOPKESSINFO[[as.character(AOPID)]]<-apdf

  }
}

openxlsx::write.xlsx(AOPKESSSData, paste(sourceDir,paste('aopkess_',format(Sys.time(), "%Y-%m-%d_%H-%M"),sep=""),'.xlsx',sep=""))

# Creates sorted data frames for each of the metrics as the sorting order
AOPKESSSMeanDataSorted<-AOPKESSSData[order(-AOPKESSSData$SSSMean),]
AOPKESSSMinDataSorted<-AOPKESSSData[order(-AOPKESSSData$SSSMin),]
AOPKEPSSMeanDataSorted<-AOPKESSSData[order(-AOPKESSSData$PSSMean),]

# Gets the AOP IDs for the top 10 emergent AOPs based on the different metrics
AOPtop10SSSMean<-head(AOPKESSSMeanDataSorted,10)$AOP
AOPtop10SSSMin<-head(AOPKESSSMinDataSorted,10)$AOP
AOPtop10PSSMean<-head(AOPKEPSSMeanDataSorted,10)$AOP

######################################################################$
#### 2.2 - Calculate the semantic similarity metrics for all eAOPs ####
######################################################################$

# Note that this section takes a long time to run (>1hr)
eAOPKESSINFO<-list()
eAOPIDs<-as.numeric(rownames(eLAOPs[-PSSEAOPs,])) # This creates the list of all the eLAOPs and removes the proper subset eAOPs that exist in the larger eLAOP list
eAOPKESSSData<-data.frame(EAOP=eLAOPs[,1],SSSMean=NA,SSSMin=NA,PSSMean=NA,SQWMean=NA,SQWMin=NA,SQQMean=NA,SQQMin=NA)

for(eAOPID in eAOPIDs){
  
  allpairs<-combn(as.numeric(eLAOPs[eAOPID,][!is.na(eLAOPs[eAOPID,])][-1]),2)
    apdf<-as.data.frame(t(allpairs))
    apdf<-data.frame(apdf,SS=NA,qwoe=NA,qquant=NA)
    for(i in 1:dim(apdf)[1]){
      apdf[i,3]<-KESS(apdf[i,1],apdf[i,2])
      apdf[i,"qwoe"]<-KEKERW(apdf[i,1],apdf[i,2],met="qwoe")
      apdf[i,"qquant"]<-KEKERW(apdf[i,1],apdf[i,2],met="qquant")}
    apdf$isadj=FALSE
    
    KETars<-eLAOPs[eAOPID,-1][!is.na(eLAOPs[eAOPID,-1])]
    KEPairs<-cbind(KETars, c(KETars[-1], KETars[1]))
    KEPairs<-head(KEPairs,-1)
    
    for(i in 1:dim(apdf)[1]){
      for(j in 1:dim(KEPairs)[1]){
        if(all(apdf[i,1:2]==KEPairs[j,])){
          apdf$isadj[i]=T
        }
      }
    }
    
    apdf$CUMMEAN[which(apdf$isadj==T)]=cummean(apdf$SS[which(apdf$isadj==T)])
    apdf$CUMMIN[which(apdf$isadj==T)]=cummin(apdf$SS[which(apdf$isadj==T)])
    #apdf$CUMSUM[which(apdf$isadj==T)]=cumsum(apdf$SS[which(apdf$isadj==T)]) # Removed since SUM metrics are highly dependent on AOP length
    apdf$CUMMEANQW[which(apdf$isadj==T)]=cummean(apdf$qwoe[which(apdf$isadj==T)])
    apdf$CUMMINQW[which(apdf$isadj==T)]=cummin(apdf$qwoe[which(apdf$isadj==T)])
    #apdf$CUMSUMWQ[which(apdf$isadj==T)]=cumsum(apdf$qwoe[which(apdf$isadj==T)]) # Removed since SUM metrics are highly dependent on AOP length
    apdf$CUMMEANQQ[which(apdf$isadj==T)]=cummean(apdf$qquant[which(apdf$isadj==T)])
    apdf$CUMMINQQ[which(apdf$isadj==T)]=cummin(apdf$qquant[which(apdf$isadj==T)])
    
    
    eAOPKESSSData[eAOPID,]$SSSMean<-tail(apdf$CUMMEAN,1)
    eAOPKESSSData[eAOPID,]$SSSMin<-tail(apdf$CUMMIN,1)
    #eAOPKESSSData[eAOPID,]$SSSSum<-tail(apdf$CUMSUM,1) # Removed since SUM metrics are highly dependent on AOP length
    eAOPKESSSData[eAOPID,]$PSSMean<-mean(apdf$SS)
    eAOPKESSSData[eAOPID,]$SQWMean<-tail(apdf$CUMMEANQW,1)
    eAOPKESSSData[eAOPID,]$SQWMin<-tail(apdf$CUMMINQW,1)
    #eAOPKESSSData[eAOPID,]$SWQSum<-tail(apdf$CUMSUMWQ,1) # Removed since SUM metrics are highly dependent on AOP length
    eAOPKESSSData[eAOPID,]$SQQMean<-tail(apdf$CUMMEANQQ,1)
    eAOPKESSSData[eAOPID,]$SQQMin<-tail(apdf$CUMMINQQ,1)
    
    eAOPKESSINFO[[as.character(eAOPID)]]<-apdf
    
}


  openxlsx::write.xlsx(eAOPKESSSData, paste(sourceDir,paste('eaopkess_',format(Sys.time(), "%Y-%m-%d_%H-%M"),sep=""),'.xlsx',sep=""))
  
  eAOPKESSSMeanDataSorted<-eAOPKESSSData[order(-eAOPKESSSData$SSSMean),]
  eAOPKESSSMinDataSorted<-eAOPKESSSData[order(-eAOPKESSSData$SSSMin),]
  eAOPKEPSSMeanDataSorted<-eAOPKESSSData[order(-eAOPKESSSData$PSSMean),]
  
  # Gets the eAOP IDs for the top 10 emergent AOPs based on SSS
  eAOPtop10SSSMean<-as.numeric(rownames(head(eAOPKESSSMeanDataSorted,10)))
  eAOPtop10SSSMin<-as.numeric(rownames(head(eAOPKESSSMinDataSorted,10)))
  eAOPtop10PSSMean<-as.numeric(rownames(head(eAOPKEPSSMeanDataSorted,10)))
  


#####################################################################$
#### 2.2.1 - Gather additional info about top ten eAOPs by metric ####
#####################################################################$
eAOPINFO<-list()
eAOPIDs<-eAOPtop10SSSMean

for(eAOPID in eAOPIDs){
  eLAOPs[eAOPID,-1][!is.na(eLAOPs[eAOPID,-1])]
  #### KE Targets to get names (titles) for 
  KETars<-eLAOPs[eAOPID,-1][!is.na(eLAOPs[eAOPID,-1])]
  #Need to check that all KEs from emergent AOPs have data in the keData data.frame()
  if(!length(which(keData$ID %in% KETars))==length(KETars)){
    missingKEindices<-which(KETars %!in% keData$ID)
    for(missingKEindex in missingKEindices){
      missingKEdf<-data.frame(ID = KETars[missingKEindex], title="NO INFO IN WIKI SNAPSHOT",LOBO="UNKNOWN")
      keData<-rbind(keData,missingKEdf)
    }
  }
  
  #Create a data frame with data for the KEs specified in KETars
  KETarsInf<-as.data.frame(keData[which(keData$ID %in% KETars),])
  #Ensure KE titles are returned in the order that the KEs were specified in KETars
  KETarsInf<-KETarsInf %>% arrange(factor(ID, levels = KETars))
  #Create columns for AOPs that KEs are MIEs, KEs, or AOs in
  KETarsInf[,c('IsMIEIn','IsKEIn','IsAOIn','SS',)]<-NA
  
  #If we want to add the AOPs that these are used in, we can do the following searches 
  #This finds all the AOPs that the KETars are used in, based on if they are used as MIEs, KEs, or AOs
  KETarsMIEs<-sapply(KETars, function(x, list) which(unlist(lapply(list,function(y,z) z %in% y, z=x))),list=aopData$mies)
  KETarsKEs<-sapply(KETars, function(x, list) which(unlist(lapply(list,function(y,z) z %in% y, z=x))),list=aopData$kes)
  KETarsAOs<-sapply(KETars, function(x, list) which(unlist(lapply(list,function(y,z) z %in% y, z=x))),list=aopData$aos)
  ### Add the MIE KE AO AOP data to KETarsInf data.frame
  for(i in 1:length(KETars)){
    KETarsInf[i,]$IsMIEIn<-paste(aopData[KETarsMIEs[[i]],]$ID,collapse=", ")
    KETarsInf[i,]$IsKEIn<-paste(aopData[KETarsKEs[[i]],]$ID,collapse=", ")
    KETarsInf[i,]$IsAOIn<-paste(aopData[KETarsAOs[[i]],]$ID,collapse=", ")
  }
  
  
  eAOPINFO[[as.character(eAOPID)]]<-KETarsInf
}

openxlsx::write.xlsx(eAOPINFO, "eAOPTop10-SSSMean.xlsx")


eAOPINFO<-list()
eAOPIDs<-eAOPtop10SSSMin

for(eAOPID in eAOPIDs){
  eLAOPs[eAOPID,-1][!is.na(eLAOPs[eAOPID,-1])]
  #### KE Targets to get names (titles) for 
  KETars<-eLAOPs[eAOPID,-1][!is.na(eLAOPs[eAOPID,-1])]
  #Need to check that all KEs from emergent AOPs have data in the keData data.frame()
  if(!length(which(keData$ID %in% KETars))==length(KETars)){
    missingKEindices<-which(KETars %!in% keData$ID)
    for(missingKEindex in missingKEindices){
      missingKEdf<-data.frame(ID = KETars[missingKEindex], title="NO INFO IN WIKI SNAPSHOT",LOBO="UNKNOWN")
      keData<-rbind(keData,missingKEdf)
    }
  }
  
  #Create a data frame with data for the KEs specified in KETars
  KETarsInf<-as.data.frame(keData[which(keData$ID %in% KETars),])
  #Ensure KE titles are returned in the order that the KEs were specified in KETars
  KETarsInf<-KETarsInf %>% arrange(factor(ID, levels = KETars))
  #Create columns for AOPs that KEs are MIEs, KEs, or AOs in
  KETarsInf[,c('IsMIEIn','IsKEIn','IsAOIn')]<-NA
  
  #If we want to add the AOPs that these are used in, we can do the following searches 
  #This finds all the AOPs that the KETars are used in, based on if they are used as MIEs, KEs, or AOs
  KETarsMIEs<-sapply(KETars, function(x, list) which(unlist(lapply(list,function(y,z) z %in% y, z=x))),list=aopData$mies)
  KETarsKEs<-sapply(KETars, function(x, list) which(unlist(lapply(list,function(y,z) z %in% y, z=x))),list=aopData$kes)
  KETarsAOs<-sapply(KETars, function(x, list) which(unlist(lapply(list,function(y,z) z %in% y, z=x))),list=aopData$aos)
  ### Add the MIE KE AO AOP data to KETarsInf data.frame
  for(i in 1:length(KETars)){
    KETarsInf[i,]$IsMIEIn<-paste(aopData[KETarsMIEs[[i]],]$ID,collapse=", ")
    KETarsInf[i,]$IsKEIn<-paste(aopData[KETarsKEs[[i]],]$ID,collapse=", ")
    KETarsInf[i,]$IsAOIn<-paste(aopData[KETarsAOs[[i]],]$ID,collapse=", ")
  }
  
  
  eAOPINFO[[as.character(eAOPID)]]<-KETarsInf
}

View(eAOPINFO)

openxlsx::write.xlsx(eAOPINFO,"eAOPTop10-SSSMin.xlsx")

eAOPINFO<-list()
eAOPIDs<-eAOPtop10PSSMean

for(eAOPID in eAOPIDs){
  eLAOPs[eAOPID,-1][!is.na(eLAOPs[eAOPID,-1])]
  #### KE Targets to get names (titles) for 
  KETars<-eLAOPs[eAOPID,-1][!is.na(eLAOPs[eAOPID,-1])]
  #Need to check that all KEs from emergent AOPs have data in the keData data.frame()
  if(!length(which(keData$ID %in% KETars))==length(KETars)){
    missingKEindices<-which(KETars %!in% keData$ID)
    for(missingKEindex in missingKEindices){
      missingKEdf<-data.frame(ID = KETars[missingKEindex], title="NO INFO IN WIKI SNAPSHOT",LOBO="UNKNOWN")
      keData<-rbind(keData,missingKEdf)
    }
  }
  
  #Create a data frame with data for the KEs specified in KETars
  KETarsInf<-as.data.frame(keData[which(keData$ID %in% KETars),])
  #Ensure KE titles are returned in the order that the KEs were specified in KETars
  KETarsInf<-KETarsInf %>% arrange(factor(ID, levels = KETars))
  #Create columns for AOPs that KEs are MIEs, KEs, or AOs in
  KETarsInf[,c('IsMIEIn','IsKEIn','IsAOIn')]<-NA
  
  #If we want to add the AOPs that these are used in, we can do the following searches 
  #This finds all the AOPs that the KETars are used in, based on if they are used as MIEs, KEs, or AOs
  KETarsMIEs<-sapply(KETars, function(x, list) which(unlist(lapply(list,function(y,z) z %in% y, z=x))),list=aopData$mies)
  KETarsKEs<-sapply(KETars, function(x, list) which(unlist(lapply(list,function(y,z) z %in% y, z=x))),list=aopData$kes)
  KETarsAOs<-sapply(KETars, function(x, list) which(unlist(lapply(list,function(y,z) z %in% y, z=x))),list=aopData$aos)
  ### Add the MIE KE AO AOP data to KETarsInf data.frame
  for(i in 1:length(KETars)){
    KETarsInf[i,]$IsMIEIn<-paste(aopData[KETarsMIEs[[i]],]$ID,collapse=", ")
    KETarsInf[i,]$IsKEIn<-paste(aopData[KETarsKEs[[i]],]$ID,collapse=", ")
    KETarsInf[i,]$IsAOIn<-paste(aopData[KETarsAOs[[i]],]$ID,collapse=", ")
  }
  
  
  eAOPINFO[[as.character(eAOPID)]]<-KETarsInf
}

openxlsx::write.xlsx(eAOPINFO,"eAOPTop10-PSSMean.xlsx")

# GET EAOP AND AOP OVERALL METRIC INFORMATION
length(!is.na(AOPKESSSData$SSSMean))
mean(AOPKESSSData$SSSMean,na.rm=T) 
mean(AOPKESSSData$SSSMin,na.rm=T) 
mean(AOPKESSSData$PSSMean,na.rm=T) 

length(!is.na(eAOPKESSSData$SSSMean)) 
mean(eAOPKESSSData$SSSMean,na.rm=T) 
mean(eAOPKESSSData$SSSMin,na.rm=T) 
mean(eAOPKESSSData$PSSMean,na.rm=T)
