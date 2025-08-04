## AOP XML EXTRACTION

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
`%!in%`<-Negate(`%in%`)

## Directory definition
sourceDir<-paste("~/Projects/GitHub/EAOP-Viz")

### Load AOP Network Analysis function from Pollesch et. al 2019
source("Source/AOP-Net-Functions.R") ## Imports custom functions from location set by sourceDir variable

## Provide AOP-Wiki XML file location

#### WORKING PRIOR VERSION ####

#fName<-paste(sourceDir,"/AOP-Wiki-XML/aop-wiki-xml-2025-01-01.xml",sep="") ##This assumes that there is a subfolder 'Data/AOPWikiSnapshots' within the working directory where the XML file is located
fName<-paste(sourceDir,"/AOP-Wiki-XML/aop-wiki-xml-2019-01-01.xml",sep="") ##This assumes that there is a subfolder 'Data/AOPWikiSnapshots' within the working directory where the XML file is located

xData<-read_xml(fName)
xData<-xml_ns_strip(xData)


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

#### ERROR PRIOR VERSION 2025 XML ####

fName<-paste(sourceDir,"/AOP-Wiki-XML/aop-wiki-xml-2025-01-01.xml",sep="") ##This assumes that there is a subfolder 'Data/AOPWikiSnapshots' within the working directory where the XML file is located
#fName<-paste(sourceDir,"/AOP-Wiki-XML/aop-wiki-xml-2019-01-01.xml",sep="") ##This assumes that there is a subfolder 'Data/AOPWikiSnapshots' within the working directory where the XML file is located

xData<-read_xml(fName)
xData<-xml_ns_strip(xData)


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


#### DEV NEW VERSION 2025 XML ####

#fName<-paste(sourceDir,"/AOP-Wiki-XML/aop-wiki-xml-2025-01-01.xml",sep="") ##This assumes that there is a subfolder 'Data/AOPWikiSnapshots' within the working directory where the XML file is located

fName<-paste(sourceDir,"/AOP-Wiki-XML/aop-wiki-xml-2019-01-01.xml",sep="") ##This assumes that there is a subfolder 'Data/AOPWikiSnapshots' within the working directory where the XML file is located

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
mies_full<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
  if("molecular-initiating-event"%in%xml_name(xml_children(x))){
      mies.out<-keID$ID[match(xml_attr(xml_find_all(x, "molecular-initiating-event"),"key-event-id"),keID$ref)]
    return(list(ref=xml_attr(x,"id"),
                ID=aopID[match(xml_attr(x,"id"),aopID$ref),"ID"],df=mies.out))
  }else{
    return(NULL)
  }
})

## Remove NULL entries and create another list of aop refs and IDs for aops with mies
mies_full_noNULL<-Filter(Negate(is.null),mies_full)
aopswithmies<-data.frame(ref=sapply(mies_full_noNULL, function(x) x$ref),ID=sapply(mies_full_noNULL, function(x) x$ID))


aos<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
  if("adverse-outcome"%in%xml_name(xml_children(x))){
    aos.out<-keID$ID[match(xml_attr(xml_find_all(x, "adverse-outcome"),"key-event-id"),keID$ref)]
    return(aos.out)}
  else{return(NULL)}})

## AOs: more than one AO possible per aop, so must return list
aos_full<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
  if("adverse-outcome"%in%xml_name(xml_children(x))){
    aos.out<-keID$ID[match(xml_attr(xml_find_all(x, "adverse-outcome"),"key-event-id"),keID$ref)]
    return(list(ref=xml_attr(x,"id"),
                ID=aopID[match(xml_attr(x,"id"),aopID$ref),"ID"],df=aos.out))
  }else{
    return(NULL)
  }
})

## Remove NULL entries and create another list of aop refs and IDs for aops with aos
aos_full_noNULL<-Filter(Negate(is.null),aos_full)
aopswithaos<-data.frame(ref=sapply(aos_full_noNULL, function(x) x$ref),ID=sapply(aos_full_noNULL, function(x) x$ID))

### THIS IS THE FIRST PROBLEM THIS RETURNS 466 instead of 509
kes<-lapply(xml_find_all(xData,"/data/aop/key-events"),FUN=function(x){
  if("key-event"%in%xml_name(xml_children(x))){
    keidName<-unique(names(unlist(xml_attrs(xml_children(x)))))
    if(keidName=="id"){
      return(keID$ID[match(xml_attr(xml_find_all(x, "key-event"),"id"),keID$ref)])}
    else if(keidName=="key-event-id"){return(keID$ID[match(xml_attr(xml_find_all(x, "key-event"),"key-event-id"),keID$ref)])}
    else (return(print("Please use an xml file with key-event attibutes named either 'id' or 'key-event-id'")))
  }else{
    return(NULL)
  }
})


kes_full <- lapply(xml_find_all(xData, "/data/aop/key-events"), FUN = function(x) {
  if ("key-event" %in% xml_name(xml_children(x))) {
    keidName <- unique(names(unlist(xml_attrs(xml_children(x)))))
    if (keidName == "id") {
      ke.out <- keID$ID[match(xml_attr(xml_find_all(x, "key-event"), "id"), keID$ref)]
    } else if (keidName == "key-event-id") {
      ke.out <- keID$ID[match(xml_attr(xml_find_all(x, "key-event"), "key-event-id"), keID$ref)]
    } else {
      print("Please use an xml file with key-event attributes named either 'id' or 'key-event-id'")
      return(NULL)
    }
    return(list(ref = xml_attr(xml_parent(x), "id"),
                ID = aopID[match(xml_attr(xml_parent(x), "id"), aopID$ref), "ID"],
                df = ke.out))
  } else {
    return(NULL)
  }
})

# Filter out NULL entries
kes_full_noNULL <- Filter(Negate(is.null), kes_full)

# Create a dataframe that contains the aopID and refs for
aopswithkes <- data.frame(
  ref = sapply(kes_full_noNULL, function(x) x$ref),
  ID = sapply(kes_full_noNULL, function(x) x$ID)
)


## KERs: more than one KER per aop, each with aop-specific "adjaceny", "quantitative understanding", and "WoE"
## So must return data frame of KERs
kers_full<-lapply(xml_find_all(xData, "/data/aop/key-event-relationships"),FUN=function(x){
  if("relationship"%in%xml_name(xml_children(x))){
    df.out<-data.frame(
      ID=kerID$ID[match(xml_attr(xml_find_all(x, "relationship"),"id"),kerID$ref)],
      adjacency=xml_text(xml_find_all(x, "relationship/adjacency")),
      quant=xml_text(xml_find_all(x, "relationship/quantitative-understanding-value")),
      woe=xml_text(xml_find_all(x, "relationship/evidence")),
      stringsAsFactors=FALSE
    )
    return(list(ref=xml_attr(xml_parent(x),"id"),
                ID=aopID[match(xml_attr(xml_parent(x),"id"),aopID$ref),"ID"],df=df.out))
  }else{
    return(NULL)
  }
})

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

kers_full_noNULL <- Filter(Negate(is.null), kers_full)

aopswithkers<-data.frame(ref=sapply(kers_full_noNULL, function(x) x$ref),ID=sapply(kers_full_noNULL, function(x) x$ID))

### USE aopswithkers and aopswithkes to find only complete aops, i.e. those with both kes and kers

aopComplete <- aopID %>%
  inner_join(aopswithkers, by = c("ref", "ID")) %>%
  inner_join(aopswithkes, by = c("ref", "ID")) %>%
  inner_join(aopswithmies, by = c("ref", "ID")) %>%
  inner_join(aopswithaos, by = c("ref", "ID"))

### Finish this process on all aopData objects by filtering prior information to only include the aops that are complete

matched_ids <- aopID$ID[match(xml_attr(xml_find_all(xData, "/data/aop"), "id"), aopID$ref)]

# Filter IDs to include only those present in aopComplete
filtered_ids <- matched_ids[matched_ids %in% aopComplete$ID]

# Create aopData data frame using only filtered IDs
aopDataComplete <- data.frame(
  ID = filtered_ids,
  oecdStatus = oecdStatus[matched_ids %in% aopComplete$ID],
  saaopStatus = saaopStatus[matched_ids %in% aopComplete$ID],
  mies = I(mies[matched_ids %in% aopComplete$ID]),
  aos = I(aos[matched_ids %in% aopComplete$ID]),
  kes = I(kes[matched_ids %in% aopComplete$ID]),
  kers = I(kers[matched_ids %in% aopComplete$ID]),
  stringsAsFactors = FALSE
)

View(aopDataComplete)
