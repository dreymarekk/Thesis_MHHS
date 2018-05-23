##########################################################################################################################
#
# Poplar Island Headstart and MH Survival Analysis
# 5/3/2018
# David Jenkins
#
#
##########################################################################################################################

###################################################
#To be used with poplar.R                         #
#If not then execute the following code           #
library(RMark)                                 #
library(reshape)                                #
library(dplyr)                                  #
library(ggplot2)                                #
#                                                 #
makeHistory<-function(x){                       #
   k<-ncol(x)                                    #
   n<-nrow(x)                                    #
   out<-array(dim=n)                             #
                                                 #
   for (i in 1:n){                               #
     y<-(x[i,]>0)*1                              #
     out[i]<-paste(y, collapse = "")}            #
                                                 #
   return(out)                                   #
                                                 #
 }                                               #
###################################################



mhhs<-read.csv("MHHS.csv")

initcaps.mh<-distinct(mhhs,pit, .keep_all = TRUE)
initcaps.mh<-initcaps.mh[order(initcaps.mh$pit),]   

#convert dates to date obj
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
mhhs$do_cap<-as.Date(mhhs$do_cap, "%m/%d/%Y")
Sys.setlocale("LC_TIME", lct)
rm(lct)

#extract yoc
mhhs<-mutate(mhhs, year = format(do_cap, "%Y"))


#shape enc hist object
junk<-melt(mutate(mhhs, detect = 1),id.var=c("pit","year"),measure.var="detect")
z=cast(junk, pit~year)

z<-z[order(z$pit),]

#replace any integers >1 with 1
for (i in 1:nrow(z)){
  for (j in 2:14){
    if(z[i,j] > 1){
      z[i,j]<-1
    }
  }
}

#Attach covartiates

z<-cbind(z, initcaps.mh$type, initcaps.mh$yob)
names(z)[15:16] <- c("type", "yob")

#remove mh from prior to 2005

for(i in 1:nrow(z)){
  if (z[i,16] <= 2004){
    z<-z[-i,]
  }
}


#give every animal initial encounter in yob
for (i in 1:nrow(z)){
  
    if(z[i,16] == 2005){
      z[i,2]<-1
    }
    else if(z[i,16] == 2006){
      z[i,3]<-1
    }
    else if(z[i,16] == 2007){
      z[i,4]<-1
    }
    else if(z[i,16] == 2008){
      z[i,5]<-1
    }
    else if(z[i,16] == 2009){
      z[i,6]<-1
    }
    else if(z[i,16] == 2010){
      z[i,7]<-1
    }
    else if(z[i,16] == 2011){
      z[i, 8]<-1
    }
    else if(z[i,16] == 2012){
      z[i,9]<-1
    }
    else if(z[i,16] == 2013){
      z[i,10]<-1
    }
    else if(z[i,16] == 2014){
      z[i,11]<-1
    }
    else if(z[i,16] == 2015){
      z[i,12]<-1
    }
    else if(z[i,16] == 2016){
      z[i,13]<-1
    }
  }

hatch<-list(pit="MH", "2005"=1, "2006"=0, "2007"=0, "2008"=0,
            "2009"=0, "2010"=0, "2011"=0, "2012"=0, "2013"=0, 
            "2014"=0, "2015"=0, "2016"=0, "2017"=0, type = "MH", yob = 2005)
write.table(t(unlist(hatch)),"clipboard")
hatchdf<-read.table("clipboard")
names(hatchdf)<-c("pit", "2005", "2006", "2007", "2008", "2009", 
                  "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "type", "yob")

z_mh<-z
#2005

for (i in 1:1357){
  z_mh<-rbind(z_mh, hatchdf)
}
#2006
hatchdf$`2005`=0
hatchdf$`2006`=1

for (i in 1:682){
  z_mh<-rbind(z_mh, hatchdf)
}
#2007
hatchdf$`2006`=0
hatchdf$`2007`=1

for (i in 1:1273){
  z_mh<-rbind(z_mh, hatchdf)
}
#2008
hatchdf$`2007`=0
hatchdf$`2008`=1

for (i in 1:1179){
  z_mh<-rbind(z_mh, hatchdf)
}
#2009
hatchdf$`2008`=0
hatchdf$`2009`=1
for (i in 1:1070){
  z_mh<-rbind(z_mh, hatchdf)
}
#2010
hatchdf$`2009`=0
hatchdf$`2010`=1
for (i in 1:429){
  z_mh<-rbind(z_mh, hatchdf)
}
#2011
hatchdf$`2010`=0
hatchdf$`2011`=1

for (i in 1:1012){
  z_mh<-rbind(z_mh, hatchdf)
}
#2012
hatchdf$`2011`=0
hatchdf$`2012`=1

for (i in 1:632){
  z_mh<-rbind(z_mh, hatchdf)
}
#2013
hatchdf$`2012`=0
hatchdf$`2013`=1

for (i in 1:861){
  z_mh<-rbind(z_mh, hatchdf)
}
#2014
hatchdf$`2013`=0
hatchdf$`2014`=1

for (i in 1:632){
  z_mh<-rbind(z_mh, hatchdf)
}
#2015
hatchdf$`2014`=0
hatchdf$`2015`=1

for (i in 1:513){
  z_mh<-rbind(z_mh, hatchdf)
}
#2016
hatchdf$`2015`=0
hatchdf$`2016`=1

for (i in 1:590){
  z_mh<-rbind(z_mh, hatchdf)
}

# determine HS who did not survive

hatchdf$pit<-"HS"
hatchdf$type<-"HS"
hatchdf$`2016`<-0

#2006
hatchdf$`2005`=0
hatchdf$`2006`=1

for (i in 1:3){
  z_mh<-rbind(z_mh, hatchdf)
}

#2008
hatchdf$`2007`=0
hatchdf$`2008`=1

z_mh<-rbind(z_mh, hatchdf)

#2010
hatchdf$`2009`=0
hatchdf$`2010`=1
for (i in 1:8){
  z_mh<-rbind(z_mh, hatchdf)
}
#2011
hatchdf$`2010`=0
hatchdf$`2011`=1

for (i in 1:17){
  z_mh<-rbind(z_mh, hatchdf)
}
#2012
hatchdf$`2011`=0
hatchdf$`2012`=1

for (i in 1:8){
  z_mh<-rbind(z_mh, hatchdf)
}
#2013
hatchdf$`2012`=0
hatchdf$`2013`=1

for (i in 1:15){
  z_mh<-rbind(z_mh, hatchdf)
}



#2016
hatchdf$`2015`=0
hatchdf$`2016`=1

for (i in 1:16){
  z_mh<-rbind(z_mh, hatchdf)
}

capt.hist.mhhs<-data.frame(ch = makeHistory(z_mh[,2:14]))

capt.hist.mhhs<-cbind(capt.hist.mhhs, z_mh[,15])
names(capt.hist.mhhs)<-c("ch", "type")


########################################################################################################
#For wholePop analysis
#
#z.hs<-filter(z_mh, type == "HS")
#write.csv(z.hs, file = "H:/Thesis/Poplar_wholePop_5-23/z.hs.csv")
#######################################################################################################

capt.hist.mhhs.processed<-process.data(capt.hist.mhhs,model="CJS",begin.time=2005, groups = c("type"), 
                                       initial.age = 0)
                                       

ddl.mhhs<-make.design.data(capt.hist.mhhs.processed, 
                           parameters = list(Phi=list(age.bins = c(0,1,4,8,12)),p=list(age.bins = c(0,1,4,8,12))), 
                           right = FALSE)


#Model definitions
Phi.dot = list(formula=~1)
Phi.type = list(formula=~type)
Phi.age = list(formula=~age)
Phi.age.type = list(formula=~age+type)
#Phi.time = list(formula=~time)
#Phi.age.time = list(formula=~age+time)
Phi.age.type.time = list(formula=~age+type+time)

p.dot = list(formula=~1)
p.age = list(formula=~age, fixed=list)
p.time = list(formula=~time)
p.age.time = list(formula=~age*time)
p.age.type = list(formula=~age*type)
p.age.type.time = list(formula=~age*type*time)

model.list=create.model.list(model="CJS")
results.mhhs=mark.wrapper(model.list, data=capt.hist.mhhs.processed,ddl=ddl.mhhs)
results.mhhs


#######################################
#Plot results of best model

best.survival<-results.mhhs$Phi.age.type.time.p.age.type.time$results$real
best.survival<-mutate(best.survival, index = c(1:122))
best.survival<-filter(best.survival, index <=70)
pim.translation<-results.mhhs$Phi.age.type.time.p.age.type.time$simplify$pim.translation

design<-ddl.mhhs$Phi
for(i in 1:156){
  design[i,1] <- pim.translation[i]
}


ref<-select(design, par.index, age, time, type)
ref<-group_by(ref, par.index)
ref<-distinct(ref, .keep_all = TRUE)

ggbest.survival<-bind_cols(select(best.survival, estimate, se, lcl, ucl), select(ref, age,time,type))
ggbest.survival2<-filter(ggbest.survival, age != "[0,1)")

ggplot(ggbest.survival2, aes (x=age, y=estimate, shape=type))+
  geom_point(position = position_dodge(width=0.5))+
  facet_wrap(~time, scales = "free_x")+
  theme_bw()+
  scale_y_continuous(limits = c(0,1))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0, position = position_dodge(width=0.5))+
  scale_shape_manual(values = c(16,1))+
  ylab(expression(Apparent~Survival~(phi)))+xlab("Age Class")



#Without first year separated
ggmhhs<-read.csv("H:/ggmhhs.csv")

type<- list(
  'HS' = "Headstarts",
  'MH' = "Wild Hatchlings"
)

type_labeller<- function(variable, value){
  return(type[value])
}

ggmhhs$age<-factor(ggmhhs$age, levels = c("Juv", "SA", "A"))

ggplot(ggmhhs, aes (x=age, y=phi))+geom_point()+facet_wrap(~type, labeller = type_labeller)+
  theme_bw()+scale_y_continuous(limits = c(0,1))+geom_errorbar(aes(ymin=lci, ymax=uci), width=.1)+
  ylab(expression(Apparent~Survival~(phi)))+xlab("Age Class")+
  theme(axis.text = element_text(size=13),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        strip.text.x=element_text(size=13))
