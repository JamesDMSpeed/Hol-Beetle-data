#Beetles analysis

rm(list=ls())
require(vegan)

#Beetle data
beetles<-read.csv('Biller2003_2009_2015Final.csv',header=T)
head(beetles)
#Make Enclosure
beetles$enclosure<-substr(beetles$plot,1,1)
#Make treatment
beetles$treatment<-beetles$enclosure
beetles$treatment[beetles$enclosure%in%c('A','E','G')]<-'Ungrazed'
beetles$treatment[beetles$enclosure%in%c('B','F','H')]<-'High sheep density'
beetles$treatment[beetles$enclosure%in%c('C','D','I')]<-'Low sheep density'

#Check plot numbers
levels(beetles$plot)#Some plots written in different ways i.e. 'A01' and 'A1'

#Diversity
beetles$richness<-specnumber(beetles[,3:75])
summary(beetles$richness)

#Some missing data
beetles[is.na(beetles$richness),]

boxplot(beetles$richness~beetles$year_,ylab='Beelte species richness')
