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

#Clean plot
beetles$plot<-as.factor(paste0(substr(beetles$plot,1,1),as.integer(substr(beetles$plot,2,3))))

#Diversity
beetles$beetlerichness<-specnumber(beetles[,3:75])
summary(beetles$beetlerichness)

#Some missing data
beetles[is.na(beetles$beetlerichness),]

boxplot(beetles$beetlerichness~beetles$year_,ylab='Beelte species richness')


#Link to vegetation data
vascdat<-read.csv('Vascular_2001_to_2015.csv',header=T)
head(vascdat)
vascdat$vascrichness<-specnumber(vascdat[,11:138])
boxplot(vascdat$vascrichness~vascdat$Year)

#Clean plot
vascdat$Plot<-as.factor(paste0(substr(vascdat$Plot,1,1),as.integer(substr(vascdat$Plot,2,3))))
#Add environmental data to beetles
#Merge with vascular data
beetles_veg<-merge(beetles,vascdat[,c(1:10,139:152)],by.x=c('plot','year_'),by.y=c('Plot','Year'),all.y=F)
with(beetles_veg,plot(vascrichness,beetlerichness))
lm1<-with(beetles_veg,lm(beetlerichness~vascrichness))
summary(lm1)
abline(lm1)#Repeated measures

lm1<-with(beetles_veg[beetles_veg$year_==2003,],lm(beetlerichness~vascrichness))
summary(lm1)

lm2<-with(beetles_veg,lm(beetlerichness~vascrichness+Altitude+year_))
summary(lm2)

#Diversity
beetles$richness<-specnumber(beetles[,3:75])
summary(beetles$richness)

#Some missing data
beetles[is.na(beetles$richness),]
boxplot(beetles$richness~beetles$year_,ylab='Beelte species richness')


#VascVegdata
vascdat<-read.csv('Vascular_2001_to_2015.csv',header=T,sep=',')
vascdat$vascrichness<-specnumber(vascdat[,11:138])
boxplot(vascdat$vascrichness~vascdat$Year)
vascdat$Plot<-as.factor(paste0(substr(vascdat$Plot,1,1),as.integer(substr(vascdat$Plot,2,3))))
beetles_veg<-merge(beetles,vascdat[,c(1:10,139:152)],by.x=c('plot','year_'),by.y=c('Plot','Year'),all.y=F)
with(beetles_veg,plot(vascrichness,beetlerichness))
lm1<-with(beetles_veg,lm(beetlerichness~vascrichness))
summary(lm1)
abline(lm1)#Repeated measures
lm1<-with(beetles_veg[beetles_veg$year_==2003,],lm(beetlerichness~vascrichness))
summary(lm1)
lm2<-with(beetles_veg,lm(beetlerichness~vascrichness+Altitude+year_))
summary(lm2)

