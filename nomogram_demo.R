library(rms)
library(foreign)
library(survival)

#load data
dev<-read.csv("/Users/Claire/Documents/research progress/nomogram/dataset/dev.csv") 
dim(dev)
head(dev)

#categorical factor
str(dev)
dev$Depth.of.invasion<-factor(dev$Depth.of.invasion,labels=c('mucosa/submucosa','proper muscle','subserosa','serosa'))
dev$Location<-factor(dev$Location,labels=c('lower','upper','middle','whole'))
dev$Age<-factor(dev$Age,labels=c('<40','40-49','50-59','60-69','>=70'))
dev$LN.dissection<-factor(dev$LN.dissection,labels=c('D2','D1 plus'))
dev$tumor.size<-factor(dev$tumor.size,labels=c('<5.0','5.0-9.9','>=10.0'))
dev$metastatic.lymph.nodes<-factor(dev$metastatic.lymph.nodes,labels=c('<0','1-2','3-6','>=7'))
dev$Lymphovascular.invasion<-factor(dev$Lymphovascular.invasion,labels=c('NO','Yes'))



#Radiomics prediction value
dev$Rad_score<-runif(1579, min=0, max=1)
str(dev)


#nomogram
ddist <- datadist(dev)
options(datadist='ddist')
units(dev$follow) <- "Month"

#Cox model
fcox <- cph(Surv(follow,death) ~ Age + tumor.size + Location + Lymphovascular.invasion + Depth.of.invasion + metastatic.lymph.nodes + LN.dissection+Rad_score,surv=T,x=T, y=T,data=dev) 

#Nomogram for survival
surv <- Survival(fcox)  
nom <- nomogram(fcox, fun=list(function(x) surv(36, x),
                               function(x) surv(60, x)),
                funlabel=c("3-years Survival Probability", 
                           "5-years Survival Probability"),lp=F)
plot(nom)

