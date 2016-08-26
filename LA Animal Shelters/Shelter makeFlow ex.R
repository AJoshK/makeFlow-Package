
library(dplyr)
library(RColorBrewer)
library(makeFlow)

# let's get the data in R:
load(file = "LAShelters.rda")

#keep what we need:
shelter1 <- lashelters %>% select(Intake.Date,Outcome.Date,OutCatg,Species)

shelter1$weeksInShelter <- difftime(time1 = shelter1$Outcome.Date,time2 = shelter1$Intake.Date,units = "weeks")

shelter1 <- shelter1[-which(is.na(shelter1$Outcome.Date)),]

shelter1$Intake.Date <- as.Date(shelter1$Intake.Date)
shelter1$Outcome.Date<- as.Date(shelter1$Outcome.Date)

#If we want an opening gate with all our data accounted for:
shelter1$all <- "All Animals"

## Factor our fields so we can specify the order we care about (Remaining will go up top):
shelter1$sub1week <-  as.factor(ifelse(shelter1$weeksInShelter <= 1, shelter1$OutCatg,"Remaining"))  ; shelter1$sub1week <-   relevel(x = shelter1$sub1week,ref="Remaining") 
shelter1$sub2week <-  as.factor(ifelse(shelter1$weeksInShelter<= 2, shelter1$OutCatg,"Remaining"))   ; shelter1$sub2week <-   relevel(x = shelter1$sub2week,ref="Remaining") 
shelter1$sub3week <-  as.factor(ifelse(shelter1$weeksInShelter<= 3, shelter1$OutCatg,"Remaining"))   ; shelter1$sub3week <-   relevel(x = shelter1$sub3week ,ref="Remaining") 
shelter1$submonth1 <- as.factor(ifelse(shelter1$weeksInShelter<= 4, shelter1$OutCatg,"Remaining"))   ; shelter1$submonth1 <-  relevel(x = shelter1$submonth1 ,ref="Remaining") 
shelter1$submonth2 <- as.factor(ifelse(shelter1$weeksInShelter<= 8, shelter1$OutCatg,"Remaining"))   ; shelter1$submonth2 <-  relevel(x = shelter1$submonth2 ,ref="Remaining") 
shelter1$submonth3 <- as.factor(ifelse(shelter1$weeksInShelter<= 12, shelter1$OutCatg,"Remaining"))  ; shelter1$submonth3 <-  relevel(x = shelter1$submonth3 ,ref="Remaining") 
shelter1$submonth4 <- as.factor(ifelse(shelter1$weeksInShelter<= 16, shelter1$OutCatg,"Remaining"))  ; shelter1$submonth4 <-  relevel(x = shelter1$submonth4 ,ref="Remaining") 
shelter1$submonth5 <- as.factor(ifelse(shelter1$weeksInShelter<= 20, shelter1$OutCatg,"Remaining"))  ; shelter1$submonth5 <-  relevel(x = shelter1$submonth5 ,ref="Remaining") 
shelter1$submonth6 <- as.factor(ifelse(shelter1$weeksInShelter<= 24, shelter1$OutCatg,"Remaining"))  ; shelter1$submonth6 <-  relevel(x = shelter1$submonth6 ,ref="Remaining") 
shelter1$submonth7 <- as.factor(ifelse(shelter1$weeksInShelter<= 28, shelter1$OutCatg,"Remaining"))  ; shelter1$submonth7 <-  relevel(x = shelter1$submonth7 ,ref="Remaining") 
shelter1$submonth8 <- as.factor(ifelse(shelter1$weeksInShelter<= 32, shelter1$OutCatg,"Remaining"))  ; shelter1$submonth8 <-  relevel(x = shelter1$submonth8 ,ref="Remaining") 
shelter1$submonth9 <- as.factor(ifelse(shelter1$weeksInShelter<= 36, shelter1$OutCatg,"Remaining"))  ; shelter1$submonth9 <-  relevel(x = shelter1$submonth9 ,ref="Remaining") 
shelter1$submonth10 <- as.factor(ifelse(shelter1$weeksInShelter<= 40, shelter1$OutCatg,"Remaining")) ; shelter1$submonth10 <- relevel(x = shelter1$submonth10,ref="Remaining")   
shelter1$submonth11 <- as.factor(ifelse(shelter1$weeksInShelter<= 44, shelter1$OutCatg,"Remaining")) ; shelter1$submonth11 <- relevel(x = shelter1$submonth11,ref="Remaining")   
shelter1$subyear1 <-  as.factor(ifelse(shelter1$weeksInShelter<= 52, shelter1$OutCatg,"Remaining"))  ; shelter1$subyear1 <-   relevel(x = shelter1$subyear1 ,ref="Remaining") 
shelter1$subyear2 <-  as.factor(ifelse(shelter1$weeksInShelter<= 104, shelter1$OutCatg,"Remaining")) #; shelter1$subyear2 <-   relevel(x = shelter1$subyear2 ,ref="Remaining")   


#makeFlow diagram 1:
makeFlow:::makeFlow(data=shelter1,classFields = names(shelter1)[6:15],fieldLabels = c("","1wk","2wk","3wk","4wk","2mo","3mo","4mo","5mo","6mo"),
                    distanceBtwnGates = 95,minVerticalBtwnGates = .2,
                    gatecolors = c("goldenrod3","cadetblue1","plum2","tomato","darkorange3","chartreuse","yellow1","black"))
  # we can add a title outside the function too:
  title(main = "Los Angeles Animal Shelter Outcomes")
  
#find the underlying information presented in the diagram:
myGates <- makeFlow:::GateSummaries(data=shelter1,classFields = names(shelter1)[6:15])
myFlows <- makeFlow:::FlowSummaries(data=shelter1,classFields = names(shelter1)[6:15])


# diagram 2:
shelter2 <- shelter1[which(shelter1$Species=="DOG"),]
shelter2$all <- "All Dogs"
makeFlow:::makeFlow(data=shelter2,classFields = names(shelter2)[6:(length(shelter2)-1)],plotTitle = "LA Shelter Dogs")

# diagram 3:
shelter3 <- shelter1[which(shelter1$Species=="CAT"),]
shelter3$all <- "All Cats"
makeFlow:::makeFlow(data=shelter3,
                    classFields = names(shelter3)[6:15],
                    bg = "white",
                    gateWidth = 20,
                    txtColor = "black",
                    percentTextColor = "black",
                    gatecolors = c("aquamarine","goldenrod4","plum2","darkorange2","blue3","tomato3","springgreen4","white"),
                    countTextColor = "black",
                    plotTitle = "Cats",
                    fieldLabels = c("","1wk","2wk","3wk","4wk","2mo","3mo","4mo","5mo","6mo"),
                    minVerticalBtwnGates = .15)
