 batting<-read.csv("Batting.csv")
 batting$BA<-batting$H/batting$AB
 batting$OBP<-(batting$H+batting$BB+batting$HBP)/(batting$AB+batting$BB+batting$HBP+batting$SF)
 batting$SLG<-((batting$H-batting$HR-batting$X2B-batting$X3B)+(2*batting$X2B)+(3*batting$X3B)+(4*batting$HR))/batting$AB
 sal<-read.csv("Salaries.csv")
 bat1985<-subset(batting,batting$yearID>1984)
 combo<-merge(bat1985,sal,by=c("playerID","yearID"))
 lost_players<-subset(combo,playerID%in%c("giambja01","damonjo01","saenzol01"))
 lost_players.2001<-subset(lost_players,yearID==2001)
 ref_players<-lost_players.2001[,c("playerID","H","X2B","X3B","HR","OBP","SLG","BA","AB")]
 lost_OBP<-sum(ref_players$OBP)
 lost_AB<-sum(ref_players$AB)
 max.spending<-15000000
 combo<-subset(combo,combo$yearID==2001)
 combo<-combo[,c("playerID","AB","OBP","salary")]
 combo$costvalue<-(combo$AB/sd(combo$AB,na.rm=T))*(combo$OBP/sd(combo$OBP,na.rm=T))/(combo$salary/100000)
 combo$valueonly<-(combo$AB/sd(combo$AB,na.rm=T))*(combo$OBP/sd(combo$OBP,na.rm=T))
 example_plot<-arrange(combo,desc(combo$value))[1:100,]
 condition1<-function(salary){
   levelsalary=c()
   for (i in 1:length(salary)) {
   if(salary[i] <=500000){
     levelsalary<-c(levelsalary,"low")
   }else if(salary[i]>500000 && salary[i] <=5000000){
     levelsalary<-c(levelsalary,"medium")
   }else{
     levelsalary<-c(levelsalary,"high")
   }
   }
   return(levelsalary)
   }
 example_plot$salarylevel<-condition1(example_plot$salary)
 pl<-ggplot(example_plot,aes(x=AB,y=OBP))+geom_point(aes(color=factor(salarylevel),size=salary))
 print(pl)
 
 # for the maximisation problem, values for AB(for at Bat) and OBP(on base Percentage) have been
 # normalized by their own standard deviation and multiplied for a combined value
 # since it it not possible to deduct from the task, in which relationship the values stand, a 
 # more adequate weighting/normalisation is not possible
 # maximisation follows by price and the compound value
 
 simple.max<-function(costvalue){
   example_plot2<-arrange(example_plot,desc(example_plot$costvalue))
   print(head(example_plot2$playerID,3))
 }
 simple.max(example_plot$costvalue)
 
 max.performance<-function(valueonly,salary){
   namen<-c()
   max <-0
#iteratives Verfahren in 3 Stufen
   for(i in 1:length(valueonly)){
     price <-0
     value<-0
     price2 <-0
     value2<-0
     price3 <-0
     value3<-0
     value<-valueonly[i]
     price<-price+salary[i]
     if(is.na(price)||price>=15000000){
       next}
     for(j in (i+1):length(valueonly)){
       value2<-value+valueonly[j]
       price2<-price+salary[j]
       if(is.na(price2)||price2>=15000000){
         next
         }
         for(h in (j+1):length(valueonly)){
         value3<-value2+valueonly[h]
         price3<-price2+salary[h]
         if(is.na(price3)||price3>=15000000){
           next}
         if(value3>=max){
           max<-value3
           namen[1]<-paste(namen,example_plot$playerID[i])
           namen[2]<-paste(namen,example_plot$playerID[j])
           namen[3]<-paste(namen,example_plot$playerID[h])
           } 
       }
     }
   }
print(namen)
return(namen)
 }
namen<-max.performance(example_plot$valueonly,example_plot$salary)

cheap_picks<-subset(arrange(example_plot,desc(example_plot$costvalue)))[1:3,]
cheap_picks$state<-c("cheapest option","cheapest option","cheapest option")
costly_picks<-subset(example_plot,example_plot$playerID%in%namen[1:3])
costly_picks$state<-c("costly/best performance","costly/best performance","costly/best performance")
ref_players.2<-lost_players.2001[,c("playerID","AB","OBP","salary")]
ref_players.2$valueonly<-(ref_players.2$AB/sd(ref_players.2$AB,na.rm=T))*(ref_players.2$OBP/sd(ref_players.2$OBP,na.rm=T))/(ref_players.2$salary/100000)
ref_players.2$valueonly<-(ref_players.2$AB/sd(ref_players.2$AB,na.rm=T))*(ref_players.2$OBP/sd(ref_players.2$OBP,na.rm=T))
ref_players.2$state[1:3]<-"lost players"
comparison<-append(cheap_picks,costly_picks,ref_players.2)
print(comparison)
