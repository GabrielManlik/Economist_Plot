pl<-ggplot(df,aes(x=CPI,y=HDI))
pl2<-pl+geom_point(size=3,aes(color=factor(Region)),shape=1)
pl3<-pl2+geom_smooth(color="red",method = "lm",se=FALSE,formula = y~log(x))
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")
pl4<-pl3+geom_text(aes(label=Country),color="gray20",data=subset(df,Country%in%pointsToLabel),
                   check_overlap=TRUE)
pl5<-pl4 + theme_bw()+scale_x_continuous(name="Corruption Perceptions Index, 2011 (10=least corrupt)",
                                         breaks = 1:10,limits=c(0.5,10))
pl6<-pl5+scale_y_continuous(name="Human Development Index, 2011 (1=best)",limits=c(0.28,0.95),breaks=c(0.3,0.5,0.7,0.9))+
  ggtitle("Corruption and Human Development")
pl7<-pl6+theme_economist_white()
print(pl7)