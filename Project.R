library(tidyverse)
library(plotly)
library(ggthemes)

prepdata <- read.csv("HDBdata.csv")
inflation <- read.csv("Inflation.csv")

data <- subset(prepdata, town == "ANG MO KIO" | town == "BUKIT BATOK")
remaininglease = 99 - (2020-data$lease_commence_date)
data$remaining_lease<- remaininglease
colnames(inflation)[2]<-"Inflation Rate"

AMKdata <- subset(data, town == "ANG MO KIO")
BBdata <- subset( data, town == "BUKIT BATOK")

agg<-aggregate(data,by=list(data$month), FUN=mean)
agg <- agg[,-c(2,3,4,5,6,7,9)]
colnames(agg)[1] <- "Dates"
agg$inflation_rate <- inflation$`Inflation Rate`
temp<- c(agg[1,5],(1+(agg$inflation_rate/100)))
agg$adj_price <- cumprod(temp)[-121]

AMKagg <- aggregate(AMKdata,by=list(AMKdata$month), FUN=mean)
AMKagg <- AMKagg[,-c(2,3,4,5,6,7,9)]
colnames(AMKagg)[1] <- "Dates"
AMKagg$inflation_rate <- inflation$`Inflation Rate`
temp<- c(AMKagg[1,5],(1+(AMKagg$inflation_rate/100)))
AMKagg$adj_price <- cumprod(temp)[-121]

BBagg <- aggregate(BBdata,by=list(BBdata$month), FUN=mean)
BBagg <- BBagg[,-c(2,3,4,5,6,7,9)]
colnames(BBagg)[1] <- "Dates"
BBagg$inflation_rate <- inflation$`Inflation Rate`
temp<- c(BBagg[1,5],(1+(BBagg$inflation_rate/100)))
BBagg$adj_price <- cumprod(temp)[-121]

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

#Part 1: Has the cost of housing kept up with the rate of inflation?

# Inflation Rate, month on month

a <- ggplot(inflation, aes(x=Date, y=`Inflation Rate`, group = 1)) + 
  geom_line(colour = "black")+
  stat_smooth(method = "gam")+
  ggtitle("Inflation Rate (Month on Month)")  +
  scale_y_continuous(name="Amount (%)", labels = scales::comma) +
  scale_x_discrete(name = "Year", breaks = every_nth(n = 36))+
  theme_economist() + scale_colour_economist()

figure1 <- ggplotly(a)
figure1

# Real vs nominal price
b <- ggplot(agg, aes(x=Dates, y=resale_price)) + 
  geom_line(aes(y=resale_price, color = "Nominal Resale Price", group = 1))+
  geom_line(aes(y=adj_price, color = "Inflation Adjusted Price", group = 1))+
  labs(color = '')+
  ggtitle("Real vs Nominal property prices")  +
  scale_y_continuous(name="Value ($)", labels = scales::comma) +
  scale_x_discrete(name = "Year", breaks = every_nth(n = 36)) +
  theme_economist(dkpanel=TRUE) + scale_colour_economist()

figure2 <- ggplotly(b)
figure2

# Difference
c <- ggplot(agg, mapping = aes(x= Dates, y=resale_price-adj_price)) + 
  geom_line(mapping = aes(x= Dates, y=(resale_price-adj_price), color = "Difference", group = 1))+
  labs(color = '')+
  ggtitle("Difference between Nominal and Inflation Predicted prices")+
  scale_y_continuous(name="Value ($)", labels = scales::comma) +
  scale_x_discrete(name = "Year", breaks = every_nth(n = 36)) +
  theme_economist(dkpanel=TRUE) + scale_colour_economist()

figure3 <- ggplotly(c)
figure3

# Real vs nominal prices in AMK

d <- ggplot(AMKagg, aes(x=Dates, y=resale_price)) + 
  geom_line(aes(y=resale_price, color = "Nominal Resale Price", group = 1))+
  geom_line(aes(y=adj_price, color = "Inflation Adjusted Price", group = 1))+
  scale_color_manual(values = c(
    'Nominal resale prices' = 'blue',
    'Inflation adjusted prices' = 'green')) +
  labs(color = '')+
  ggtitle("Real vs Nominal property prices in Ang Mo Kio")  +
  scale_y_continuous(name="Value ($)", labels = scales::comma) +
  scale_x_discrete(name = "Year", breaks = every_nth(n = 36)) +
  theme_economist(dkpanel=TRUE) + scale_colour_economist()

figure4 <- ggplotly(d)
figure4

# Real vs nominal prices in BB

e <- ggplot(BBagg, aes(x=Dates, y=resale_price)) + 
  geom_line(aes(y=resale_price, color = "Nominal resale prices", group = 1))+
  geom_line(aes(y=adj_price, color = "Inflation adjusted prices", group = 1))+
  scale_color_manual(values = c(
    'Nominal resale prices' = 'blue',
    'Inflation adjusted prices' = 'green')) +
  labs(color = '')+
  ggtitle("Real vs Nominal property prices in Bukit Batok")  +
  scale_y_continuous(name="Value ($)", labels = scales::comma) +
  scale_x_discrete(name = "Year", breaks = every_nth(n = 36)) +
  theme_economist(dkpanel=TRUE) + scale_colour_economist()

figure5 <- ggplotly(e)
figure5

# What factors contributed the most to the changes in the price of housing?

# Is it because of the floor area?

f <- ggplot(agg, aes(x=Dates, y=floor_area_sqm, group =1)) + 
  geom_line(colour="black")+
  stat_smooth(method="gam")+
  ggtitle("Average floor space of flats sold over time")  +
  scale_y_continuous(name="Floor Space (sqm)", labels = scales::comma) +
  scale_x_discrete(name = "Year", breaks = every_nth(n = 36)) +
  theme_economist() + scale_colour_economist()
figure6 <- ggplotly(f)
figure6

AMKagg$Town <- "Ang Mo Kio"
BBagg$Town <- "Bukit Batok"
test <- rbind(AMKagg, BBagg)
test <- arrange(test, Dates)

figure7 <- plot_ly(test, x=~Dates, y =~floor_area_sqm, z=~resale_price, mode = "markers", type = "scatter3d", color = test$Town, colors = c("#132B43", "#56B1F7")) %>%
  layout(title = "Changes in resale price by floor area over time")
figure7

# Is it because of storey range?

data4 <-data
aggS <- aggregate(data4, by = list(data4$storey_range), FUN=mean)
aggS <- aggS[,-c(2,3,4,5,6,7,9)]

g <- ggplot(aggS, aes(x=Group.1, y=resale_price, group = 1)) + 
  geom_point()+
  stat_smooth(method = "lm")+
  ggtitle("Relation between floor level and mean resale price")  +
  scale_y_continuous(name="Value ($)", labels = scales::comma) +
  scale_x_discrete(name = "Floor Level", breaks = every_nth(n = 3))+
  theme_economist() + scale_colour_economist()

figure8 <- ggplotly(g)
figure8

data5 <- data
data5$meanfloor <- (as.numeric(substr(data5$storey_range, start = 1, stop = 2)) + as.numeric(substr(data5$storey_range, start = 7, stop = 8)))/2
aggSR <- aggregate(data5, by = list(data5$month), FUN=mean)
aggSR <- aggSR[,-c(2,3,4,5,6,7,9)]

h <- ggplot(aggSR, aes(x=Group.1, y=meanfloor, group = 1)) + 
  geom_line(colour = "black")+
  stat_smooth(method = "gam")+
  ggtitle("Mean Floor Height of flats sold over time")  +
  scale_y_continuous(name="Height (Floor Level)", labels = scales::comma) +
  scale_x_discrete(name = "Year", breaks = every_nth(n = 36))+
  theme_economist() + scale_colour_economist()

figure9 <- ggplotly(h)
figure9

# Is it because of flat model?

# NOT USED IN MARKDOWN DUE TO SPACE
#aggtest <- data %>%
  #group_by(flat_model) %>%
  #summarize(n=n(),avg=mean(resale_price, na.rm = TRUE))

#i <- aggtest %>%
  #ggplot(aes(x=reorder(flat_model, -avg),y=avg))+
  #geom_bar(stat='identity')+
  #ggtitle("Average cost of each Flat type")+
  #scale_y_continuous(name="Value ($)", labels = scales::comma) +
  #scale_x_discrete(name = "Flat Type", labels = abbreviate)+
  #theme_economist(dkpanel=TRUE) + scale_colour_economist()

#figure10 <- ggplotly(i)
#figure10

agg6<-aggregate(subset(data, flat_model == "Adjoined flat"),by=list(subset(data, flat_model == "Adjoined flat")$month), FUN = length)
agg7<-aggregate(subset(data, flat_model == "Apartment"),by=list(subset(data, flat_model == "Apartment")$month), FUN = length)
agg8<-aggregate(subset(data, flat_model == "DBSS"),by=list(subset(data, flat_model == "DBSS")$month), FUN = length)
agg9<-aggregate(subset(data, flat_model == "Improved"),by=list(subset(data, flat_model == "Improved")$month), FUN = length)
agg10<-aggregate(subset(data, flat_model == "Maisonette"),by=list(subset(data, flat_model == "Maisonette")$month), FUN = length)
agg11<-aggregate(subset(data, flat_model == "Model A"),by=list(subset(data, flat_model == "Model A")$month), FUN = length)
agg12<-aggregate(subset(data, flat_model == "Model A2"),by=list(subset(data, flat_model == "Model A2")$month), FUN = length)
agg13<-aggregate(subset(data, flat_model == "New Generation"),by=list(subset(data, flat_model == "New Generation")$month), FUN = length)
agg14<-aggregate(subset(data, flat_model == "Simplified"),by=list(subset(data, flat_model == "Simplified")$month), FUN = length)

j <- ggplot() + 
  geom_line(data=agg6, aes(x=Group.1,y=resale_price, colour="Adjoined flat", group = 1))+ 
  geom_line(data=agg7, aes(x=Group.1,y=resale_price, colour="Apartment", group = 1))+
  geom_line(data=agg8, aes(x=Group.1,y=resale_price, colour="DBSS", group = 1))+ 
  geom_line(data=agg9, aes(x=Group.1,y=resale_price, colour="Improved", group = 1))+ 
  geom_line(data=agg10, aes(x=Group.1,y=resale_price, colour="Maisonette", group = 1))+ 
  geom_line(data=agg11, aes(x=Group.1,y=resale_price, colour="Model A", group = 1))+ 
  geom_line(data=agg12, aes(x=Group.1,y=resale_price, colour="Model A2", group = 1))+
  geom_line(data=agg13, aes(x=Group.1,y=resale_price, colour="New Generation", group = 1))+ 
  geom_line(data=agg14, aes(x=Group.1,y=resale_price, colour="Simplified", group = 1))+
  ggtitle("Supply of flats by model over time") +
  labs(color = '')+
  scale_y_continuous(name="Flats sold", labels = scales::comma) +
  scale_x_discrete(name = "Year", breaks = every_nth(n = 36))+
  theme_economist(dkpanel=TRUE) + scale_colour_economist()

figure10 <- ggplotly(j)
figure10

# Is it because of flat type?

data3 <- data
aggFT <- aggregate(data3, by = list(data3$flat_type), FUN=mean)
aggFT <- aggFT[,-c(2,3,4,5,6,7,9)]

k <- ggplot(data=aggFT,aes(x=reorder(Group.1, -resale_price),y=resale_price))+
  geom_bar(stat='identity')+
  ggtitle("Price of flats by type")+
  scale_y_continuous(name="Value ($)", labels = scales::comma) +
  scale_x_discrete(name = "Flat Type")+
  theme_economist(dkpanel=TRUE) + scale_colour_economist()

figure11 <- ggplotly(k)
figure11

agg2<-aggregate(subset(data, flat_type == "2 ROOM"),by=list(subset(data, flat_type == "2 ROOM")$month), FUN = length)
agg3<-aggregate(subset(data, flat_type == "3 ROOM"),by=list(subset(data, flat_type == "3 ROOM")$month), FUN = length)
agg4<-aggregate(subset(data, flat_type == "4 ROOM"),by=list(subset(data, flat_type == "4 ROOM")$month), FUN = length)
agg5<-aggregate(subset(data, flat_type == "5 ROOM"),by=list(subset(data, flat_type == "5 ROOM")$month), FUN = length)
aggm<-aggregate(subset(data, flat_type == "EXECUTIVE"),by=list(subset(data, flat_type == "EXECUTIVE")$month), FUN = length)


l <- ggplot() + 
  geom_line(data=agg2, aes(x=Group.1,y=resale_price, colour="2 Room", group = 1))+ 
  geom_line(data=agg3, aes(x=Group.1,y=resale_price, colour="3 Room", group = 1))+
  geom_line(data=agg4, aes(x=Group.1,y=resale_price, colour="4 Room", group = 1))+ 
  geom_line(data=agg5, aes(x=Group.1,y=resale_price, colour="5 Room", group = 1))+ 
  geom_line(data=aggm, aes(x=Group.1,y=resale_price, colour="Executive", group = 1))+ 
  ggtitle("Supply of flats by type over time") +
  labs(color = '')+
  scale_y_continuous(name="Flats sold", labels = scales::comma) +
  scale_x_discrete(name = "Year", breaks = every_nth(n = 36))+
  theme_economist(dkpanel=TRUE) + scale_colour_economist()

figure12 <- ggplotly(l)
figure12

# NOT USED IN MARKDOWN DUE TO SPACE

#agg2.1<-aggregate(subset(data, flat_type == "2 ROOM"),by=list(subset(data, flat_type == "2 ROOM")$month), FUN = mean)
#agg3.1<-aggregate(subset(data, flat_type == "3 ROOM"),by=list(subset(data, flat_type == "3 ROOM")$month), FUN = mean)
#agg4.1<-aggregate(subset(data, flat_type == "4 ROOM"),by=list(subset(data, flat_type == "4 ROOM")$month), FUN = mean)
#agg5.1<-aggregate(subset(data, flat_type == "5 ROOM"),by=list(subset(data, flat_type == "5 ROOM")$month), FUN = mean)
#aggm.1<-aggregate(subset(data, flat_type == "EXECUTIVE"),by=list(subset(data, flat_type == "EXECUTIVE")$month), FUN = mean)

#j <- ggplot() + 
  #geom_line(data=agg2.1, aes(x=Group.1,y=resale_price, colour="2 Room", group = 1))+ 
  #geom_line(data=agg3.1, aes(x=Group.1,y=resale_price, colour="3 Room", group = 1))+
  #geom_line(data=agg4.1, aes(x=Group.1,y=resale_price, colour="4 Room", group = 1))+ 
  #geom_line(data=agg5.1, aes(x=Group.1,y=resale_price, colour="5 Room", group = 1))+ 
  #geom_line(data=aggm.1, aes(x=Group.1,y=resale_price, colour="Executive", group = 1))+ 
  #ggtitle("Mean resale price by flat type over time") +
  #labs(color = '')+
  #scale_y_continuous(name="Value ($)", labels = scales::comma) +
  #scale_x_discrete(name = "Year", breaks = every_nth(n = 36))+
  #theme_economist(dkpanel=TRUE) + scale_colour_economist()

#figure14 <- ggplotly(j)
#figure14

