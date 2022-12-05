library(ggplot2)
library(dplyr)
library(lubridate)

OECD <- OECD_short_term_economic_indicators
IMF<- IMF_monetary_policy
Enq13<- FRBNY_SCE_Public_Microdata_Complete_13_16
Enq17 <- FRBNY_SCE_Public_Microdata_Complete_17_19

inflation_US<-subset(OECD, OECD$Country=="United States" & OECD$Subject2=="Consumer prices: all items")
inflation_US2<-data.frame(
  day=as.Date(inflation_US$Time,format='%Y-%m-%d'),
  Inflation=inflation_US$Value
)

plot(inflation_US2$day,inflation_US2$Inflation,,type='l',xlab='Date',ylab="Prix à la consommation",col='royalblue3')

mean(Enq17$Q8v2part2, na.rm = TRUE)

Enq17$date=ymd(paste(Enq17$date,'01',sep=''))
Enq13$date=ymd(paste(Enq13$date,'01',sep=''))

anticipations=Enq17 %>%                        
group_by(date) %>%                        
  summarise_at(vars(Q8v2part2),
               list(name = mean),na.rm = TRUE)




ggplot(anticipations) + geom_line(aes(x = date, y = name))+ylab('Expected inflation - 12 months')

