library(ggplot2)
library(dplyr)
library(lubridate)

OECD <- OECD_short_term_economic_indicators
IMF<- IMF_monetary_policy
Enq13<- FRBNY_SCE_Public_Microdata_Complete_13_16
Enq17 <- FRBNY_SCE_Public_Microdata_Complete_17_19

# Graphique d'indice des prix à la consommation --------------------

inflation_US<-subset(OECD, OECD$Country=="United States" & OECD$Subject2=="Consumer prices: all items")
inflation_US2<-data.frame(
  day=as.Date(inflation_US$Time,format='%Y-%m-%d'),
  Inflation=as.numeric(inflation_US$Value)
)

plot(inflation_US2$day,inflation_US2$Inflation,,type='l',xlab='Date',ylab="Prix à la consommation",col='royalblue3')

mean(Enq17$Q8v2part2, na.rm = TRUE)

#--------------- calcul du taux d'inflation -------------

inflation_US3 <-data.frame(
  date=inflation_US2$day,
  tx_evol_ann_pct = (inflation_US2$Inflation / lag(inflation_US2$Inflation,12) - 1) * 100)
  


# Conversion des formats nuls de time en date -------------

Enq17$date=ymd(paste(Enq17$date,'01',sep=''))
Enq13$date=ymd(paste(Enq13$date,'01',sep=''))

# ---------- Graphique des anticipations d'inflation à court terme ---------------------

anticipations_short_13=Enq13 %>%                        
group_by(date) %>%                        
  summarise_at(vars(Q8v2part2),
               list(name = mean),na.rm = TRUE)

anticipations_short_17=Enq17 %>%                        
  group_by(date) %>%                        
  summarise_at(vars(Q8v2part2),
               list(name = mean),na.rm = TRUE)

anticipations_short=rbind(anticipations_short_13, anticipations_short_17)


ggplot(anticipations_short) + geom_line(aes(x = date, y = name))+ylab('Expected inflation - 12 months')

# ---------- Graphique des anticipations d'inflation à moyen terme ---------------------

anticipations_mid_13=Enq13 %>%                        
  group_by(date) %>%                        
  summarise_at(vars(Q9bv2part2),
               list(name = mean),na.rm = TRUE)

anticipations_mid_17=Enq17 %>%                        
  group_by(date) %>%                        
  summarise_at(vars(Q9bv2part2),
               list(name = mean),na.rm = TRUE)

anticipations_mid=rbind(anticipations_mid_13, anticipations_mid_17)


ggplot(anticipations_mid) + geom_line(aes(x = date, y = name))+ylab('Expected inflation - in 36 months')

# ---------- Création de bases de comparaison ---------------------

# ---------- Comparaison pour du court terme ----------------------

comp_infl_short<-data.frame(
  merge(inflation_US3,anticipations_short,by="date")
)

comp_infl_short$expected=lag(comp_infl_short$name,12)
comp_infl_short$dif=comp_infl_short$expected-comp_infl_short$tx_evol_ann_pct

ggplot(comp_infl_short) + geom_line(aes(x = date, y = expected,colour='Expected'))+geom_line(aes(x = date, y = tx_evol_ann_pct,colour='Real'))+ylab('Inflation rate')+scale_color_manual(name = "Inflation", values = c("Real"='red', "Expected"='blue'))+scale_x_date(limits = c(as.Date('2014-05-01'),as.Date('2020-01-01')))+ggtitle('Comparaison inflation anticipée à m-12 et inflation réalisée')

# ---------- Comparaison pour du moyen terme ----------------------
comp_infl_mid<-data.frame(
  merge(inflation_US3,anticipations_mid,by="date")
)
comp_infl_mid$expected=lag(comp_infl_mid$name,36)
comp_infl_mid$dif=comp_infl_mid$expected-comp_infl_mid$tx_evol_ann_pct

ggplot(comp_infl_mid) + geom_line(aes(x = date, y = expected,colour='Expected'))+geom_line(aes(x = date, y = tx_evol_ann_pct,colour='Real'))+ylab('Inflation rate')+scale_color_manual(name = "Inflation", values = c("Real"='red', "Expected"='blue'))+scale_x_date(limits = c(as.Date('2016-05-01'),as.Date('2020-01-01')))+ggtitle('Comparaison inflation anticipée à m-36 et inflation réalisée')
