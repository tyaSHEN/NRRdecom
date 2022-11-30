library(tidyverse)
library(readxl)


# name of the country
c="Zimbabwe"


p = c("1980-1985","1985-1990","1990-1995","1995-2000","2000-2005","2005-2010","2010-2015","2015-2020")
yeard = 5

# data cleaning ----
SRB <- read_excel("Data/WPP2019_FERT_F02_SEX_RATIO_AT_BIRTH.xlsx",sheet = "ESTIMATES", range = "C17:U272") %>% 
  filter(Type == "Country/Area") %>% select(c(1,6:19)) %>% pivot_longer(2:15,names_to = "Period")
Fertility <- read_excel("Data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx", sheet = "ESTIMATES", range = "C17:O3587") %>% 
  filter(Type == "Country/Area") %>% select(c(1,6:13)) %>% pivot_longer(3:9,names_to = "Age")
Fertility$Age <- substr(Fertility$Age,1,2)
Fertility <- left_join(Fertility,SRB,by = c("Region, subregion, country or area *", "Period"))

Mortility <- read_excel("Data/WPP2019_MORT_F15_3_LIFE_TABLE_SURVIVORS_FEMALE.xlsx", sheet = "ESTIMATES", range = "C17:AD3587") %>% 
  filter(Type == "Country/Area") %>% select(c(1,6:28)) %>% pivot_longer(3:24,names_to = "Age")
Mortility$value = as.numeric(Mortility$value)/100000

Repro <- read_excel("Data/WPP2019_FERT_F05_NET_REPRODUCTION_RATE.xlsx", 
                    sheet = "ESTIMATES", range = "C17:U272") %>% 
  filter(Type == "Country/Area") %>% select(c(1,6:19)) %>% pivot_longer(2:15,names_to = "Period")


# TFR & NRR ----
B <- left_join(Fertility,Mortility,by = c("Region, subregion, country or area *", "Period", "Age"))

B = B %>% filter(`Region, subregion, country or area *`==c,Period %in% p)
colnames(B) <- c("Country","Year","Age","ASFR","SRB","lx")
B = B %>% mutate(ASFR= as.numeric(ASFR)*5/1000) %>% mutate(F.per=(1-as.numeric(SRB)/2.05)) %>% mutate(ASNRR = ASFR*lx*F.per)
B$Year <- substr(B$Year,1,4)
B$Year <- as.numeric(B$Year)
NRR <- Repro %>% filter(`Region, subregion, country or area *`==c,Period %in% p)
NRR$Period <- substr(NRR$Period,1,4)
NRR$Period <- as.numeric(NRR$Period)
NRR$value <- as.numeric(NRR$value)
Sums = B %>% group_by(Year) %>% summarise(TFR = sum(ASFR),NRR = sum(ASNRR))
Sums$NRR = as.numeric(NRR$value)

ggplot(Sums %>% pivot_longer(c(2,3)))+
  geom_line(aes(x=Year,y=value,color=name))+
  scale_color_manual("",values = c("orange","green4"))+
  theme(panel.background = element_rect("transparent"),legend.position = "bottom",axis.line.x = element_line(),
        panel.grid.major.y = element_line(colour = "grey90",linetype=1),axis.ticks.y =element_blank() )+
  scale_y_continuous("",breaks = seq(-16,16,0.5),limits = c(0,(as.integer(max(Sums$TFR)/0.5)+1)*0.5))+
  ggtitle(c)+
  scale_x_continuous(breaks = seq(1990,2015,5),labels = c("1990-1995","1995-2000","2000-2005","2005-2010","2010-2015","2015-2020"))


# main decomposition ----
result <- c()
for (y in rev(Sums$Year[-1])) {
  r_nrr <- log(NRR$value[which(NRR$Period==y)]/NRR$value[which(NRR$Period==(y-yeard))])/yeard
  d_nrr <- sqrt(NRR$value[which(NRR$Period==y)]*NRR$value[which(NRR$Period==(y-yeard))])*r_nrr
  
  r_tfr <- log(B$ASFR[which(B$Year==y)]/B$ASFR[which(B$Year==(y-yeard))])/yeard
  r_tfr[is.na(r_tfr)] <- 0
  r_tfr[is.nan(r_tfr)] <- 0
  r_tfr[is.infinite(r_tfr)] <- 0
  r_lx <- log(B$lx[which(B$Year==y)]/B$lx[which(B$Year==(y-yeard))])/yeard
  r_per <- log(B$F.per[which(B$Year==y)]/B$F.per[which(B$Year==(y-yeard))])/yeard
  
  m_tfr <- sqrt(B$ASFR[which(B$Year==y)]*B$ASFR[which(B$Year==(y-yeard))])
  m_lx <- sqrt(B$lx[which(B$Year==y)]*B$lx[which(B$Year==(y-yeard))])
  m_per <- sqrt(B$F.per[which(B$Year==y)]*B$F.per[which(B$Year==(y-yeard))])
  
  d_tfr <- sum(m_tfr*m_lx*m_per*r_tfr)
  d_lx <- sum(m_tfr*m_lx*m_per*r_lx)
  d_per <- d_nrr-d_tfr-d_lx
  
  result <- rbind(result,c(y,d_nrr,d_tfr,d_lx,d_per))
}

result <- as.data.frame(result)
colnames(result) <- c("Year","NRR","Fertility","Survival","SRB")
#write_csv(result,paste0("result/",c,".csv"))
data = result
data = data %>% pivot_longer(c(2:5))
data$name <- factor(data$name, levels = c("NRR","SRB","Fertility","Survival"))


ggplot() +
  geom_bar(data = subset(data, !(name %in%  c("NRR"))), stat = "identity",position = "stack", aes(x = Year, y = value, fill = name)) + 
  geom_line(data = subset(data, name == "NRR"), aes(x = Year, y = value,linetype=name)) +
  geom_point(data = subset(data, name == "NRR"), aes(x = Year, y = value,shape=name), colour = 'black', size = 1)+
  guides(fill = guide_legend(reverse = T))+
  scale_linetype_manual("",values=1,label="NRR")+
  scale_shape_manual("",values=19,label="NRR")+
  scale_fill_manual("", values = c("#c9c5c5","#3d87ff","#ffa53d"),label = c("Percentage Sex at Birth","TFR","Survival"))+
  #color for res:"#6c7187"
  labs(y = "Change in NRR", x = "Year") +
  ggtitle(c)+
  geom_hline(yintercept = 0,linetype =2)+
  theme(panel.background = element_rect("transparent"),legend.position = "bottom",axis.line = element_line())+
  scale_y_continuous(breaks = seq(-16,16,0.02))+
  scale_x_continuous(breaks = seq(1995,2015,5),labels = c("1995-2000","2000-2005","2005-2010","2010-2015","2015-2020"))


# age decomposition ----
# survival by age
  lx <- Mortility %>% filter(`Region, subregion, country or area *`==c,Period %in% p)
  colnames(lx) <- c("Country","Year","Age","lx")
  lx$Year <- substr(lx$Year,1,4)
  lx$Year<- as.numeric(lx$Year)
  lx$Age <- as.numeric(lx$Age)
  
  agec <- c()
  for (y in rev(Sums$Year[-1])) {
    m_nrr <- sqrt(B$ASNRR[which(B$Year==y)]*B$ASNRR[which(B$Year==(y-yeard))])
    m_nrr <- append(rep(0,4),m_nrr)
    
    tem <- lx %>% filter(Year %in% c(y,y-yeard)) %>% filter(Age <=45)
    tem <- tem %>% group_by(Year) %>%  mutate(px = lx/lag(lx))
    tem <- tem %>% group_by(Age) %>% mutate(r_px = log(last(px)/first(px))/yeard) %>% filter(Year==y)
    tem <- cbind(tem,m_nrr)
    colnames(tem)[7] <- "m_nrr"
    tem <- tem %>% ungroup() %>% mutate(SUM = rev(cumsum(rev(m_nrr)))) %>% mutate(rx=lead(r_px*SUM))
    tem$rx[is.na(tem$rx)] <- 0
    agec <- rbind(agec,tem)
  }

  ggplot()+
    geom_line(data=agec,aes(x=Age,y=rx,group=Year,color=as.character(Year)))+
    theme(panel.background = element_blank(),axis.line = element_line())+
    labs(y="Change in NRR",color="Year")+
    geom_hline(yintercept = 0,linetype=2,color="grey50")+
    ggtitle(c)+
    scale_y_continuous(breaks = seq(-2,2,0.002))

  
  # fertiltiy by age  
  agef <- c()
  for (y in rev(Sums$Year[-(1:(yeard/5))])) {
    r_tfr <- log(B$ASFR[which(B$Year==y)]/B$ASFR[which(B$Year==(y-yeard))])/yeard
    r_tfr[is.na(r_tfr)] <- 0
    r_tfr[is.nan(r_tfr)] <- 0
    r_tfr[is.infinite(r_tfr)] <- 0
    
    m_tfr <- sqrt(B$ASFR[which(B$Year==y)]*B$ASFR[which(B$Year==(y-yeard))])
    m_lx <- sqrt(B$lx[which(B$Year==y)]*B$lx[which(B$Year==(y-yeard))])
    m_per <- sqrt(B$F.per[which(B$Year==y)]*B$F.per[which(B$Year==(y-yeard))])
    
    df = data.frame(age=seq(15,45,5),Year=y,rxf = m_tfr*m_lx*m_per*r_tfr)
    agef = rbind(agef,df)

  }
  
  ggplot()+
    geom_line(data=agef,aes(x=age,y=rxf,group=Year,color=as.character(Year)))+
    theme(panel.background = element_blank(),axis.line = element_line())+
    labs(y="Change in NRR",color="Year")+
    geom_hline(yintercept = 0,linetype=2,color="grey50")+
    ggtitle(c)+
    scale_y_continuous(breaks = seq(-2,2,0.002))
