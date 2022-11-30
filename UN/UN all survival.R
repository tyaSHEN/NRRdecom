library(tidyverse)
library(readxl)

# can try other periods 
p = c("1950-1955","1955-1960")
#p = c("1975-1980","1980-1985")
#p = c("2005-2010","2010-2015")

yeard = as.numeric(substr(p[2],0,4))-as.numeric(substr(p[1],0,4))

SRB <- read_excel("Data/WPP2019_FERT_F02_SEX_RATIO_AT_BIRTH.xlsx",sheet = "ESTIMATES", range = "C17:U272") %>% 
  filter(Type %in% c("Country/Area","SDG region","World")) %>% select(c(1,6:19)) %>% pivot_longer(2:15,names_to = "Period")
Fertility <- read_excel("Data/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx", sheet = "ESTIMATES", range = "C17:O3587") %>% 
  filter(Type %in% c("Country/Area","SDG region","World")) %>% select(c(1,6:13)) %>% pivot_longer(3:9,names_to = "Age")
Fertility$Age <- substr(Fertility$Age,1,2)
Fertility <- left_join(Fertility,SRB,by = c("Region, subregion, country or area *", "Period"))


Mortility <- read_excel("Data/WPP2019_MORT_F15_3_LIFE_TABLE_SURVIVORS_FEMALE.xlsx", sheet = "ESTIMATES", range = "C17:AD3587") %>% 
  filter(Type %in% c("Country/Area","SDG region","World")) %>% select(c(1,6:28)) %>% pivot_longer(3:24,names_to = "Age")
Mortility$value = as.numeric(Mortility$value)/100000

Repro <- read_excel("Data/WPP2019_FERT_F05_NET_REPRODUCTION_RATE.xlsx", 
                    sheet = "ESTIMATES", range = "C17:U272") %>% 
  filter(Type %in% c("Country/Area","SDG region","World")) %>% select(c(1,6:19)) %>% pivot_longer(2:15,names_to = "Period")

all = c()
for (c in unique(Fertility$`Region, subregion, country or area *`)) {

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
  gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
  nrr = gm_mean(Sums$NRR)
  
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
    
    result <- rbind(result,c(y,c,nrr,d_nrr,d_tfr,d_lx,d_per))
  }
  
  result <- as.data.frame(result)
  colnames(result) <- c("Year","Country","NRR","dNRR","Fertility","Survival","SRB")
  all = rbind(all,result)
  
}

all = all %>% mutate_at(c("NRR","dNRR", "Fertility","Survival","SRB"),as.character) %>%
  mutate_at(c("NRR","dNRR", "Fertility","Survival","SRB"),as.numeric) %>%
  mutate(per = Survival/dNRR*100) %>% mutate(sign= ifelse(Survival <0,"negative","positive"))
all$Country =as.character(all$Country)
all$Country[which(all$Country=="OCEANIA (EXCLUDING AUSTRALIA AND NEW ZEALAND)")] <- "OTHER OCEANIA"
all$Country[which(all$Country=="China, Hong Kong SAR")] <- "China, Hong Kong"
all$Country[which(all$Country=="China, Macao SAR")] <- "China, Macao"

all$Region <- NA
for (i in 1:nrow(all)) {
  if(str_length(all[i,2]) %in% unlist(gregexpr("[A-Z]", all[i,2]))){
    Re=as.character(all[i,2])
  }
  all$Region[i]=Re
}

all$Region <- factor(all$Region,levels = c("WORLD","SUB-SAHARAN AFRICA","NORTHERN AFRICA AND WESTERN ASIA","CENTRAL AND SOUTHERN ASIA","EASTERN AND SOUTH-EASTERN ASIA","LATIN AMERICA AND THE CARIBBEAN","OTHER OCEANIA","AUSTRALIA/NEW ZEALAND","EUROPE AND NORTHERN AMERICA"))
all$Country[which(all$Country=="WORLD")] <- "World"
all=all%>%filter(Country!=Region)%>% group_by(Region) 

all=all%>% arrange(desc(Survival),.by_group=TRUE)
all$Country = factor(all$Country,levels = all$Country)

all$Survival <- ifelse(all$Survival >0.05,Inf,all$Survival)
all$per <- ifelse(all$per < -200,-Inf,all$per)
ggplot(all)+
  geom_point(aes(x=Country,y=Survival,color=sign,size=NRR),shape=1)+
  #scale_y_continuous(limits = c(-200,200))+
  scale_color_manual("Survival",values = c("red","black"),breaks = c("negative","positive"))+
  scale_radius(range = c(1,10))+
  geom_hline(yintercept = 0,color="grey",linetype=2)+
  annotate("segment",x=c(50.5,75.5,89.5,108.5,146.5,159.5),xend=c(50.5,75.5,89.5,108.5,146.5,159.5),y=-Inf,yend = Inf,linetype = 2,color="grey90")+
  theme(panel.background = element_blank(),legend.position = "bottom",axis.text.x =element_text(angle=90,vjust = 0.5))
