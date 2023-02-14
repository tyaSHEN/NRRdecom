library(tidyverse)
library(scales)
library(HMDHFDplus)

### parameters ###
# Enter your HFD username and password
username.hfd = "tianyu.shen@anu.edu.au"
password.hfd = "JCimOL202#"
# Enter your HMD username and password
username.hmd = "tianyu.shen@anu.edu.au"
password.hmd = "JCimOL202#"
### 

# Names<-c("AUT","BLR","BGR","CAN","CHE","CHL","CZE","DEUTE","DEUTNP","DEUTW",
#          "DNK","GBR_NIR","GBR_NP","GBR_SCO","GBRTENW","ESP","EST","FIN",
#          "FRATNP","HRV","HUN","ISL","ITA","JPN","LTU",
#          "NLD","NOR","POL","PRT","RUS","SVK","SVN",
#          "KOR","SWE","TWN","UKR","USA")
# select a country from Names
Country = "USA"

yeard = 5

# Require `birthsTR` & `exposTR` from HFD
# `Births`, ` Deaths_lexis` & `Population` from HMD to be downloaded and stored in the Data folder

# data cleaning ----
# Birth <- read.table("Data/birthsRR.txt", header = TRUE, fill = TRUE, skip = 2)
Birth <- readHFDweb(CNTRY = Country, item = "birthsRR", username =
                      username.hfd, password = password.hfd)%>% select(-OpenInterval)

Pop <- readHFDweb(CNTRY = Country, item = "exposRR", username =
                    username.hfd, password = password.hfd)%>% select(-OpenInterval)
# Pop <- read.table("Data/exposRR.txt", header = TRUE, fill = TRUE, skip = 2)
Birth <- left_join(Birth,Pop)


# Tot.birth = read.table(paste0("Data/Births/",Country,".Births.txt"), header = TRUE, fill = TRUE, skip = 1)
Tot.birth = readHMDweb(CNTRY = Country, item = "Births", username =
                         username.hmd, password = password.hmd)
if(Country == "TWN"){
  Tot.birth = Tot.birth %>% filter(Year >=1949) %>%mutate(Total = as.numeric(as.character(Total)),Female = as.numeric(as.character(Female))) %>%  mutate(F.per = Female/Total)
}else{Tot.birth = Tot.birth %>% mutate(F.per = Female/Total)}


lx = readHMDweb(CNTRY = Country, item = "fltper_1x1", username =
                         username.hmd, password = password.hmd)[,c(1,2,6)]
# lx2 = read.table(paste0("Data/fltper_1x1/",Country,".fltper_1x1.txt"), header = TRUE, fill = TRUE, skip = 1)[,c(1,2,6)]
lx$Age = gsub("[+]", "", lx$Age)
lx$Age = as.numeric(lx$Age)

Birth.C = Birth # %>% filter(Code == Country)

# TFR & NRR
Birth.C = inner_join(Birth.C,Tot.birth[,c(1,5)])
Birth.C = Birth.C %>% mutate(Female = Total * F.per)

Birth.C = Birth.C %>% mutate(ASFR = Total/Exposure)
Birth.C = inner_join(Birth.C,lx)
Birth.C = Birth.C %>% mutate(lx = lx/100000) %>% mutate(ASNRR = ASFR * lx * F.per)

NRR <- Birth.C %>%group_by(Year) %>% summarise(sum(ASNRR))

Sums = Birth.C %>% group_by(Year) %>% summarise(TFR = sum(ASFR), NRR = sum(ASNRR))

ggplot(Sums %>% pivot_longer(c(2,3)))+
  geom_line(aes(x=Year,y=value,color=name))+
  scale_color_manual("",values = c("orange","green4"))+
  theme(panel.background = element_rect("transparent"),legend.position = "bottom",axis.line.x = element_line(),
        panel.grid.major.y = element_line(colour = "grey90",linetype=1),axis.ticks.y =element_blank(),plot.margin = unit(c(0,1,0,0),"cm"))+
  scale_y_continuous("TFR & NRR",breaks = seq(-16,16,1),limits = c(0,NA))+
  ggtitle(Country)

# main decomposition ----
result <- c()
for (y in rev(unique(Birth.C$Year)[-(1:yeard)])) {
  r_nrr <- log(NRR$`sum(ASNRR)`[which(NRR$Year==y)]/NRR$`sum(ASNRR)`[which(NRR$Year==(y-yeard))])/yeard
  d_nrr <- sqrt(NRR$`sum(ASNRR)`[which(NRR$Year==y)]*NRR$`sum(ASNRR)`[which(NRR$Year==(y-yeard))])*r_nrr
  
  r_tfr <- log(Birth.C$ASFR[which(Birth.C$Year==y)]/Birth.C$ASFR[which(Birth.C$Year==(y-yeard))])/yeard
  r_tfr[is.na(r_tfr)] <- 0
  r_tfr[is.nan(r_tfr)] <- 0
  r_tfr[is.infinite(r_tfr)] <- 0
  r_lx <- log(Birth.C$lx[which(Birth.C$Year==y)]/Birth.C$lx[which(Birth.C$Year==(y-yeard))])/yeard
  r_per <- log(Birth.C$F.per[which(Birth.C$Year==y)]/Birth.C$F.per[which(Birth.C$Year==(y-yeard))])/yeard
  
  m_tfr <- sqrt(Birth.C$ASFR[which(Birth.C$Year==y)]*Birth.C$ASFR[which(Birth.C$Year==(y-yeard))])
  m_lx <- sqrt(Birth.C$lx[which(Birth.C$Year==y)]*Birth.C$lx[which(Birth.C$Year==(y-yeard))])
  m_per <- sqrt(Birth.C$F.per[which(Birth.C$Year==y)]*Birth.C$F.per[which(Birth.C$Year==(y-yeard))])
  
  d_tfr <- sum(m_tfr*m_lx*m_per*r_tfr)
  d_lx <- sum(m_tfr*m_lx*m_per*r_lx)
  d_per <- d_nrr-d_tfr-d_lx
  
  result <- rbind(result,c(y,d_nrr,d_tfr,d_lx,d_per))
}
result <- as.data.frame(result)
colnames(result) <- c("Year","NRR","Fertility","Survival","SRB")


data = result
data = data %>% pivot_longer(c(2:5))
data$name <- factor(data$name, levels = c("NRR","SRB","Fertility","Survival"))

ggplot() +
  geom_bar(data = subset(data, !(name %in%  c("NRR"))), stat = "identity",position = "stack", aes(x = Year, y = value, fill = name)) + 
  geom_line(data = subset(data, name == "NRR"), aes(x = Year, y = value,linetype=name)) +
  geom_point(data = subset(data, name == "NRR"), aes(x = Year, y = value,shape=name), colour = 'black', size = 1)+
  guides(fill = guide_legend(reverse = T))+
  scale_linetype_manual("",values=1,label=c("NRR"))+
  scale_shape_manual("",values=19,label=c("NRR"))+
  scale_fill_manual("", values = rev(c("#ffa53d","#3d87ff","#c9c5c5")),label = rev(c("Survival","Fertiltiy","Percentage Sex at Birth")))+
  labs(y = "Change in NRR", x = "Year") +
  ggtitle(Country)+
  theme(panel.background = element_rect("transparent"),plot.title = element_text(hjust = 0.5),legend.position = "bottom",axis.line = element_line())+
  scale_y_continuous(breaks = seq(-2,2,0.02))+
  scale_x_continuous(breaks = seq(1800,2020,10))

# age decomposition ----
# survival by age
agec <- c()
for (y in rev(unique(Birth.C$Year)[-(1:yeard)])) {
  m_nrr <- sqrt(Birth.C$ASNRR[which(Birth.C$Year==y)]*Birth.C$ASNRR[which(Birth.C$Year==(y-yeard))])
  m_nrr <- append(rep(0,12),m_nrr)
  tem <- lx %>% filter(Year %in% c(y,y-yeard)) %>% filter(Age <=55) %>% mutate(lx=lx/100000)
  tem <- tem %>% group_by(Year) %>%  mutate(px = lx/lag(lx))
  tem <- tem %>% group_by(Age) %>% mutate(r_px = log(last(px)/first(px))/yeard) %>% filter(Year==y)
  tem <- cbind(tem,m_nrr)
  colnames(tem)[6] <- "m_nrr"
  tem <- tem %>% ungroup() %>% mutate(SUM = rev(cumsum(rev(m_nrr)))) %>% mutate(rx=lead(r_px*SUM))
  tem$rx[is.na(tem$rx)] <- 0
  agec <- rbind(agec,tem)
}
agec %>% group_by(Year) %>% summarise(sum(rx))

MAX = agec$Year[which(agec$rx==max(agec$rx))]
MIN = agec$Year[which(agec$rx==min(agec$rx))]
text = data.frame(Age=c(agec$Age[which(agec$rx==min(agec$rx))],agec$Age[which(agec$rx==max(agec$rx))],7,45),
                  rx=c(min(agec$rx)-(max(agec$rx)/20),max(agec$rx)+(max(agec$rx)/20),agec$rx[which(agec$Age==5&agec$Year==min(agec$Year))]+2*(max(agec$rx)/20),agec$rx[which(agec$Age==0&agec$Year==max(agec$Year))]+2*(max(agec$rx)/20)),
                  label=c(MIN,MAX,min(agec$Year),max(agec$Year)),
                  color=c("Year with lowest value","Year with highest value","First year","Last year"))
text = data.frame(Age=c(agec$Age[which(agec$rx==min(agec$rx))],agec$Age[which(agec$rx==max(agec$rx))],7,45),
                  rx=c(min(agec$rx)-0.001,max(agec$rx)+0.001,agec$rx[which(agec$Age==5&agec$Year==min(agec$Year))]+0.001,agec$rx[which(agec$Age==0&agec$Year==max(agec$Year))]+0.001),
                  label=c(MIN,MAX,min(agec$Year),max(agec$Year)),
                  color=c("Year with lowest value","Year with highest value","First year","Last year"))
agec <- agec %>% mutate(sel = ifelse(Year ==MIN,"X",ifelse(Year == MAX,"Y",ifelse(Year==min(Year),"A",ifelse(Year==max(Year),"Z","N")))))
agec$sel <- factor(agec$sel,levels = c("A","X","Y","Z","N"))
colors <- c("Other years" = "grey70", "Year with highest value" = "darkorange", "Year with lowest value" = "green4","First year"="red","Last year"="blue")
ggplot()+
  geom_line(data=agec %>% filter(sel == "N"),aes(x=Age,y=rx,group=Year,color = "Other years"),alpha=0.8)+
  geom_line(data=agec %>% filter(sel == "X"),aes(x=Age,y=rx,group=Year,color = "Year with lowest value"),size=1.03)+
  geom_line(data=agec %>% filter(sel == "Y"),aes(x=Age,y=rx,group=Year,color = "Year with highest value"),size=1.03)+
  geom_line(data=agec %>% filter(sel == "A"),aes(x=Age,y=rx,group=Year,color = "First year"),size=1.03)+
  geom_line(data=agec %>% filter(sel == "Z"),aes(x=Age,y=rx,group=Year,color = "Last year"),size=1.03)+
  theme(panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5))+
  geom_text(data=text, aes(x=Age,y=rx,label=label,color=color),show.legend = F)+
  scale_x_continuous(breaks = breaks_pretty(n = 4))+
  scale_color_manual(values = colors)+
  labs(y="Change in NRR")+
  ggtitle(Country)+
  geom_hline(yintercept = 0,linetype=2,color="grey50")

# fertiltiy by age
agef = c()
for (y in rev(unique(Birth.C$Year)[-(1:yeard)])) {
  r_tfr <- log(Birth.C$ASFR[which(Birth.C$Year==y)]/Birth.C$ASFR[which(Birth.C$Year==(y-yeard))])/yeard
  r_tfr[is.na(r_tfr)] <- 0
  r_tfr[is.nan(r_tfr)] <- 0
  r_tfr[is.infinite(r_tfr)] <- 0
  
  m_tfr <- sqrt(Birth.C$ASFR[which(Birth.C$Year==y)]*Birth.C$ASFR[which(Birth.C$Year==(y-yeard))])
  m_lx <- sqrt(Birth.C$lx[which(Birth.C$Year==y)]*Birth.C$lx[which(Birth.C$Year==(y-yeard))])
  m_per <- sqrt(Birth.C$F.per[which(Birth.C$Year==y)]*Birth.C$F.per[which(Birth.C$Year==(y-yeard))])
  
  df = data.frame(age=12:55,year=y,rxf = m_tfr*m_lx*m_per*r_tfr)
  agef = rbind(agef,df)
}


ggplot(agef)+
  geom_line(aes(x=age,y=rxf,color=year,group=year))+
  scale_color_gradient(low = "yellow", high = "red", na.value = NA)+
  labs(y="Change in NRR")+
  ggtitle(Country)+
  theme(panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0,linetype=2,color="grey50")
