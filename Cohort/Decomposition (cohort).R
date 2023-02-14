library(tidyverse)
library(scales)
library(HMDHFDplus)

### parameters ###
# Enter your HFD username and password
username.hfd = ""
password.hfd = ""
# Enter your HMD username and password
username.hmd = ""
password.hmd = ""
### 

# Names<-c("AUT","BLR","BGR","CAN","CHE","CHL","CZE","DEUTE","DEUTNP","DEUTW",
#          "DNK","GBR_NIR","GBR_NP","GBR_SCO","GBRTENW","ESP","EST","FIN",
#          "FRATNP","HRV","HUN","ISL","ITA","JPN","LTU",
#          "NLD","NOR","POL","PRT","RUS","SVK","SVN",
#          "KOR","SWE","TWN","UKR","USA")
# select a country from Names
Country = "ESP"

yeard = 5

# Require `birthsTR` & `exposTR` from HFD
# `Births`, ` Deaths_lexis` & `Population` from HMD to be downloaded and stored in the Data folder

# data cleaning ----
Birth <- readHFDweb(CNTRY = Country, item = "birthsTR", username =
                      username.hfd, password = password.hfd)%>% select(-OpenInterval)

# Birth <- read.table("Data/birthsTR.txt", header = TRUE, fill = TRUE, skip = 2) %>% filter(Code == Country) 
Birth[which(Birth$Age == 12),"Cohort"] <- Birth[which(Birth$Age == 12)+1,"Cohort"]
Birth[which(Birth$Age == 55),"Cohort"] <- Birth[which(Birth$Age == 55)-1,"Cohort"]
# Birth$Age = gsub("[+]", "", Birth$Age)
# Birth$Age = gsub("[-]", "", Birth$Age)
# Birth$Age <- as.numeric(Birth$Age)

Tot.birth = readHMDweb(CNTRY = Country, item = "Births", username =
                   username.hmd, password = password.hmd)
# Tot.birth = read.table(paste0("Data/Births/",Country,".Births.txt"), header = TRUE, fill = TRUE, skip = 1)
if(Country == "TWN"){
  Tot.birth = Tot.birth %>% filter(Year >=1949) %>% mutate(Total = as.numeric(as.character(Total)),Female = as.numeric(as.character(Female))) %>%  mutate(F.per = Female/Total)
}else{Tot.birth = Tot.birth %>% mutate(F.per = Female/Total)}

Birth <- left_join(Birth, Tot.birth[,c(1,5)])
Birth$Birthf <- Birth$Total*Birth$F.per

Birtht<-Birth %>% group_by(Age,Cohort) %>% summarise(Birtht = sum(Total))
Birtht$Cohort <- as.numeric(as.character(Birtht$Cohort))

Birthf <- Birth %>% group_by(Age,Cohort) %>% summarise(Birthf = sum(Birthf))
Birthf$Cohort <- as.numeric(as.character(Birthf$Cohort))

Pop <- readHFDweb(CNTRY = Country, item = "exposTR", username =
                      username.hfd, password = password.hfd)%>% select(-OpenInterval)

# Pop2 <- read.table("Data/exposTR.txt", header = TRUE, fill = TRUE, skip = 2) %>% filter(Code == Country) %>% select(-Code)
Pop <- Pop %>% group_by(Age,Cohort) %>% summarise(Exposure = sum(Exposure))

Birth.C <- left_join(Birtht,Birthf)
Birth.C <- left_join(Birth.C,Pop)

Death = readHMDweb(CNTRY = Country, item = "Deaths_lexis", username =
                     username.hmd, password = password.hmd)[,c(1,2,3,4)]
# Death = read.table(paste0("Data/Deaths_lexis/",Country,".Deaths_lexis.txt"), header = TRUE, fill = TRUE, skip = 1)[,c(1,2,3,4)]
Death$Age <- as.numeric(as.character(Death$Age))
Death$Cohort <- as.numeric(as.character(Death$Cohort))


Pop = readHMDweb(CNTRY = Country, item = "Population", username =
                     username.hmd, password = password.hmd)[,c(1,2,4)]
# Pop2 = read.table(paste0("Data/Population/",Country,".Population.txt"), header = TRUE, fill = TRUE, skip = 1)[,c(1,2,3)]
colnames(Pop)[3] = "Female"
Pop$Year <- Pop$Year-1
Exposure = Death %>% group_by(Year,Age) %>% slice(1) %>% left_join(Pop,by = c("Year", "Age")) %>%ungroup()
Exposure$Exposure <- Exposure$Female.x+Exposure$Female.y
Exposure <- Exposure %>% select(-Year,-Female.x,-Female.y)

Death <- Death %>% group_by(Age,Cohort) %>% summarise(Death = sum(Female))

qx = left_join(Death,Exposure)
qx$qx= qx$Death/qx$Exposure
qx <- qx%>%filter(!is.na(qx))
qx <- qx%>%filter(!is.na(Age))


y=max(sort(unique(qx$Cohort))[-c(1:109)][1],sort(unique(Birth.C$Cohort))[-c(1:43)][1])
B <- Birth.C %>% filter(Cohort >= y)
qx_l <- qx %>% filter(Cohort >= y,Age <=55)


cutoff=as.numeric(names(which(sapply(split(B,B$Cohort), nrow) == 38)))+0.5
if(length(cutoff)<1){warning("please select another country")}
B= B%>% mutate(ASFR = Birtht/Exposure,F.per=Birthf/Birtht)

qx_l$Age <- as.numeric(qx_l$Age)
qx_l <- qx_l %>% group_by(Cohort) %>% arrange(Age) %>% mutate(lx = cumprod(1-qx))
qx_l$Cohort <- as.numeric(qx_l$Cohort)
qx_l$Age <- qx_l$Age+1

# TFR & NRR ----
B.tfr <- B %>% filter(Cohort < cutoff)

B.tfr = inner_join(B.tfr,qx_l[,c(1,2,6)])
B.tfr = B.tfr %>% mutate(ASMR = ASFR*F.per) %>% mutate(ASNRR = ASMR * lx, Am = (Age+0.5)*ASMR)
Sums = B.tfr %>% group_by(Cohort) %>% summarise(TFR = sum(ASFR,na.rm = T), NRR = sum(ASNRR,na.rm = T))

ggplot(Sums %>% pivot_longer(c(2,3)))+
  geom_line(aes(x=Cohort,y=value,color=name))+
  scale_color_manual("",values = c("orange","green4"))+
  theme(panel.background = element_rect("transparent"),legend.position = "bottom",axis.line.x = element_line(),
        panel.grid.major.y = element_line(colour = "grey90",linetype=1),axis.ticks.y =element_blank(),plot.margin = unit(c(0,1,0,0),"cm"))+
  scale_y_continuous("TFR & NRR",breaks = seq(-16,16,1),limits = c(0,NA))+
  ggtitle(Country)

# main decomposition ----
result <- c()
#assume cohort fertility complete at age 49, to look at incomplete cohort adjust 49 below (to eg 30)
cut = 49

for (coh in rev(unique(B$Cohort))[-c((1:(cut-13)),(length(unique(B$Cohort)):(length(unique(B$Cohort))-yeard+1)))]) {
  Age.range = B %>% filter(Cohort==coh) %>% pull(Age)
  B.sel <- rbind(B %>% filter(Cohort==coh), B %>% filter(Cohort == coh-yeard, Age %in% Age.range))
  B.sel[is.na(B.sel$F.per),7] <- 0
  B.sel = inner_join(B.sel,qx_l[,c(1,2,6)])
  B.sel = B.sel %>% mutate(ASNRR = ASFR * lx * F.per)
  NRR = B.sel %>% group_by(Cohort) %>% summarise(NRR = sum(ASNRR))
  if(TRUE %in% is.na(NRR$NRR)){break()}
  ####
  
  y = (NRR$Cohort)[-1]
  r_nrr <- log(NRR$NRR[which(NRR$Cohort==y)]/NRR$NRR[which(NRR$Cohort==(y-yeard))])/yeard
  d_nrr <- sqrt(NRR$NRR[which(NRR$Cohort==y)]*NRR$NRR[which(NRR$Cohort==(y-yeard))])*r_nrr
  
  r_tfr <- log(B.sel$ASFR[which(B.sel$Cohort==y)]/B.sel$ASFR[which(B.sel$Cohort==(y-yeard))])/yeard
  r_tfr[is.na(r_tfr)] <- 0
  r_tfr[is.nan(r_tfr)] <- 0
  r_tfr[is.infinite(r_tfr)] <- 0
  r_lx <- log(B.sel$lx[which(B.sel$Cohort==y)]/B.sel$lx[which(B.sel$Cohort==(y-yeard))])/yeard
  r_per <- log(B.sel$F.per[which(B.sel$Cohort==y)]/B.sel$F.per[which(B.sel$Cohort==(y-yeard))])/yeard
  r_per[is.na(r_per)] <- 0
  r_per[is.nan(r_per)] <- 0
  r_per[is.infinite(r_per)] <- 0
  
  m_tfr <- sqrt(B.sel$ASFR[which(B.sel$Cohort==y)]*B.sel$ASFR[which(B.sel$Cohort==(y-yeard))])
  m_lx <- sqrt(B.sel$lx[which(B.sel$Cohort==y)]*B.sel$lx[which(B.sel$Cohort==(y-yeard))])
  m_per <- sqrt(B.sel$F.per[which(B.sel$Cohort==y)]*B.sel$F.per[which(B.sel$Cohort==(y-yeard))])
  
  d_tfr <- sum(m_tfr*m_lx*m_per*r_tfr)
  d_lx <- sum(m_tfr*m_lx*m_per*r_lx)
  d_per <- d_nrr-d_tfr-d_lx
  
  result <- rbind(result,c(y,d_nrr,d_tfr,d_lx,d_per))
}
result <- as.data.frame(result)
if(nrow(result)==0){next()}
colnames(result) <- c("Cohort","NRR","Fertility","Survival","SRB")

data = result
data = data %>% pivot_longer(c(2:5))
data$name <- factor(data$name, levels = c("NRR","SRB","Fertility","Survival"))


ggplot() +
  geom_bar(data = subset(data, !(name %in%  c("NRR"))), stat = "identity",position = "stack", aes(x = Cohort, y = value, fill = name)) + 
  geom_line(data = subset(data, name == "NRR"), aes(x = Cohort, y = value,linetype=name)) +
  geom_point(data = subset(data, name == "NRR"), aes(x = Cohort, y = value,shape=name), colour = 'black', size = 1)+
  guides(fill = guide_legend(reverse = T))+
  scale_linetype_manual("",values=1,label=c("NRR"))+
  scale_shape_manual("",values=19,label=c("NRR"))+
  scale_fill_manual("", values = c("#c9c5c5","#3d87ff","#ffa53d"),label = c("Percentage Sex at Birth","Fertility","Survival"))+
  #color for res:"#6c7187" "Fertility Age Structure",
  labs(y = "Change in NRR", x = "Cohort") +
  ggtitle(Country)+
  theme(panel.background = element_rect("transparent"),plot.title = element_text(hjust = 0.5),legend.position = "bottom",axis.line = element_line())+
  scale_y_continuous(breaks = seq(-2,2,0.01))+
  scale_x_continuous(breaks = seq(1800,2020,5))+
  theme(plot.margin = margin(r=10))

# age decomposition ----
# survival by age
agec <- c()
for (coh in rev(unique(B$Cohort))[-c((1:(cut-13)),(length(unique(B$Cohort)):(length(unique(B$Cohort))-yeard+1)))]) {
  Age.range = B %>% filter(Cohort==coh) %>% pull(Age)
  B.sel <- rbind(B %>% filter(Cohort==coh), B %>% filter(Cohort == coh-yeard, Age %in% Age.range))
  B.sel[is.na(B.sel$F.per),7] <- 0
  B.sel = inner_join(B.sel,qx_l[,c(1,2,6)])
  B.sel = B.sel %>% mutate(ASNRR = ASFR * lx * F.per)
  NRR = B.sel %>% group_by(Cohort) %>% summarise(NRR = sum(ASNRR))
  if(TRUE %in% is.na(NRR$NRR)){break()}
  m_nrr <- sqrt(B.sel$ASNRR[which(B.sel$Cohort==coh)]*B.sel$ASNRR[which(B.sel$Cohort==(coh-yeard))])
  m_nrr <- append(rep(0,12),m_nrr)
  
  tem <- qx_l %>% filter(Cohort %in% c(coh,coh-yeard)) %>% filter(Age < length(m_nrr))
  tem[nrow(tem) + 1,] <- list(0,coh,NA,NA,NA,1)
  tem[nrow(tem) + 1,] <- list(0,coh-yeard,NA,NA,NA,1)
  tem <- tem %>% arrange(Age) %>% group_by(Cohort) %>%  mutate(px = lx/lag(lx))
  tem <- tem %>% group_by(Age) %>% mutate(r_px = log(last(px)/first(px))/yeard) %>% filter(Cohort==coh)
  
  tem <- cbind(tem,m_nrr)
  colnames(tem)[9] <- "m_nrr"
  tem <- tem %>% ungroup() %>% mutate(SUM = rev(cumsum(rev(m_nrr)))) %>% mutate(rx=lead(r_px*SUM))
  tem$rx[is.na(tem$rx)] <- 0
  agec <- rbind(agec,tem)
}
agec %>% group_by(Cohort) %>% summarise(sum(rx))
MAX = agec$Cohort[which(agec$rx==max(agec$rx))]
MIN = agec$Cohort[which(agec$rx==min(agec$rx))]
text = data.frame(Age=c(agec$Age[which(agec$rx==min(agec$rx))],agec$Age[which(agec$rx==max(agec$rx))],7,40),
                  rx=c(min(agec$rx)-(max(agec$rx)/20),max(agec$rx)+(max(agec$rx)/20),agec$rx[which(agec$Age==5&agec$Cohort==min(agec$Cohort))]+2*(max(agec$rx)/20),agec$rx[which(agec$Age==0&agec$Cohort==max(agec$Cohort))]+2*(max(agec$rx)/20)),
                  label=c(MIN,MAX,min(agec$Cohort),max(agec$Cohort)),
                  color=c("Cohort with lowest value","Cohort with highest value","First cohort","Last cohort"))
text = data.frame(Age=c(agec$Age[which(agec$rx==min(agec$rx))],agec$Age[which(agec$rx==max(agec$rx))],7,40),
                  rx=c(min(agec$rx)-0.002,max(agec$rx)+0.002,agec$rx[which(agec$Age==5&agec$Cohort==min(agec$Cohort))]+0.002,agec$rx[which(agec$Age==0&agec$Cohort==max(agec$Cohort))]+0.002),
                  label=c(MIN,MAX,min(agec$Cohort),max(agec$Cohort)),
                  color=c("Cohort with lowest value","Cohort with highest value","First cohort","Last cohort"))
agec <- agec %>% mutate(sel = ifelse(Cohort ==MIN,"X",ifelse(Cohort == MAX,"Y",ifelse(Cohort==min(Cohort),"A",ifelse(Cohort==max(Cohort),"Z","N")))))
agec$sel <- factor(agec$sel,levels = c("A","X","Y","Z","N"))

colors <- c("Other cohorts" = "grey70", "Cohort with highest value" = "darkorange", "Cohort with lowest value" = "green4","First cohort"="red","Last cohort"="blue")
ggplot()+
  geom_line(data=agec %>% filter(sel == "N"),aes(x=Age,y=rx,group=Cohort,color = "Other cohorts"),alpha=0.8)+
  geom_line(data=agec %>% filter(sel == "X"),aes(x=Age,y=rx,group=Cohort,color = "Cohort with lowest value"),size=1.03)+
  geom_line(data=agec %>% filter(sel == "Y"),aes(x=Age,y=rx,group=Cohort,color = "Cohort with highest value"),size=1.03)+
  geom_line(data=agec %>% filter(sel == "A"),aes(x=Age,y=rx,group=Cohort,color = "First cohort"),size=1.03)+
  geom_line(data=agec %>% filter(sel == "Z"),aes(x=Age,y=rx,group=Cohort,color = "Last cohort"),size=1.03)+
  theme(panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5))+
  geom_text(data=text, aes(x=Age,y=rx,label=label,color=color),show.legend = F)+
  scale_x_continuous(breaks = breaks_pretty(n = 4))+
  scale_color_manual(values = colors)+
  labs(y="Change in NRR")+
  ggtitle(Country)+
  geom_hline(yintercept = 0,linetype=2,color="grey50")

# fertiltiy by age
agef=c()
for (coh in rev(unique(B$Cohort))[-c((1:(cut-13)),(length(unique(B$Cohort)):(length(unique(B$Cohort))-yeard+1)))]) {
  Age.range = B %>% filter(Cohort==coh) %>% pull(Age)
  B.sel <- rbind(B %>% filter(Cohort==coh), B %>% filter(Cohort == coh-yeard, Age %in% Age.range))
  B.sel[is.na(B.sel$F.per),7] <- 0
  B.sel = inner_join(B.sel,qx_l[,c(1,2,6)])
  B.sel = B.sel %>% mutate(ASNRR = ASFR * lx * F.per)
  NRR = B.sel %>% group_by(Cohort) %>% summarise(NRR = sum(ASNRR))
  if(TRUE %in% is.na(NRR$NRR)){break()}
  y = (NRR$Cohort)[-1]
  
  r_tfr <- log(B.sel$ASFR[which(B.sel$Cohort==y)]/B.sel$ASFR[which(B.sel$Cohort==(y-yeard))])/yeard
  r_tfr[is.na(r_tfr)] <- 0
  r_tfr[is.nan(r_tfr)] <- 0
  r_tfr[is.infinite(r_tfr)] <- 0
  
  m_tfr <- sqrt(B.sel$ASFR[which(B.sel$Cohort==y)]*B.sel$ASFR[which(B.sel$Cohort==(y-yeard))])
  m_lx <- sqrt(B.sel$lx[which(B.sel$Cohort==y)]*B.sel$lx[which(B.sel$Cohort==(y-yeard))])
  m_per <- sqrt(B.sel$F.per[which(B.sel$Cohort==y)]*B.sel$F.per[which(B.sel$Cohort==(y-yeard))])
  
  df = data.frame(age=12:(11+length(r_tfr)),cohort=y,rxf = m_tfr*m_lx*m_per*r_tfr)
  agef = rbind(agef,df)
}
ggplot(agef)+
  geom_line(aes(x=age,y=rxf,color=cohort,group=cohort))+
  scale_color_gradient(low = "yellow", high = "red", na.value = NA)+
  labs(y="Change in NRR")+
  ggtitle(Country)+
  theme(panel.background = element_blank(),axis.line = element_line(),plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = 0,linetype=2,color="grey50")

