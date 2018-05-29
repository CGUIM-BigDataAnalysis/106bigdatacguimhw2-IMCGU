install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("choroplethr")
install.packages("choroplethrMaps")
library(readr)
library(dplyr)
library(ggplot2)
C103<- read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
U103<- read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
C104<- read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
U104<- read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
C105<- read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
U105<- read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
C106<- read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
U106<- read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")
tw<-read_csv("Student_RPT_07.csv")
world<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")


#1
C103<-data.frame(country=C103$國別,people103=C103$`學位生-正式修讀學位外國生`+C103$`學位生-僑生(含港澳)`+C103$`學位生-正式修讀學位陸生`+
                   C103$`非學位生-外國交換生`+C103$`非學位生-外國短期研習及個人選讀`+
                   C103$`非學位生-大專附設華語文中心學生`+C103$`非學位生-大陸研修生`+
                   C103$`非學位生-海青班`+C103$境外專班)

C104<-data.frame(country=C104$國別,people104=C104$`學位生-正式修讀學位外國生`+C104$`學位生-僑生(含港澳)`+C104$`學位生-正式修讀學位陸生`+
                   C104$`非學位生-外國交換生`+C104$`非學位生-外國短期研習及個人選讀`+
                   C104$`非學位生-大專附設華語文中心學生`+C104$`非學位生-大陸研修生`+
                   C104$`非學位生-海青班`+C104$境外專班)
C103104<-full_join(C103,C104,by="country")

C105<-data.frame(country=C105$國別,people105=C105$`學位生_正式修讀學位外國生`+C105$`學位生_僑生(含港澳)`+C105$`學位生_正式修讀學位陸生`+
                   C105$`非學位生_外國交換生`+C105$`非學位生_外國短期研習及個人選讀`+
                   C105$`非學位生_大專附設華語文中心學生`+C105$`非學位生_大陸研修生`+
                   C105$`非學位生_海青班`+C105$境外專班)
C103104105<-full_join(C103104,C105,by="country")

C106<-data.frame(country=C106$國別,people106=C106$`學位生_正式修讀學位外國生`+C106$`學位生_僑生(含港澳)`+C106$`學位生_正式修讀學位陸生`+
                   C106$`非學位生_外國交換生`+C106$`非學位生_外國短期研習及個人選讀`+
                   C106$`非學位生_大專附設華語文中心學生`+C106$`非學位生_大陸研修生`+
                   C106$`非學位生_海青班`+C106$境外專班)
C103104105106<-full_join(C103104105,C106,by="country")
C103104105106[is.na(C103104105106)]=0
C103104105106$total<-C103104105106$people103+C103104105106$people104+C103104105106$people105+C103104105106$people106
C103104105106<-arrange(C103104105106,desc(C103104105106$total))
C103104105106[1:10,c(1,6)]


U103$`非學位生-大陸研修生`<-gsub("…","0",U103$`非學位生-大陸研修生`)
U103$`非學位生-大陸研修生`<-as.numeric(U103$`非學位生-大陸研修生`)
U103<-data.frame(school=U103$學校名稱,people103=U103$`學位生-正式修讀學位外國生`+U103$`學位生-僑生(含港澳)`+U103$`學位生-正式修讀學位陸生`+
                   U103$`非學位生-外國交換生`+U103$`非學位生-外國短期研習及個人選讀`+U103$`非學位生-大專附設華語文中心學生`+
                   U103$`非學位生-大陸研修生`+U103$`非學位生-海青班`+U103$境外專班)

U104$`非學位生-大陸研修生`<-gsub("…","0",U104$`非學位生-大陸研修生`)
U104$`非學位生-大陸研修生`<-as.numeric(U104$`非學位生-大陸研修生`)
U104<-data.frame(school=U104$學校名稱,people104=U104$`學位生-正式修讀學位外國生`+U104$`學位生-僑生(含港澳)`+U104$`學位生-正式修讀學位陸生`+
                   U104$`非學位生-外國交換生`+U104$`非學位生-外國短期研習及個人選讀`+U104$`非學位生-大專附設華語文中心學生`+
                   U104$`非學位生-大陸研修生`+U104$`非學位生-海青班`+U104$境外專班)
U103104<-full_join(U103,U104,by="school")

U105$`非學位生_大陸研修生`<-gsub("…","0",U105$`非學位生_大陸研修生`)
U105$`非學位生_大陸研修生`<-as.numeric(U105$`非學位生_大陸研修生`)
U105<-data.frame(school=U105$學校名稱,people105=U105$`學位生_正式修讀學位外國生`+U105$`學位生_僑生(含港澳)`+U105$`學位生_正式修讀學位陸生`+
                   U105$`非學位生_外國交換生`+U105$`非學位生_外國短期研習及個人選讀`+U105$`非學位生_大專附設華語文中心學生`+
                   U105$`非學位生_大陸研修生`+U105$`非學位生_海青班`+U105$境外專班)
U103104105<-full_join(U103104,U105,by="school")

U106$`非學位生_大陸研修生`<-gsub("…","0",U106$`非學位生_大陸研修生`)
U106$`非學位生_大陸研修生`<-as.numeric(U106$`非學位生_大陸研修生`)
U106<-data.frame(school=U106$學校名稱,people106=U106$`學位生_正式修讀學位外國生`+U106$`學位生_僑生(含港澳)`+U106$`學位生_正式修讀學位陸生`+
                   U106$`非學位生_外國交換生`+U106$`非學位生_外國短期研習及個人選讀`+U106$`非學位生_大專附設華語文中心學生`+
                   U106$`非學位生_大陸研修生`+U106$`非學位生_海青班`+U106$境外專班)
U103104105106<-full_join(U103104105,U106,by="school")
U103104105106[is.na(U103104105106)]=0
U103104105106$total<-U103104105106$people103+U103104105106$people104+U103104105106$people105+U103104105106$people106
U103104105106<-arrange(U103104105106,desc(U103104105106$total))




#2
library(ggplot2)
C103104105106n<-C103104105106[,-c(2:5)]
groupcountry<-C103104105106n%>%
  group_by(country)%>%
  tally(total,sort=TRUE)%>%
  group_by(country = factor(c(country[1:10], rep("Other", n() -10)),
                            levels = c(country[1:10], "Other")))%>%
  tally(n)
colnames(groupcountry)<-c("country","total")
ggplot()+geom_bar(data=groupcountry,
                  aes(x=country,y=total),
                  stat = "identity",
                  fill = "green")








#3
library(choroplethr)
library(choroplethrMaps)
countryname<-read_csv("CountriesComparisionTable.csv")
colnames(countryname)<-c("ISO3","English","country")
TotalCountry<-merge(C103104105106[,c(1,6)],countryname,by="country")
colnames(TotalCountry)<-c("country","value","ISO3","region")
TotalCountry[5,2]<-TotalCountry[5,2]+TotalCountry[91,2]+TotalCountry[159,2]
TotalCountry[107,2]<-TotalCountry[107,2]+TotalCountry[108,2]
TotalCountry<-TotalCountry%>%
  subset(region!="Unmatch")%>%
  subset(country!="索馬利蘭共和國")
A3<-country_choropleth(TotalCountry)
A3






#4
library(dplyr)
a<-subset(tw,`學年度`>102)%>%
  group_by(`對方學校(機構)國別(地區)`)%>%
  summarise(ncountry=sum(`小計`))
  b<-arrange(a,desc(ncountry))
  head(b,10)
c<-subset(tw,`學年度`>102)%>%
  group_by(`學校名稱`)%>%
  summarise(nschool=sum(小計))
  d<-arrange(c,desc(nschool))
  head(d,10)





#5
groupcountry1<-b%>%
  group_by(`對方學校(機構)國別(地區)`)%>%
  tally(ncountry,sort=TRUE)%>%
  group_by(`對方學校(機構)國別(地區)` = factor(c(`對方學校(機構)國別(地區)`[1:10], rep("Other", n() - 10)),
                                     levels = c(`對方學校(機構)國別(地區)`[1:10], "Other")))%>%tally(n)
colnames(groupcountry1)<-c("對方學校(機構)國別(地區)","ncountry")
ggplot()+geom_bar(data=groupcountry1,aes(x=`對方學校(機構)國別(地區)`,y=ncountry),
                  stat = "identity",
                  fill = "green")


#6
library(choroplethr)
library(choroplethrMaps)
countryname<-read_csv("CountriesComparisionTable.csv")
colnames(countryname)<-c("ISO3","English","中文")
outNum<-a
colnames(outNum)<-c("國名","count")
EoutNum<-merge(outNum,countryname,by.x="國名",by.y="中文")
colnames(EoutNum)<-c("國名","value","ISO3","region")
EoutNum<-EoutNum%>%
  subset(region!="Unmatch")
A6<-country_choropleth(EoutNum,num_colors=9)
A6

#7
world$X4=NULL
world$X5=NULL
world$X6=NULL
world<-arrange(world,desc(world$總人數))
world[1:10,c(2,3)]

#8

countryname<-read_csv("CountriesComparisionTable.csv")
colnames(countryname)<-c("ISO3","English","國別")
Eworld<-merge(world,countryname,by="國別")
colnames(Eworld)<-c("國名","洲別","value","ISO3","region")
A8<-country_choropleth(Eworld)
A8
#9
ggplot()+geom_bar(data=groupcountry,
                  aes(x=country,y=total),
                  stat = "identity",
                  fill = "green")

ggplot()+geom_bar(data=groupcountry1,
                  aes(x=`對方學校(機構)國別(地區)`,y=ncountry),
                  stat = "identity",
                  fill = "green")
#10


