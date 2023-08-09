library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)

gisaid_Moz <- read_excel("data/gisaid_Moz.xlsx")
gisaid_Moz$days<-as.Date(cut(gisaid_Moz$date,
                             breaks = "day",
                             start.on.monday = FALSE))

gisaid_Moz$date<-as.Date(cut(gisaid_Moz$date,
                             breaks = "2 week",
                             start.on.monday = FALSE))

Re_update <- read_excel("data/Re_update.xlsx")

Re_update$date<-as.Date(cut(Re_update$date,
                            breaks = "day",
                            start.on.monday = FALSE))

Re_update=subset(subset(Re_update,data_type=="Confirmed cases"),estimate_type=="Cori_slidingWindow")

data1 <- read_excel("data/owid-covid-data.xlsx")

data1$days<-as.Date(cut(data1$date, breaks = "day",start.on.monday = FALSE))

data1$date<-as.Date(cut(data1$date,
breaks = "week",
start.on.monday = FALSE))

dateVec <- seq(from = as.Date("2020-03-15"), to = as.Date("2022-04-15"),by = "days")

pEpi_Moz<-ggplot()+
theme_classic()+
geom_bar(data=data1, aes(x=days, y=new_cases_smoothed,fill='Cases'),width=1,stat='identity',color='cadetblue')+
geom_rug(data= gisaid_Moz, aes(x=days, color='Genomes'),alpha=0.2,outside =FALSE,length = unit(0.03, "npc"),show.legend=TRUE) +
theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
theme(axis.text.x = element_text(color="black", size=11))+
theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
theme(axis.text.y = element_text(color="black", size=11))+
scale_x_date(limits = c(min(dateVec), max=max(dateVec)), date_labels = "%b\n%Y",date_breaks = "2 month")+
geom_line(data=data1, aes(x=days, y=total_deaths, color='Deaths'), linewidth=1.4)+
geom_line(data = Re_update, aes(x = date, y = median_R_mean*900, color = "Re"), size=1.4) +
scale_color_manual(values=c('lightsalmon','grey40','deeppink3'), name='')+
scale_fill_manual(values=c('cadetblue'), name='')+
geom_hline(yintercept=900, color='black', linetype=2) +
xlab(' ')+
ylab(' ')+
scale_y_continuous(
# Features of the first axis
name = "Daily Cases / Cumulative Deaths",
# Add a second axis and specify its features
sec.axis = sec_axis(~./900, name="Re")
)+
theme(plot.title = element_text(hjust = 0.5))+
theme(legend.position="top",legend.text= element_text(size=11))

pEpi_Moz


gisaid_Moz$pango<-factor(gisaid_Moz$pango,levels = c("Other","Other B lineages","B.1.1/B.1.1.1", "B.1.1.375","C.1/C.1.1", "Beta","Alpha","Delta", "Omicron BA.1", "Omicron BA.2", "Omicron BA.3"))

P_pangolin<-ggplot(data=subset(gisaid_Moz, !is.na(pango)), aes(x = date,fill=pango, color = pango))+
geom_bar(width=10,color='black', size=0.2)+
theme_classic()+
theme(axis.text.x = element_text(color="black", size=16))+
xlab("Sampling Date")+
scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 months")+
theme(axis.title.x = element_text(color="black", size=12, face="bold"))+
theme(axis.text.x = element_text(color="black", size=12))+
theme(axis.title.y = element_text(color="black", size=12, face="bold"))+
theme(axis.text.y = element_text(color="black", size=12))+
scale_fill_manual(values=c('white','darkseagreen3','#ea877c','#96938D', 'coral4', 'goldenrod2', 'burlywood4', 'darkorange2', 'light blue', '#9FA8DA', '#CE93D8'), name='', labels=c("Other lineages","Other B lineages", "B.1.1/B.1.1.1", "B.1.1.375", "C.1/C.1.1", "Beta (B.1.351)","Alpha (B.1.1.7)","Delta (B.1.617.2/AY.x)", "Omicron BA.1", 'Omicron BA.2','Omicron BA.3'))+
theme(legend.text = element_text(size=11))+
theme(legend.title = element_text(size=12,  face="bold"))+
theme(legend.position = "bottom")+
ylab('Genome Count')

P_pangolin

