## New important figures, draft started 8/22/24, most recent update 8/7/26
library(ggplot2)
library(egg)
library(tidyverse)
library(readxl)
library(scales)
#### Import data frames ####

# Import and split dataframes from one file, All_Long_Term_Rates
# The file includes data for all three current categories of long-term fluxes: 
# Erupted flux, crustal growth rate, and mantle-derived magma supply rate

# Define df with all rates

All_Rates<-read_excel("All_Long_Term_Rates_Updated_8_26.xlsx")

# Define Erupted Flux df

Erupted_Flux_All<-All_Rates%>%
  filter(CrustalorMantleRates=="Erupted")%>%
  mutate(EruptedVolume=`Flux(km3/myr)`*(`Duration (ka)`*.001))

# Isolate from this df erupted fluxes from the Cascades
Cascades_Erupted<-filter(Erupted_Flux_All, Arc=="Cascades")
# Define Crustal Growth Rate df

Crustal_Growth_Rate_All<-All_Rates%>%
  filter(CrustalorMantleRates=="CrustalRate")

# Define Mantle-Derived Magma Supply Rate df

Mantle_Supply_Rate_All<-All_Rates%>%
  filter(CrustalorMantleRates=="MantleRate")

# Collect all arc averaged fluxes (this would be like joining Crustal_Growth_Rate_All and Mantle_Supply_Rate_All)
Arc_Averaged_Fluxes<-All_Rates%>%
  filter(CrustalorMantleRates=="CrustalRate"|CrustalorMantleRates=="MantleRate")
# Import data to make cumulative volume plots
# Define function to do min/max normalization
min_max_normalization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# Import data, mutate to do min-max scaling
Cumulative_Volumes<-read_excel("Cumulative_Volume_Data.xlsx")%>%
  group_by(Volcano)%>%
  mutate(Min_Max_Scaled_CumVol=min_max_normalization(CumVol))

#### Do statistics ####

# Statistics on mantle supply rates
min(Mantle_Supply_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #90
max(Mantle_Supply_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #290
mean(Mantle_Supply_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #181
sd(Mantle_Supply_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #48
median(Mantle_Supply_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #180
IQR(Mantle_Supply_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #65.3
quantile(Mantle_Supply_Rate_All$`Flux(km3/km/myr)`,probs=.75,na.rm=TRUE)#214
quantile(Mantle_Supply_Rate_All$`Flux(km3/km/myr)`,probs=.25,na.rm=TRUE)#149

## Statistics on erupted flux
min(Erupted_Flux_All$`Flux(km3/myr)`,na.rm = TRUE) #1.3, volcanic field northern cascades
max(Erupted_Flux_All$`Flux(km3/myr)`,na.rm = TRUE) #71428.57, 7 year integrated time
mean(Erupted_Flux_All$`Flux(km3/myr)`,na.rm = TRUE) #1166
sd(Erupted_Flux_All$`Flux(km3/myr)`,na.rm = TRUE) #4529
median(Erupted_Flux_All$`Flux(km3/myr)`,na.rm = TRUE) #290
IQR(Erupted_Flux_All$`Flux(km3/myr)`,na.rm = TRUE) #626
quantile(Erupted_Flux_All$`Flux(km3/myr)`,probs=.75,na.rm=TRUE)#725.8
quantile(Erupted_Flux_All$`Flux(km3/myr)`,probs=.25,na.rm=TRUE)#100

## Statistics on crustal rates
min(Crustal_Growth_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #0.8, volcanic field northern cascades
max(Crustal_Growth_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #750
mean(Crustal_Growth_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #80
sd(Crustal_Growth_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #128
median(Crustal_Growth_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #45
IQR(Crustal_Growth_Rate_All$`Flux(km3/km/myr)`,na.rm = TRUE) #65.25
quantile(Crustal_Growth_Rate_All$`Flux(km3/km/myr)`,probs=.75,na.rm=TRUE)#80
quantile(Crustal_Growth_Rate_All$`Flux(km3/km/myr)`,probs=.25,na.rm=TRUE)#14.75

#### Figures and associated Statistics ####
#### Figure 6 ####
ggplot(Erupted_Flux_All)+
  geom_histogram(aes(x=`Flux(km3/myr)`, after_stat(count)),binwidth=200, color="black")+
  geom_boxplot(aes(x=`Flux(km3/myr)`,y=115), width=25,color="black",alpha=0.5)+
  theme_article()+
  coord_cartesian( xlim = c(0, 5000))+
  labs(y="Count",x=expression("Flux (km"^"3"*"/myr)"))+
  theme(axis.text=element_text(color="black"))

#### Figure 7 ####
# Express how flux changes w duration binned
Binned_Erupted_Flux<-Erupted_Flux_All%>%
  filter(`Duration (ka)`!="NA")%>%
  filter(!is.na(`Flux(km3/myr)`))%>%
  mutate( bin=cut_width(`Duration (ka)`, width=200, boundary=0) )
#options(scipen=999)
ggplot(filter(Binned_Erupted_Flux), aes(y = bin, x = `Flux(km3/myr)`)) +
  geom_boxplot()+
  coord_cartesian(xlim = c(0,5000))+
  theme_article()+
  labs(y="Binned Duration (kyr)",x=expression("Flux (km"^"3"*"/myr)"))+
  theme(axis.text = element_text(color="black", size=10))
# for this figure, y-axis labels need to be changed to non-scientific by hand

#### Figure 8 ####
ggplot(data=filter(All_Rates, CrustalorMantleRates!="Erupted"))+
  geom_histogram(aes(x=`Flux(km3/km/myr)`, after_stat(count)), color="black",binwidth = 50)+
  geom_boxplot(aes(x=`Flux(km3/km/myr)`,y=40),color="black",width=10,alpha=0.5)+
  facet_wrap(~CrustalorMantleRates,nrow=2,
             labeller = labeller(CrustalorMantleRates = c("CrustalRate" = "Crustal Addition Rate",
                                                          "MantleRate" = "Mantle Supply Rate")))+
  theme_article()+
  labs(x=expression("Flux (km"^"3"*"/km/myr)"),
       y="Count" )+
  theme(#axis.text.y=element_blank(),
    #axis.title.y = element_blank(),
    #axis.ticks.y = element_blank(),
    axis.text=element_text(color="black"))+
  lims(x=c(0,1000))

#### Figure 11 ####

CumVolColors<-c("darkgreen","chocolate1","red4", "darkgoldenrod1")
CumVolFigure<-ggplot(data=Cumulative_Volumes, aes(x=Age,y=Min_Max_Scaled_CumVol,color=Volcano))+
  geom_step(size=1)+
  theme_article()+
  scale_x_reverse()+
  labs(y=expression("Min-Max Scaled Cumulative Volume (km"^"3"*")"),x="Age (ka)")+
  scale_color_manual(values=CumVolColors)

CumVolFigure

#### Figure 12 and associated statistics ####
# Isolate Time average calderas, calculate line
Time_Avg_Calderas<-Erupted_Flux_All%>%
  filter(`Time averaged?`== "Yes"&VolcanoType=="Caldera")%>%
  mutate(Duration_myr=`Duration (ka)`*10^-3)
Caldera_Line<-lm(Time_Avg_Calderas$EruptedVolume~Time_Avg_Calderas$`Duration_myr`+0)
summary(Caldera_Line) # 350+/-28 r2=.85
# Isolate Time average volcanic fields, calculate line
Time_Avg_Field<-Erupted_Flux_All%>%
  filter(`Time averaged?`== "Yes"&VolcanoType=="Field",EruptedVolume<5000)%>% #NOTE, WE ARE EXCLUDING VERY HIGH ESTIMATES OF FLUX HERE FROM SNAKE RIVER PLANE AND LAKE BAIKAL
  mutate(Duration_myr=`Duration (ka)`*10^-3)
Field_Line<-lm(Time_Avg_Field$EruptedVolume~Time_Avg_Field$`Duration_myr`+0)
summary(Field_Line) # 23 +/- 4 r2=0.68
# Isolate Time average stratocones, calculate line
Time_Avg_Stratocone<-Erupted_Flux_All%>%
  filter(`Time averaged?`== "Yes"&VolcanoType=="Stratocone")%>%
  mutate(Duration_myr=`Duration (ka)`*10^-3)
Stratocone_Line<-lm(Time_Avg_Stratocone$EruptedVolume~Time_Avg_Stratocone$`Duration_myr`+0)
summary(Stratocone_Line) #133 +/- 20, r2=0.25
# Make plot
ggplot(filter(Erupted_Flux_All, `Time averaged?`=="Yes", VolcanoType=="Caldera"|VolcanoType=="Stratocone"|VolcanoType=="Field", EruptedVolume<5000), 
       aes(y=EruptedVolume,x=`Duration (ka)`/1000, color=VolcanoType),)+
  geom_point()+
  theme_article()+
  geom_smooth(method=lm,
              formula=y~x+0,se=FALSE)+
  scale_color_manual(values=c("coral2",
                              "black",
                              "darkolivegreen3"))+
  theme(axis.text = element_text(color="black"))+
  labs(x="Time Interval (myr)", y=expression("Erupted Volume (km"^"3"*")"), color="Volcano Type")

#### Figure 13 and associated statistics ####
# First, define summarized dataframes from the cascades
# calculate average estimates of flux from Hildreth 2007
Hildreth_averages<-Erupted_Flux_All%>%
  filter(Citation=="Hildreth 2007")%>%
  group_by(GeneralName)%>%
  summarise(EruptedVolume=mean(EruptedVolume),`Flux(km3/myr)`=mean(`Flux(km3/myr)`),`Duration (ka)`=mean(`Duration (ka)`))%>%
  mutate(Arc="Cascades")
# Join those with fluxes from other contributions
Cascades_averages<-Erupted_Flux_All%>%
  filter(Citation=="Schmidt and Grunder 2009"|Citation=="Calvert (2018) Geopshere")%>%
  filter(`Time averaged?`=="Yes")%>%
  bind_rows(Hildreth_averages)
# Find time averaged estimates from CVZ and SVZ
CVZ_SVZ_averages<-filter(Erupted_Flux, Arc=="SVZ"|Arc=="CVZ", `Time averaged?`=="Yes")
# Find time averaged estimates from Alaska Aleutian arc
Alaska_Aleutian_averages<-Erupted_Flux_All%>%
  filter(Arc=="Aleutian-Alaska")%>%
  filter(`Time averaged?`=="Yes")%>%
  mutate(`Duration (myr)`=`Duration (ka)`*10^-3)
# For only oceanic in AA arc
Alaska_Aleutian_averages_oceanic<-Erupted_Flux_All%>%
  filter(Arc=="Aleutian-Alaska")%>%
  filter(`Time averaged?`=="Yes")%>%
  mutate(`Duration (myr)`=`Duration (ka)`*10^-3)%>%
  filter(GeneralName=="Akutan"|GeneralName=="Seguam"|GeneralName=="Tanaga")
# For only continental in AA arc
Alaska_Aleutian_averages_continental<-Erupted_Flux_All%>%
  filter(Arc=="Aleutian-Alaska")%>%
  filter(`Time averaged?`=="Yes")%>%
  mutate(`Duration (myr)`=`Duration (ka)`*10^-3)%>%
  filter(GeneralName!="Akutan"&GeneralName!="Seguam"&GeneralName!="Tanaga")%>%
  filter(EstimateType=="Volcano")
# collect all those data frames
Arc_Averages<-full_join(Cascades_averages,CVZ_SVZ_averages)%>%
  full_join(.,Alaska_Aleutian_averages_continental)%>%
  mutate(`Duration (myr)`=`Duration (ka)`*10^-3)

# make figure
Averages_Figure<-ggplot()+
  geom_point(data=Arc_Averages, 
             aes(y=`EruptedVolume`,x=`Duration (myr)`, color=Arc))+
  geom_smooth(data=filter(Arc_Averages),aes(y=`EruptedVolume`,x=`Duration (myr)`, color=Arc),
              method=lm,formula=y~x+0,se=FALSE)+
  theme_article()+
  labs(y=expression("Erupted Volume (km"^"3"*")"))+
  theme(element_text(color="black"), axis.text = element_text(color="black"),
        legend.position = "none")+
  scale_color_manual(values=c("dodgerblue","pink","orange","darkred"))
Averages_Boxplot<-
  ggplot(data=Arc_Averages)+
  geom_boxplot(aes(y=`Flux(km3/myr)`,x=fct_reorder(Arc, `Flux(km3/myr)`, .fun='median'),color=Arc))+
  theme_article()+
  labs(y=expression("Erupted Flux (km"^"3"*"/myr)"))+
  theme(legend.position ="none" ,axis.title.x = element_blank())+
  theme(element_text(color="black"), axis.text = element_text(color="black"))+
  scale_color_manual(values=c("dodgerblue","pink","orange","darkred"))

ggarrange(Averages_Figure,Averages_Boxplot, nrow=1)
# get data about lines
# include all cascades points
Cascades_Line_Data_All<-filter(Arc_Averages,Arc=="Cascades")
Cascades_Line_All<-lm(Cascades_Line_Data_All$EruptedVolume~Cascades_Line_Data_All$`Duration (myr)`+0)
summary(Cascades_Line_All)
# slope= 290 /pm 54, r2=.53
# exclude high flux points
# isolate smaller fluxes from the cascades
Cascades_Small<-filter(Arc_Averages, EruptedVolume<400)
Cascades_Line_Data<-filter(Cascades_Small,Arc=="Cascades")
Cascades_Line<-lm(Cascades_Line_Data$EruptedVolume~Cascades_Line_Data$`Duration (myr)`+0)
summary(Cascades_Line)
# slope= 204 /pm 19, r2=.83

SVZ_Line_Data<-filter(Cascades_Small,Arc=="SVZ")
SVZ_Line<-lm(SVZ_Line_Data$EruptedVolume~SVZ_Line_Data$`Duration (myr)`+0)
summary(SVZ_Line)
# slope=205 /pm 66, r2=.52

CVZ_Line_Data<-filter(Cascades_Small,Arc=="CVZ")
CVZ_Line<-lm(CVZ_Line_Data$EruptedVolume~CVZ_Line_Data$`Duration (myr)`+0)
summary(CVZ_Line)
#slope=64 /pm 10, r2=.71

# include only continental Aleutian points
Aleutian_Line_Data<-filter(Arc_Averages,Arc=="Aleutian-Alaska")
Aleutian_Line<-lm(Aleutian_Line_Data$EruptedVolume~Aleutian_Line_Data$`Duration (myr)`+0)
summary(Aleutian_Line)
# slope= 41 /pm 32, r2=.15

#### Figure 14 ####
# needs manual addition of number of estimates when regenerated
Erupted_Hist<-ggplot(data=Erupted_Flux_All)+
  geom_histogram(aes(x=`Flux(km3/myr)`), fill="grey", color="black", binwidth=0.5)+
  geom_histogram(data=Arc_Averaged_Fluxes,aes(x=`Flux(km3/km/myr)`*20), fill="cornflowerblue", color="black", binwidth=0.5, alpha=0.6)+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)), limits=c(1e-1,1e6))+
  annotation_logticks(sides="b")+
  labs(x=expression("Flux (km"^"3"*"/myr)"), y="Number Estimates", title="Eruptive flux")+
  theme_article()+
  theme(axis.text = element_text(size=16, color="black"), axis.title = element_text(size=24, color="black"))

Average_Hist<-ggplot(data=Arc_Averaged_Fluxes)+
  geom_histogram(aes(x=`Flux(km3/km/myr)`), fill="grey", color="black", binwidth=0.5)+
  # geom_histogram(aes(x=`Flux(km3/km/myr)`*20), fill="cornflowerblue", color="black", binwidth=0.5, alpha=0.6)+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)), limits=c(1e-1,1e6))+
  annotation_logticks(sides="b")+
  labs(x=expression("Flux (km"^"3"*"/km/myr)"), y="Number Estimates", title="Arc-normalized supply rate")+
  theme_article()+
  theme(axis.text = element_text(size=16, color="black"), axis.title = element_text(size=24, color="black"))

ggarrange(Erupted_Hist,Average_Hist,nrow=1)




#### Trash Bin ####
## Start plots 

# Plot showing the difference in magnitude between crustal and mantle rates



# Plot showing crustal rates
ggplot(data=filter(Crustal_Growth_Rate_All, !is.na(`Flux(km3/km/myr)`)))+
  geom_histogram(aes(x=`Flux(km3/km/myr)`), color="black", binwidth=20)+
  theme_article()+
  labs(x=expression("Flux (km"^"3"*"/km/myr)"))+
  theme(axis.text.y=element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text=element_text(color="black"))


# Plot showing crustal rates by method
ggplot(data=filter(Crustal_Growth_Rate_All, !is.na(`Flux(km3/km/myr)`)))+
  geom_histogram(aes(x=`Flux(km3/km/myr)`, after_stat(ncount)), color="black", binwidth=20)+
  facet_wrap(~Method,ncol=1)+
  theme_article()+
  labs(x=expression("Flux (km"^"3"*"/km/myr)"))+
  theme(axis.text.y=element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text=element_text(color="black"))




# Show distribution in duration in erupted flux database
ggplot(data=Erupted_Flux_All)+
  geom_histogram(aes(x=`Duration (ka)`), color="black")+
  theme_article()+
  labs(y="Number of estimates")






 # Plot the volume vs. duration relationship for Cascsades, SVZ, and CVZ


Check_Fig_Sum<-filter(Erupted_Flux_All, `Time averaged?`=="Yes", VolcanoType=="Caldera"|VolcanoType=="Stratocone"|VolcanoType=="Field")
## Summary Fig of whole database

Time_Avg_Calderas<-Erupted_Flux_All%>%
  filter(`Time averaged?`== "Yes"&VolcanoType=="Caldera")%>%
  mutate(Duration_myr=`Duration (ka)`*10^-3)
Caldera_Line<-lm(Time_Avg_Calderas$EruptedVolume~Time_Avg_Calderas$`Duration_myr`+0)
summary(Caldera_Line) # 350+/-28 r2=.85
Time_Avg_Field<-Erupted_Flux_All%>%
  filter(`Time averaged?`== "Yes"&VolcanoType=="Field",EruptedVolume<5000)%>% #NOTE, WE ARE EXCLUDING VERY HIGH ESTIMATES OF FLUX HERE FROM SNAKE RIVER PLANE AND LAKE BAIKAL
  mutate(Duration_myr=`Duration (ka)`*10^-3)
Field_Line<-lm(Time_Avg_Field$EruptedVolume~Time_Avg_Field$`Duration_myr`+0)
summary(Field_Line)
Time_Avg_Stratocone<-Erupted_Flux_All%>%
  filter(`Time averaged?`== "Yes"&VolcanoType=="Stratocone")%>%
  mutate(Duration_myr=`Duration (ka)`*10^-3)
Stratocone_Line<-lm(Time_Avg_Stratocone$EruptedVolume~Time_Avg_Stratocone$`Duration_myr`+0)
summary(Stratocone_Line)

ggplot(filter(Erupted_Flux_All, `Time averaged?`=="Yes", VolcanoType=="Caldera"|VolcanoType=="Stratocone"|VolcanoType=="Field", EruptedVolume<5000), 
       aes(y=EruptedVolume,x=`Duration (ka)`/1000, color=VolcanoType),)+
  geom_point()+
  theme_article()+
  geom_smooth(method=lm,
              formula=y~x+0,se=FALSE)+
  scale_color_manual(values=c("coral2",
                              "black",
                              "darkolivegreen3"))+
  theme(axis.text = element_text(color="black"))+
  labs(x="Time Interval (myr)", y=expression("Erupted Volume (km"^"3"*")"))

Check_no_points<-filter(Erupted_Flux_All, `Time averaged?`=="Yes", VolcanoType=="Caldera"|VolcanoType=="Stratocone"|VolcanoType=="Field")%>%
  filter(!is.na(EruptedVolume))

ggplot(filter(Erupted_Flux_All, `Time averaged?`=="Yes", VolcanoType=="Caldera"|VolcanoType=="Stratocone"|VolcanoType=="Field"), 
       aes(y=EruptedVolume,x=`Duration (ka)`*1000, color=VolcanoType))+
  geom_point()+
  theme_article()+
  lims(y=c(0,1600),x=c(0,7.5e6))+
  labs(x="Time Interval (years)", y=expression("Erupted Volume (km"^"3"*")"))+
  geom_smooth(data=filter(Erupted_Flux_All, `Time averaged?`=="Yes", VolcanoType=="Caldera"), 
              aes(x=`Duration (ka)`*1000,y=EruptedVolume),
              method=lm,
              formula=y~x+0, color="coral",se=FALSE)+
  geom_abline(aes(intercept=0,slope=1e-3), linetype="dashed")+
  geom_abline(aes(intercept=0,slope=1e-4), linetype="dashed")+
  annotate("text",x=5e6,y=620,label="1e-4",angle=25)+
  annotate("text",x=1e6,y=1400,label="1e-3",angle=80)+
  scale_color_manual(values=c("coral2",
                              "black",
                              "darkolivegreen3"))+
  theme(axis.text = element_text(color="black"))




## Histograms for Christy, 5/29/24

# Generate erupted flux histogram (flux vs. count)



Managed_Data<-Arc_Averaged_Fluxes%>%
  mutate(Flux_Erupted=`Flux(km3/km/myr)`*20)
median(Managed_Data$Flux_Erupted, na.rm = TRUE)
median(Arc_Averaged_Fluxes$`Flux(km3/km/myr)`, na.rm = TRUE)






ggplot(Erupted_Flux_All)+
  geom_boxplot(aes(y=`Flux(km3/myr)`))+
  theme_article()+
  coord_cartesian( ylim = c(0, 5000))

ggplot(Alaska_Aleutian_averages, 
       aes(y=`EruptedVolume`,x=`Duration (myr)`, color=GeneralName))+
  geom_point()+
  # geom_smooth(data=Cascades_Small,
  # method=lm,formula=y~x+0,se=FALSE,linetype="dashed", alpha=0)+
  # geom_smooth(data=filter(Arc_Averages),
  #         method=lm,formula=y~x+0,se=FALSE)+
  theme_article()+
  labs(y=expression("Erupted Volume (km"^"3"*")"))
scale_color_manual(values=c("dodgerblue","pink","orange","darkred"))
ggplot(Arc_Averages, 
       aes(y=`EruptedVolume`,x=`Duration (ka)`, color=Arc))+
  geom_point()+
  #geom_smooth(data=Cascades_Small,
  #  method=lm,formula=y~x+0,se=FALSE,linetype="dashed")+
  theme_article()+
  labs(y=expression("Erupted Volume (km"^"3"*")"))+
  scale_color_manual(values=c("dodgerblue","pink","orange","darkred"))
