#WONDER CDC (https://wonder.cdc.gov/controller/datarequest/D77;jsessionid=D95A8935AF753DEC613DE0C576FA) and 
#Covid (https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-Sex-and-Age/9bhg-hcku) 
#data analysis

wonder = read.table("Multiple Cause of Death, 1999-2019 age and year T40 with zeros no totals.txt",
           sep = "\t", header = T)

#combine provisional with prior years
wonder.prov = read.table("Provisional Mortality Statistics, 2020 2021 by Year and T40 Code.txt",
                         sep = "\t", header = T)
wonder.prov = wonder.prov[c(1,2,5,6,3,4,7,8,9)]; wonder = rbind(wonder,wonder.prov)

wonder = subset(wonder,!(wonder$Ten.Year.Age.Groups %in% c("Not Stated")))
wonder = subset(wonder,!(wonder$Year.Code == 2021))

# Adding column based on other column:
library(dplyr)
wonder <- wonder %>%
  mutate(Age.Groups = case_when(
    Ten.Year.Age.Groups %in% c("< 1 year","1-4 years","5-14 years","15-24 years") ~ "<25 years",
    Ten.Year.Age.Groups %in% c("25-34 years","35-44 years") ~ "25-44 years",
    Ten.Year.Age.Groups %in% c("45-54 years","55-64 years") ~ "45-64 years",
    Ten.Year.Age.Groups %in% c("65-74 years","75-84 years","85+ years") ~ "65+ years",
  ))
#plot it!
library(ggplot2)
wonder.agg = aggregate(Deaths ~ Year.Code + Age.Groups, data=wonder, FUN=sum)

  ggplot(wonder.agg, aes(x=Year.Code,y=Deaths,color=Age.Groups))+geom_line()+geom_point()+theme_bw()+
    facet_wrap(~Age.Groups, nrow = 1)

#COVID data by month
covid = read.table("Provisional_COVID-19_Deaths_by_Sex_and_Age.tsv", sep = "\t", header = T)
covid = subset(covid,covid$Sex == "All Sexes")
covid = subset(covid,covid$State == "United States")
covid = subset(covid,covid$Group == "By Month")
covid = subset(covid,covid$Age.Group %in% c("Under 1 year","1-4 years","5-14 years","15-24 years","25-34 years","35-44 years","45-54 years","55-64 years","65-74 years","75-84 years","85 years and over"))
covid$End.Date = as.Date(covid$End.Date, format = "%m/%d/%Y")

covid.agg = aggregate(COVID.19.Deaths ~ End.Date + Age.Group, data=covid, FUN=sum)
ggplot(covid.agg, aes(x=End.Date,y=COVID.19.Deaths,color=Age.Group))+geom_line()+geom_point()+theme_bw()

covid <- covid %>%
  mutate(Age.Groups = case_when(
    Age.Group %in% c("Under 1 year","1-4 years","5-14 years","15-24 years") ~ "<25 years",
    Age.Group %in% c("25-34 years","35-44 years") ~ "25-44 years",
    Age.Group %in% c("45-54 years","55-64 years") ~ "45-64 years",
    Age.Group %in% c("65-74 years","75-84 years","85 years and over") ~ "65+ years"
  ))

covid.agg = aggregate(COVID.19.Deaths ~ End.Date + Age.Groups, data=covid, FUN=sum)
ggplot(covid.agg, aes(x=End.Date,y=COVID.19.Deaths,color=Age.Groups))+geom_line()+geom_point()+theme_bw()+
  facet_wrap(~Age.Groups, nrow = 1)

#Opiod deaths by month
# Adding column based on other column:
library(dplyr)
wonder.prov <- wonder.prov %>%
  mutate(Age.Groups = case_when(
    Ten.Year.Age.Groups %in% c("< 1 year","1-4 years","5-14 years","15-24 years") ~ "<25 years",
    Ten.Year.Age.Groups %in% c("25-34 years","35-44 years") ~ "25-44 years",
    Ten.Year.Age.Groups %in% c("45-54 years","55-64 years") ~ "45-64 years",
    Ten.Year.Age.Groups %in% c("65-74 years","75-84 years","85+ years") ~ "65+ years",
  ))

wonder.prov$Month.Code = as.Date(wonder.prov$Month.Code, "%d%y/%m")

library(ggplot2)
wonder.prov.agg = aggregate(Deaths ~ Month.Code + Age.Groups, data=wonder.prov, FUN=sum)

ggplot(wonder.prov.agg, aes(x=Month.Code,y=Deaths,color=Age.Groups))+geom_line()+geom_point()+theme_bw()+
  facet_wrap(~Age.Groups, nrow = 1)

#COVID data by year
covid = read.table("Provisional_COVID-19_Deaths_by_Sex_and_Age.tsv", sep = "\t", header = T)
covid = subset(covid,covid$Sex == "All Sexes")
covid = subset(covid,covid$State == "United States")
covid = subset(covid,covid$Group == "By Year")
covid = subset(covid,covid$Age.Group %in% c("Under 1 year","1-4 years","5-14 years","15-24 years","25-34 years","35-44 years","45-54 years","55-64 years","65-74 years","75-84 years","85 years and over"))

covid.agg = aggregate(COVID.19.Deaths ~ Year + Age.Group, data=covid, FUN=sum)
ggplot(covid.agg, aes(x=Year,y=COVID.19.Deaths,color=Age.Group))+geom_line()+geom_point()+theme_bw()

covid <- covid %>%
  mutate(Age.Groups = case_when(
    Age.Group %in% c("Under 1 year","1-4 years","5-14 years","15-24 years") ~ "<25 years",
    Age.Group %in% c("25-34 years","35-44 years") ~ "25-44 years",
    Age.Group %in% c("45-54 years","55-64 years") ~ "45-64 years",
    Age.Group %in% c("65-74 years","75-84 years","85 years and over") ~ "65+ years"
  ))

covid.agg = aggregate(COVID.19.Deaths ~ Year + Age.Groups, data=covid, FUN=sum)
ggplot(covid.agg, aes(x=as.character(Year),y=COVID.19.Deaths,color=Age.Groups))+geom_line()+geom_point()+theme_bw()+
  facet_wrap(~Age.Groups, nrow = 1)

library(plyr)
both.agg = join(covid.agg,wonder.agg,by = "Age.Groups"); both.agg = subset(both.agg,both.agg$Year.Code == 2020);both.agg = subset(both.agg,both.agg$Year == 2020)
library(ggplot2)
ggplot(both.agg,aes(x = ))