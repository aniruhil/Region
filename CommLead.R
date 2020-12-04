---
  title: "Community Leaders Briefing: COVID-19 Statistics"
date: "December 11, 2020"
output: powerpoint_presentation 

---
  
  
  
  
  \pagenumbering{gobble}
\clearpage
\thispagestyle{empty}

\begin{titlepage}

\end{titlepage}
\captionsetup[table]{
  labelsep=newline,
  justification=justified,
  singlelinecheck=false, %textfont=it,
}
\clearpage
\tableofcontents

\newpage

\listoffigures
\clearpage

\pagenumbering{arabic}



```{r results='asis', echo=FALSE, include=FALSE,}
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
library(kableExtra)
#library(hrbrthemes)
library(tidyverse)
library(scales)
library(lubridate)
library(forecast)
library(zoo)
extrafont::loadfonts()
library(knitr)
```





```{r, echo=FALSE, message=FALSE}
CV121<-read_csv("c:/dBase5/en/covid/CV121.csv")

REGFIN<-read_csv("c:/dBase5/en/covid/REGFIN.csv")
REGCEN<-summarize(group_by(REGFIN,REG),CENSUS=sum(C10))

left_join(CV121,REGFIN, by="NAME")->CV121

CV121%>%
  complete(NAME,
           nesting(DATE),
           fill = list(CCNT = 0, DCNT = 0, HCNT =0)) -> CV121

CV121 %>% mutate_if(is.numeric, funs(replace_na(.,0))) -> CV121
CV121<-read_csv("c:/dBase5/en/covid/CV121.csv")
CV121$DATE = as.Date(CV121$DATE)
REGFIN<-read_csv("c:/dBase5/en/covid/REGFIN.csv")
REGFIN<-filter(REGFIN, NAME!="Ohio")
REGFIN$Cleaders->REGFIN$REG
REGCEN<-summarize(group_by(REGFIN,REG),CENSUS=sum(C10))

left_join(CV121,REGFIN, by="NAME")->CV121

CV121%>%
  complete(NAME,
           nesting(DATE),
           fill = list(CCNT = 0, DCNT = 0, HCNT =0)) -> CV121
CV121$WEEK<- isoweek(CV121$DATE)
CV121 %>% mutate_if(is.numeric, funs(replace_na(.,0))) -> CV121
CV121%>% drop_na(REG)->CV121
CV121%>% drop_na(AGE)->CV121
CV121<-filter(CV121, AGE!="Unknown")

CV121$CASEMA = ma(CV121$CCNT, order=7)
CV121$DEATHMA = ma(CV121$DCNT, order=7)
CV121$HOSPMA = ma(CV121$HCNT, order=7)
CVREGSUM<-summarize(group_by(CV121,REG,DATE),CASEMA=sum(CASEMA),HOSPMA=sum(HOSPMA), DEATHMA=sum(DEATHMA)) 
left_join(CVREGSUM,REGCEN, by="REG")->CVREGSUM
CVREGSUM%>% mutate(RCASEMA = ((CASEMA/CENSUS)*100000))->CVREGSUM 
CVREGSUM%>% mutate(RHOSPMA = ((HOSPMA/CENSUS)*100000))->CVREGSUM
CVREGSUM%>% mutate(RDEATHMA = ((DEATHMA/CENSUS)*100000))->CVREGSUM
CVREGSUM%>% mutate(RDH = ((DEATHMA/HOSPMA)*100))->CVREGSUM
CVREGSUM%>% drop_na(REG)->CVREGSUM
#CVREGSUM<-filter(CVREGSUM, DATE>'2019-12-31' & DATE<'2020-11-13')
CV121$CASEMA = ma(CV121$CCNT, order=7)
CV121$DEATHMA = ma(CV121$DCNT, order=7)
CV121$HOSPMA = ma(CV121$HCNT, order=7)
CVREGSUM<-summarize(group_by(CV121,REG,DATE),CASEMA=sum(CASEMA),HOSPMA=sum(HOSPMA), DEATHMA=sum(DEATHMA)) 
left_join(CVREGSUM,REGCEN, by="REG")->CVREGSUM
CVREGSUM%>% mutate(RCASEMA = ((CASEMA/CENSUS)*100000))->CVREGSUM 
CVREGSUM%>% mutate(RHOSPMA = ((HOSPMA/CENSUS)*100000))->CVREGSUM
CVREGSUM%>% mutate(RDEATHMA = ((DEATHMA/CENSUS)*100000))->CVREGSUM
CVREGSUM%>% mutate(RDH = ((DEATHMA/HOSPMA)*100))->CVREGSUM
CVREGSUM%>% mutate(RDC = ((DEATHMA/CASEMA)*100))->CVREGSUM
CVREGSUM%>% drop_na(REG)->CVREGSUM
#CVREGSUM<-filter(CVREGSUM, DATE>'2019-12-31' & DATE<'2020-11-13')
CVAGESUM<-summarize(group_by(CV121,AGE),CASE=sum(CCNT),HOSP=sum(HCNT), DEATH=sum(DCNT))


CVAGESUM%>% drop_na(AGE)->CVAGESUM
CVAGESUM %>% mutate(DEATH2=as.integer(DEATH))->CVAGESUM

CVAGE2SUM<-summarize(group_by(CV121,AGE,DATE),CASE=sum(CCNT),HOSP=sum(HCNT), DEATH=sum(DCNT))

CVSUM<-summarize(group_by(CV121,DATE),CASE=sum(CCNT),HOSP=sum(HCNT), DEATH=sum(DCNT))

CVCO<-filter(CV121, Cleaders=='CL')
CVCOS<-summarize(group_by(CVCO,DATE),CASE=sum(CCNT),HOSP=sum(HCNT), DEATH=sum(DCNT))
CVAGESUMC<-summarize(group_by(CVCO,AGE),CASE=sum(CCNT),HOSP=sum(HCNT), DEATH=sum(DCNT))



CVMAP<-summarize(group_by(CV121,NAME),CASE=sum(CCNT),HOSP=sum(HCNT), DEATH=sum(DCNT))

left_join(CVMAP,REGFIN, by="NAME")->CVMAP

CVMAP%>% mutate(RCASE = ((CASE/C10)*100000))->CVMAP 
CVMAP%>% mutate(RHOSP = ((HOSP/C10)*100000))->CVMAP
CVMAP%>% mutate(RDEATH = ((DEATH/C10)*100000))->CVMAP

CVMAP %>% mutate_if(is.numeric, funs(replace_na(.,0))) -> CVMAP
write.csv(CVMAP,"C:\\dBASE5\\en\\covid\\CVMAP.csv", row.names = FALSE)
```
```{r, echo=FALSE, message=FALSE}
CV121 <- CV121 %>% mutate(MDATE = floor_date(as_date(DATE),"month"))
CVMOSUM<-summarize(group_by(CV121,MDATE),CASE=sum(CCNT),HOSP=sum(HCNT),DEATH=sum(DCNT)) 
CVMOSUM %>% mutate(H2I=round((HOSP/CASE*100),2))->CVMOSUM
CVMOSUM %>% mutate(D2H=round((DEATH/HOSP*100),2))->CVMOSUM
CVMOSUM<-filter(CVMOSUM, MDATE>"2020-02-29" & MDATE<"2020-11-01")
```

```{r, echo=FALSE, message=FALSE}
CV121 <- CV121 %>% mutate(MDATE = floor_date(as_date(DATE),"month"))
CVCOMOSUM<-summarize(group_by(CV121,NAME,MDATE),CASE=sum(CCNT),HOSP=sum(HCNT),DEATH=sum(DCNT)) 
CVCOMOSUM %>% mutate(H2I=round((HOSP/CASE*100),2))->CVCOMOSUM
CVCOMOSUM %>% mutate(D2H=round((DEATH/HOSP*100),2))->CVCOMOSUM
CVCOMOSUM<-filter(CVCOMOSUM, MDATE>"2020-02-29" & MDATE<"2020-11-01",NAME=="Ross")

CVMOSUM %>% mutate(NAME="Ohio")->CVMOSUM
CVCOMOSUM2<-rbind(CVCOMOSUM,CVMOSUM)
left_join(CVCOMOSUM2,REGFIN, by="NAME")->CVCOMOSUM2
CVCOMOSUM2 %>% mutate(I2P=round((CASE/C10*100000),2))->CVCOMOSUM2
```


```{r, echo=FALSE, message=FALSE}
CV121 <- CV121 %>% mutate(MDATE = floor_date(as_date(DATE),"month"))
CVCOMOSUM3<-summarize(group_by(CV121,REG,MDATE),CASE=sum(CCNT),HOSP=sum(HCNT),DEATH=sum(DCNT)) 
CVCOMOSUM3 %>% mutate(H2I=round((HOSP/CASE*100),2))->CVCOMOSUM3
CVCOMOSUM3 %>% mutate(D2H=round((DEATH/HOSP*100),2))->CVCOMOSUM3
CVCOMOSUM3<-filter(CVCOMOSUM3, MDATE>"2020-06-29" & MDATE<"2020-12-01") 
```



# Introduction
The following presentation summarizes Ohio COVID-19 data obtained from the Department of Health COVID-19 warehouse. Data for the state of Ohio and Region are presented. In addition to descriptive reports for the state and individual counties available on the state website, the Ohio Alliance for Innovation in Population Health has summarized trends by region of state and has calculated county level outcome indicators that measure disease transmission, population susceptability to adverse outcomes and the effectiveness of health systems at mitigating the effects of COVID-19. These measures will be explained at greater length later in this report. 

\newpage

# Southeast Ohio Region

```{r pressure, echo=FALSE, fig.align ='center', out.width = '100%'}
knitr::include_graphics("c:/dBase5/en/covid/cl.jpeg")
```

# Age Groups
The next series of graphs show the total number of Ohio cases, hospitalizations and deaths attributable to COVID-19 by  reported age group of infected persons. Infections are negatively skewed with higher numbers of younger Ohioans represented in the data. 

Hospitalizations and deaths are positively skewed with increasingly disproportionate numbers of Older Ohioans experiencing adverse outcomes caused by the disease. While this is generally known, the overall magnitude of deaths among older Ohioans verses younger Ohioans is still surprising. 

\newpage

# Ohio Infections by Age
```{r echo=FALSE, dpi=300}
ggplot(CVAGESUM, aes(x=AGE, y=CASE)) + geom_bar(fill="#053f26",stat="identity",alpha=0.7) +
  labs(caption = "Source: Ohio Department of Health", y="Cases", x="Age", color=NULL)+ 
  theme_minimal()+theme(axis.title.y=element_text(vjust=0.95,size=9,colour="gray50"))+geom_text(aes(label = scales::comma(round(HOSP), accuracy=1)), position=position_dodge(width=0.8), size=2, hjust=0.5,vjust=-1.0)
```



\newpage

# Ohio Hospitalizations by Age

```{r echo=FALSE, dpi=300}
ggplot(CVAGESUM, aes(x=AGE, y=HOSP)) + geom_bar(fill="#053f26",stat="identity",alpha=0.7) +
  labs(caption = "Source: Ohio Department of Health", y="Cases", x="Age", color=NULL)+ 
  theme_minimal()+theme(axis.title.y=element_text(vjust=0.95,size=9,colour="gray50"))+geom_text(aes(label = scales::comma(round(HOSP), accuracy=1)), position=position_dodge(width=0.8), size=2, hjust=0.5,vjust=-1.0)
```


\newpage


# Ohio Deaths by Age

```{r echo=FALSE, dpi=300}
ggplot(CVAGESUM, aes(x=AGE, y=DEATH)) + geom_bar(fill="#053f26",stat="identity",alpha=0.7) +
  labs(caption = "Source: Ohio Department of Health", y="Cases", x="Age", color=NULL)+ 
  theme_minimal()+theme(axis.title.y=element_text(vjust=0.95,size=9,colour="gray50"))+geom_text(aes(label = scales::comma(round(DEATH), accuracy=1)), position=position_dodge(width=0.8), size=2, hjust=0.5,vjust=-1.0)
```
\newpage

# Over Time

The following graphs show the number of infections, hospitalizations and deaths by day since the beginning of the pandemic. The number of infections in recent weeks has increased dramatically and is generally consistent with temporal pattern associated with influenza. Hospitalization data displays a multi-modal pattern with peaks in the spring, summer and fall. Deaths, however, have declined since the spring peak. A series of graphs summarizing these data by region of state is also presented.

It is important to note that recorded cases, hospitalizations and deaths lag behind actual events so data points graphed for the last several days of the reporting period are lower than actual incidence and should be interpreted with caution.

\newpage

# Ohio Infections Over Time

```{r echo=FALSE, message=FALSE, dpi=300}
ggplot(CVSUM, aes(x=DATE, y=CASE)) +geom_area(fill="#053f26", alpha=0.3) + geom_line(color="#053f26",size=0.1) + 
  geom_smooth(color="#2D4159",span=0.2, se=FALSE,size=0.5)+ labs(caption = "Source: Ohio Department of Health", y="Cases", x="Month", color=NULL)+scale_x_date(labels = date_format("%m")) +
  theme_minimal()+theme(axis.title.y=element_text(hjust=0.7,size=10,colour="gray50"))
```

\newpage

# Ohio Hospitalizations Over Time

```{r echo=FALSE, message=FALSE, dpi=300}
ggplot(CVSUM, aes(x=DATE, y=HOSP)) +geom_area(fill="#053f26", alpha=0.3) +     geom_line(color="#053f26",size=0.1) + 
  geom_smooth(color="#2D4159",span=0.2, se=FALSE,size=0.5)+ labs(caption = "Source: Ohio Department of Health", y="Cases", x="Month", color=NULL)+scale_x_date(labels = date_format("%m")) +
  theme_minimal()+theme(axis.title.y=element_text(hjust=0.7,size=10,colour="gray50"))
```

\newpage

# Ohio Deaths Over Time

```{r echo=FALSE, message=FALSE, dpi=300}
ggplot(CVSUM, aes(x=DATE, y=DEATH)) +geom_area(fill="#053f26", alpha=0.3) +     geom_line(color="#053f26",size=0.1) + 
  geom_smooth(color="#2D4159",span=0.2, se=FALSE,size=0.5)+ labs(caption = "Source: Ohio Department of Health", y="Cases", x="Month", color=NULL)+scale_x_date(labels = date_format("%m")) +
  theme_minimal()+theme(axis.title.y=element_text(hjust=0.7,size=10,colour="gray50"))
```


\newpage


# Rate per 100,000 Infections for Region and State

```{r echo=FALSE, message=FALSE, dpi=300}
ggplot(CVREGSUM, aes(x=DATE, y=RCASEMA)) +geom_area(fill="#053f26", alpha=0.3) +     geom_line(color="#053f26",size=0.1) + 
  geom_smooth(color="#2D4159",span=0.2, se=FALSE,size=0.5)+ labs(caption = "Source: Ohio Department of Health", y="Cases", x="Month", color=NULL)+ facet_wrap(~REG) +scale_x_date(labels = date_format("%m")) +
  theme_minimal()+theme(axis.title.y=element_text(hjust=0.7,size=10,colour="gray50"))
```


\newpage

# Rate per 100,000 Hospitalizations by Region and State

```{r echo=FALSE, message=FALSE, dpi=300}
ggplot(CVREGSUM, aes(x=DATE, y=RHOSPMA)) +geom_area(fill="#053f26", alpha=0.3) +     geom_line(color="#053f26",size=0.1) + 
  geom_smooth(color="#2D4159",span=0.2, se=FALSE,size=0.5)+ labs(caption = "Source: Ohio Department of Health", y="Cases", x="Month", color=NULL)+ facet_wrap(~REG) +scale_x_date(labels = date_format("%m")) +
  theme_minimal()+theme(axis.title.y=element_text(hjust=0.7,size=10,colour="gray50"))
```

\newpage

# Rate per 100,000 Deaths by Region of State

```{r echo=FALSE, message=FALSE, dpi=300}
ggplot(CVREGSUM, aes(x=DATE, y=RDEATHMA)) +geom_area(fill="#053f26", alpha=0.3) +     geom_line(color="#053f26",size=0.1) + 
  geom_smooth(color="#2D4159",span=0.2, se=FALSE,size=0.5)+ labs(caption = "Source: Ohio Department of Health", y="Cases", x="Month", color=NULL)+ facet_wrap(~REG) +scale_x_date(labels = date_format("%m")) +
  theme_minimal()+theme(axis.title.y=element_text(hjust=0.7,size=10,colour="gray50"))
```

\newpage

# Regional Data

\newpage


# Region Infections by Age

```{r echo=FALSE, dpi=300}
ggplot(CVAGESUMC, aes(x=AGE, y=CASE)) + geom_bar(fill="#1d3d6e",stat="identity",alpha=0.7) +
  labs(caption = "Source: Ohio Department of Health", y="Cases", x="Age", color=NULL)+ 
  theme_minimal()+theme(axis.title.y=element_text(vjust=0.95,size=9,colour="gray50"))+geom_text(aes(label = scales::comma(round(CASE), accuracy=1)), position=position_dodge(width=2.0), size=2, hjust=0.5,vjust=-1.0)
```


\newpage

# Region Hospitalizations by Age

```{r echo=FALSE, dpi=300}
ggplot(CVAGESUMC, aes(x=AGE, y=HOSP)) + geom_bar(fill="#1d3d6e",stat="identity",alpha=0.7) +
  labs(caption = "Source: Ohio Department of Health", y="Cases", x="Age", color=NULL)+ 
  theme_minimal()+theme(axis.title.y=element_text(vjust=0.95,size=9,colour="gray50"))+geom_text(aes(label = scales::comma(round(HOSP), accuracy=1)), position=position_dodge(width=0.8), size=2, hjust=0.5,vjust=-1.0)
```


\newpage


# Region Deaths by Age

```{r echo=FALSE, dpi=300}
ggplot(CVAGESUMC, aes(x=AGE, y=DEATH)) + geom_bar(fill="#1d3d6e",stat="identity",alpha=0.7) +
  labs(caption = "Source: Ohio Department of Health", y="Cases", x="Age", color=NULL)+ 
  theme_minimal()+theme(axis.title.y=element_text(vjust=0.95,size=9,colour="gray50"))+geom_text(aes(label = scales::comma(round(DEATH), accuracy=1)), position=position_dodge(width=0.8), size=2, hjust=0.5,vjust=-1.0)
```

\newpage

# Rate of Hospitalizations to Infections

The ratio of hospitalizations to infections  (H2I) is a proposed measure for how susceptible the population of a geographic area is to adverse outcomes associated with COVID-19. This measure is calculated by dividing the number of hospitalizations by the total number of infections. 

For the purposes of this exercise, it is assumed that high ratios of hospitalizations to infections is indicative of a geographic region that has a higher percentage of persons struggling with underlying conditions and therefore more susceptible to adverse outcomes.

\newpage

# Community Leaders Region Rate of Hospitalizations to Infections

```{r echo=FALSE, dpi=300}
ggplot(CVCOMOSUM3, aes(x=MDATE, y=H2I,fill=REG)) +geom_col(alpha=0.7,position="dodge") +  labs(caption = "Source: Ohio Department of Health", y="Cases", x="Month", color=NULL)+scale_x_date(labels = date_format("%b")) + theme_minimal()+theme(axis.title.y=element_text(hjust=0.7,size=10,colour="gray50"))+scale_fill_manual(values=c("#1d3d6e","#053f26"))
```

\newpage

# Rate of Deaths to Hospitalizations

The ratio of hospitalizations to Deaths (D2H) is a proposed measure for the effectiveness of local health systems at mitigating the effects of COVID-19. This measure is calculated by dividing the number of hospitalizations by the total number of deaths. It is assumed that severity of illness for hospital admissions is similar across Ohio communities and therefore survival rates indicate greater or lesser success in mitigating the effects of the disease. 

\newpage

# Community Leaders Rate of Deaths to Hospitalizations

```{r echo=FALSE, dpi=300}
ggplot(CVCOMOSUM3, aes(x=MDATE, y=D2H,fill=REG)) +geom_col(alpha=0.7,position="dodge") +  labs(caption = "Source: Ohio Department of Health", y="Cases", x="Month", color=NULL)+scale_x_date(labels = date_format("%b")) + theme_minimal()+theme(axis.title.y=element_text(hjust=0.7,size=10,colour="gray50"))+scale_fill_manual(values=c("#1d3d6e","#053f26"))
```