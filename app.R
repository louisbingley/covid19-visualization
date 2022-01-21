
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(usmap)
library(plyr)
require(data.table)
library(grid)
library(stringi)
library(zoo)
library(egg)
library(urbnmapr)
library(readxl)

#############################
# IMPORTING DATA
#############################

#County, Race/Eth, Month, Vacc Count, Pop, Vacc Rate
fips_KS <- read_excel("C:/Users/mingxiao/Dropbox/KUMC STUDY/RA/Cov19/ShinyR/fips_KS.xlsx")
fips_KS$fips=fips_KS$FIPS

VR <- read_excel("C:/Users/mingxiao/Dropbox/KUMC STUDY/RA/Cov19/ShinyR/All COVID-19 data_2021-10-06 16_17_27.xlsx",
                 sheet=1)
VE <- read_excel("C:/Users/mingxiao/Dropbox/KUMC STUDY/RA/Cov19/ShinyR/All COVID-19 data_2021-10-06 16_17_27.xlsx",
                 sheet=2)
TR <- read_excel("C:/Users/mingxiao/Dropbox/KUMC STUDY/RA/Cov19/ShinyR/All COVID-19 data_2021-10-06 16_17_27.xlsx",
                 sheet=3)
TE <- read_excel("C:/Users/mingxiao/Dropbox/KUMC STUDY/RA/Cov19/ShinyR/All COVID-19 data_2021-10-06 16_17_27.xlsx",
                 sheet=4)


#############################
# DATA CLEANING
#############################

ten=c("Wyandotte","Saline", "Sedgwick","Seward","Riley","Lyon","Johnson","Douglas","Crawford","Finney")

VR$RE=VR$Race         
VE$RE=VE$Ethnicity   
TR$RE=TR$Race         
TE$RE=TE$Ethnicity    
rb=rbind.fill(VR,VE,TR,TE)
rb$yearmondate <- as.yearmon(paste(rb$Month, rb$Year), "%b %Y")
colnames(rb)[12]<-"Positivity Rate"

rb10=rb[which(rb$County%in%ten),]       # dataset for line plot
tt<- subset(rb, rb$Race == "Total")     # dataset for heat map


############################
#                          #
#     Plot Example         #
#                          #
############################

ratechoice="Testing Rate"
county="Johnson"
re="White"
startdate="Mar 2020"
enddate="Jul 2021"


############ 
# LINE PLOT  
############ 

# WIDE TO LONG

rb_long<-rb10 %>% gather(key, value, c(`Vaccination Rate`, `Testing Rate`, `Positivity Rate`))  
plotdata <- subset(rb_long, !is.na(value)) # remove all missing values for the plots

# PLOT

data00 <- subset(plotdata, key == ratechoice & 
                   County == county &
                   RE == re &
                   yearmondate>=startdate &
                   yearmondate<=enddate)
head(data00)

ggplot(data00, aes(x = yearmondate, y = value)) +
  geom_point(size = 3)+
  geom_line() +
  scale_x_yearmon(breaks = data00$yearmondate)+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  labs(title = paste(ratechoice,"of",county ,"County"), 
       subtitle = paste("Race/Ethnicity:", re), 
       x = "Date", 
       y = paste(ratechoice))



########### 
# HEAT MAP  
########### 

ttt=tt[which(tt$yearmondate>=startdate & tt$yearmondate<=enddate),]
popperct = as.data.frame(unique(cbind(ttt$County,ttt$Population)))       # population for each county

# AVG VACCINATION RATE FOR EACH COUNTY

vaccountperct=aggregate(ttt$'Vaccination Count', by = list(County = ttt$County), FUN = sum,na.rm=T)  # total vaccination for each county
colnames(popperct)<-c("County","Pop")
colnames(vaccountperct)<-c("County","vacttcount")
vacrate_dat=merge(vaccountperct,popperct,by="County")
vacrate_dat$vac_rate_1K=1000*vacrate_dat$vacttcount/as.numeric(vacrate_dat$Pop)                   
vacrate_dat
vacrate_dat=merge(vacrate_dat,fips_KS,by="County")
head(vacrate_dat)

# AVG TESTING RATE FOR EACH COUNTY

testcountperct=aggregate(ttt$'Total Tests', by = list(County = ttt$County), FUN = sum,na.rm=T)  # total #tests for each county
colnames(testcountperct)<-c("County","testttcount")
testrate_dat=merge(testcountperct,popperct,by="County")
testrate_dat$test_rate_1K=1000*testrate_dat$testttcount/as.numeric(testrate_dat$Pop)                   
testrate_dat
testrate_dat=merge(testrate_dat,fips_KS,by="County")
head(testrate_dat)

# AVG POSITIVITY RATE FOR EACH COUNTY

posrate_dat=aggregate(cbind(ttt$`PCR Positive Count`,ttt$`PCR Negative Count`), by = list(County = ttt$County), FUN = sum,na.rm=T)   
colnames(posrate_dat)<-c("County","ttPOS","ttNEG")
posrate_dat$pos_rate_1K=1000*posrate_dat$ttPOS/(posrate_dat$ttPOS+posrate_dat$ttNEG)                 
posrate_dat
posrate_dat=merge(posrate_dat,fips_KS,by="County")
head(posrate_dat)

# STACK RATE TABLES

names(vacrate_dat);vacrate_=vacrate_dat[,c(7,4)];vacrate_
names(testrate_dat);testrate_=testrate_dat[,c(7,4)];testrate_
names(posrate_dat);posrate_=posrate_dat[,c(7,4)];posrate_
rates=merge(vacrate_,testrate_,by="fips")
rates=merge(rates,posrate_,by="fips")
head(rates)
colnames(rates)[2:4]<-c("Vaccination Rate","Testing Rate","Positivity Rate" )
head(rates)


# HEAT MAP PLOT 

rates_long<-rates %>% gather(key, value, c(`Vaccination Rate`,
                                           `Testing Rate`, 
                                           `Positivity Rate`))  
head(rates_long)
data00=rates_long[which(rates_long$key==ratechoice),]
plot_usmap(data = data00, values ="value", include = "KS") + 
  labs(title = paste("Average COVID-19",ratechoice,"Per Month in Kansas (per 1,000)"), 
       subtitle = paste(startdate,"to",enddate)) +
  scale_fill_gradientn(colors = c("deepskyblue","blue4")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.box.margin = margin(-10,-10,-10,-10))

# OR

plot_usmap(data = rates, values = ratechoice, include = "KS") + 
  labs(title = paste("Average COVID-19",ratechoice,"Per Month in Kansas (per 1,000)"), 
       subtitle = paste(startdate,"to",enddate)) +
  scale_fill_gradientn(colors = c("deepskyblue","blue4")) +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.box.margin = margin(-10,-10,-10,-10))


# TRY FUNCTION

heatmap=function(data,ratechoice,startdate,enddate){
  
  {
    # AVG VACCINATION RATE FOR EACH COUNTY
    
    vaccountperct=aggregate(ttt$'Vaccination Count', by = list(County = ttt$County), FUN = sum,na.rm=T)  # total vaccination for each county
    colnames(popperct)<-c("County","Pop")
    colnames(vaccountperct)<-c("County","vacttcount")
    vacrate_dat=merge(vaccountperct,popperct,by="County")
    vacrate_dat$vac_rate_1K=1000*vacrate_dat$vacttcount/as.numeric(vacrate_dat$Pop)                   
    vacrate_dat
    vacrate_dat=merge(vacrate_dat,fips_KS,by="County")
    head(vacrate_dat)
    
    # AVG TESTING RATE FOR EACH COUNTY
    
    testcountperct=aggregate(ttt$'Total Tests', by = list(County = ttt$County), FUN = sum,na.rm=T)  # total #tests for each county
    colnames(testcountperct)<-c("County","testttcount")
    testrate_dat=merge(testcountperct,popperct,by="County")
    testrate_dat$test_rate_1K=1000*testrate_dat$testttcount/as.numeric(testrate_dat$Pop)                   
    testrate_dat
    testrate_dat=merge(testrate_dat,fips_KS,by="County")
    head(testrate_dat)
    
    # AVG POSITIVITY RATE FOR EACH COUNTY
    
    posrate_dat=aggregate(cbind(ttt$`PCR Positive Count`,ttt$`PCR Negative Count`), by = list(County = ttt$County), FUN = sum,na.rm=T)   
    colnames(posrate_dat)<-c("County","ttPOS","ttNEG")
    posrate_dat$pos_rate_1K=1000*posrate_dat$ttPOS/(posrate_dat$ttPOS+posrate_dat$ttNEG)                 
    posrate_dat
    posrate_dat=merge(posrate_dat,fips_KS,by="County")
    head(posrate_dat)
    
    # STACK RATE - TABLES
    
    names(vacrate_dat);vacrate_=vacrate_dat[,c(7,4)];vacrate_
    names(testrate_dat);testrate_=testrate_dat[,c(7,4)];testrate_
    names(posrate_dat);posrate_=posrate_dat[,c(7,4)];posrate_
    rates=merge(vacrate_,testrate_,by="fips")
    rates=merge(rates,posrate_,by="fips")
    head(rates)
    colnames(rates)[2:4]<-c("Vaccination Rate","Testing Rate","Positivity Rate" )
    head(rates)
    rates_long<-rates %>% gather(key, value, c(`Vaccination Rate`,
                                               `Testing Rate`, 
                                               `Positivity Rate`))  
    rates_long
  }
  
  if(ratechoice=="Vaccination Rate"){col=c("deepskyblue","blue4")}else
    if(ratechoice=="Positivity Rate"){col=c("yellow", "red")}else
    {col=c("yellow", "forestgreen")}
  plot_usmap(data = rates, values = ratechoice, include = "KS") + 
    labs(title = paste("Average COVID-19",ratechoice), 
         subtitle = paste(subtitle = paste(startdate,"to",enddate)))  +
    scale_fill_gradientn(colors = col) +
    theme(legend.position = "top",
          legend.justification = "right",
          legend.box.margin = margin(-10,-10,-10,-10))
}

heatmap(data=ttt,ratechoice=ratechoice,startdate = startdate,enddate = enddate)



#############################
# SHINY APP
#############################


ui<-shinyUI(
  fluidPage(
    navbarPage("COVID Plots",
               tabPanel("Line Plot and Heat Map",
                        sidebarPanel(
                          
                          #select key outcome 
                          selectInput("ratechoice", label = strong("Outcome"),
                                      choices = unique(plotdata$key)),
                          
                          #select county
                          selectInput("county", label = strong("County"),
                                      choices = unique(plotdata$County)),
                          
                          #select race
                          selectInput("re", label = strong("Race/Ethnicity"),
                                      choices = unique(plotdata$RE)),
                          
                          #put dates
                          selectInput("startdate", label=("Starting Date"),choices=unique(plotdata$yearmondate),selected = "Mar 2020"),
                         
                          
                          selectInput("enddate", label=("Ending Date"),choices=unique(plotdata$yearmondate),selected="Oct 2021"),
                          
                        ), #sidebarPanel
                        
                        mainPanel(
                          
                          plotOutput("lineplot", height = "300px") ,
                         
                          plotOutput("heatmap", height = "300px")
                         
                        )# mainPanel  
               )  
     
               
    )#navbarPage
  )
)





server <- function(input, output) {
   
  # LINE PLOT
  
  data1 <- reactive({  
    plotdata %>%
      filter(
        key == input$ratechoice, 
        County == input$county,
        RE == input$re,
        yearmondate>=input$startdate,
        yearmondate<=input$enddate
        )
    
  })
   
  
 output$lineplot <- renderPlot({
 
   ggplot(data1(), aes(x = yearmondate, y = value)) +
     geom_point(size = 3)+
     geom_line() +
     scale_x_yearmon(breaks = data1()$yearmondate)+
     theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
     labs(title = paste(input$ratechoice,"of",input$county ,"County"), 
          subtitle = paste("Race/Ethnicity:", input$re), 
          x = "Date", y = paste(input$ratechoice))
   
  })

  
 # HEAT MAP
 
 data2<-reactive({
   
   ttt=tt  %>%
     filter(
       yearmondate>=input$startdate ,
       yearmondate<=input$enddate
     )
 })  
 
 output$heatmap <- renderPlot({
   
   heatmap(data=data2(),ratechoice=input$ratechoice,startdate=input$startdate,enddate=input$enddate)
   
 })
 

}


shinyApp (ui=ui, server = server)


