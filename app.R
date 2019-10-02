
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("rgdal")) install.packages("rgdal")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("dplyr")) install.packages("dplyr")
library(tidyverse)
library(dplyr)
library(shinyWidgets)
library(rgdal)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
Sys.setlocale(locale="Czech")
data<-read.csv('medicinska-oprema-u-radiologiji-2017.csv',header = TRUE,encoding="UTF-8")
head(data)

datane<-data%>%filter(Status=="Ne koristi se")%>%group_by(Okrug)%>%summarise(n=n())
datada<-data%>%filter(Status=="Koristi se")%>%group_by(Okrug)%>%summarise(n=n())

datar<-data%>%
   group_by(Okrug)%>%summarise(Koristi_se=sum(Status == "Koristi se"))
datara<-data%>%
   group_by(Okrug)%>%summarise(Ne_koristi_se=sum(Status == "Ne koristi se"))%>%select("Ne_koristi_se")

podgrupa<-array(unique(data$Podgrupa))
t=podgrupa[7]
podgrupa[7]=podgrupa[11]
podgrupa[11]=t
a<-(matrix( nrow=26, ncol=length(podgrupa)))
i=1
for (variable in podgrupa) {dse<-data%>%filter(Status=="Koristi se")%>%
   group_by(Okrug)%>%summarise(statistika=sum(Podgrupa == variable))

a[,i]=dse$statistika

i=i+1
}
colnames(a)<-podgrupa

DF<-data.frame(datar, datara,a)

fd<-DF%>%filter(Okrug=='Raški' | Okrug=='Raški - Novi Pazar')
a<-colSums(fd[,2:ncol(fd)])
DF[16,]=c('Raški',a)
DF <- DF[-c(17),] 
DF<-data.frame(DF)
DF$Okrug<-factor(DF$Okrug)
ND<-data.frame(DF)




temp1<-data.frame('Kosovsko-Mitrovački',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
temp2<-data.frame('Pećki',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
temp3<-data.frame('Kosovski',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
temp4<-data.frame('Prizrenski',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
temp5<-data.frame('Kosovsko-Pomoravski',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
colnames(temp1)=colnames(ND)
colnames(temp2)=colnames(ND)
colnames(temp3)=colnames(ND)
colnames(temp4)=colnames(ND)
colnames(temp5)=colnames(ND)
DF=rbind(DF,temp1,temp2,temp3,temp4,temp5)
NK=rbind(ND,temp1,temp2,temp3,temp4,temp5)

NK[24,]=DF[1,]
NK[27,]=DF[21,]
NK[10,]=DF[2,]
NK[12,]=DF[24,]
NK[13,]=DF[17,]
NK[14,]=DF[18,]
NK[6,]=DF[5,]
NK[18,]=DF[10,]
NK[16,]=DF[20,]
NK[7,]=DF[6,]
NK[26,]=DF[8,]
NK[25,]=DF[7,]
NK[30,]=DF[23,]
NK[17,]=DF[9,]
NK[11,]=DF[25,]
NK[9,]=DF[12,]
NK[21,]=DF[15,]
NK[20,]=DF[14,]
NK[22,]=DF[16,]
NK[23,]=DF[22,]
NK[8,]=DF[19,]
NK[29,]=DF[4,]
NK[4,]=DF[27,]
NK[28,]=DF[11,]
NK[3,]=DF[27,]
NK[4,]=DF[26,]
NK[2,]=DF[29,]
NK[5,]=DF[28,]
NK[1,]=DF[30,]
NK[15,]=DF[3,]
NK[19,]=DF[13,]








ND$Okrug<-factor(ND$Okrug)


url<-'http://alas.matf.bg.ac.rs/~mi09109/sr_regional.geojson'

#res <- readOGR(dsn = url)
res <- readOGR('sr_kos_regional.geojson.txt')


colnames(ND)<-c('Okrug','Ukupan broj u upotrebi','Ukupno nije u upotrebi',podgrupa)
colnames(NK)<-c('Okrug','Ukupan broj u upotrebi','Ukupno nije u upotrebi',podgrupa)
colnames(data)<-c('Sifra','Naziv_aparata','Status','Status_(razlog)','Statistcki_region','Okrug','Godina_proizvodnje','Godina_nabavke','Sifra_grupe','Grupa','Sifra_podgrupe','Podgrupa')
data$Sifra_grupe=NULL
data$Sifra_podgrupe=NULL
data$Grupa=NULL
list<-colnames(ND)
list<-list[list!='Okrug']



filter<-colnames(data)

linkovi<-c('#','#','#','#','#',
           'http://juznobacki.okrug.gov.rs/sr/index.php',
           'http://juznobanatski.okrug.gov.rs/',
           'http://www.srednjobanatski.okrug.gov.rs/index.php?lang=sr', 
           'http://pirotski.okrug.gov.rs/',
           'http://borski.okrug.gov.rs/',
           'http://zlatiborski.okrug.gov.rs/index.php/sr',
           'http://www.zapadnobacki.okrug.gov.rs/',
           'http://www.severnobacki.okrug.gov.rs/',
           'http://www.severnobanatski.okrug.gov.rs/',
           'http://branicevski.okrug.gov.rs/',
           'http://www.sremski.okrug.gov.rs/',
           'https://moravicki.okrug.gov.rs/',
           'http://nis.okrug.gov.rs/',
           'http://www.podunavskiokrug.rs/',
           'http://puo.rs/',
           'http://rasinski.okrug.gov.rs/?lng=cir',
           'http://raski.okrug.gov.rs/',
           'http://www.toplicki.okrug.gov.rs/',
           'http://www.beograd.rs/',
           'http://www.kolubarski.okrug.gov.rs/',
           'http://macvanski.okrug.gov.rs/macokr/',
           'http://www.sumadijski.okrug.gov.rs/',
           'http://www.pcinjski.okrug.gov.rs/',
           'http://jablanicki.okrug.gov.rs/',
           'http://zajecarski.okrug.gov.rs/'
           
           
           
           
)



ui <-fluidPage(theme = shinytheme("cerulean"),
               tabsetPanel(
                  tabPanel('vizuelni statisticki prikaz',h1('Broj dostupne radioloske opreme po regionima Srbije',align="center"),
                           
                           
                           sidebarPanel(     h3('Izbor opreme',align='center'),
                                             radioButtons("izbor",NULL , list)
                                             
                           ),
                           
                           mainPanel(
                              leafletOutput(outputId = "mymap",height=800)
                           )
                           
                  ),
                  tabPanel('pregled i pretraga',h1('Pregled i pretraga radioloske opreme',align="center"),
                           
                           
                           sidebarPanel(h4('Parametri  pretrage',align="center"),width=2,
                                        selectizeGroupUI(
                                           id="my-filters",
                                           inline=FALSE,
                                           params = list(
                                              filt1=list(inputId=filter[1],title=filter[1],placeholder='izbor'),
                                              filt2=list(inputId=filter[2],title=filter[2],placeholder='izbor'), 
                                              filt3=list(inputId=filter[3],title=filter[3],placeholder='izbor'),
                                              filt4=list(inputId=filter[4],title=filter[4],placeholder='izbor'),
                                              filt5=list(inputId=filter[5],title=filter[5],placeholder='izbor'),
                                              filt6=list(inputId=filter[6],title=filter[6],placeholder='izbor'),
                                              filt7=list(inputId=filter[7],title=filter[7],placeholder='izbor'),
                                              filt8=list(inputId=filter[8],title=filter[8],placeholder='izbor'),
                                              filt9=list(inputId=filter[9],title=filter[9],placeholder='izbor')
                                           )
                                        ) 
                           ),
                           
                           
                           mainPanel(div(tableOutput('table'),style = "font-size:90%")))
                  
                  
                  
               )
)

server <- function(input, output,session) {
   output$mymap <- renderLeaflet({
      
      t<-input$izbor
      maks= max(as.numeric(ND[,t]))
      bins<-seq(0,round(maks+0.2*maks),by=round(maks/7))
      pal<-colorBin("YlOrRd", domain=NK[,t],bins=bins)
      
      
      
      labels=paste("<a href=",linkovi,'>',NK$Okrug," okrug",'</a>',"<p>",NK[,t],"</p>",sep = "")
      
      
      
      leaflet(options = leafletOptions(minZoom = 7.4)) %>%
         addProviderTiles(providers$JusticeMap)%>%
         setView( lng = 20.772202
                  , lat = 44.320409
                  , zoom = 7.4 )%>%
         setMaxBounds( lng1 = 18.636848
                       , lat1 = 46.124564
                       , lng2 = 22.781947
                       , lat2 = 42.143551 ) %>%
         addPolygons(data=res,color = 'royalblue',weight = 1.5,smoothFactor=0.5,fillOpacity=0.8,fillColor = pal(as.numeric(NK[,t])),
                     
                     highlight=highlightOptions(weight=5,color='royalblue',fillOpacity=0.7,bringToFront = TRUE),
                     label = paste(NK$Okrug," okrug"),popup = lapply(labels, HTML))%>%addLegend(pal = pal,values = NK[,t],opacity = 0.7)
      
      
   })
   
   res_mod <- callModule(
      module = selectizeGroupServer,
      id = 'my-filters',
      data=data,
      vars=filter
      
   )
   output$table<-renderTable({
      res_mod()}
   )
   
}

shinyApp(ui = ui, server = server)

