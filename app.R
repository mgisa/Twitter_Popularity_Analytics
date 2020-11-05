
#Loading Packages
#________________
#install.packages(c("shiny","shinydashboard","semantic.dashboard",
                   #"DT","plotly","lattice","scales"))
#Loading libraries
#____________________
if (interactive()){
    library(shiny)
    library(graphics)
    library(shinydashboard)
    library(graphics)
    library(semantic.dashboard)
    library(tidyverse)
    library(DT)
    library(plotly)
    library(scales)
    library(lattice)
    library(leaflet)
    library(leaflet.providers)
    #Setting working directory
    setwd("~/Desktop/Twitter_Popularity_Dashboard/")
    
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #++++                TWITTER POPULARITY SHINNY APP.  +++++++++++++++
    #+++++               EAST AFRICA CENTRAL BANKS       ++++++++++++++++
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    
    ui <-dashboardPage(
        dashboardHeader(title = "Dashboard Demo",color = "blue", 
                        inverted = TRUE, disable = FALSE #,
                        # titleWidth = 450
        ),
        dashboardSidebar(side = "left",size = 'thin', color = "teal",
                         sidebarMenu(
                             menuItem(tabName = "home",  text = "Get Started",icon = icon("home")),
                             menuItem(tabName = "home1",  text = "Total Twitter Users",icon = icon("table")),
                             menuItem(tabName = "main",  text = "Twitter Followers",icon = icon("car")),
                             menuItem(tabName = "main1", text =  "Twitter Friends ",icon = icon("car")),
                             menuItem(tabName = "main2", text =  "Tweet Favorites ",icon = icon("car")),
                             menuItem(tabName = "main3", text =  "Tweet Listed ",icon = icon("car")),
                             menuItem(tabName = "main4", text =  "Tweet Statuses ",icon = icon("car")),
                             #-------------------------------------------------------
                             menuItem(tabName = "main5", text = "Golden Ratio ",icon = icon("heart")),
                             menuItem(tabName = "main6", text = "Banks Geolocation",icon = icon("home")),
                             menuItem(tabName = "extra", text = "Data Tables",icon = icon("table"))
                         )),
        dashboardBody(
            tabItems(
                selected = 1,
                tabItem(
                    tabName = "home",
                    fluidRow(
                        box(width = 16,height = 20,
                            title = "DashBoard Overview",
                            color = "green", ribbon = TRUE, title_side = "top right",
                            column(width = 10,
                                   htmlOutput("Dash_Description")
                            )
                        )
                    )
                ), #End of first panel
                
                tabItem(
                  tabName = "home1",
                  fluidRow(
                    box(width = 16,height = 20,
                        title = "Country Total Active Twitter Users (Population Percentage)",
                        color = "green", ribbon = TRUE, title_side = "top right",
                        column(width = 10,
                               plotOutput("barplot00")
                        )
                    )
                  )
                ), 
                tabItem(
                    tabName = "main",
                    fluidRow(
                        box(width = 16,height = 20,
                            title = "Banks Twitter Followers",
                            color = "green", ribbon = TRUE, title_side = "top right",
                            column(width = 10,
                                   plotOutput("barplot")
                            )
                        )
                    )
                ),
                #selected = 1,
                tabItem(
                    tabName = "main1",
                    fluidRow(
                        box(width = 16,height = 20,
                            title = "Banks Twitter Friends",
                            color = "green", ribbon = TRUE, title_side = "top right",
                            column(width = 10,
                                   plotOutput("barplot1")
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "main2",
                    fluidRow(
                        box(width = 16,height = 20,
                            title = "Tweet Favorites",
                            color = "green", ribbon = TRUE, title_side = "top right",
                            column(width = 10,
                                   plotOutput("barplot2")
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "main3",
                    fluidRow(
                        box(width = 16,height = 20,
                            title = "Tweet Listed",
                            color = "green", ribbon = TRUE, title_side = "top right",
                            column(width = 10,
                                   plotOutput("barplot3")
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "main4",
                    fluidRow(
                        box(width = 16,height = 20,
                            title = "Tweet Statuses",
                            color = "green", ribbon = TRUE, title_side = "top right",
                            column(width = 10,
                                   plotOutput("barplot4")
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "main5",
                    fluidRow(
                        box(width = 16,height = 20,
                            title = "Twitter Popularity Mesure (Golden Ratio)",
                            color = "green", ribbon = TRUE, title_side = "top right",
                            column(width = 10,
                                   plotOutput("barplot5")
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "main6",
                    fluidRow(
                        box(width =16,height = 30,
                            title = "Visually Map Presentation
              of Central Bank Follower's Counts",
                            color = "green", ribbon = TRUE, title_side = "top right",
                            column(width = 10,
                                   leafletOutput("map")
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "extra",
                    fluidRow(
                        DT::dataTableOutput("carstable")
                    )
                )
            )
        ), theme = "cerulean" 
    )
    
    server <- shinyServer(function(input, output, session) {
        #Importing and Prepare data
EA_CBanks <-read.csv("EA_CBanks.csv",
                     stringsAsFactors = FALSE, header = TRUE)
        EA_data<-EA_CBanks%>%
            mutate(lng=as.numeric(Longitude),
                   lat=as.numeric(Latitude))%>%
            dplyr::select(Twitter_screen_name,Central_Bank_name,lng, lat, Followers)
        #__________________
        #Rendering the plots
        #--------Step1: Rendering Home Info_______
        output$Dash_Description<-renderUI({
            tags$div( 
                HTML('<p style="color:red; font-size:14pt">
      <h1 style="color:blue; font-family:courier;text-align:center;"> 
      Twitter Influence and Popularity Analytics </h1>
      
      <p style="color:red; font-size:14pt">
      <h3 style="color:black; font-family: verdana;"> Case Study: East African Central Banks </h3>
            
      
      <p style="color:black; font-size: 12pt">
      It says that social influence on Twitter is mainly related to popularity rather than reputation, 
      but this growth of influence with popularity is sublinear. 
        According to Twitter’s Q3 2019 repot, Twitter has 145 million daily active users. 
        That makes the several individual celebrities, private and government entities use 
        Twitter as a perfect channel to publicly communicate with their audiences. 
        Here we are experimenting with the popularity measures among the East African Central Banks
        on Twitter Social Media Communication Platform. </p>
        
        <p style="color:black; font-size: 12pt">
        Understanding your Twitter popularity and the popularity of other related
        Tweeters allows you to boost your twitter online influence. With this, we 
        scraped twitter data behind those central banks and filter the below popularity metrics to be 
        compared:</p>
        
        <ul style="list-style-type:disc;">
     
  <li> <h4 style="color:red;"> Twitter Followers</h4> </li> 
  This refers to the tracking of twitter users who follow your posted communication. The identification of exact number
  of the twitter followers and their networks has become a crucial tool to detect the level of
 pupularity, cerebrity and how far the info disseminated can reach in Twitter Network.
  <li><h4 style="color:red;"> Twitter Friends </h4> </li>
  It refers to those twitter users followed by a referential user in Twitter Network.
   <li><h4 style="color:red;"> Tweet Favorite </h4> </li>
   Favorite insdicates that a tweet 
   is well-liked or popular among twitter users in network. It also indicates how many
   times the tweet has been liked by users.
   <li><h4 style="color:red;"> Tweet Statuses </h4> </li>
   This is a collection of the most recent tweets posted by the authenticating
   user and the users they follow.
   <li><h4 style="color:red;"> Tweet List </h4> </li>
   With this people are re-categorizing, and rating the people they really want to follow.
   This is a new indicator of popularity. The Listed Count
   shows how many people have added you to a list.
  <li><h4 style="color:red;"> Golden Ratio </h4> </li>
  This the ratio between followers and friends counts for the Twitter user. It identifies and quantifies 
  the level of user popularity and influence on Twitter Network. For more details read 
  <p><a href="https://techcrunch.com/2009/08/26/twitters-golden-ratio-that-no-one-likes-to-talk-about/">Golden Ratio</a></p>
</ul>

  <p style="color:black; font-size: 12pt">
  This dashboard is visually presenting the above influence and popularity metrics on 
   East Africa Central Banks. This helps to efficiently compare them basing on those Twitter 
   popularity measures and see how their audience are behaving towards the entire bank communication policy. 
   Next time, we wish to dig deep into those followers and friends to widely identify
  who are they?, what are the economic sectors they belong to?,where are the followers from?,
 what kind of lifestyle do they lead?,what are their interests/hobbies?,etc. And the visually dashborad will
 accurately provide an critical image and overview on the audience type of those central banks. Hence, to know who to
         disseminate and communicate the central bank policies and strategies.</p>
         
         <p style="color:blue; font-size: 14pt">
         
         Here, Enjoy this visually Dashboard by selecting any popularity metric!!!
         
         </p>')
            )
        })
        #_____________________________________________________________
        
        #__________Step2: Rendering the Total Twitter Users barplot________
        pal1 <- RColorBrewer::brewer.pal(n = 10, name = 'Set3')
        output$barplot00<-renderPlot({EA_CBanks%>%
            ggplot( aes(x = reorder(Country,Total_Twittter_Users), y =Total_Twittter_Users))+
            geom_bar(stat = 'identity', aes(fill = Country)) +
            labs(title = "Country Total Active Twitter Users at the end of October 2020 ", 
                 #subtitle = "Country Total Twitter Users at the end October 2020 ",
                 x = 'Country', y = 'Total Twitter Users ',
                 caption = "Twitter Analytics by Mgisa") +
            scale_fill_manual(values = pal1, name = "EA Central Banks") +
            theme_bw() +
            #facet_grid(screenname~.) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            geom_text(aes(label=str_c(Total_Twittter_Users,"%")),vjust=-0.1,
                      angle= 360, size = 6,color="black") +
            #ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
            theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                  axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                  axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
                  axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
                  plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none") 
        })
          #______________________________Step3: Rendering The Followers Barplot
        output$barplot <- renderPlot({EA_CBanks%>%
                ggplot( aes(x = reorder(Country,Followers), y =Followers))+
                geom_bar(stat = 'identity', aes(fill = Country)) +
                labs(title = "Eastern African Central Banks Popularity Scores", 
                     subtitle = "Twitter Followers at the end of October 2020 ",
                     x = 'Country', y = 'Followers Count',
                     caption = "Twitter Analytics by Mgisa") +
                scale_fill_manual(values = pal1, name = "EA Central Banks") +
                theme_bw() +
                #facet_grid(screenname~.) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                geom_text(aes(label=str_c(Followers)),vjust=-0.1,
                          angle= 360, size = 6,color="black") +
                #ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
                theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
                      axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
                      plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none") 
        })
        
        #__________Step4: Rendering Friends barplot________
        
        output$barplot1 <- renderPlot({
            EA_CBanks%>%
                ggplot( aes(x = reorder(Country,Friends), y =Friends))+
                geom_bar(stat = 'identity', aes(fill = Country)) +
                labs(title = "Eastern African Central Banks Popularity Scores", 
                     subtitle = "Twitter Friends at the end of October 2020 ",
                     x = 'Country', y = 'Friends Count',
                     caption = "Twitter Analytics by Mgisa") +
                scale_fill_manual(values = pal1, name = "EA Central Banks") +
                theme_bw() +
                #facet_grid(screenname~.) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                geom_text(aes(label=str_c(Friends)),vjust=-0.1,
                          angle= 360, size = 6,color="black") +
                #ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
                theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
                      axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
                      plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none")
        })
        #_______________________Step5: Rendering Tweet favorites
        output$barplot2 <- renderPlot({
            EA_CBanks%>%
                ggplot( aes(x = reorder(Country,Favourites), y =Favourites))+
                geom_bar(stat = 'identity', aes(fill = Country)) +
                labs(title = "Eastern African Central Banks Popularity Scores", 
                     subtitle = "Total Tweet Favourites at the end of October 2020 ",
                     x = 'Country', y = 'Favourites Count',
                     caption = "Twitter Analytics by Mgisa") +
                scale_fill_manual(values = pal1, name = "EA Central Banks") +
                theme_bw() +
                #facet_grid(screenname~.) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                geom_text(aes(label=str_c(Favourites)),vjust=-0.1,
                          angle= 360, size = 6,color="black") +
                #ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
                theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
                      axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
                      plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none")
            
        })
        #__________________________Step6: Rendering Tweet Listed
        output$barplot3 <- renderPlot({
            EA_CBanks%>%
                ggplot( aes(x = reorder(Country,Listed), y =Listed))+
                geom_bar(stat = 'identity', aes(fill = Country)) +
                labs(title = "Eastern African Central Banks Popularity Scores", 
                     subtitle = "Total Tweet Listed at the end of October 2020 ",
                     x = 'Country', y = 'Total Tweet Listed Count',
                     caption = "Twitter Analytics by Mgisa") +
                scale_fill_manual(values = pal1, name = "EA Central Banks") +
                theme_bw() +
                #facet_grid(screenname~.) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                geom_text(aes(label=str_c(Listed)),vjust=-0.1,
                          angle= 360, size = 6,color="black") +
                #ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
                theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
                      axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
                      plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none")
        })
        #_______________________Step7: Rendering Tweet Statuses
        output$barplot4 <- renderPlot({
            EA_CBanks%>%
                ggplot( aes(x = reorder(Country,Statuses), y =Statuses))+
                geom_bar(stat = 'identity', aes(fill = Country)) +
                labs(title = "Eastern African Central Banks Popularity Scores", 
                     subtitle = "Total Tweet Statuses at the end of October 2020 ",
                     x = 'Country', y = 'Total Tweet Statuses Count',
                     caption = "Twitter Analytics by Mgisa") +
                scale_fill_manual(values = pal1, name = "EA Central Banks") +
                theme_bw() +
                #facet_grid(screenname~.) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                geom_text(aes(label=str_c(Statuses)),vjust=-0.1,
                          angle= 360, size = 6,color="black") +
                #ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
                theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
                      axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
                      plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none") 
            
            
        })
        #__________Step8: Rendering Golden Ratio barplot________
        
        output$barplot5 <- renderPlot({
            EA_CBanks[-2,]%>% 
                ggplot(aes(x = reorder(Country,Popularity_Rate), y =Popularity_Rate))+
                geom_bar(stat = 'identity', aes(fill = Country)) +
                labs(title = "Eastern African Central Banks Popularity Scores", 
                     subtitle = "Twitter Popularity Rate (Golden Ratios) a the end of October 2020 ",
                     x = 'Country', y = 'East African Popularity Ratio',
                     caption = "Twitter Analytics by Mgisa") +
                scale_fill_manual(values = pal1, name = "EA Central Banks") +
                theme_bw() +
                #facet_grid(screenname~.) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
            geom_text(aes(label=str_c(Popularity_Rate, "%")),vjust=-0.1,
                      angle= 360, size = 6,color="black")+
                #ggtitle("Overall Sentiment Frequencies behind the BNR Tweets by User's Sentence") +
                theme(axis.text.x = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.text.y = element_text(angle = 360,face = "bold",colour = "black",size = 15),
                      axis.title.x = element_text(size = 10,angle = 90,vjust = 5,face = "bold"),
                      axis.title.y = element_text(size = 15,angle = 90,vjust = 0.5,face = "bold",colour = "black"),
                      plot.title = element_text(size = 25,face = "bold",colour="red"),legend.position = "none")
        })
        #__________Step9: Rendering Followers Map________
        #_________________Setting Intervals of Bank Followers Concetration________________
        
        # Call the color function (colorNumeric) to create a new palette function
        pal <- colorNumeric(c("darkgreen", "darkblue", "red"), 1:6)
        # Pass the palette function a data vector to get the corresponding colors
        pal(c(1,3,6))
        #__________________________
        p = colorFactor(palette = c("darkgreen","darkblue","red"), 
                        domain = c("High","Moderate","Poor"),
                        ordered = T)
        
        getColor <- function(EA_data) {
            sapply(EA_data$Followers, function(Followers) {
                if(Followers >= 100000) {
                    "darkgreen"
                } else if(Followers < 100000 & Followers > 50000) {
                    "darkblue"
                } else {
                    "red"
                } })
        }
        #_______________________________
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = getColor(EA_data)
        )
        output$map <- renderLeaflet({
            leaflet(data=EA_data) %>% 
                addProviderTiles("OpenStreetMap") %>%
                addTiles() %>%
                setView(lat = 1.9577, lng = 37.2972, zoom = 8) %>% 
                #setMaxBounds(lng1 = 27, lat1 = 4, lng2 = 32, lat2=4) %>%
                addCircleMarkers(~lng,~lat,label= ~as.character(Central_Bank_name)) %>% 
                addAwesomeMarkers(~lng, ~lat, icon = icons, label= ~as.character(Central_Bank_name))%>%
                addLegend(position = "topright", pal = p, 
                          values = c("High","Moderate","Poor"),
                          title = "Central Bank \n Popularity Score")
        })
        
        #__________Step10: Rendering Data table________
        output$carstable <- DT::renderDataTable(
            DT::datatable(data=EA_CBanks,
                          extensions = "Buttons",
                          options = list(
                              dom="lfrtBip",#Bottom of table      #"Blfrtip"(Top of table),
                              buttoms=
                                  list("copy",list(
                                      extend = "collection",
                                      buttons=c("copy","csv","excel","pdf","print"),
                                      text="Download"
                                  )),                     # End of buttons customization
                              lengthMenu=list(c(10,20,50,100,-1),         #Declare values
                                              c(10,20,50,100, "All")      #Declare titles
                              ),                          #End of lengthMenu Customization
                              pageLength=10
                          ) #End of options
            )#End of dataTables
        ) #End of REnderDataTable
    })
    
    shinyApp(ui, server)
    
} #End of interactive R session#__________________

