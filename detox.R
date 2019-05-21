                                                      #The Rejection Detox

# All the libraries needed to make this app
#for the images used in the app
library(png)                                            

#for the shiny App
library(shiny)
library(shinydashboard)

#UI PART
#Fluidrow will help to create rows in a page and within the  row you can decide how many boxes you want and what should be in the boxes


#First Row
#It takes in three inputs ie the name of the person and the city where he lives plus the level of pain arisen from the rejection
frow1<-fluidRow(
  
  
box(
     
   title = "Pain Level",status = "danger", solidHeader = TRUE,collapsible = TRUE,
      
   textInput("name", "What is Your Name?", value = "", width = 600,
                placeholder = NULL),
   textInput("place", "Which city do you live in?(eg Boston,MA)", value = "", width = 600,
                placeholder = NULL),
   # Sidebar layout with a input and output definitions ----    
   sliderInput("Bin", "Pain Level:", 0, 100, 0,width = 700),
      
    width='400px'
     
    ))
    
#Now based on the ouput of the input we need to have UI for the Output
#The second row would have one box which gives the personalized message 
frow2<-fluidRow({
  
  box(
       title="Result"
      ,status="success"
      ,solidHeader=TRUE
      ,collapsible=TRUE
      , width="300px",height="200px",
      verbatimTextOutput("value",placeholder = TRUE),  
      tags$head(tags$style("#value{color:black; font-size:20px; font-style:bold;  width:100%; background_color:#48ca3b;}"))
    )
                  })
#The third row would have two boxes ,each showcase an image of a quote ,all this based on your input
frow3<-fluidRow(
  
  box(title="Quote for you"
      ,status="primary"
      ,solidHeader=TRUE
      ,collapsible=TRUE,
      background  ="black",
      imageOutput("image2")
      
  ),
  
      
      box(title="Quote for you"
          ,status="primary"
          ,solidHeader=TRUE
          ,collapsible=TRUE,
          background  ="black",
          imageOutput("image3")

))

#The fourth row contains two boxes ,first box requires the user to chose between alcohol or desserts and based on that answer plus the city
#We use the Yelp Api to gather data of nearby bars or dessert places
#second box showcase list of 15 tv shows to watch when we are depressed.
frow4<-fluidRow(
  box(title="What do you like most?"
      ,status="warning"
      ,solidHeader=TRUE
      ,collapsible=TRUE,
      background  ="black",
      height="600px",
      radioButtons("rb", "Choose one:",
                   choiceNames = list("Alcohol","Desserts"
                     
                   ),
                   choiceValues = list("Alcohol","Desserts"))
      ,tableOutput('text')
        ),
  box( title="Some TV shows to watch when you are feeling crappy"
         ,status="warning"
       ,solidHeader=TRUE
       ,collapsible=TRUE,
       
       tableOutput('text2')
))
# combine the fluid rows to make the body
 
ui <- dashboardPage(
  #Dashboard header carrying the title of the dashboard
  header <- dashboardHeader(title = "Rejection Detox"),  
  #Sidebar content of the dashboard
  sidebar <- dashboardSidebar(
  sidebarMenu(menuItem("Step_ONE", tabName = "dashboard", icon = icon("dashboard")))),
  body <- dashboardBody(tabItems(tabItem(tabName= "dashboard",frow1,frow2,frow3,frow4))))
  
 
  
  
  # create the server functions for the dashboard  
  server <- function(input, output,session) { 
    
    #For the pseronalised message
    output$value <- renderPrint({
      
      if(input$Bin >1 && input$Bin<30)
      {
        cat("Hey",input$name," what are you doing here,You are already handling it well")
        
        
      }
      
      if(input$Bin >30 && input$Bin<70)
      {
        cat("Hey",input$name,", Dont worry,I have got you. Some minion quotes and a small(if you are calorie conscious) scoop of ice-cream will not make you perfectly fine but it will comfort you for a while.")
       
      }
      if(input$Bin >70)
      {
        width=600
       cat("Let me guess",input$name," ,this was your dream job....Its gonna hurt but you are gonna rebound soon.For starters put your PJ's and watch the some weird shows (like Toddlers vs Tiara).Plus dont care about the calories, if u want to eat an entire tub of Ben and Jerry's ice-cream ,you derserve it")
      }
      
      width = getOption("900px")
    })
   
    #For the quotes
     output$image2 <- renderImage({
    if(input$Bin ==0)
    {
      return(list(
        src = "C:\\Users\\prabh\\Desktop\\black.png",
        filetype = "image/png",
        width="100%",
        height="100%",
        aligh="left"
      ))
    }
    
    if(input$Bin >1 && input$Bin<30) {
      return(list(
        src = "C:\\Users\\prabh\\Desktop\\strong.png",
        filetype = "image/png",
        width="100%",
        height="100%",
        aligh="left"
        
        
      ))
    } else if (input$Bin >30 && input$Bin<=70) {
      return(list(
        src = "C:\\Users\\prabh\\Desktop\\minion.jpg",
        filetype = "image/jpg",
        width="100%",
        height="100%",
        aligh="left"
        
      ))
    }  else if (input$Bin >70) {
      return(list(
        src = "C:\\Users\\prabh\\Desktop\\funnyrr.png",
        filetype = "image/png",
        width="100%",
        height="100%",
        aligh="left"
      ))
    }
    
  }, deleteFile = FALSE)
   
   output$image3 <- renderImage({
      if(input$Bin ==0)
      {
        return(list(
          src = "C:\\Users\\prabh\\Desktop\\black.png",
          filetype = "image/png",
          width="100%",
          height="100%",
          aligh="right"
          
        ))
      }
      
      if(input$Bin >1 && input$Bin<30) {
        return(list(
          src = "C:\\Users\\prabh\\Desktop\\strong.png",
          filetype = "image/png",
          width="100%",
          height="100%",
          aligh="right"
        ))
      } else if (input$Bin >30 && input$Bin<=70) {
        return(list(
          src = "C:\\Users\\prabh\\Desktop\\funnyr.png",
          filetype = "image/png",
          width="100%",
          height="100%",
          aligh="right"
        ))
      }  else if (input$Bin >70) {
        return(list(
          src = "C:\\Users\\prabh\\Desktop\\funyrrr.jpg",
          filetype = "image/jpg",
          width="100%",
          height="100%",
          aligh="right"
          
        ))
      }
      
    }, deleteFile = FALSE)
  
 
  
  #For Showcasing nearby bar and dessert places using YELP API
  output$text<-renderTable({
    #install.packages("tidyverse")
    require(tidyverse)
    require(httr)
    
    
    
    yelp <- "https://api.yelp.com"
    categories <- NULL
    limit <- 10
    radius <- 8000
    
    url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                      query = list(term = input$rb, location = input$place, 
                                   limit = limit,
                                   radius = radius))
    res <- GET(url, add_headers('Authorization' = paste("bearer",Put your API KEY here in double quote)))
    
    results <- content(res)
    
    yelp_httr_parse <- function(x) {
      
      parse_list <- list(id = x$id, 
                         name = x$name, 
                         rating = x$rating, 
                         review_count = x$review_count, 
                         latitude = x$coordinates$latitude, 
                         longitude = x$coordinates$longitude, 
                         address1 = x$location$address1, 
                         city = x$location$city, 
                         state = x$location$state, 
                         distance = x$distance)
      
      parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
      
      df <- data_frame(name=parse_list$name, 
                       rating = parse_list$rating, 
                       distance= parse_list$distance)
      df
    }
    
    results_list <- lapply(results$businesses, FUN = yelp_httr_parse)
    
    payload <- do.call("rbind", results_list)
    payload
    
  })
  
  #For showcasing the list of tv Shows
  output$text2<-renderTable({
  l<- list(c("Black-ish",	"Parks and Recreation",	"Whose Line Is It Anyway?",	"One Day at a Time",	"Supergirl",	"Brooklyn Nine-Nine",	"The Bachelor",	"Yuri!!! On Ice",	"Terrace House: Boys and Girls in the City" ,"The Joy of Painting",	"Nostalgia Critic",	"Ugly Betty",	"Big Cat Derek",	"The Ellen DeGeneres Show",	"Jane the Virgin"))
  r<-do.call(cbind.data.frame, l)
  colnames(r) <- "TV SHOW"
  r
    
  })
  }
  
  


     
      
  ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')
  
  shinyApp(ui, server)
