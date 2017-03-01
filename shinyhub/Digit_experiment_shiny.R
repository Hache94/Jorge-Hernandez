### Jorge Hernandez Perceptual experiment
#More detailed explanation available in Git-Hub.

#Ensure all libraries are installed
library(ggplot2)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(e1071)
library(class)
library(randomForest)

#Setting up the data
train <- read.table("train.txt")
test <- read.table("test.txt")
train[,1] <- as.factor(train[,1])

#Creating the color palette for visualization
CUSTOM_COLORS <- colorRampPalette(colors = c("white", "black"))
CUSTOM_COLORS_PLOT <- colorRampPalette(brewer.pal(10, "Set3"))

#Function to add lines
lineas <- function(mat,percentage = 10){
  x <- round(percentage/100*16)
  blines <- sample(1:16, x)
  puntos <- c()
  for (linea in blines){
    puntos <- append(puntos, seq(linea, 256, by = 16))
  }
  mat[puntos] <-  1
  return(mat)
}

#Function to add squares
squares <- function(mat,length = 2, number= 1){
  for (num in 1:number){
    x <- sample(1:(17-length), 1)
    y <- sample(1:(17-length), 1)
    punto <- c(x,y)
    mat[punto[1]:(punto[1]+length-1), punto[2]:(punto[2]+length-1)] <-  1
  }
  return(mat)
}

#Function to add zeroing noise
zeroing <- function(mat,percentage = 10, type="Black"){
  x <- round(percentage/100*256)
  points <- sample(1:256, x)
  if (type=="Black"){
    mat[points] <- 1
  } else {
    mat[points] <- -1
  }
  return(mat)
}

#Function to add white noise
white <- function(mat, percentage = 10, type="Normal"){
  x <- round(percentage/100*256)
  points <- sample(1:256, x)
  for (point in points){
    if (type=="Normal"){
      mat[point] <- mat[point] + rnorm(1,0,1)
    } else if (type=="Uniform") {
      mat[point] <- mat[point] + runif(1,-2,2)
    } else {
      mat[point] <- sample(c(1,-1),1)
    }
  }
  mat[mat > 1] <- 1
  mat[mat < -1] <- -1
  return(mat)
}




ui <- dashboardPage(
  
  dashboardHeader(title = "P E"),
  
  #Sidebar Menu to navigate between the different tabs
  dashboardSidebar(sidebarMenu(
    menuItem("Design", tabName = "design", icon = icon("dashboard")),
    menuItem("Bot", tabName = "bot", icon= icon("gear")),
    menuItem("Play", tabName = "play", icon= icon("play")),
    menuItem("Results", tabName = "results", icon= icon("table"))
    
  )),
  
  dashboardBody(
    
    
    tabItems(
      
      #Tab to design the experiment noise. It has conditional panels based on checkbox inputs.
      tabItem(tabName = "design",
              box(
                "Type of noise",
                checkboxInput("white", "White Noise", value = F),
                conditionalPanel(condition = "input.white",
                                 numericInput("wpercentage", "Percentage",value = 0,min = 0,max=100),
                                 selectInput("wtype", "Type of noise:", c("None","Uniform", "Normal", "Pure"))),
                
                checkboxInput("zeroing", "Zeroing Noise", value = F),
                conditionalPanel(condition = "input.zeroing",
                                 numericInput("zpercentage", "Percentage",value = 0,min = 0,max=100),
                                 selectInput("ztype", "Type of noise:", c("None","Black", "White"))),
                
                
                checkboxInput("lines", "Vertical lines", value = F),
                conditionalPanel(condition = "input.lines",
                                 numericInput("lpercentage", "Percentage",value = 0,min = 0,max=100)),
                
                checkboxInput("squares", "Small squares", value = F),
                conditionalPanel(condition = "input.squares",
                                 numericInput("spercentage", "Percentage",value = 0,min = 0,max=100),
                                 numericInput("size", "Size", value = 0,min = 0,max=16))
                
              )),
      
      #Tab to choose algoritm, similar to the noise tab but some algorithms require training which is stored
      # as a reactive value
      tabItem(tabName = "bot",
              
              box(
                selectInput("bot", "Type of Bot:", c("None","Suppor Vector Machine" = "SVM",
                                                    "K-Nearest-Neighbours"= "KNN","Random Forest"="RF")),
                conditionalPanel(condition = "input.bot == 'SVM'",
                                 selectInput("svmkernel", "Type of kernel:", c("Linear"="linear",
                                                                               "Polynomial"="polynomial",
                                                                               "Radial Basis" = "radial basis",
                                                                               "Sigmoid" = "sigmoid")),
                                 actionButton("train", "Train model"),
                                 textOutput("trainingsvm")),
                conditionalPanel(condition = "input.bot == 'KNN'",
                                 sliderInput("neighbours", "Number of Neighbours",value = 1,min = 1,max=100)),
                
                conditionalPanel(condition = "input.bot == 'RF'",
                                 sliderInput("ntrees", "Number of Trees",value = 20,min = 10,max=50),
                                 sliderInput("strees", "Sample size of tree",value = 1000,min = 200,max=2000),
                                 actionButton("trainRF", "Train model"),
                                 textOutput("trainingrf"))
                
                
              )),
      
      #Tab which allows user to play
      
      tabItem(tabName = "play",
              
              #Represents the image of the number with a fixed size
              plotOutput("imagen", width = "600px"),
              
              #Allows selection of number and obtaining a new number
              box(
                
                numericInput("guess", "Guess the number:", min=0, max=9, step=1, value= 0),
                actionButton("select", "Select"),
                actionButton("new", "New number")
              ),
              
              #Shows if user is right and what the algorithm thinks is right
              box(textOutput("acierto"),
                  textOutput("predict"))),
      
      
      #Tab with a datatable with the results
      tabItem(tabName = "results",
              dataTableOutput("tabla"))
      
    ))
)

server <- function(input, output) {
  
  
  #event reactive number which chooses a new number at random
  number <- eventReactive(input$new,{
    sample(1:2007,1)
  })
  
  #Reactive Values
  cont <- reactiveValues(count = 0, count2 = 0, #Counters of numbers and answers given
                         #table to store the results
                         tabla3= data.frame(Try= numeric(0), Number= integer(0), Guess = integer(0),
                                            # Bot_Guess = numeric(0),
                                            Type_of_bot=character(0), White_Noise_Type= character(0),
                                            White_Percentage = integer(0), Zeroing_Noise_Type= character(0),
                                            Zeroing_Percentage= integer(0), Number_of_Squares= integer(0),
                                            Size_of_Squares= integer(0),
                                            Lines= integer(0), 
                                            Right= character(0), stringsAsFactors = F),
                         #svm model
                         svm= 0,
                         trainsvm= "Train your bot",
                         #rf model
                         rf=0,
                         trainRF="Train your bot",
                         #dataframe where number after noise will be stored
                         datos= test[1,2:257])
  
  #trains the svm and says if the model is training. Work in progress.
  observeEvent(input$train,
               {
                 
                 cont$trainsvm = "Training the model. Please wait."
               })
  
  
  
  observeEvent(input$train,
               {
                 cont$trainsvm = "If gray the model is training"
                 cont$svm= svm(train[,2:257], train[,c(1)],  kernel= input$svmkernel,
                               type = "C-classification", degree = 2, coef0 = 1)
                 
               })
  
  #trains the rf and says if the model is training. Work in progress.
  observeEvent(input$trainRF,
               {
                 cont$trainrf = "Training the model. Please wait."
                 cont$trainrf = "If gray the model is training"
                 cont$rf= randomForest(train[,-1],train[,1], type= "classification"
                                       ,sampsize=c(input$strees),do.trace=TRUE,importance=TRUE,ntree=input$ntrees,
                                       forest=TRUE, keep.forest = T)
                 
                 
               })
  
  #Render text of whether the model is training
  
  output$trainingsvm <- renderText(cont$trainsvm)
  output$trainingrf <- renderText(cont$trainrf)
  
  #reactive with the random number chosen from the dataframe
  datos <- reactive(test[number(),])
  
  #reactive where the 16x16 array to represent the number is created
  aray <- reactive(array(as.vector(as.matrix(datos()[,-1])), dim = c(16, 16)))
  

  
  #Applies all the noise functions
  
  pwhite <- reactive(if (input$wtype != "None"){
    white(aray(), percentage = input$wpercentage, type= input$wtype )
  } else {
    aray()
  }
  ) 
  
  pzero <- reactive(if (input$ztype != "None"){
    zeroing(pwhite(), percentage = input$zpercentage, type= input$ztype)
  } else {
    pwhite()
  }
  ) 
  
  psquares <- reactive(if (input$squares == T){
    squares(pzero(), length = input$spercentage, number= input$size)
  } else {
    pzero()
  }
  ) 
  
  plines <- reactive(if (input$lines == T){
    lineas(psquares(), percentage = input$lpercentage)
  } else {
    psquares()
  }
  ) 
  
  #It restarts the try counter to 0 and stores the modified in the cont$datos dataframe
  
  observeEvent(input$new,{
    cont$count = 0
    cont$datos[1,1:256]=plines()
  })
  
  #Work in progress to add the bot prediction to the results dataframe
  
  # prediction <- reactive(if (cont$count>=1){
  #   if (input$bot == "KNN"){
  #     as.numeric(knn(train[,2:257], cont$datos[,1:256], train[,1], k =input$neighbours, l=0))-1
  #   }
  #   else if (input$bot == "SVM") {
  #     if (cont$svm==0){
  #       "None"
  #     } else {
  #     as.numeric(predict(cont$svm, cont$datos[,1:256]))-1
  #   }}
  #   else if (input$bot == "RF") {
  #     ifelse(cont$rf==0,"None",as.numeric(predict(cont$rf, cont$datos[,1:256], type="response")))
  #     
  #   }
  #   else {
  #     "None"
  #   }
  # }
  # )
  
  #when select is chosen if it is the first time it adds the results to the data frame of cont$tabla3
  
  observeEvent(input$select,{
    cont$count = cont$count + 1
    if (cont$count==1){
      cont$count2= cont$count2 +1
    }
    if (cont$count==1){
      if (input$guess == datos()[1,1]){
        cont$tabla3[cont$count2,] = c(cont$count2, datos()[1,1], input$guess, #prediction(),
                                      input$bot, input$wtype,
                                      input$wpercentage,input$ztype, input$zpercentage, input$spercentage,
                                      input$size, input$lpercentage,  "Yes")
      } else {
        cont$tabla3[cont$count2,] = c(cont$count2, datos()[1,1], input$guess, #prediction(),
                                      input$bot, input$wtype,
                                      input$wpercentage,input$ztype, input$zpercentage, input$spercentage,
                                      input$size, input$lpercentage,  "No")
      }
    } else {
      
    }
  })
  
  #When select is chosen it sees if it is the first time it is chosen and whether it is the right answer and creates
  # an appropiate response
  
  right <- eventReactive(input$select,{
    if (cont$count==1){
      if (input$guess == datos()[1,1]){
        "You are right!"
      } else {
        paste("You are WRONG! \n It is a", datos()[1,1])
      }
    } else {
      paste("You already answered. It is still a", datos()[1,1])
    }
  })
  
  # When select is chosen if there is an algorithm selected it gives the answer of what it classifies the number as.
  # Work in progress for when the algorithm is not train so it tells the user instead of showing an error.
  
  output$predict <- renderText(if (cont$count>=1){
    if (input$bot == "KNN"){
      paste("KNN bot thinks it is:", knn(train[,2:257], cont$datos[,1:256], train[,1], k =input$neighbours, l=0))
    }
    else if (input$bot == "SVM") {
      paste("SVM thinks it is:", predict(cont$svm, cont$datos[,1:256]) )
    }
    else if (input$bot == "RF") {
      # paste("RF thinks it is:", ifelse(cont$svm==0, "none", predict(cont$rf, cont$datos[,1:256])))
      paste("RF thinks it is:", predict(cont$rf, cont$datos[,1:256]))
    }
    else {
      ""
    }
  }
    )
  
  #Renders the image out of the modified array
  
  output$imagen <- renderPlot(image(1:16, 1:16, plines()[,16:1],
                                    main = NULL, col = CUSTOM_COLORS(256), xlab="", ylab=""))
  
  #Renders the text of whether the user is right
  output$acierto <- renderText(right())
  
  #Renders the datatable with the results
  output$tabla <- renderDataTable(cont$tabla3)
  
  
}

shinyApp(ui, server)

