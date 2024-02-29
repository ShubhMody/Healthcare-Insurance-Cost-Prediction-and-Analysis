library(tidyverse)
library(ggplot2)
library(splitstackshape)
library(ggmap)
library(maps)
library(mapproj)
library(kernlab)
library(caret)
library(DataExplorer)
library(glmnet)
library(reshape2)
library(ggplot2)
library(caTools)
library(networkD3)
library(naniar)
library(e1071)
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(splitstackshape)
library(ggmap)
library(maps)
library(mapproj)
library(kernlab)
library(caret)
library(DataExplorer)
library(naniar)
library(DT)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
#install.packages("htmlTable")
library(htmlTable)
library(jtools)



library(tidyverse)


# Define UI
ui <- fluidPage(
                navbarPage(
                  "HMO Data Analytics",
                  tabPanel("Upload File",
                           sidebarPanel(
                             tags$h3("Input Cost Test File:"),
                             fileInput(inputId = "ABC", label = "Input File", multiple = FALSE, accept = NULL,
                                       width = NULL, buttonLabel = "Browse...",
                                       placeholder = "No file selected"),
                             
                             actionButton(inputId = "submit", label = "Submit"),
                           ),
                           mainPanel(
                             dataTableOutput("test_display")
                           )),
                  tabPanel("EDA",
                              tags$h4("Age Groups"),
                              plotOutput("plot1"),
                              br(),br(),br(),
                              tags$h4("Plot2"),
                              plotOutput("plot2"),
                              br(),br(),br(),
                              tags$h4("Plot3"),
                              plotOutput("plot3"),
                              br(),br(),br(),
                            
                             # tags$h4("Plot5"),
                              #plotOutput("plot5"),
                              br(),br(),br(),
                              tags$h4("Plot6"),
                              plotOutput("plot6"),
                              br(),br(),br(),
                              tags$h4("Plot7"),
                              plotOutput("plot7"),
                              br(),br(),br(),
                           ),
                  tabPanel("Regression", 
                           tags$h4("regressionplot"),
                           plotOutput("plot4"),
                           br(),br(),br(),
                             textOutput("reg_summary")),
                  
                  
                  
                  
                  #tabPanel("Classification", 
                           
                   #        verbatimTextOutput("class_metrics"))
              
                )
) # fluidPage

# Define server function  
server <- function(input, output) {
  df <- read_csv("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv")
  head(df)
  
  
  test_data <- read_csv('/Users/shubhmody/Downloads/IST_687_Final_Project/HMO_TEST_data_sample.csv')
  
  df = subset(df, select = -X)
  str(df)
  summary(df)
  
  #NULL values only in bmi and hypertension column
  sum(is.na(df$bmi))
  sum(is.na(df$hypertension))
  
  df <- as.data.frame(unclass(df),stringsAsFactors = TRUE)
  
  df$gender <- ifelse(df$gender == "male", 1, 0)
  df$smoker <- ifelse(df$smoker == "yes", 1, 0)
  df$location_type <- ifelse(df$location_type  == "Urban", 1, 0)
  df$yearly_physical <- ifelse(df$yearly_physical == "Yes", 1, 0)
  df$exercise <- ifelse(df$exercise == "Active", 1, 0)
  df$married <- ifelse(df$married == "Married", 1, 0)
  df$hypertension <- ifelse(df$hypertension == "1", 1, 0)
  
  test_data$gender <- ifelse(test_data$gender == "male", 1, 0)
  test_data$smoker <- ifelse(test_data$smoker == "yes", 1, 0)
  test_data$location_type <- ifelse(test_data$location_type  == "Urban", 1, 0)
  test_data$yearly_physical <- ifelse(test_data$yearly_physical == "Yes", 1, 0)
  test_data$exercise <- ifelse(test_data$exercise == "Active", 1, 0)
  test_data$married <- ifelse(test_data$married == "Married", 1, 0)
  test_data$hypertension <- ifelse(test_data$hypertension == "1", 1, 0)
  
  library(imputeTS)
  df$bmi <-  na_interpolation(df$bmi)
  
  df[,"hypertension"][is.na(df[,"hypertension"])] <- 0 
  
  
  
  ##### Linear Model ######
  
  library(MASS)
  
  lmOut <- lm(cost ~ ., data = df)
  summary(lmOut)
  
  
  lm_pred <- predict(lmOut, test_data)
  lm_pred
  
  output$reg_summary <- renderPrint({summary(lmOut)})
  
  ##### SVM Model #######
  df$expensive <- as.factor(ifelse(df$cost > quantile(df$cost, 0.75), 1, 0))
  
  library(caret)
  library(kernlab)
  library(reshape2)
  
  set.seed(111)
  trainList <- createDataPartition(y=df$expensive, p=0.80, list = FALSE)
  str(trainList)
  
  trainSet <- df[trainList, ]
  trainSet
  
  testSet <- df[-trainList, ]
  
  set.seed(111)
  svmModel <- ksvm(expensive ~ age + bmi + children + smoker + location + location_type +
                     education_level + yearly_physical + exercise + married + hypertension + gender, 
                   data = trainSet, C = 5, cross =3, prob.model =TRUE)
  
  
  predOut <- predict(svmModel, newdata = testSet, type = "response")
  predOut
  
  confusionMatrix(predOut, testSet$expensive)
  
  
  ###### MAP ########
  
  #MAP creation depicting location wise healthcare cost
  df_map <- df %>% group_by(location) %>% summarise(mean(cost))
  
  us <- map_data("state")
  us$state_name <- tolower(us$region)
  coord_df2 <- data.frame(loc=tolower(df_map$location),avg_cost=df_map$`mean(cost)`)
  us_with_coords2 <- merge(us,coord_df2, by.x='state_name',by.y='loc',all.x=TRUE, all.y = TRUE)
  us_with_coords2 <- us_with_coords2 %>% arrange(order)
  
  Mymap2 <- ggplot(us_with_coords2,aes(map_id= region)) + geom_polygon(color="black",aes(x=long,y=lat,group=group,fill=avg_cost))  +
    expand_limits(x=us_with_coords2$long, y=us_with_coords2$lat)+coord_map("mercator")+
    ggtitle("Entire USA Map shaded") + scale_fill_gradient(low = "white", high = "navy blue")
  Mymap2
  
  us <- map_data("state")
  us$state_name <- tolower(us$region)
  coord_df2 <- data.frame(loc=tolower(df_map$location),avg_cost=df_map$`mean(cost)`)
  us_with_coords2 <- merge(us,coord_df2, by.x='state_name',by.y='loc')
  us_with_coords2 <- us_with_coords2 %>% arrange(order)
  
  Mymap3 <- ggplot(us_with_coords2,aes(map_id= region)) + geom_polygon(color="black",aes(x=long,y=lat,group=group,fill=avg_cost))  +
    expand_limits(x=us_with_coords2$long, y=us_with_coords2$lat)+coord_map("mercator") +
    ggtitle("Northeast USA Map") + scale_fill_gradient(low = "White", high = "Red")
  Mymap3
  
  
  
  ##### Tree (Rpart) #####
  
  library(rpart)
  library(rpart.plot)
  
  tree <- rpart(formula = expensive ~ .,data = trainSet[-13])
  rpart.plot(tree)
  
  
  rpartpred <- predict(tree, newdata = testSet, type = 'class')
  
  

  
  
  
  ############## Visualizations ############
  
  df$ageGroup <-  as.factor(ifelse(df$age <= 25, 'young adults',
                                   ifelse(df$age > 25 & df$age <= 40, 'adults',
                                          ifelse(df$age > 40 & df$age <= 55, 'older adults',
                                                 ifelse(df$age > 55, 'senior citizens', 'NA')))))
  
  
  library(ggplot2)
  
  plot1 <- ggplot(data = df, aes(x = reorder(ageGroup, age), fill = expensive)) + 
    geom_bar() + scale_fill_brewer(palette = 18)
  plot1 <- plot1 + labs(title = 'Expensive and In-Expensive by Age Groups', x = 'Age Groups')
  plot1
  
  plot2 <- ggplot(data = df, aes(x = exercise, fill = expensive)) + 
    geom_bar() + scale_fill_brewer(palette = 14)
  plot2 <- plot2 + labs(title = 'Active Vs Non-Active', x = 'Activity')
  plot2
  
  plot3 <- ggplot(data = df, aes(x=smoker, y= cost, group = smoker, fill = smoker)) + 
    geom_boxplot() + labs(title = 'Smoker Vs Non-Smoker')
  plot3
  
  plot4 <- ggplot(data=df) + aes(x=bmi, y=cost) + geom_point(color= 'orange') +
    geom_smooth(method="lm", se=FALSE)
  plot4
  
  library(plotly)
  
  plot_5 <- plot_ly(df, x= ~age, y= ~bmi, z= ~cost, type="scatter3d", mode="markers", 
                    color= ~expensive, marker = list(size = 4))
  plot_5
  
  plot_6 <- plot_ly(df, x= ~age, y= ~smoker, z= ~cost, type="scatter3d", mode="markers", color= ~expensive, marker = list(size = 4))
  plot_6
  
  plot_7 <- plot_ly(df, x= ~age, y= ~exercise, z= ~cost, type="scatter3d", mode="markers", color= ~expensive, marker = list(size = 4))
  plot_7
  
  
    observeEvent( input$submit, {
    #data_test <- read.csv(input$ABC$datapath, header = TRUE, sep = ",")
    #output$test_display<- renderDataTable(datatable(data_test),options = list(autoWidth = FALSE))
    output$plot1<- renderPlot({ggplot(data = df, aes(x = reorder(ageGroup, age), fill = expensive)) + 
        geom_bar() + scale_fill_brewer(palette = 18)+ labs(title = 'Expensive and In-Expensive by Age Groups', x = 'Age Groups')})
    output$plot2<- renderPlot({ggplot(data = df, aes(x = exercise, fill = expensive)) + 
                                geom_bar() + scale_fill_brewer(palette = 14) + labs(title = 'Active Vs Non-Active', x = 'Activity')})
    output$plot3<- renderPlot({ggplot(data = df, aes(x=smoker, y= cost, group = smoker, fill = smoker)) + 
                                geom_boxplot() + labs(title = 'Smoker Vs Non-Smoker')})
    output$plot4<- renderPlot({ggplot(data=df) + aes(x=bmi, y=cost) + geom_point(color= 'orange') +
                                geom_smooth(method="lm", se=FALSE)})
    #output$plot5<- renderPlot({plot_ly(df, x= ~age, y= ~bmi, z= ~cost, type="scatter3d", mode="markers", 
     #                                 color= ~expensive, marker = list(size = 4))})
    output$plot6<- renderPlot({Mymap2})
    output$plot7<- renderPlot({Mymap3})

    
   
    
    #output$class_metrics<- renderText( confusionMatrix(rpartpred, testSet$expensive))
  
    
    })

}
# Create Shiny object
shinyApp(ui = ui, server = server)
