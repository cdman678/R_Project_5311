library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
require(dplyr)
library( plyr )
library(tidyverse)
library(stringr)
library(DT)




ui <- dashboardPage(
  dashboardHeader(title = "NBA Fantasy League"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("NBA Player Data", tabName = "data", icon = icon("table")),
      menuItem("NBA Master Model Data", tabName = "model_data", icon = icon("play")),
      menuItem("NBA Gameweeks", tabName = "gameweek", icon = icon("exchange"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(
      HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}')
    )),
    
    tabItems(
      # First tab content
      tabItem("data",
              titlePanel("Data Filters"),
              sidebarPanel(
                selectInput('data_type', 'Select the type of data', choices = c("train","test")), 
                selectInput('team_name', 'Select the team name', choices = c("Boston Celtics","Brooklyn Nets","New York Knicks","Philadelphia 76ers","Toronto Raptors",
                                                                             "Chicago Bulls","Cleveland Cavaliers","Detroit Pistons","Indiana Pacers","Milwaukee Bucks",
                                                                             "Atlanta Hawks","Charlotte Hornets","Miami Heat","Orlando Magic","Washington Wizards",
                                                                             "Denver Nuggets","Minnesota Timberwolves","Oklahoma City Thunder","Portland Trail Blazers","Utah Jazz",
                                                                             "Golden State Warriors","Los Angeles Clippers","Los Angeles Lakers","Phoenix Suns","Sacramento Kings",
                                                                             "Dallas Mavericks","Houston Rockets","Memphis Grizzlies","New Orleans Pelicans","San Antonio Spurs"))
              ),
              tableOutput('table'),
      ),
      
      # Second tab content
      tabItem("model_data",
              titlePanel("Player Models Actual vs Predicted Fantasy Score"),
              tableOutput('table2')
      ),
      
      # Third tab Content
      tabItem("gameweek",
              titlePanel("Gameweek Analysis"),
              selectInput('gameweek', 'Select the gameweek', choices = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)),
              titlePanel("Games"),
              
              DT::dataTableOutput('x1'),
              titlePanel("Combined team"),
              DT::dataTableOutput("com"),
              fluidRow(
                
                
                column(width = 9,  titlePanel("Optimal team")),
                column(width = 9,  tableOutput("op")),
                column(width = 9,  titlePanel("Predicted team")),
                column(width = 9, tableOutput("fan")),
                column(width = 9,  titlePanel("Random team")),
                column(width = 9,  tableOutput("ran"))
              )
              
              
              
      )
    )
  )
)

server <- function(input, output) { 
  
  output$table <- renderTable({
    train_file_path <- 'D:\\rr\\Training_Data'
    test_file_path <- 'D:\\rr\\Test_Data'
    
    if (is.na(input$data_type) == FALSE){
      if(input$data_type == "train"){
        data_path <- paste0(train_file_path,input$team_name)
      } else{
        data_path <- paste0(test_file_path,input$team_name)
      }
    }
    
    #The list of files of specific filetype
    Files <- list.files(data_path, pattern = "*.csv", full.names = TRUE, recursive = TRUE)
    
    final.file.data <- data.frame()
    
    for (file in Files){
      
      #Read the data from csv files
      file.data <- read.csv(file)
      
      final.file.data <- rbind(final.file.data,file.data)
    }
    
    final.file.data
  })
  
  output$table2 <- renderTable({
    master_filepath <- "D:\\rr\\DM.csv"
    
    master_data <- as.data.frame(read.csv(master_filepath))
    
    master_data
  })
  
  #fixture list file path
  q<-read.csv("D:\\rr\\FixtureList.csv")
  
  
  
  output$x1 = DT::renderDataTable(  q %>% filter( Week == input$gameweek), server = FALSE, options = list(
    pageLength = 25),
    selection = 'single',
    callback = JS(
      'table.on("click.dt","tr", function() {
                var data=table.row(this).data();
                   
                Shiny.setInputValue("T1", data[2]);
                Shiny.setInputValue("T2", data[3]);
                Shiny.setInputValue("Td", data[1]);
                
                
})'    
    ))
  output$op=renderPrint(
    input$T1 
  )
  observe({
    if(length( input$T1)>0){
      
      w<-maximizer(input$T1,input$T2,input$Td)
      print(w[[1]])
      output$com<- DT::renderDataTable( as.data.frame( w[[4]]))
      output$op<- renderTable( w[[1]])
      output$fan<- renderTable( as.data.frame( w[[2]]))
      output$ran<- renderTable( as.data.frame( w[[3]]))
    }
  })
  
 
  
}
maximizer<-function(v1,v2,v3){
  
  team1 <- v1
  team2 <- v2
  game_date <- v3
  print(team1)
  print(team2)
  
  team1_player_counts <- 0
  team2_player_counts <- 0
  sg_pos <- 0
  pg_pos <- 0
  c_pos <- 0
  f_pos <- 0
  
  fantasy_team <- list()
  optimal_team <- list()
  random_team <- list()
  
  master_data <- read.csv("D:\\rr\\DM.csv")
  print(master_data)
  team1_players <- master_data %>% filter(Team == team1, Opp == team2, Date == game_date)
  team2_players <- master_data %>% filter(Team == team2, Opp == team1, Date == game_date)
  print(team1_players)
  combined_team <- rbind(team1_players, team2_players)
  
  #############Create the Fantasy Team############################
  create.fantasyteam <- function(combined_team){
    for (i in 1:nrow(combined_team)){
      
      position <- combined_team[i,2]
      player <- combined_team[i,1]
      print(player)
      team <- combined_team[i,3]
      if(team == team1){
        if(team1_player_counts <3){
          
          if(position == "SG" & sg_pos < 1){
            
            sg_pos <<- 1
            team1_player_counts <<- team1_player_counts + 1
            fantasy_team <<- append(fantasy_team, player)
          } 
          else if(position == "PG" & pg_pos < 1){
            
            pg_pos <<- 1
            team1_player_counts <<- team1_player_counts + 1
            fantasy_team <<- append(fantasy_team, player)
          } 
          else if(position == "C" & c_pos < 1){
            
            c_pos <<- 1
            team1_player_counts <<- team1_player_counts + 1
            fantasy_team <<- append(fantasy_team, player)
          } 
          else if(position == "F" & f_pos < 2){
            
            f_pos <<- f_pos + 1
            team1_player_counts <<- team1_player_counts + 1
            fantasy_team <<- append(fantasy_team, player)
          } 
          else{
            next()
          }
        }
      } 
      
      if(team == team2){
        if(team2_player_counts <3){
          
          if(position == "SG" & sg_pos < 1){
            
            sg_pos <<- 1
            team2_player_counts <<- team2_player_counts + 1
            fantasy_team <<- append(fantasy_team, player)
          } 
          else if(position == "PG" & pg_pos < 1){
            
            pg_pos <<- 1
            team2_player_counts <<- team2_player_counts + 1
            fantasy_team <<- append(fantasy_team, player)
          } 
          else if(position == "C" & c_pos < 1){
            
            c_pos <<- 1
            team2_player_counts <<- team2_player_counts + 1
            fantasy_team <<- append(fantasy_team, player)
          } 
          else if(position == "F" & f_pos < 2){
            
            f_pos <<- f_pos + 1
            team2_player_counts <<- team2_player_counts + 1
            fantasy_team <<- append(fantasy_team, player)
          } 
          else{
            next()
          }
        }
      } 
    }
    return(fantasy_team)
  }
  
  combined_team <- combined_team[order(-combined_team$predictions_2019),]
  
  create.fantasyteam(combined_team)
  #############Fantasy Team#######################################
  
  
  #############Create the Optimal Team############################
  create.optimalteam <- function(combined_team){
    for (i in 1:nrow(combined_team)){
      
      position <- combined_team[i,2]
      player <- combined_team[i,1]
      team <- combined_team[i,3]
      if(team == team1){
        if(team1_player_counts <3){
          
          if(position == "SG" & sg_pos < 1){
            
            sg_pos <<- 1
            team1_player_counts <<- team1_player_counts + 1
            optimal_team <<- append(optimal_team, player)
          } 
          else if(position == "PG" & pg_pos < 1){
            
            pg_pos <<- 1
            team1_player_counts <<- team1_player_counts + 1
            optimal_team <<- append(optimal_team, player)
          } 
          else if(position == "C" & c_pos < 1){
            
            c_pos <<- 1
            team1_player_counts <<- team1_player_counts + 1
            optimal_team <<- append(optimal_team, player)
          } 
          else if(position == "F" & f_pos < 2){
            
            f_pos <<- f_pos + 1
            team1_player_counts <<- team1_player_counts + 1
            optimal_team <<- append(optimal_team, player)
          } 
          else{
            next()
          }
        }
      } 
      
      if(team == team2){
        if(team2_player_counts <3){
          
          if(position == "SG" & sg_pos < 1){
            
            sg_pos <<- 1
            team2_player_counts <<- team2_player_counts + 1
            optimal_team <<- append(optimal_team, player)
          } 
          else if(position == "PG" & pg_pos < 1){
            
            pg_pos <<- 1
            team2_player_counts <<- team2_player_counts + 1
            optimal_team <<- append(optimal_team, player)
          } 
          else if(position == "C" & c_pos < 1){
            
            c_pos <<- 1
            team2_player_counts <<- team2_player_counts + 1
            optimal_team <<- append(optimal_team, player)
          } 
          else if(position == "F" & f_pos < 2){
            
            f_pos <<- f_pos + 1
            team2_player_counts <<- team2_player_counts + 1
            optimal_team <<- append(optimal_team, player)
          } 
          else{
            next()
          }
        }
      } 
    }
    return(optimal_team)
  }
  
  team1_player_counts <- 0
  team2_player_counts <- 0
  sg_pos <- 0
  pg_pos <- 0
  c_pos <- 0
  f_pos <- 0
  
  combined_team <- combined_team[order(-combined_team$Fantasy_Score),]
  
  create.optimalteam(combined_team)
  #############Create the Optimal Team############################
  
  
  #############Create the Random Team############################
  create.randomteam <- function(combined_team){
    for (i in 1:nrow(combined_team)){
      
      position <- combined_team[i,2]
      player <- combined_team[i,1]
      team <- combined_team[i,3]
      if(team == team1){
        if(team1_player_counts <3){
          
          if(position == "SG" & sg_pos < 1){
            
            sg_pos <<- 1
            team1_player_counts <<- team1_player_counts + 1
            random_team <<- append(random_team, player)
          } 
          else if(position == "PG" & pg_pos < 1){
            
            pg_pos <<- 1
            team1_player_counts <<- team1_player_counts + 1
            random_team <<- append(random_team, player)
          } 
          else if(position == "C" & c_pos < 1){
            
            c_pos <<- 1
            team1_player_counts <<- team1_player_counts + 1
            random_team <<- append(random_team, player)
          } 
          else if(position == "F" & f_pos < 2){
            
            f_pos <<- f_pos + 1
            team1_player_counts <<- team1_player_counts + 1
            random_team <<- append(random_team, player)
          } 
          else{
            next()
          }
        }
      } 
      
      if(team == team2){
        if(team2_player_counts <3){
          
          if(position == "SG" & sg_pos < 1){
            
            sg_pos <<- 1
            team2_player_counts <<- team2_player_counts + 1
            random_team <<- append(random_team, player)
          } 
          else if(position == "PG" & pg_pos < 1){
            
            pg_pos <<- 1
            team2_player_counts <<- team2_player_counts + 1
            random_team <<- append(random_team, player)
          } 
          else if(position == "C" & c_pos < 1){
            
            c_pos <<- 1
            team2_player_counts <<- team2_player_counts + 1
            random_team <<- append(random_team, player)
          } 
          else if(position == "F" & f_pos < 2){
            
            f_pos <<- f_pos + 1
            team2_player_counts <<- team2_player_counts + 1
            random_team <<- append(random_team, player)
          } 
          else{
            next()
          }
        }
      } 
    }
    return(optimal_team)
  }
  
  team1_player_counts <- 0
  team2_player_counts <- 0
  sg_pos <- 0
  pg_pos <- 0
  c_pos <- 0
  f_pos <- 0
  
  combined_team <- rbind(team1_players, team2_players)
  
  create.randomteam(combined_team)
  #############Create the Random Team############################
  
  delta_optimal <- setdiff(optimal_team, fantasy_team)
  
  percentage_correct_with_optimal <- 100 - (length(delta_optimal)*20)
  
  delta_random <- setdiff(random_team, fantasy_team)
  print(fantasy_team)
  
  percentage_correct_with_random <- 100 - (length(delta_random)*20)
  
  print(percentage_correct_with_optimal)
  
  print(percentage_correct_with_random)
  
  d1<-as.data.frame(optimal_team)
  d2<-as.data.frame(fantasy_team)
  d3<-as.data.frame(random_team)
  d4<-as.data.frame(combined_team)
  df<-list(d1,d2,d3,d4)
  return (df)
}



shinyApp(ui, server)

