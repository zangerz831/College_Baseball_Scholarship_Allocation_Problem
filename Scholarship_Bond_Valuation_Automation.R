library(readxl)
library(dplyr)
ecdf_fun <- function(x,perc) {
  ecdf(x)(perc)
}

roberto <- function(type, player_name) {
  ###IF PITCHER ###
  if(type=="Pitcher") {
    
  player_performance<-as.data.frame(wcc_pitchers %>%
                  filter(Name == player_name) %>%
                  filter(Year == "201718"))
  
  scholarship_allocation_table<-data.frame(read_excel("D:/Scholarship_Allocation_Table.xlsx"))
  allocation_amount<-select(filter(scholarship_allocation_table, Name == player_name), Allocated)
  allocation_amount<-as.numeric(allocation_amount)
  ###HARD CODED SHOLARSHIP AMOUNTS REPRESENTS EQUAL SPLIT OF ALL LEVELS ###
  if (allocation_amount >= .83) {
    allocation_rating<-("AAA")
  } else if (allocation_amount >= .66){
    allocation_rating<-("AA")
  } else if (allocation_amount >=.50){
    allocation_rating<-("A")
  } else if (allocation_amount >= .33){
    allocation_rating<-("BBB")
  } else if (allocation_amount >= .16){
    allocation_rating<-("BB")
  } else if (allocation_amount >= 0){
    allocation_rating<-("C")
  } else {
    return("BLEH")
  }
  
  ###SPREAD/ALPHA_Calculation###
  post_performance_discount_rate<-mean(ecdf_fun(filter(wcc_pitchers, IP>10)$IP, wcc_pitchers %>%
                                                  filter(Name == player_name) %>%
                                                  filter(Year == "201718") %>%
                                                  select(IP)), ecdf_fun(filter(wcc_pitchers, IP>10)$IP, wcc_pitchers %>%
                                                                          filter(Name == player_name) %>%
                                                                          filter(Year == "201718") %>%
                                                                          select(IP)))
  ###ASSIGN GRADE FOR PERFORMANCE AGAINST BENCHMARK###
  post_performance_rating <- if (1-post_performance_discount_rate >= .83) {
    post_performance_rating<-("C")
  } else if (1-post_performance_discount_rate >= .66){
    post_performance_rating<-("BBB")
  } else if (1-post_performance_discount_rate >=.50){
    post_performance_rating<-("BB")
  } else if (1-post_performance_discount_rate >= .33){
    post_performance_rating<-("A")
  } else if (1-post_performance_discount_rate >= .16){
    post_performance_rating<-("AA")
  } else if (1-post_performance_discount_rate < .16){
    post_performance_rating<-("AAA")
  } else {
    return("BLEH")
  }
  
  post_performance_rating
  Comparison_Table<- data.frame(allocation_rating, post_performance_rating)
  Comparison_Table
  
  ###REPEAT FOR HITTER###
  } else if(type=="Hitter") {
    
  player_performance<-as.data.frame(wcc_hitters %>%
                                      filter(Name == player_name) %>%
                                      filter(Year == "2017-18"))
  
  scholarship_allocation_table<-data.frame(read_excel("D:/Scholarship_Allocation_Table.xlsx"))
  scholarship_allocation_table
  scholarship_allocation_table["player_name",]
  allocation_amount<-select(filter(scholarship_allocation_table, Name == player_name), Allocated)
  allocation_amount<-as.numeric(allocation_amount)
  allocation_amount
  
  if (allocation_amount >= .83) {
    allocation_rating<-("AAA")
  } else if (allocation_amount >= .66){
    allocation_rating<-("AA")
  } else if (allocation_amount >=.50){
    allocation_rating<-("A")
  } else if (allocation_amount >= .33){
    allocation_rating<-("BBB")
  } else if (allocation_amount >= .16){
    allocation_rating<-("BB")
  } else if (allocation_amount >= 0){
    allocation_rating<-("C")
  } else {
    return("BLEH")
  }
  
  allocation_rating
  
  ###Alpha_Calculation###
  post_performance_discount_rate<-mean(ecdf_fun(filter(wcc_hitters, PA>30)$PA, wcc_hitters %>%
                  filter(Name == player_name) %>%
                  filter(Year == "2017-18") %>%
                  select(PA)), ecdf_fun(filter(wcc_hitters, PA>30)$OBP, wcc_hitters %>%
                                          filter(Name == player_name) %>%
                                          filter(Year == "2017-18") %>%
                                          select(OBP)))
  
  


  post_performance_rating <- if (1-post_performance_discount_rate >= .83) {
    post_performance_rating<-("C")
  } else if (1-post_performance_discount_rate >= .66){
    post_performance_rating<-("BBB")
  } else if (1-post_performance_discount_rate >=.50){
    post_performance_rating<-("BB")
  } else if (1-post_performance_discount_rate >= .33){
    post_performance_rating<-("A")
  } else if (1-post_performance_discount_rate >= .16){
    post_performance_rating<-("AA")
  } else if (1-post_performance_discount_rate < .16){
    post_performance_rating<-("AAA")
  } else {
    return("BLEH")
  }

  post_performance_rating
  
  Comparison_Table<- 
    data.frame(allocation_rating, post_performance_rating)
  Comparison_Table
  }    
}


roberto("Pitcher", "Steele, Joey")
roberto("Pitcher", "Slominski, Daniel")
roberto("Pitcher", "Ornido, Riley")
roberto("Pitcher", "Pham, Alex")
roberto("Pitcher", "Bourassa, Landen")
roberto("Pitcher", "Young, Grant")
roberto("Hitter", "Winkler, Jack")
roberto("Hitter", "Villaroman, Tyler")
roberto("Hitter", "Allen, Jonathan")
roberto("Hitter", "Yovetich, Nick")
roberto("Hitter", "Emery, Robert")
roberto("Hitter", "Helland, Riley")
roberto("Hitter", "Barchus, Jordan")
roberto("Pitcher", "Parker, Scott")
roberto("Hitter", "Kreske, Jason")
roberto("Pitcher", "Steele, Joey")

