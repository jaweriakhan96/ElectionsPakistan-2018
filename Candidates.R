library(rvest)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

#---------------------------------------------------------------------------------------------------------#
#--------------------------------------Scraping off data from ECP-----------------------------------------#
#---------------------------------------------------------------------------------------------------------#
i<-1
data <- data.frame(constituency_no = numeric(), candidate_name = character(), party = character(), votes_per_candidate = numeric())

for(i in 1:272){
  if(i %in% c(60, 103)){
    i <- i+1
  }
  url <- paste('https://www.ecp.gov.pk/ConstResult.aspx?Const_Id=NA-',i,'&type=NA&Election_ID=10070&Election=GENERAL%20ELECTION%2025%20JUL%202018',sep = '')
  url_html <- read_html(url) #read html from url
  
  candidate_name_html<- html_nodes(url_html,".text+ .text td:nth-child(1) p")
  candidate_name <- html_text(candidate_name_html)
  
  party_html<- html_nodes(url_html,".text+ .text td:nth-child(2) p")
  party<- html_text(party_html)
  
  votes_per_candidate_html<- html_nodes(url_html,"td~ td+ td p")
  votes_per_candidate<- html_text(votes_per_candidate_html)
  constituency_no = i
  
  df<- data.frame(constituency_no = constituency_no , candidate_name= candidate_name , party= party, votes_per_candidate = votes_per_candidate)
  data <- rbind(data, df)
  print(i)
}

write.csv(data, file="Elections2018_allcandidates.csv" )
