#install.packages("rvest")
library(rvest)
library(dplyr)
library(ggplot2)
#install.packages("plotrix")
library(plotrix)
election_data <- read_html("https://www.ecp.gov.pk/ResultDetails.aspx?EleId=10070&Election=General%20Election%2025%20Jul%202018")


constituency_data_html <- html_nodes(election_data,'font')
constituency_No <- html_text(constituency_data_html)
head(constituency_No)

status_html<- html_nodes(election_data,'td:nth-child(2) p')
status <- html_text(status_html)
head(status)

win_name_html<- html_nodes(election_data,'td:nth-child(3) p')
win_name <- html_text(win_name_html)
head(win_name)


win_party_html<- html_nodes(election_data,'td:nth-child(4) p')
win_party <- html_text(win_party_html)
head(win_party)

win_votes_html<- html_nodes(election_data,'td:nth-child(5) p')
win_votes <- html_text(win_votes_html)
head(win_votes)


elections_data_winner <- data.frame(Constituency_No = constituency_No, status = status, win_name= win_name, win_party = win_party, win_votes = win_votes)

write.csv( elections_data_winner, file="Elections2018_win.csv" )

ggplot( data = elections_data_winner, aes(x = win_party))+geom_bar()+
  scale_y_continuous(breaks =seq(1,120,2))


