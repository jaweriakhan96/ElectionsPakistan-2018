library(rvest)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

#---------------------------------------------------------------------------------------------------------#
#--------------------------------------Scraping off data from ECP-----------------------------------------#
#---------------------------------------------------------------------------------------------------------#

links <- read.csv("Links - Sheet1.csv") #Read all links from the CSV into a dataframe
links$Links <- as.character(links$Links)
voter_reg <- array(dim = c(272,1,0))
votes_cast <- array(dim = c(272,1,0))
validated_votes <- array(dim = c(272,1,0))
votes_disq <- array(dim = c(272,1,0))
turnout <- array(dim = c(272,1,0))
constituency_detail<- array(dim = c(272,1,0))
assembly <- array(dim = c(272,1,0))

for(i in 1:272){ #for loop to populate each row from the urls
  url <- paste('https://www.ecp.gov.pk/ConstResult.aspx?Const_Id=NA-',i,'&type=NA&Election_ID=10070&Election=GENERAL%20ELECTION%2025%20JUL%202018',sep = '')
  url_html <- read_html(url) #read html from url
  
  #read particular element from html
  voter_reg_html <- html_nodes( url_html,"#ContentPlaceHolder1_lblRegVoters") 
  voter_reg[i] <- html_text(voter_reg_html) #convert into text
  
 
  votes_cast_html <- html_nodes( url_html,"#ContentPlaceHolder1_lblVotesPolled") 
  votes_cast[i] <- html_text(votes_cast_html) 
  
  validated_votes_html <- html_nodes( url_html,"#ContentPlaceHolder1_lblValidVotes") 
  validated_votes[i] <- html_text(validated_votes_html) 
  
  votes_disq_html <- html_nodes( url_html,"#ContentPlaceHolder1_lblRejVotes") 
  votes_disq[i] <- html_text(votes_disq_html) 
  
  turnout_html <- html_nodes( url_html,"#ContentPlaceHolder1_lblTO") 
  turnout[i] <- html_text(turnout_html) 
  
  constituency_detail_html <- html_nodes( url_html,"#ContentPlaceHolder2_lblSubHeading") 
  constituency_detail[i] <- html_text(constituency_detail_html) 
  
  assembly[i]<- "National"
  print(i)
}


#combine all arrays into a dataframe
data <- data.frame( constituency_detail = constituency_detail,
                    assembly = assembly,
                    constituency_No = c(1:272), 
                    voter_reg = voter_reg, 
                    votes_cast = votes_cast, 
                    validated_votes = validated_votes,
                    votes_disq = votes_disq, 
                    turnout = turnout)

#---------------------------------------------------------------------------------------------------------#
#------------------------------------------Data Pre-Processing--------------------------------------------#
#---------------------------------------------------------------------------------------------------------#

data$constituency_detail <- as.character(data$constituency_detail)
#split details
for(i in 1:272){
  data$other[i] <- strsplit(data$constituency_detail[i], split= '[()]') 
}

#unlist, convert into matrix and then to dataframe
mydata <- data$other
mydata <- unlist(mydata)
mydata_matrix <- matrix(mydata, ncol = 3, byrow = T)
mydata_df <- data.frame( cons_no = mydata_matrix[,1], cons_name = mydata_matrix[,2], status = mydata_matrix[,3])

mydata_df$status <- as.character(mydata_df$status)
mydata_df <- mydata_df%>% separate(status, c("a", "b", "c",  sep = " "))
mydata_df$a <- NULL
mydata_df$b <- NULL
mydata_df$c <- NULL
mydata_df$cons_no <- NULL
names(mydata_df)[2] <- "status"

#bind the two datasets
data<- cbind(mydata_df, data)

#drop unnecessary columns
data$constituency_detail <- NULL
data$other <- NULL


data$votes_cast<-as.numeric(as.character(data$votes_cast))
data$voter_reg <- as.numeric(as.character(data$voter_reg))
#calculate turnout
for(i in 1:272){
  data$turnout_pct[i] <- (data$votes_cast[i]/data$voter_reg[i])*100
}

write.csv(data, file="Elections2018_allconstituencies.csv" )
