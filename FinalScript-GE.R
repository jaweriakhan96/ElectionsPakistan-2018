library(rvest)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plyr)
library(grid)
library(gtable)
library(gridExtra)
library(tidyr)


#----------------------# #----------------------# #----------------------# #----------------------# 
#-------------------------------# ETL and Preprocessing of Data #---------------------------------# 
#----------------------# #----------------------# #----------------------# #----------------------# 

#combine constituency details and winners
cons_details <- read.csv("Elections2018_allconstituencies.csv") #need to drop scraped turnout
cons_details$X <- NULL 
cons_details$turnout <- NULL

winners <- read.csv("Elections2018_win.csv") #drop X and drop status
winners$X <-NULL 
winners$status <-NULL

elections_data <- cbind(cons_details, winners)
elections_data$Constituency_No <- NULL
names(elections_data)[4]<- 'constituency_no'


#ALL CANDIDATES RESULT
all_candidates_result <- read.csv("Elections2018_allcandidates.csv")
all_candidates_result$X <-NULL

#Arrange by seat number and by votes in descending order 
all_candidates_result <- arrange(all_candidates_result, 
                                 all_candidates_result$constituency_no, 
                                 desc(all_candidates_result$votes_per_candidate))

#split by constituency number
split_df<- split(all_candidates_result, all_candidates_result$constituency_no)

#select 2nd and 3rd position candidates for each constituency
split_df_3 = lapply(split_df, function(x) x[2:3,])

#convertinto dataframe
df <- ldply(split_df_3, data.frame)
df$.id<- NULL

#add ranks and subset
df$rank <- c("second", "third")
second <- subset(df, df$rank == "second")
second$rank <- NULL
third <- subset(df, df$rank == "third")
third$rank <- NULL

names(second)[2]<-"second_name"
names(second)[3]<-"second_party"
names(second)[4]<-"second_votes"

names(third)[2]<-"third_name"
names(third)[3]<-"third_party"
names(third)[4]<-"third_votes"

#merge all together
elections_data <- merge(elections_data, second, by = "constituency_no", all = T)

elections_data <- merge(elections_data, third, by = "constituency_no", all = T)

#add derived columns
for(i in 1:272){
  elections_data$disq_pct[i] <- elections_data$votes_disq[i]/elections_data$votes_cast[i]
  elections_data$MOV_votes[i] <- elections_data$win_votes[i] - elections_data$second_votes[i]
  elections_data$MOV_pct[i] <- elections_data$MOV_votes[i]/elections_data$validated_votes[i]
  elections_data$win_pct[i] <- elections_data$win_votes[i]/elections_data$validated_votes[i]
  elections_data$second_pct[i] <- elections_data$second_votes[i]/elections_data$validated_votes[i]
  elections_data$third_pct[i] <- elections_data$third_votes[i]/elections_data$validated_votes[i]
  elections_data$remainder_votes[i] <- 
  elections_data$validated_votes[i] - (elections_data$win_votes[i] +elections_data$second_votes[i] + elections_data$third_votes[i])
  elections_data$remainder_pct[i] <- elections_data$remainder_votes[i]/elections_data$validated_votes[i]
  elections_data$election_year[i] <- 2018
  elections_data$election_date[i] <- "25/07/2018"
  elections_data$delimit[i]<- "Delimitation 2018"
  elections_data$election_type[i] <- "General Election"
}

#add province
for(i in 1:272){
  if(elections_data$constituency_no[i] %in% c(1:39))
    elections_data$province[i]<- "KPK"
  if(elections_data$constituency_no[i] %in% c(40:54))
    elections_data$province[i]<- "Isl&FATA"
  if(elections_data$constituency_no[i] %in% c(55:195))
    elections_data$province[i]<- "Punjab"
  if(elections_data$constituency_no[i] %in% c(196:256))
    elections_data$province[i]<- "Sindh"
  if(elections_data$constituency_no[i] %in% c(257:272))
    elections_data$province[i]<- "Balochistan"
}
elections_data$status <- as.character(elections_data$status)
elections_data$status[elections_data$status == "Announced"] <- "Contested"
#Reorder the columns
elections_data <- elections_data[c("election_date", "election_type", "status","constituency_no","cons_name",
                          "province","assembly", "voter_reg", "votes_cast","turnout_pct",
                          "votes_disq","disq_pct","validated_votes", "MOV_votes",
                          "MOV_pct", "win_votes","win_pct","win_party", "win_name",
                          "second_votes","second_pct","second_party", "second_name",
                          "third_votes","third_pct","third_party", "third_name",
                          "remainder_votes","remainder_pct","delimit","election_year"
                          )]
#make variables constant
names(elections_data)[10]<-"turnout"

#final 2018 results
write.csv(elections_data, "finalGE2018.csv")

#----------------------# #----------------------# #----------------------# #----------------------# 
#----------------------------# Merge old and new data #-------------------------------# 
#----------------------# #----------------------# #----------------------# #----------------------# 
old_data <- read.csv("nAssembly_clean.csv")
old_data$ID <- NULL
old_data$constituency_ID <- NULL
old_data$X <- NULL
names(old_data)[4] <- "constituency_no"
names(old_data)[3] <- "status"

for(i in 1:nrow(old_data)){
  old_data$turnout[i]<- old_data$turnout[i]*100
}

new_data <- elections_data

new_data<- new_data %>% separate(cons_name, 
                                   c("city", "x", "a", "b", "c"), sep = "-")



new_data$x[is.na(new_data$x)]<- "0"
new_data$a[is.na(new_data$a)]<- "0"
new_data$b[is.na(new_data$b)]<- "0"
new_data$c[is.na(new_data$c)]<- "0"
new_data$voter_

#Separate city 
for(i in 1:272){
  if(new_data$x[i] == "cum"){
    new_data$city[i] <- paste(new_data$city[i],new_data$x[i], new_data$a[i], sep = " ")
  }
  if(new_data$b[i] == "cum"){
    new_data$city[i] <- paste(new_data$city[i],new_data$x[i], new_data$a[i],new_data$b[i],new_data$c[i], sep = " ")
  }
}
  
new_data$x <- NULL
new_data$a <- NULL
new_data$b <- NULL
new_data$c <- NULL

temp_cast <- new_data$voter_reg[263]
temp_reg <- new_data$votes_cast[263]

new_data$votes_cast[263] <-temp_cast
new_data$voter_reg[263] <- temp_reg

new_data$turnout[263] <- new_data$voter_reg[263]/new_data$votes_cast[263]

new_data <- subset(new_data, !(constituency_no %in% c(263, 39)))
#----------------------# #----------------------# #----------------------# #----------------------# 
#-----------------------------------------# Combine and process #------------------------------------------# 
#----------------------# #----------------------# #----------------------# #----------------------# 
nAssembly <- rbind(old_data, new_data)
nAssembly$win_party[nAssembly$win_party == "Pakistan Muslim League (Nawaz)"]<- "Pakistan Muslim League (N)"
nAssembly$second_party[nAssembly$second_party == "Pakistan Muslim League (Nawaz)"]<- "Pakistan Muslim League (N)"
nAssembly$third_party[nAssembly$third_party == "Pakistan Muslim League (Nawaz)"]<- "Pakistan Muslim League (N)"

nAssembly$win_party[nAssembly$win_party == "Muttahida Qaumi Movement Pakistan"]<- "Muttahida Qaumi Movement"
nAssembly$second_party[nAssembly$second_party == "Muttahida Qaumi Movement Pakistan"]<- "Muttahida Qaumi Movement"
nAssembly$third_party[nAssembly$third_party == "Muttahida Qaumi Movement Pakistan"]<- "Muttahida Qaumi Movement"

nAssembly$win_party[nAssembly$win_party == "Independent"]<- "Independents"
nAssembly$second_party[nAssembly$second_party == "Independent"]<- "Independents"
nAssembly$third_party[nAssembly$third_party == "Independent"]<- "Independents"

write.csv(nAssembly, "GE1993-2018.csv")
#----------------------# #----------------------# #----------------------# #----------------------# 
#-----------------------------------------# Dataframes #------------------------------------------# 
#----------------------# #----------------------# #----------------------# #----------------------# 

nAssembly <- subset(nAssembly, nAssembly$status != "Postponed")

#Subset of Major Parties
nAssembly.subset= subset(nAssembly, 
                         win_party == "Pakistan Peoples Party Parliamentarians" | 
                           win_party == "Pakistan Muslim League (N)" | 
                           win_party == "Independents" | 
                           win_party == "Pakistan Tehreek-e-Insaf" | 
                           win_party == "Muttahida Qaumi Movement"|
                           win_party == "Muttahidda Majlis-e-Amal Pakistan"|
                           win_party == "Awami National Party"|
                           win_party == "Jamiat Ulema-e Islam (Fazl)"|
                           win_party == "Balochistan Awami Party"|
                           win_party == "Pakistan Muslim League (Qaid-e-Azam)")

#Groupby Year 

detach(package:plyr)
nAssembly_byYear <- nAssembly%>%
  group_by(election_year)%>%
  filter(!is.na(voter_reg))%>%
  filter(!is.na(votes_cast))%>%
  filter(!is.na(votes_disq))%>%
  summarise(
    t_voter_reg = sum(voter_reg),
    t_votes_cast = sum(votes_cast),
    t_votes_disq = sum(votes_disq),
    t_validated_votes = sum(validated_votes),
    mean_turnout = t_votes_cast/t_voter_reg
  )

#group by year and province

nAssembly_byYear.province <-  subset(nAssembly)%>%
  group_by(election_year, province)%>%
  filter(!is.na(voter_reg))%>%
  filter(!is.na(votes_cast))%>%
  filter(!is.na(votes_disq))%>%
  summarise(t_voter_reg = sum(voter_reg),
            t_votes_cast = sum(votes_cast),
            t_votes_disq = sum(votes_disq),
            t_validated_votes = sum(validated_votes),
            mean_turnout = t_votes_cast/t_voter_reg
  )

#group by year and city

nAssembly_byYear.city <-  nAssembly%>%
  group_by(election_year, city)%>%
  filter(!is.na(voter_reg))%>%
  filter(!is.na(votes_cast))%>%
  filter(!is.na(votes_disq))%>%
  summarise(t_voter_reg = sum(voter_reg),
            t_votes_cast = sum(votes_cast),
            t_votes_disq = sum(votes_disq),
            t_validated_votes = sum(validated_votes),
            mean_turnout = t_votes_cast/t_voter_reg
  )


nAssembly_byYear.cons.province <-  subset(nAssembly)%>%
  group_by(election_year, province, constituency_no)%>%
  filter(!is.na(voter_reg))%>%
  filter(!is.na(votes_cast))%>%
  filter(!is.na(votes_disq))%>%
  summarise(t_voter_reg = sum(voter_reg),
            t_votes_cast = sum(votes_cast),
            t_votes_disq = sum(votes_disq),
            votes_disq_pct = t_votes_disq/t_votes_cast,
            t_validated_votes = sum(validated_votes),
            mean_turnout = mean(turnout),
            MOV_pct = mean(MOV_pct),
            t_mov_votes = sum(MOV_votes)
  )


#Groupby Year and win party
nAssembly_byYear.party <- nAssembly.subset %>%
  group_by(election_year, win_party)%>%
  summarise( t_win_votes = sum(win_votes),
             n =n())%>%
  arrange(election_year, n)

#Groupby Year and win party and province
nAssembly_byYear.party.province <- nAssembly.subset %>%
  group_by(election_year, win_party,province)%>%
  summarise( 
    t_win_votes = sum(win_votes),
    n =n())

#SubsetForProvinces
sindh <-subset(nAssembly.subset, province == "Sindh")
punjab <-subset(nAssembly.subset, province == "Punjab")
balochistan <-subset(nAssembly.subset, province == "Balochistan")
KPK <-subset(nAssembly, province == "KPK")
IslandFATA <-subset(nAssembly, province == "Isl&FATA")
#----------------------# #----------------------# #----------------------# #----------------------# 
#-----------------------------------------# Visualization #------------------------------------------# 
#----------------------# #----------------------# #----------------------# #----------------------# 


#----------------------# #----------------------# #----------------------# #----------------------# 
#---------------------------------------# Country level #-----------------------------------------# 
#----------------------# #----------------------# #----------------------# #----------------------# 

ggplot(data= nAssembly_byYear.province, aes(x = election_year, y = t_votes_cast/1000000, 
                                            group = province, fill = province))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(breaks = seq(0,70, 2))+
  scale_x_continuous(breaks = c(1993, 1997, 2002, 2008, 2013, 2018))+
  ylab('Votes Casted (milions)')+ xlab('Year of Election')
ggsave('VotesCastedOverTheYears.png')

#Votes Registers, Casted and Validated over the Years
ggplot(data= nAssembly_byYear.province)+ 
  geom_bar(aes(x = election_year, y = t_voter_reg/1000000),stat ="identity", fill = 'yellow') +
  geom_bar(aes(x = election_year, y = t_votes_cast/1000000),stat ="identity", fill = 'red')+ 
  geom_bar(aes(x = election_year, y = t_validated_votes/1000000),stat ="identity", fill = 'green') +
  scale_y_continuous(breaks = seq(0,120, 5))+
  scale_x_continuous(breaks = c(1993, 1997, 2002, 2008, 2013, 2018))+
  ylab("Voters Registered, Votes Casted and Validated (millions)") +xlab('Year of Election')

#  sum(nAssembly_byYear$t_votes_cast)/sum(nAssembly_byYear$t_voter_reg)
# nAssembly_byYear <- subset(nAssembly_byYear, election_year != "2018")
# sum(nAssembly_byYear$t_votes_cast)/sum(nAssembly_byYear$t_voter_reg)

#Increase in Turnout each year
ggplot() +
  geom_line(data = nAssembly_byYear, aes(x = election_year, y = t_votes_cast*100/t_voter_reg))+
  geom_line(data = nAssembly_byYear.province, 
            aes(x = election_year, y = t_votes_cast*100/t_voter_reg, group = province, color = province), linetype = 2)+
  xlab('Election Year')+
  ylab('Turnout (%)')+
  scale_x_continuous(breaks = c(1993, 1997, 2002, 2008, 2013, 2018))+
  scale_y_continuous(breaks = seq(0,100, 2))


#How many seats had more than 50% turnout each year

nAssembly.cons <- subset(nAssembly_byYear.cons.province, mean_turnout >= 50) 
ggplot(data = nAssembly.cons, aes(x = election_year))+geom_bar(fill = "blue")+
  xlab('Election Year')+
  ylab( "Number of Contituencies")+
  scale_x_continuous(breaks = c(1993, 1997, 2002, 2008, 2013, 2018))+
  scale_y_continuous(breaks = seq(0, 225, 10))

table(nAssembly.cons$election_year)

#number of votes rejected
ggplot(data= nAssembly_byYear.province, aes(x = election_year, y = t_votes_disq/1000000, 
                                            group = province, fill = province))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0,1.7, 0.05))+
  scale_x_continuous(breaks = c(1993, 1997, 2002, 2008, 2013, 2018))+
  ylab('Votes discarded (milions)')+ xlab('Year of Election')

#Votes Rejected over the Years in each Province
ggplot() +
  geom_line(data = nAssembly_byYear, aes(x = election_year, y = t_votes_disq*100/t_votes_cast))+
  geom_line(data = nAssembly_byYear.province, 
            aes(x = election_year, y = t_votes_disq*100/t_votes_cast, group = province, color = province), linetype = 2)+
  xlab('Election Year')+
  ylab('Percentage of Votes Rejected')+
  scale_x_continuous(breaks = c(1993, 1997, 2002, 2008, 2013, 2018))+
  scale_y_continuous(breaks = seq(0,10, 0.5))

#Party popularity trend over the years
ggplot( data = nAssembly_byYear.party, aes(x = election_year , y = n, group = win_party , color = win_party))+
  geom_point(aes(size = t_win_votes/1000000))+
  geom_line() + xlab("Year of Election") + ylab("Seats Won") + 
  scale_x_continuous(breaks = c(1993, 1997, 2002, 2008, 2013,2018))+
  scale_y_continuous(breaks = seq(0,150,2)) 
#bar plot
ggplot(data= nAssembly_byYear.party, aes(x = election_year, y = n, 
                                            group = win_party, fill = win_party))+
  geom_bar(stat = "identity", position = "dodge")

#Percentage of seats won each year
ggplot(data = subset(nAssembly_byYear.party, election_year =="2002" |election_year =="2008" |election_year =="2013"|election_year =="2018"), 
       aes(x = election_year , y = n/272, group = win_party , color = win_party))+
  geom_point(aes(size = n))+
  geom_line() + xlab("Year of Election") + ylab("Seats Won") +
  scale_y_continuous(labels = scales :: percent )+
  scale_x_continuous(breaks = c( 2002, 2008, 2013,2018))

#Party popularity trend over the years in each provice
ggplot( data = nAssembly_byYear.party.province, aes(x = election_year , y = n, group = win_party , color = win_party))+
  geom_point(aes(size = n))+ geom_line()+
  xlab("Year of Election") + ylab("Seats Won") + 
  scale_x_continuous(breaks = c(1993, 1997, 2002, 2008, 2013))+
  facet_wrap(~province)

#----------------------# #----------------------# #----------------------# #----------------------# 
#---------------------------------------# Provincial Level #-----------------------------------------# 
#----------------------# #----------------------# #----------------------# #----------------------# 

#Sindh
ggplot( data = subset(sindh, election_year =="2002" |election_year =="2008" |election_year =="2013"|election_year =="2018") , 
        aes(x= constituency_no, y = turnout))+ 
  geom_point(aes(color= win_party, size = MOV_pct)) +
  scale_x_continuous(breaks = seq(195, 258 , 2))+
  xlab("Constituency Number (NA)")+
  ylab("Turnout(%)")+
  facet_wrap(~election_year, ncol = 1)

ggplot(data = subset(nAssembly_byYear.province, election_year =="2002" |election_year =="2008" |election_year =="2013"|election_year =="2018"),
       aes(x = election_year, y = mean_turnout*100 , group = province, color = province)) + geom_line()+
  scale_x_continuous(breaks = c( 2002, 2008, 2013, 3018))+
  scale_y_continuous(breaks = seq(0,60,2))

#Constituencies in Punjab 

ggplot( data = subset(punjab, election_year =="2002" |election_year =="2008" |election_year =="2013"|election_year =="2018") , 
        aes(x= constituency_no, y = turnout))+ 
  geom_point(aes(color= win_party, size = MOV_pct)) +
  scale_x_continuous(breaks = seq(50, 197 , 5))+
  xlab("Constituency Number (NA)")+
  ylab("Turnout(%)")+
  facet_wrap(~election_year, ncol = 1)

#Constituencies in Balochistan
ggplot( data = subset(balochistan, election_year =="2002" |election_year =="2008" |election_year =="2013"|election_year =="2018") , 
        aes(x= constituency_no, y = turnout))+ 
  geom_point(aes(color= win_party, size = MOV_pct)) +
  scale_x_continuous(breaks = seq(257, 272 , 1))+
  xlab("Constituency Number (NA)")+
  ylab("Turnout(%)")+
  facet_wrap(~election_year, ncol = 1)

#Constituencies in KPK
KPK$election_year <- as.factor(KPK$election_year)
ggplot( data = subset(KPK, election_year =="2002" |election_year =="2008" |election_year =="2013"|election_year =="2018"), 
        aes(x= constituency_no, y = turnout ))+ 
  geom_point(aes(color = win_party , size = MOV_pct))+
  scale_x_continuous(breaks = seq(1, 35 , 1))+
  xlab("Constituency Number (NA)")+
  ylab("Turnout(%)")+
  facet_wrap(~election_year, ncol = 1)


nAssembly_byYear.cons$election_year <-as.factor(nAssembly_byYear.cons$election_year)

x <- subset(nAssembly_byYear.cons.province, !(constituency_no %in% c(263, 39) & election_year == 2018)  )
#turnout vs Disq vote more than 5%
ggplot(x, aes(x=votes_disq_pct*100, y= mean_turnout, group = province, color = province))+
  geom_point()+
  scale_x_continuous(breaks = c(5,12))+
  facet_wrap(~election_year, ncol= 1)+
  geom_vline(xintercept =5, linetype = 1 , color= "black")+
  xlab('Votes Discarded(%)')+
  ylab('Turnout(%)')

u <- subset(x, election_year == 2018 & votes_disq_pct >= 0.05)

a<- subset(x, election_year == 2013)
avg.turnout <- sum(a$t_votes_cast)/sum(a$t_voter_reg)*100
avg.disq <- sum(a$t_votes_disq)/sum(a$t_voter_reg)*100

#turnout less than 30%
ggplot(a, aes(x = constituency_no, y = mean_turnout,group = province, color = province))+geom_point()+
  geom_hline(yintercept = avg.turnout, linetype = 1 , color= "black")+
  geom_hline(yintercept = 30, linetype = 1 , color= "yellow")+
  scale_x_continuous(breaks = c(39,55, 195, 257))+
  scale_y_continuous( breaks = seq(0, 80, 10))+
  ylab('Turnout(%)')+
  xlab("Constituency Number")

#turnout of each year
ggplot(nAssembly_byYear.cons.province, aes(x = constituency_no, y = mean_turnout,group = province, color = province))+geom_point()+
  geom_hline(yintercept = 30, linetype = 1 , color= "yellow")+
  geom_hline(yintercept = 60, linetype = 1 , color= "yellow")+
  scale_x_continuous(breaks = c(39,55, 195, 257))+
  scale_y_continuous( breaks = seq(0, 80, 10))+
  facet_wrap(~election_year, nrow= 1)+
  ylab("Turnout(%)")+
  xlab("Constituency Number")

  
#discarded votes in 2018 
ggplot(a, aes(x = constituency_no, y = votes_disq_pct *100,group = province, color = province))+
  geom_point(aes(size = MOV_pct))+
  geom_hline(yintercept = avg.disq, linetype = 1 , color= "black")+
  geom_hline(yintercept = 5, linetype = 1 , color= "yellow")+
  scale_x_continuous(breaks = c(39,55, 195, 257, 268))+
  scale_y_continuous( breaks = c(5, 12,55))+
  ylab('Discarded(%)')+
  xlab("Constituency Number")

#disc vs MOV
overall<-subset(nAssembly, nAssembly$votes_disq >= nAssembly$MOV_votes
                & election_year =="2008")
nrow(overall)



