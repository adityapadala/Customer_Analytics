
library("dplyr")
library("ggplot2")
setwd('path')

#splitting the sheets into two new csv files and importing

user_revenue <- read.csv('Networkrevenue.csv')
user_network <- read.csv("Networkfreq.csv")

#question 1

#removing the duplicate networks
user_network <- user_network[user_network$fromNode < user_network$toNode,]

#finding the number of times the user went to the store
freq_a <- data.frame(user_network%>%
                       group_by(fromNode)%>%
                       summarise(freq = sum(strength)))
freq_b <- data.frame(user_network%>%
                       group_by(toNode)%>%
                       summarise(freq = sum(strength)))
colnames(freq_b) <- colnames(freq_a)
user_links <- rbind(freq_a,freq_b)
user_strength <- data.frame(user_links%>%
                              group_by(fromNode)%>%
                              summarise(freq = sum(freq)))

#finding the revenue spent per visit by the user
user_strength<- merge(user_revenue,user_strength, by.x = "node", by.y = "fromNode")
user_strength$revpervist <- round(user_strength$revenueYear/user_strength$freq,2)
write.csv(user_strength,"user_strength.csv")

#computing the combined revenue when users co-shop
user_network$fromNode_visit <- merge(user_network,user_strength, by.x = "fromNode",
                                     by.y = "node", all.x = TRUE)$revpervist
user_network <- merge(user_network,user_strength, by.x = "toNode", by.y = "node", all.x = TRUE)
user_network <- user_network[,c(2,1,3,4,5,8)]
user_network <- user_network%>%arrange(fromNode)
colnames(user_network) <- c("fromNode","toNode","zip","strength","fromNode_rev_pervisit","toNode_rev_pervisit" )

user_network$co_shop_rev <- user_network$strength * user_network$fromNode_rev_pervisit + user_network$strength * user_network$toNode_rev_pervisit
write.csv(user_network,"co_visit_revenue.csv")

#getting the total revenue brought by each user with the help of his network
rev_a <- data.frame(user_network%>%
                      group_by(fromNode)%>%
                      summarise(co_shop_rev = sum(co_shop_rev),
                                num = n()))
rev_b <- data.frame(user_network%>%
                       group_by(toNode)%>%
                       summarise(co_shop_rev = sum(co_shop_rev),
                                 num = n()))
colnames(rev_b) <- colnames(rev_a)
rev_ab <- rbind(rev_a,rev_b)

rev_per_visit <- data.frame(rev_ab%>%
                              group_by(fromNode)%>%
                              summarise(co_shop_rev = sum(co_shop_rev),
                                        num_of_frnds = sum(num)))
write.csv(rev_per_visit,"rev_per_visit.csv")

rev_per_visit <- rev_per_visit %>% arrange(desc(co_shop_rev),desc(num_of_frnds))

#imported the data into excel and visualized the data. 
#looking at the revenue and network, 1971 is the best to target with 29991.49 revenue and 14 friends


#question 2

#finding relationship between strength, number of relations and the revenue
user_behaviour <- merge(user_strength,rev_per_visit,by.x = "node",by.y = "fromNode")
colnames(user_behaviour) <-  c("node","revenueYear","strength","revpervist","co_shop_rev","relations")
M<-cor(user_behaviour[,c(2,3,6)])
library('corrplot') #package corrplot
corrplot(M, method = "circle") #plot matrix

#high correlation between number of relations and strength


#question 3

library(zipcode)
data("zipcode")
user_map <- merge(user_network,zipcode,by.x = "zip",by.y = "zip",all.x = TRUE)
user_map<-data.frame(user_map%>%group_by(zip,latitude,longitude)%>%summarise(strength = mean(strength)))

#internet connection needed for seattle map download
library(ggmap)
seattle <- get_map(location = 'Seattle', zoom = 11)
ggmap(seattle) + geom_point(data = user_map, aes(x = longitude, y = latitude, alpha = factor(strength)), 
                            colour = 'red', size = 6) + 
  geom_text(data = user_map, aes(x = longitude, y = latitude, label = zip),
            size = 3, vjust = 0, hjust = 0) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) 

user_map$labels <- ifelse(user_map$strength > 5 | user_map$strength < 4, user_map$zip,NA)

ggplot(data = user_map, aes(x=zip, y=strength, label = labels)) + geom_line() + 
  geom_text(size=3)  + geom_hline(yintercept=4.5, colour = "red") +theme_bw()

#Ballard (98117 ) and Capitol Hill (98134, 98164) have more strength and zip code - 98195 have minimal strength


#question 4

#adding the number of friends for each user
user_network$fromNode_frnds <- merge(user_network, rev_per_visit, by="fromNode", all.x = TRUE)$num_of_frnds
user_network <- merge(user_network, rev_per_visit, by.x = "toNode", by.y = "fromNode",all.x = TRUE)
user_network <- user_network[,c(2,1,3,4,5,6,7,8,10)]
user_network <- user_network%>%arrange(fromNode)
colnames(user_network)[9] <- "toNode_frnds"

#adding the revenue to each user
user_network$fromNode_rev <- merge(user_network, user_revenue, by.x = "fromNode", by.y = "node",
                                   all.x = TRUE)$revenueYear
user_network <- merge(user_network, user_revenue, by.x = "toNode", by.y = "node",all.x = TRUE)
user_network <- user_network[,c(2,1,3,4,5,6,7,8,9,10,11)]
user_network <- user_network%>%arrange(fromNode)
colnames(user_network)[11] <- "toNode_rev"

write.csv(user_network,"user_network_question4.csv")
user_network[user_network$fromNode_frnds == user_network$toNode_frnds,]

user_network$equalfrnds <- user_network$fromNode_frnds == user_network$toNode_frnds
user_network%>%group_by(equalfrnds)%>%summarise(strength = sum(strength))

#imported the data into excel and visualized
