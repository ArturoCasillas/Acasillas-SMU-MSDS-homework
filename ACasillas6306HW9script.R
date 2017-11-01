install.packages("XML")
library(XML)
library(xml2)
library(RCurl)
library(rvest)

URL1<-getURL('http://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1')
URL2<-getURL('http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs')
page1<-read_html(URL1)
page2<-read_html(URL2)

#I followed the Demo
table1<-html_table(html_nodes(page1, 'table'))
#table1_2<-getNodeSet(page1)
# table1<-readHTMLTable(URL1)
# table2<-readHTMLTable(URL2)

#Check out what I have eo look at
str(table1)
#It appears the third 'node' is what I am after
test<-data.frame(table1[3])
str(test)

#unclean first rows
test[1:10,]
head(test, 10)
Cast<-data.frame(test[2:length(test[,2]), c(2,4)])
names(Cast)<-c('Actor','Character')
#str(Cast)
#kable(head(Cast))
#head(Cast)

#Clean the second column
#tail(Cast)
Cast$Character<-gsub('\n  ','',Cast$Character)
Cast=Cast[!grepl('alphabetically:', Cast$Character),]

###Fix Warick Davis
#find row
warwick=Cast[grepl('/ ', Cast$Character),]
warwick
#test fix
warwick=cbind.data.frame(  warwick[,1], trimws(strsplit(warwick[,2], '/')[[1]]))
names(warwick)=names(Cast)
warwick

#Create Test to know I am not adding or deleting rows
test.indexes<-c((grep('/', Cast$Character)-1):(grep('/', Cast$Character)+2))
testR = Cast[test.indexes,]

## Merge back
Cast=rbind.data.frame(Cast[1:(grep('/', Cast$Character)-1),],                  #Before warwick
                      warwick,                                            #New warwick
                      Cast[(grep('/', Cast$Character)+1):length(Cast$Character),])  #After Warwick

#apply test
Cast[test.indexes,]
testR

#Get better row names
row.names(Cast)<-1:length(Cast$Actor)

#make intuitive names
names(Cast)=c('Actor', 'Character')



###Create new first and last name columns
#create first and last name function
#THis is to get the data in the form of a vector
#Later, I will look for a simpler way of doing this. 
first.last <- function(x, sep=' '){
  
#x<-Cast$actor[80:86]
D<-#data.frame(
              cbind(
                    rep( NA_character_, length(x)), 
                    rep( NA_character_, length(x))
                    )#)

#names(D)<-c('first','last')

for(i in 1:length(x)){
  
  #i = 2
  y<-strsplit(x[i], sep)[[1]]                           #split name
  first<-paste(y[-length(y)], collapse=' ')             #get fitst name (not last name)
  last<-y[length(y)]                                    #get last name
  
  D[i,]<-cbind(first, last)                             #combine
  #names(D)<-c('First', 'Last')
}

#format and output
  out=data.frame(D)
  names(out)<-c('FirstName','SurName')
  out
}

#test function
test<-cbind.data.frame(first.last(Cast$Actor), Cast$Character)
str(test)

# test<-apply(Cast$Actor, first.last)
# 
# x<-Cast$Actor[80]
# strsplit(x, ' ')[[1]]  
# (strsplit(x, ' ')[[1]])[1]
# (strsplit(x, ' ')[[1]])[2]
# 
# FirstTest=lapply(Cast$Actor[80:86], function(x) (strsplit(x, ' ')[[1]])[1])
# LastTest=lapply(Cast$Actor[80:86], function(x) (strsplit(x, ' ')[[1]])[2])

head(test)

x<-Cast$Actor[80]
strsplit(x, ' ')[[1]]  

################# 2. #####################

URL2<-getURL('http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs')
page2<-read_html(URL2)

#
table2<-html_table(html_nodes(page2, 'table'))
str(table2)
# It appears that node 2 is what I'm looking for
test2<-data.frame(table2[2])
str(test2)

#unclean first rows
test2[1:10,]
head(test2, 10)


#Get Names and remove nonsense first rows
names(test2)=test2[2,]
shootStats<-data.frame(test2[3:(length(test2[,2]) - 1), ])
str(shootStats)

#Better Names
names(shootStats)<-   
  c('Player', 'FG.Made', 'FG.Attempt', 'FG.Percent', 'FG3P.Made', 
    'FG3P.Attempt','FG3P.Percent', 'FT.Made', 'FT.Attmempt',
    'FT.Percent', 'FG2P.Made', 'FG2P.Attempt','FG2P.Percent',
    'Points.per.Sot', 'Adj.FG.Percent');
row.names(shootStats)<-1:13

#Separate player and position
#I will recycle my funciton but I have to change the names
testP<-first.last(shootStats[,1], sep=',')
names(testP)=c('Name', 'Position')
testP

shootStats=cbind.data.frame(testP, shootStats[,-1])
shootStats

#Convert character to numeric
str(shootStats)
apply(shootStats, 2, class)

#use apply() function
m<-apply(shootStats[,-c(1:2)], 2, as.numeric)
# m
# apply(m, 2, class)

shootStats=cbind.data.frame(shootStats[,c(1:2)], m)
#Check shootStats
str(shootStats)
apply(shootStats, 2, class)

# BarPlot #
library(ggplot2)

ggplot(dat=shootStats, aes(x=Name, y=FG2P.Percent, fill=Position)) + 
  geom_bar(stat='identity') + coord_flip()  + theme_minimal() +                             #Format
  ggtitle("Plot of Field Goal Percentage \n per Player per Game") +                         #Title
  theme(plot.title = element_text(hjust = 0.5)) +                                           #Center Title
  theme(legend.position = c(0.9, 0.5)) +                                                    #Legend Position
  xlab("Player Name") + ylab("Field Goal % per Game") +                                     #axis labels
  scale_fill_manual(values=c("#000000", "#005511", "#000099", "#777777", "#551100")) #+     #Manual set colors
#scale_color_gradient(low = "#00000000", high = "#0091ff")


