# HW9
Arturo Casillas  
October 30, 2017  

## Intro

Problems 1 and 2 are here. The full tables after cleaning can be found in the appendix. The link for the files on github is:


https://github.com/ArturoCasillas/Acasillas-SMU-MSDS-homework


#1. Harry Potter Cast

###a.-b. In the IMDB, there are listings of full cast members for movies. Navigate to http://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1. Feel free to View Source to get a good idea of what the page looks like in code. Scrape the page with any R package that makes things easy for you. Of particular interest is the table of the Cast in order of crediting.

I initalized the packages and saved the data into a data.frame object below. I only did very basic cleaning


```r
#1.a. I clicked on the links to view the data
#     The cleaning will be 
#install.packages("XML")
library(XML)
library(xml2)
library(RCurl)
library(rvest)
library(knitr)

#1.b. I will install the packages and import/scrape the data in a very basic way
#      followed the Demo
URL1<-getURL('http://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1')
page1<-read_html(URL1)
table1<-html_table(html_nodes(page1, 'table'))

#    Check out what I have to look at
#str(table1)
#    It appears the third 'node' is what I am after
#    I know this from 1.a. when I looked at the htmls
test<-data.frame(table1[3])
str(test)
```

```
## 'data.frame':	148 obs. of  4 variables:
##  $ X1: chr  "" "" "" "" ...
##  $ X2: chr  "" "Ralph Fiennes" "Michael Gambon" "Alan Rickman" ...
##  $ X3: chr  "" "..." "..." "..." ...
##  $ X4: chr  "" "Lord Voldemort" "Professor Albus Dumbledore" "Professor Severus Snape" ...
```

###c.Clean up the table

#### -It should not have blank observations or rows, a row that should be column names, or just '.'

#### -It should have intuitive column names (ideally 2 to start - Actor and Character)

#### -In the film, Mr. Warwick plays two characters, which makes his row look a little weird. Please replace his character column with just "Griphook / Professor Filius Flitwick" to make it look better. . One row might result in "Rest of cast listed alphabetically" - remove this observation.

Now that it is loaded, I save the Cast data set with minor cleaning


```r
#1.c.
Cast<-data.frame(test[2:length(test[,2]), c(2,4)])
names(Cast)<-c('Actor','Character')
#str(Cast)
#kable(head(Cast))
head(Cast)
```

```
##              Actor                  Character
## 2    Ralph Fiennes             Lord Voldemort
## 3   Michael Gambon Professor Albus Dumbledore
## 4     Alan Rickman    Professor Severus Snape
## 5 Daniel Radcliffe               Harry Potter
## 6     Rupert Grint                Ron Weasley
## 7      Emma Watson           Hermione Granger
```

```r
tail(Cast)
```

```
##                 Actor                                       Character
## 143       Nick Turner            Death Eater \n  \n  \n  (uncredited)
## 144      Aaron Virdee      Gryffindor Senior \n  \n  \n  (uncredited)
## 145       John Warman Railway Station Porter \n  \n  \n  (uncredited)
## 146   Spencer Wilding     Knight of Hogwarts \n  \n  \n  (uncredited)
## 147         Amy Wiles      Slytherin Student \n  \n  \n  (uncredited)
## 148 Thomas Williamson       Hogwarts Student \n  \n  \n  (uncredited)
```

I need to do more cleaning on the data.


```r
#Clean the second column
#tail(Cast)
Cast$Character<-gsub('\n  ','',Cast$Character)
Cast=Cast[!grepl('alphabetically:', Cast$Character),]

#DIm, head and tail
#dim(Cast)
head(Cast)
```

```
##              Actor                  Character
## 2    Ralph Fiennes             Lord Voldemort
## 3   Michael Gambon Professor Albus Dumbledore
## 4     Alan Rickman    Professor Severus Snape
## 5 Daniel Radcliffe               Harry Potter
## 6     Rupert Grint                Ron Weasley
## 7      Emma Watson           Hermione Granger
```

```r
tail(Cast)
```

```
##                 Actor                           Character
## 143       Nick Turner            Death Eater (uncredited)
## 144      Aaron Virdee      Gryffindor Senior (uncredited)
## 145       John Warman Railway Station Porter (uncredited)
## 146   Spencer Wilding     Knight of Hogwarts (uncredited)
## 147         Amy Wiles      Slytherin Student (uncredited)
## 148 Thomas Williamson       Hogwarts Student (uncredited)
```

After basic cleaning, I fix Warick Davis' row


```r
###Fix Warick Davis
#find row
warwick=Cast[grepl('/ ', Cast$Character),]
#Create index to see rows I changed
test.indexes<-c((grep('/', Cast$Character)-1):(grep('/', Cast$Character)+2))

#Fix
warwick=cbind.data.frame(  warwick[,1], trimws(strsplit(warwick[,2], '/')[[1]]))
names(warwick)=names(Cast)
#warwick

## Merge back
Cast=rbind.data.frame(Cast[1:(grep('/', Cast$Character)-1),],                       #Before warwick
                      warwick,                                                      #New warwick
                      Cast[(grep('/', Cast$Character)+1):length(Cast$Character),])  #After Warwick
row.names(Cast)<-1:length(Cast$Actor)

#Look at the rows I changed
testR = Cast[test.indexes,]
testR
```

```
##             Actor                 Character
## 9  Clémence Poésy            Fleur Delacour
## 10  Warwick Davis                  Griphook
## 11  Warwick Davis Professor Filius Flitwick
## 12      John Hurt                Ollivander
```


###d. Split the Actor's name into two columns: FirstName and Surname. Keep in mind that some actors/actresses have middle names as well. Please make sure that the middle 2 names are in the FirstName column, in addition to the first name (example: given the actor Frank Jeffrey Stevenson, the FirstName column would say "Frank Jeffrey.")

I will create a function to do this. 


```r
#1.d.
###Create new first and last name columns
#create first and last name function
#THis is to get the data in the form of a vector

first.last <- function(x, sep=' '){
  
#x<-Cast$actor[80:86]
D<-cbind(
         rep( NA_character_, length(x)), 
         rep( NA_character_, length(x))
         )

for(i in 1:length(x)){
  
  #i = 2
  y<-strsplit(x[i], sep)[[1]]                           #split name
  first<-paste(y[-length(y)], collapse=' ')             #get fitst name (not last name)
  last<-y[length(y)]                                    #get last name
  
  D[i,]<-cbind(first, last)                             #combine
                      }

#format and output
  out=data.frame(D)
  names(out)<-c('FirstName','SurName')
  out
}
```


After creating a function to separate into first and last names, I will implement it on the cast data.


```r
#1.d. Continued
testF<-cbind.data.frame(first.last(Cast$Actor), Cast$Character)
#str(testF)
Cast<-testF
names(Cast)<-c('FirstName','SurName', 'Character')
str(Cast)
```

```
## 'data.frame':	147 obs. of  3 variables:
##  $ FirstName: Factor w/ 129 levels "Aaron","Adrian",..: 102 91 4 29 107 40 41 36 27 127 ...
##  $ SurName  : Factor w/ 138 levels "Adkins","Allgood",..: 41 44 109 105 52 133 82 48 104 34 ...
##  $ Character: Factor w/ 116 levels "Aberforth Dumbledore",..: 57 76 82 39 89 41 59 15 29 37 ...
```

####e. Present the first 10 rows of the data.frame() - It should have only FirstName, Surname, and Character columns.

The first ten columns are below. The full table is also in the appendix.


```r
head(Cast)
```

```
##   FirstName   SurName                  Character
## 1     Ralph   Fiennes             Lord Voldemort
## 2   Michael    Gambon Professor Albus Dumbledore
## 3      Alan   Rickman    Professor Severus Snape
## 4    Daniel Radcliffe               Harry Potter
## 5    Rupert     Grint                Ron Weasley
## 6      Emma    Watson           Hermione Granger
```


# 2.

###a. On the ESPN website, there are statistics of each NBA player. Navigate to the San Antonio Spurs current statistics (likely http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs). You are interested in the Shooting Statistics table.

###b. Scrape the page with any R package that makes things easy for you. There are a few tables on the page, so make sure you are targeting specifically the Shooting Statistics table.

I will scrape the table like in 1a.-1b.


```r
#2.a.-b.
#     Like the Demo
URL2<-getURL('http://www.espn.com/nba/team/stats/_/name/sa/san-antonio-spurs')
page2<-read_html(URL2)
table2<-html_table(html_nodes(page2, 'table'))

#    Check out what I have to look at
#str(table2)
#    It appears the third 'node' is what I am after
#    I know this from 2.a. when I looked at the htmls
test2<-data.frame(table2[2])
str(test2)
```

```
## 'data.frame':	16 obs. of  15 variables:
##  $ X1 : chr  "SHOOTING STATISTICS" "PLAYER" "LaMarcus Aldridge, PF" "Rudy Gay, SF" ...
##  $ X2 : chr  "SHOOTING STATISTICS" "FGM" "9.1" "4.4" ...
##  $ X3 : chr  "SHOOTING STATISTICS" "FGA" "18.4" "9.1" ...
##  $ X4 : chr  "SHOOTING STATISTICS" "FG%" ".496" ".484" ...
##  $ X5 : chr  "SHOOTING STATISTICS" "3PM" "0.6" "0.7" ...
##  $ X6 : chr  "SHOOTING STATISTICS" "3PA" "1.7" "2.0" ...
##  $ X7 : chr  "SHOOTING STATISTICS" "3P%" ".333" ".357" ...
##  $ X8 : chr  "SHOOTING STATISTICS" "FTM" "4.7" "2.6" ...
##  $ X9 : chr  "SHOOTING STATISTICS" "FTA" "6.0" "3.4" ...
##  $ X10: chr  "SHOOTING STATISTICS" "FT%" "0.79" "0.75" ...
##  $ X11: chr  "SHOOTING STATISTICS" "2PM" "8.5" "3.7" ...
##  $ X12: chr  "SHOOTING STATISTICS" "2PA" "16.7" "7.1" ...
##  $ X13: chr  "SHOOTING STATISTICS" "2P%" ".513" ".520" ...
##  $ X14: chr  "SHOOTING STATISTICS" "PPS" "1.279" "1.328" ...
##  $ X15: chr  "SHOOTING STATISTICS" "AFG%" "0.51" "0.52" ...
```


###c. Clean up the table (You might get some warnings if you're working with tibbles)

#### -You'll want to create an R data.frame() with one observation for each player. Make sure that you do not accidentally include blank rows, a row of column names, or the Totals row in the table as observations.

#### -The column PLAYER has two variables of interest in it: the player's name and their position, denoted by 1-2 letters after their name. Split the cells into two columns, one with Name and the other Position.

#### -Check the data type of all columns. Convert relevant columns to numeric. Check the data type of all columns again to confirm that they have changed!



```r
#2.c.
##Get Names and remove nonsense first rows
names(test2)=test2[2,]
shootStats<-data.frame(test2[3:(length(test2[,2]) - 1), ])

#Better Names
names(shootStats)<-   #'Player'
  c('Player', 'FG.Made', 'FG.Attempt', 'FG.Percent', 'FG3P.Made', 
    'FG3P.Attempt','FG3P.Percent', 'FT.Made', 'FT.Attmempt',
    'FT.Percent', 'FG2P.Made', 'FG2P.Attempt','FG2P.Percent',
    'Points.per.Sot', 'Adj.FG.Percent');
row.names(shootStats)<-1:13
```

Now to separate player and position in the first column


```r
#Separate player and position
#I will recycle my earlier funciton but I have to change the names
testP<-first.last(shootStats[,1], sep=',')
names(testP)=c('Name', 'Position')
shootStats=cbind.data.frame(testP, shootStats[,-1])
```

Now to convert character columns to numeric


```r
###Convert character to numeric
#CHeck shootStats
str(shootStats)
```

```
## 'data.frame':	13 obs. of  16 variables:
##  $ Name          : Factor w/ 13 levels "Brandon Paul",..: 9 13 3 12 5 8 10 11 7 1 ...
##  $ Position      : Factor w/ 5 levels " C"," PF"," PG",..: 2 4 5 1 3 4 5 3 1 5 ...
##  $ FG.Made       : chr  "9.1" "4.4" "4.3" "3.9" ...
##  $ FG.Attempt    : chr  "18.4" "9.1" "9.9" "8.1" ...
##  $ FG.Percent    : chr  ".496" ".484" ".435" ".474" ...
##  $ FG3P.Made     : chr  "0.6" "0.7" "2.0" "0.4" ...
##  $ FG3P.Attempt  : chr  "1.7" "2.0" "4.6" "1.4" ...
##  $ FG3P.Percent  : chr  ".333" ".357" ".438" ".300" ...
##  $ FT.Made       : chr  "4.7" "2.6" "0.6" "2.0" ...
##  $ FT.Attmempt   : chr  "6.0" "3.4" "1.0" "2.9" ...
##  $ FT.Percent    : chr  "0.79" "0.75" "0.57" "0.70" ...
##  $ FG2P.Made     : chr  "8.5" "3.7" "2.3" "3.5" ...
##  $ FG2P.Attempt  : chr  "16.7" "7.1" "5.3" "6.7" ...
##  $ FG2P.Percent  : chr  ".513" ".520" ".432" ".511" ...
##  $ Points.per.Sot: chr  "1.279" "1.328" "1.130" "1.246" ...
##  $ Adj.FG.Percent: chr  "0.51" "0.52" "0.54" "0.50" ...
```

```r
#use apply() function
m<-apply(shootStats[,-c(1:2)], 2, as.numeric)

#merge back
shootStats=cbind.data.frame(shootStats[,c(1:2)], m)

#Check shootStats again
str(shootStats)
```

```
## 'data.frame':	13 obs. of  16 variables:
##  $ Name          : Factor w/ 13 levels "Brandon Paul",..: 9 13 3 12 5 8 10 11 7 1 ...
##  $ Position      : Factor w/ 5 levels " C"," PF"," PG",..: 2 4 5 1 3 4 5 3 1 5 ...
##  $ FG.Made       : num  9.1 4.4 4.3 3.9 3.7 3.1 2.5 2.4 2.7 1.7 ...
##  $ FG.Attempt    : num  18.4 9.1 9.9 8.1 9.1 7.4 8.2 7.7 7 2.6 ...
##  $ FG.Percent    : num  0.496 0.484 0.435 0.474 0.406 0.423 0.306 0.315 0.381 0.667 ...
##  $ FG3P.Made     : num  0.6 0.7 2 0.4 0 0.3 0.7 0.9 0 1 ...
##  $ FG3P.Attempt  : num  1.7 2 4.6 1.4 0.3 0.7 3.3 3.4 0.3 1.6 ...
##  $ FG3P.Percent  : num  0.333 0.357 0.438 0.3 0 0.4 0.2 0.25 0 0.636 ...
##  $ FT.Made       : num  4.7 2.6 0.6 2 1.3 1.3 2 0.4 0.7 0.7 ...
##  $ FT.Attmempt   : num  6 3.4 1 2.9 2 2 2.3 0.4 1 1 ...
##  $ FT.Percent    : num  0.79 0.75 0.57 0.7 0.64 0.64 0.86 1 0.67 0.71 ...
##  $ FG2P.Made     : num  8.5 3.7 2.3 3.5 3.7 2.8 1.8 1.5 2.7 0.7 ...
##  $ FG2P.Attempt  : num  16.7 7.1 5.3 6.7 8.8 6.7 4.9 4.3 6.7 1 ...
##  $ FG2P.Percent  : num  0.513 0.52 0.432 0.511 0.419 0.426 0.379 0.367 0.4 0.714 ...
##  $ Points.per.Sot: num  1.279 1.328 1.13 1.246 0.953 ...
##  $ Adj.FG.Percent: num  0.51 0.52 0.54 0.5 0.41 0.44 0.35 0.37 0.38 0.86 ...
```

```r
head(shootStats)
```

```
##                Name Position FG.Made FG.Attempt FG.Percent FG3P.Made
## 1 LaMarcus Aldridge       PF     9.1       18.4      0.496       0.6
## 2          Rudy Gay       SF     4.4        9.1      0.484       0.7
## 3       Danny Green       SG     4.3        9.9      0.435       2.0
## 4         Pau Gasol        C     3.9        8.1      0.474       0.4
## 5   Dejounte Murray       PG     3.7        9.1      0.406       0.0
## 6     Kyle Anderson       SF     3.1        7.4      0.423       0.3
##   FG3P.Attempt FG3P.Percent FT.Made FT.Attmempt FT.Percent FG2P.Made
## 1          1.7        0.333     4.7         6.0       0.79       8.5
## 2          2.0        0.357     2.6         3.4       0.75       3.7
## 3          4.6        0.438     0.6         1.0       0.57       2.3
## 4          1.4        0.300     2.0         2.9       0.70       3.5
## 5          0.3        0.000     1.3         2.0       0.64       3.7
## 6          0.7        0.400     1.3         2.0       0.64       2.8
##   FG2P.Attempt FG2P.Percent Points.per.Sot Adj.FG.Percent
## 1         16.7        0.513          1.279           0.51
## 2          7.1        0.520          1.328           0.52
## 3          5.3        0.432          1.130           0.54
## 4          6.7        0.511          1.246           0.50
## 5          8.8        0.419          0.953           0.41
## 6          6.7        0.426          1.058           0.44
```

###d. Create a colorful bar chart that shows the Field Goals Percentage Per Game for each person. It will be graded on the following criteria.

#### -Informative Title, centered

#### -Relevant x and y axis labels (not simply variables names!)

#### -Human-readable axes with no overlap (you might have to flip x and y to fix that). Note: You do not have to convert the decimal to a percentage.

#### -Color the columns by the team member's position (so, all PF's should have the same color, etc.)

I can use ggplot to accomplish this


```r
# BarPlot #
library(ggplot2)

ggplot(dat=shootStats, aes(x=Name, y=FG2P.Percent, fill=Position)) + 
  geom_bar(stat='identity') + coord_flip()  + theme_minimal() +                             #Format
  ggtitle("Plot of Field Goal Percentage \n per Player per Game") +                         #Title
  theme(plot.title = element_text(hjust = 0.5)) +                                           #Center Title
  theme(legend.position = c(0.9, 0.5)) +                                                    #Legend Position
  xlab("Player Name") + ylab("Field Goal % per Game") +                                     #axis labels
  scale_fill_manual(values=c("#000000", "#005511", "#000099", "#777777", "#551100")) #+     #Manual set colors
```

![](ACasillas6306HW9_files/figure-html/no2e-1.png)<!-- -->

```r
  #scale_color_gradient(low = "#00000000", high = "#0091ff")
```

This is a link to my GitHub repository where the files and codebook are contained:
https://github.com/ArturoCasillas/Acasillas-SMU-MSDS-homework

#Appendix

Full Cast Data

```r
kable(Cast)
```



FirstName                 SurName        Character                                                  
------------------------  -------------  -----------------------------------------------------------
Ralph                     Fiennes        Lord Voldemort                                             
Michael                   Gambon         Professor Albus Dumbledore                                 
Alan                      Rickman        Professor Severus Snape                                    
Daniel                    Radcliffe      Harry Potter                                               
Rupert                    Grint          Ron Weasley                                                
Emma                      Watson         Hermione Granger                                           
Evanna                    Lynch          Luna Lovegood                                              
Domhnall                  Gleeson        Bill Weasley                                               
Clémence                  Poésy          Fleur Delacour                                             
Warwick                   Davis          Griphook                                                   
Warwick                   Davis          Professor Filius Flitwick                                  
John                      Hurt           Ollivander                                                 
Helena Bonham             Carter         Bellatrix Lestrange                                        
Graham                    Duff           Death Eater                                                
Anthony                   Allgood        Gringotts' Guard                                           
Rusty                     Goffe          Aged Gringotts' Goblin                                     
Jon                       Key            Bogrod                                                     
Kelly                     Macdonald      Helena Ravenclaw                                           
Jason                     Isaacs         Lucius Malfoy                                              
Helen                     McCrory        Narcissa Malfoy                                            
Tom                       Felton         Draco Malfoy                                               
Ian                       Peck           Hogsmeade Death Eater                                      
Benn                      Northover      Hogsmeade Death Eater (as Benjamin Northover)              
Ciarán                    Hinds          Aberforth Dumbledore                                       
Hebe                      Beardsall      Ariana Dumbledore                                          
Matthew                   Lewis          Neville Longbottom                                         
Devon                     Murray         Seamus Finnigan                                            
Jessie                    Cave           Lavender Brown                                             
Afshan                    Azad           Padma Patil                                                
Isabella                  Laughland      Leanne                                                     
Anna                      Shaffer        Romilda Vane                                               
Georgina                  Leonidas       Katie Bell                                                 
Freddie                   Stroma         Cormac McLaggen                                            
Alfred                    Enoch          Dean Thomas (as Alfie Enoch)                               
Katie                     Leung          Cho Chang                                                  
William                   Melling        Nigel                                                      
Sian Grace                Phillips       Screaming Girl                                             
Bonnie                    Wright         Ginny Weasley                                              
Ralph                     Ineson         Amycus Carrow                                              
Suzanne                   Toase          Alecto Carrow                                              
Maggie                    Smith          Professor Minerva McGonagall                               
Jim                       Broadbent      Professor Horace Slughorn                                  
Scarlett                  Byrne          Pansy Parkinson                                            
Josh                      Herdman        Gregory Goyle                                              
Louis                     Cordice        Blaise Zabini                                              
Amber                     Evans          Twin Girl 1                                                
Ruby                      Evans          Twin Girl 2                                                
Miriam                    Margolyes      Professor Pomona Sprout                                    
Gemma                     Jones          Madam Pomfrey                                              
George                    Harris         Kingsley Shacklebolt                                       
David                     Thewlis        Remus Lupin                                                
Julie                     Walters        Molly Weasley                                              
Mark                      Williams       Arthur Weasley                                             
James                     Phelps         Fred Weasley                                               
Oliver                    Phelps         George Weasley                                             
Chris                     Rankin         Percy Weasley                                              
David                     Bradley        Argus Filch                                                
Guy                       Henry          Pius Thicknesse                                            
Nick                      Moran          Scabior                                                    
Natalia                   Tena           Nymphadora Tonks                                           
Phil                      Wright         Giant                                                      
Garry                     Sayer          Giant                                                      
Tony                      Adkins         Giant                                                      
Dave                      Legeno         Fenrir Greyback                                            
Penelope                  McGhie         Death Eater                                                
Emma                      Thompson       Professor Sybil Trelawney                                  
Ellie                     Darcey-Alden   Young Lily Potter                                          
Ariella                   Paradise       Young Petunia Dursley                                      
Benedict                  Clarke         Young Severus Snape                                        
Leslie                    Phillips       The Sorting Hat (voice)                                    
Alfie                     McIlwain       Young James Potter                                         
Rohan                     Gotobed        Young Sirius Black                                         
Geraldine                 Somerville     Lily Potter                                                
Adrian                    Rawlins        James Potter                                               
Toby                      Papworth       Baby Harry Potter                                          
Timothy                   Spall          Wormtail                                                   
Robbie                    Coltrane       Rubeus Hagrid                                              
Gary                      Oldman         Sirius Black                                               
Peter G.                  Reed           Death Eater                                                
Judith                    Sharp          Death Eater                                                
Emil                      Hostina        Death Eater                                                
Bob Yves Van Hellenberg   Hubar          Death Eater                                                
Granville                 Saxton         Death Eater                                                
Tony                      Kirwood        Death Eater                                                
Ashley                    McGuire        Death Eater                                                
Arthur                    Bowen          Albus Severus Potter - 19 Years Later                      
Daphne de                 Beistegui      Lily Potter - 19 Years Later                               
Will                      Dunn           James Potter - 19 Years Later (as William Dunn)            
Jade                      Gordon         Astoria Malfoy - 19 Years Later                            
Bertie                    Gilbert        Scorpius Malfoy - 19 Years Later                           
Helena                    Barlow         Rose Weasley - 19 Years Later                              
Ryan                      Turner         Hugo Weasley - 19 Years Later                              
Jon                       Campling       Death Eater in Gringotts (scenes deleted)                  
Karen                     Anderson       Gringotts Goblin (uncredited)                              
Michael                   Aston          Wizard Parent (uncredited)                                 
Michael Henbury           Ballan         Gringotts Goblin (uncredited)                              
Lauren                    Barrand        Gringotts Goblin (uncredited)                              
David                     Barron         Wizard with Dog in Painting (uncredited)                   
Josh                      Bennett        Gringotts Goblin (uncredited)                              
Johann                    Benét          Deatheater (uncredited)                                    
Sean                      Biggerstaff    Oliver Wood (uncredited)                                   
Jada                      Brevett        Hogwarts Student (uncredited)                              
Ben                       Champniss      Parent (uncredited)                                        
Collet                    Collins        Snatcher (uncredited)                                      
Christoph                 Cordell        Snatcher (uncredited)                                      
Christian                 Coulson        Tom Marvolo Riddle (archive footage) (uncredited)          
Gioacchino Jim            Cuffaro        Wizard Parent (uncredited)                                 
Valerie                   Dane           Wizard Parent (uncredited)                                 
Paul                      Davies         Deatheater (uncredited)                                    
David                     Decio          Chief Snatcher (uncredited)                                
Ninette                   Finch          Augusta Longbottom (uncredited)                            
Grace Meurisse            Francis        Senior Gryffindor (uncredited)                             
Sean Francis              George         Wizard Parent (uncredited)                                 
Diane                     Gibbins        Gringotts Goblin (uncredited)                              
Hattie                    Gotobed        Young Girl in Epilogue (uncredited)                        
Melissa                   Gotobed        Hogwart's First Year Epilogue (uncredited)                 
Ian                       Hart           Professor Quirinus Quirrell (archive footage) (uncredited) 
Stephen                   Hawke          Wedding Guest (The Weasley's) (uncredited)                 
David                     Heyman         Dining Wizard in Painting (uncredited)                     
Harper                    Heyman         Baby of Dining Wizard Family in Portrait (uncredited)      
Matthew                   Hodgkin        Hogwarts Student (uncredited)                              
Steven                    Hopwood        One-Legged Wizard (uncredited)                             
Joe                       Kallis         Death Eater (uncredited)                                   
Gemma                     Kayla          Ravenclaw Senior (uncredited)                              
Hrvoje                    Klecz          Death Eater (uncredited)                                   
Maxwell                   Laird          Gringotts Goblin (uncredited)                              
Debra                     Leigh-Taylor   Wizard Teacher (uncredited)                                
Christina                 Low            Ravenclaw Student (uncredited)                             
Sarah                     Lowe           Ministry Wizard (uncredited)                               
Tony                      Montalbano     Passenger (uncredited)                                     
Sha'ori                   Morris         Slytherin Girl (uncredited)                                
Luke                      Newberry       Teddy Lupin (uncredited)                                   
Sarah Jane                O'Neill        Wizard Parent (uncredited)                                 
Lisa                      Osmond         Gringotts Goblin (uncredited)                              
Elisabeth                 Roberts        Death Eater (uncredited)                                   
Keijo                     Salmela        Gringotts Goblin (uncredited)                              
Mark                      Sealey         Gringotts Goblin (uncredited)                              
Arti                      Shah           Gringotts Goblin (uncredited)                              
Glen                      Stanway        Death Eater (uncredited)                                   
Albert                    Tang           Hogwarts Teacher (uncredited)                              
Richard                   Trinder        Augustus Rookwood (uncredited)                             
Nick                      Turner         Death Eater (uncredited)                                   
Aaron                     Virdee         Gryffindor Senior (uncredited)                             
John                      Warman         Railway Station Porter (uncredited)                        
Spencer                   Wilding        Knight of Hogwarts (uncredited)                            
Amy                       Wiles          Slytherin Student (uncredited)                             
Thomas                    Williamson     Hogwarts Student (uncredited)                              

Full Shooting Statistics Data

```r
kable(shootStats)
```



Name                Position    FG.Made   FG.Attempt   FG.Percent   FG3P.Made   FG3P.Attempt   FG3P.Percent   FT.Made   FT.Attmempt   FT.Percent   FG2P.Made   FG2P.Attempt   FG2P.Percent   Points.per.Sot   Adj.FG.Percent
------------------  ---------  --------  -----------  -----------  ----------  -------------  -------------  --------  ------------  -----------  ----------  -------------  -------------  ---------------  ---------------
LaMarcus Aldridge   PF              9.1         18.4        0.496         0.6            1.7          0.333       4.7           6.0         0.79         8.5           16.7          0.513            1.279             0.51
Rudy Gay            SF              4.4          9.1        0.484         0.7            2.0          0.357       2.6           3.4         0.75         3.7            7.1          0.520            1.328             0.52
Danny Green         SG              4.3          9.9        0.435         2.0            4.6          0.438       0.6           1.0         0.57         2.3            5.3          0.432            1.130             0.54
Pau Gasol           C               3.9          8.1        0.474         0.4            1.4          0.300       2.0           2.9         0.70         3.5            6.7          0.511            1.246             0.50
Dejounte Murray     PG              3.7          9.1        0.406         0.0            0.3          0.000       1.3           2.0         0.64         3.7            8.8          0.419            0.953             0.41
Kyle Anderson       SF              3.1          7.4        0.423         0.3            0.7          0.400       1.3           2.0         0.64         2.8            6.7          0.426            1.058             0.44
Manu Ginobili       SG              2.5          8.2        0.306         0.7            3.3          0.200       2.0           2.3         0.86         1.8            4.9          0.379            0.939             0.35
Patty Mills         PG              2.4          7.7        0.315         0.9            3.4          0.250       0.4           0.4         1.00         1.5            4.3          0.367            0.796             0.37
Joffrey Lauvergne   C               2.7          7.0        0.381         0.0            0.3          0.000       0.7           1.0         0.67         2.7            6.7          0.400            0.857             0.38
Brandon Paul        SG              1.7          2.6        0.667         1.0            1.6          0.636       0.7           1.0         0.71         0.7            1.0          0.714            2.000             0.86
Bryn Forbes         SG              1.1          3.4        0.333         0.4            1.4          0.300       0.0           0.0         0.00         0.7            2.0          0.357            0.792             0.40
Davis Bertans       SF              0.8          2.8        0.273         0.3            1.8          0.143       0.5           0.8         0.67         0.5            1.0          0.500            0.818             0.32
Derrick White       PG              0.0          0.7        0.000         0.0            0.0          0.000       0.3           0.7         0.50         0.0            0.7          0.000            0.500             0.00
