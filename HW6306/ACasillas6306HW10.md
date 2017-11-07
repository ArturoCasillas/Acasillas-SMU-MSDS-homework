# ACasillas6306HW10
Arturo Casillas  
November 6, 2017  


###1. Mental Health Clinics

####a. This data set is a survey of every known healthcare facility that offers mental health services in the United States in 2015. Navigate to https://datafiles.samhsa.gov/study-dataset/national-mental-health-services-survey-2015-n-mhss-2015-ds0001-nid17098 and select the R download. Look through the codebook PDF for an explanation on certain variables. Upon opening the RDA file, the data set should be inserted into your global environment, which you can then reference.

I loaded it from the dropdown menus and used the str() function to look at it (not included).


```r
load("C:/Users/acasi/Downloads/N-MHSS-2015-DS0001-data-r.rda")
#str(mh2015_puf)
```

####b. Please create code which lists the State abbreviations without their counts, one abbreviation per State value. It does not have to in data frame format. A vector is fine.

I did it two ways:


```r
#As long as we are just looking at a list of state abbreviations and not a different object...
#Here are two different ways of getting the list
levels(mh2015_puf$LST)
```

```
##  [1] "AK    " "AL    " "AR    " "AS    " "AZ    " "CA    " "CO    "
##  [8] "CT    " "DC    " "DE    " "FL    " "GA    " "GU    " "HI    "
## [15] "IA    " "ID    " "IL    " "IN    " "KS    " "KY    " "LA    "
## [22] "MA    " "MD    " "ME    " "MI    " "MN    " "MO    " "MS    "
## [29] "MT    " "NC    " "ND    " "NE    " "NH    " "NJ    " "NM    "
## [36] "NV    " "NY    " "OH    " "OK    " "OR    " "PA    " "PR    "
## [43] "RI    " "SC    " "SD    " "TN    " "TX    " "UT    " "VA    "
## [50] "VI    " "VT    " "WA    " "WI    " "WV    " "WY    "
```

```r
#This is the more robust way in case the data set changes
unique(mh2015_puf$LST)
```

```
##  [1] AL     AK     AZ     AR     CA     CO     CT     DE     DC     FL    
## [11] GA     HI     ID     IL     IN     IA     KS     KY     LA     ME    
## [21] MD     MA     MI     MN     MS     MO     MT     NE     NV     NH    
## [31] NJ     NM     NY     NC     ND     OH     OK     OR     PA     RI    
## [41] SC     SD     TN     TX     UT     VT     VA     WA     WV     WI    
## [51] WY     AS     GU     PR     VI    
## 55 Levels: AK     AL     AR     AS     AZ     CA     CO     ... WY
```

####c. Filter the data.frame from 1A. We are only interested in the Veterans Administration (VA) medical centers in the mainland United States-create a listing of counts of these centers by state, including only mainland locations. Alaska, Hawaii, and U.S. territories should be omitted. DC, while not a state, is in the mainland, so it should remain included. Convert this to data.frame()

I create filter vectors of TRUE/FALSE.


```r
###Filter

F1= trimws(mh2015_puf$LST) %in% c('AS', 'GU', 'PR', 'VI', 'HI', 'AK' )
#F1[1:10]

#I am going to filter by FACILITYTYPE not by PUBLICAGENCY
#unique(mh2015_puf$FACILITYTYPE)
F2= grepl('(VAMC)', mh2015_puf$FACILITYTYPE )
#F2[1:10]

#Apply filters
MHreduced<-mh2015_puf[F2 & !F1, ]
```

Below, I test whether they worked


```r
#Original dimensions
dim(mh2015_puf)
```

```
## [1] 12826   139
```

```r
#Reduced dimensions
dim(MHreduced)
```

```
## [1] 355 139
```

```r
#No need to convert to a data.frame
class(MHreduced)
```

```
## [1] "data.frame"
```

####d. Create a ggplot barchart of this filtered data set. Vary the bar's colors by what State it has listed. Give it an appropriately professional title that is centered. Make sure you have informative axis labels. The State axis should be readable, not layered over each other. You're welcome to have a legend or not.

First, I used tapply() to put the data in a small enough objsect that can be read easily by ggplot(). Then, I created a vector of colors from the colors() list of names.


```r
##Barplot

#I'm going to use tapply to define a dataset that can be easily red by ggplot
stateFREQ<-data.frame(tapply(MHreduced$CASEID, MHreduced$LST, function(y) sum(!is.na(y), na.rm=TRUE)))
#trimws(row.names(stateFREQ))
stateFREQ<-cbind.data.frame('State'=trimws(row.names(stateFREQ)), 'Freq'=stateFREQ[,1])
stateFREQ<-stateFREQ[!(stateFREQ$State %in% c('AS', 'GU', 'PR', 'VI', 'HI', 'AK' )),]
str(stateFREQ)
```

```
## 'data.frame':	49 obs. of  2 variables:
##  $ State: Factor w/ 55 levels "AK","AL","AR",..: 2 3 5 6 7 8 9 10 11 12 ...
##  $ Freq : int [1:49(1d)] 4 7 4 18 7 1 1 1 27 9 ...
##   ..- attr(*, "dimnames")=List of 1
##   .. ..$ : chr  "AL    " "AR    " "AZ    " "CA    " ...
```

```r
library(ggplot2)

#Define color vector
c=colors()
#Below is a relatively parsimonious way to get 50+ colors that are all visible
c1=c[grepl('1', c) & !grepl('gray', c) & !grepl('grey', c) & !grepl('gold', c) & !grepl('corn', c)
     & !grepl('light', c) & !grepl('white', c) & !grepl('orchid', c) & !grepl('lavender', c)
     & !grepl('wood', c) & !grepl('khaki', c) & !grepl('ivory', c) & !grepl('snow', c)
     & !grepl('tan', c) & !grepl('azure', c) & !grepl('honeydew', c) & !grepl('shell', c)]
```

Now for the actual plot


```r
#use ggplot with c1 as color vector 
p<-ggplot(data=stateFREQ, aes(x=State, y=Freq)) + geom_bar(stat="identity", fill=c1[1:48]) + ggtitle('Chart') 
p<- p +   labs(title="Barplot of VAMC Facilities per State") + xlab("State") + ylab("Number of Veterans Administration Health Facilities")
p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=8.5)) + coord_flip()
#p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
p
```

```
## Warning: Removed 1 rows containing missing values (position_stack).
```

![](ACasillas6306HW10_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#North Dakota has no VAMC facilities so I get a warning that it is missing
```

###2. Cleaning and Bringing in New Features (60%)

####a. This graph (1D) might be somewhat misleading, as bigger states may have more hospitals, but could be more sparsely located. Read statesize.csv into your R environment. This contains essentially a vector of square miles for each state. In trying to merge it with your data.frame() from 1C, you find that they don't match. Use paste() on your LST column in 1C to see what the matter is, and write what you observe in a comment.

I take it from what I saw above that the leading and trailing blanks will prevent the merge


```r
#Read data
statesize<-read.csv("C:/Users/acasi/Downloads/statesize.csv")

#Try to merge
MHreducedS<-merge(MHreduced, statesize, by.x = 'LST', by.y = 'Abbrev', all.x = TRUE)
tail(MHreducedS)[142:140]
```

```
##     Region SqMiles StateName
## 350   <NA>      NA      <NA>
## 351   <NA>      NA      <NA>
## 352   <NA>      NA      <NA>
## 353   <NA>      NA      <NA>
## 354   <NA>      NA      <NA>
## 355   <NA>      NA      <NA>
```

```r
#use paste to see the exact text value of LST
ptest=paste(MHreduced$LST, sep=';')
ptest2=paste(statesize$Abbrev, sep=';')
ptest[1:10]
```

```
##  [1] "AL    " "AL    " "AL    " "AZ    " "AZ    " "AZ    " "AR    "
##  [8] "AR    " "AR    " "AR    "
```

```r
ptest2[1:10]
```

```
##  [1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT" "DE" "FL" "GA"
```

Indeed, the two variables are not identical due to leading and trailing blanks

####b. Correct the problem with the LST column using any method in R that is programmatic and easily understandable. Once you have made these state abbreviations identical to statesize.csv's Abbrev column, merge the data.frame() from 1C and statesize.csv in order to add size information.

I'll just replace the variable with a trimws() version of itself and remerge


```r
#2.b.
#I used trimws to remove the leading and trailing spaces from LST
MHreduced$LST=trimws(MHreduced$LST)

#double check
unique(statesize$Abbrev)
```

```
##  [1] AL AK AZ AR CA CO CT DE FL GA HI ID IL IN IA KS KY LA ME MD MA MI MN
## [24] MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA
## [47] WA WV WI WY
## 50 Levels: AK AL AR AZ CA CO CT DE FL GA HI IA ID IL IN KS KY LA MA ... WY
```

```r
sort(unique(MHreduced$LST))
```

```
##  [1] "AL" "AR" "AZ" "CA" "CO" "CT" "DC" "DE" "FL" "GA" "IA" "ID" "IL" "IN"
## [15] "KS" "KY" "LA" "MA" "MD" "ME" "MI" "MN" "MO" "MS" "MT" "NC" "NE" "NH"
## [29] "NJ" "NM" "NV" "NY" "OH" "OK" "OR" "PA" "RI" "SC" "SD" "TN" "TX" "UT"
## [43] "VA" "VT" "WA" "WI" "WV" "WY"
```

```r
#Also, I notice that statesize does not have DC

#Remerge
MHreducedS2<-merge(MHreduced, statesize, by.x = 'LST', by.y = 'Abbrev', all.x = TRUE)
#check
tail(MHreducedS)[142:140]
```

```
##     Region SqMiles StateName
## 350   <NA>      NA      <NA>
## 351   <NA>      NA      <NA>
## 352   <NA>      NA      <NA>
## 353   <NA>      NA      <NA>
## 354   <NA>      NA      <NA>
## 355   <NA>      NA      <NA>
```

####c. Calculate a new variable in your combined data.frame() which indicates the VA hospitals per thousand square miles.

The way the data is structured, the new variable cannot be the total count of facilities per 1000 square miles, but a variable that will allow me to calculate that later per a grouping of my choice.


```r
#This does not indicate the number of VA hospitals per thousand miles inasmuch as it lets me calculate it via summation
MHreducedS2$HospPKM <- 1*1000/MHreducedS2$SqMiles
```

####d. Create another ggplot which considers the VAs per square thousand miles, rather than just frequency.
##### Make sure the State axis is readable, like before. Change the title and axes as appropriate.
##### Modify the ggplot syntax to make your bars in descending order (there are StackOverflow topics for this, and I have demonstrated how in Live Coding in prior classes).
##### Color-code the bars based on Region (see the merged data.frame)-however, change the color scheme from the default. Any set of colors is fine, so long as it is readable.
##### Keep the legend-you should have four regions and therefore four colors.

Once again, first tapply(), then ggplot.


```r
#Once again, use tapply to be easily read by ggplot2
Test=data.frame(tapply(MHreducedS2$HospPKM, MHreducedS2$LST, function(y) sum(y, na.rm=TRUE)))
Test<-cbind.data.frame('State'=trimws(row.names(Test)), 'Freq'=Test[,1])
Test<-Test[!(Test$State %in% c('AS', 'GU', 'PR', 'VI', 'HI', 'AK' )),]
str(Test)
```

```
## 'data.frame':	48 obs. of  2 variables:
##  $ State: Factor w/ 48 levels "AL","AR","AZ",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ Freq : num [1:48(1d)] 0.0788 0.1344 0.0352 0.1154 0.0675 ...
##   ..- attr(*, "dimnames")=List of 1
##   .. ..$ : chr  "AL" "AR" "AZ" "CA" ...
```

```r
#Now use ggplot again
p2<-ggplot(data=Test, aes(x=State, y=Freq)) + geom_bar(stat="identity", fill=c1[1:48]) + ggtitle('Chart') 
p2<- p2 +   labs(title="Barplot of VAMC Facilities per State per Thousand Square Miles") + xlab("State") + ylab("Number of Veterans Administration Health Facilities per 1000 Miles")
p2<- p2  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=8.5)) + coord_flip()
#p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
p2
```

![](ACasillas6306HW10_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

####e. What patterns do you see? By this metric, is there any region that seems relatively high for VA medical centers per thousand square miles? How about low? Given these data, what advice might you give your boss before you start modeling (and why)?

Rhode Island seems to be high in VA medical centers per thousand miles while Wyoming seems to be relatively low in the same number. I think that both are related to each having either relatively high or low are in square miles per its population. Rhode Island has low area with a decent size population, while Wyoming has a high area per its low population.
