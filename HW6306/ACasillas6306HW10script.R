load("C:/Users/acasi/Downloads/N-MHSS-2015-DS0001-data-r.rda")
data<-read.csv("C:/Users/acasi/Downloads/N-MHSS-2015-DS0001-data-excel.csv")
str(data)
str(mh2015_puf)
#ls(str(mh2015_puf))

head(mh2015_puf)
tail(mh2015_puf)

#As long as we are just looking at a list of state abbreviations and not a different object...

mh2015_puf$CASEID[1:3]
mh2015_puf$LST[1:3]
#substr(mh2015_puf$CASEID[1:3], nchar(mh2015_puf$CASEID[1:3]) - 2, nchar(mh2015_puf$CASEID[1:3]))
levels(mh2015_puf$LST)
unique(mh2015_puf$LST)


###Filter

F1= trimws(mh2015_puf$LST) %in% c('AS', 'GU', 'PR', 'VI', 'HI', 'AK' )
#F1[1:10]

#unique(mh2015_puf$FACILITYTYPE)
F2= grepl('(VAMC)', mh2015_puf$FACILITYTYPE )
#F2[1:10]

MHreduced<-mh2015_puf[F2 & !F1, ]
dim(mh2015_puf)
dim(MHreduced)
class(MHreduced)


##testthe filter
testFilter = paste(MHreduced$FACILITYTYPE, MHreduced$LST, sep=';')
#testFilter[1:10]
unique(testFilter)
unique(mh2015_puf$LST[!F1])

NDtest=mh2015_puf[trimws(mh2015_puf$LST) == 'ND' ,]
dim(NDtest)
unique(NDtest$FACILITYTYPE)
##testthe filter


###Barplot of something

#still c
#!!! Fix Levels
stateFREQ<-data.frame(tapply(MHreduced$CASEID, MHreduced$LST, function(y) sum(!is.na(y), na.rm=TRUE)))
#trimws(row.names(stateFREQ))
stateFREQ<-cbind.data.frame('State'=trimws(row.names(stateFREQ)), 'Freq'=stateFREQ[,1])
stateFREQ<-stateFREQ[!(stateFREQ$State %in% c('AS', 'GU', 'PR', 'VI', 'HI', 'AK' )),]
str(stateFREQ)
#now d

par(las=2)
barplot(height=stateFREQ$Freq, horiz = TRUE, col = "seashell1")

# colors = c("tomato2", "white", "springgreen3")
# palette()
# colors()
c=colors()
c1=c[grepl('1', c) & !grepl('gray', c) & !grepl('grey', c) & !grepl('gold', c) & !grepl('corn', c)
     & !grepl('light', c) & !grepl('white', c) & !grepl('orchid', c) & !grepl('lavender', c)
     & !grepl('wood', c) & !grepl('khaki', c) & !grepl('ivory', c) & !grepl('snow', c)
     & !grepl('tan', c) & !grepl('azure', c) & !grepl('honeydew', c) & !grepl('shell', c)]

# c1=c[seq(10, 480, by=10)]
# c2<-sample(colors(), 48)
# c2



library(ggplot2)


ggplot(MHreduced, aes(x=factor(LST))) + 
  #scale_fill_brewer(palette="Dark2") +
  scale_colour_manual(values=c[1:48]) +
  geom_bar(stat="count") + ggtitle('Chart') + 
  #scale_fill_brewer(palette="Blues") +
  labs(title="Click-through rate by age group") +
  xlab("Prob. of Click-Through Rate") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

xis.text.x = element_text(size=)

#ggplot(data=stateFREQ, aes(x=State, y=Freq)) + geom_bar(stat="identity", fill="steelblue")

p<-ggplot(data=stateFREQ, aes(x=State, y=Freq)) + geom_bar(stat="identity", fill=c1[1:48]) + ggtitle('Chart') 
p<- p +   labs(title="Barplot of VAMC Facilities per State") + xlab("State") + ylab("Number of Veterans Administration Health Facilities")
p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=6.5)) + coord_flip()
#p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
p


####### No. 2. ########

statesize<-read.csv("C:/Users/acasi/Downloads/statesize.csv")
str(statesize)

MHreducedS<-merge(MHreduced, statesize, by.x = 'LST', by.y = 'Abbrev')
str(MHreducedS)
ptest=paste(MHreduced$LST, sep=';')
ptest[1:10]
#leading + trailing blanks are preventing the merging


#2.b.
MHreduced$LST=trimws(MHreduced$LST)

unique(statesize$Abbrev)
sort(unique(MHreduced$LST))
#Also it does not have DC

MHreducedS<-merge(MHreduced, statesize, by.x = 'LST', by.y = 'Abbrev', all.x = TRUE)
MHreducedS2<-merge(MHreduced, statesize, by.x = 'LST', by.y = 'Abbrev', all.x = TRUE)
str(MHreducedS)

head(MHreducedS)

MHreducedS2$HospPKM <- 1*1000/MHreducedS2$SqMiles

#I will leave the trailing and leading blanks unfixed for later
colorframe<-data.frame('LST'=unique(MHreducedS2$LST), 'Colorz'=c1[1:48])
str(colorframe)
str(c1)
MHreducedS3<-merge(x=MHreducedS2, y=colorframe, by = 'LST', all.x = TRUE)
str(MHreducedS3)
names(MHreducedS3)
head(MHreducedS3$Colorz)
length(na.omit(MHreducedS3$Colorz))

MHreducedS3[MHreducedS3$LST == 'DC',]

COLORZ=as.character(MHreducedS3$Colorz[MHreducedS3$LST != 'DC'])
#I do this since ggplot will ignore DC


p<-ggplot(data=MHreducedS3, aes(x=LST, y=HospPKM)) + geom_bar(stat="identity", fill=COLORZ) + ggtitle('Chart') 
p<- p +   labs(title="Barplot of VAMC Facilities per State") + xlab("State") + ylab("Number of Veterans Administration Health Facilities")
p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=6.5)) + coord_flip()
#p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
p

mode(c[1:354])
mode(MHreducedS3$Colorz)
as.character(head(MHreducedS3$Colorz))


Test=data.frame(tapply(MHreducedS2$HospPKM, MHreducedS2$LST, function(y) sum(y, na.rm=TRUE)))
Test<-cbind.data.frame('State'=trimws(row.names(Test)), 'Freq'=Test[,1])
Test<-Test[!(Test$State %in% c('AS', 'GU', 'PR', 'VI', 'HI', 'AK' )),]
str(Test)

p<-ggplot(data=Test, aes(x=State, y=Freq)) + geom_bar(stat="identity", fill=c1[1:48]) + ggtitle('Chart') 
p<- p +   labs(title="Barplot of VAMC Facilities per State per Thousand Miles") + xlab("State") + ylab("Number of Veterans Administration Health Facilities per 1000 Miles")
p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_text(size=6.5)) + coord_flip()
#p<- p  + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + coord_flip()
p