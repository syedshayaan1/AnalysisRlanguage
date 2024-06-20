; SYED SHAYAAN HASNAIN AHMAD
; 20I - 0647
; SECTION A
; ASSIGNMENT 2 - PROBABILITY AND STATISTICS
;R LANGUAGE CODES

 ;Q1

library(MASS)
data1=painters$School
data1.freq=table(data1)
barplot(data1.freq)
boxplot(Composition~School,data1=data1)
boxplot(Drawing~School,data1=data1)
boxplot(Colour~School,data1=data1)
boxplot(Expression~School,data1=data1)

 ;Q2

library(qcc)
set1=c(state.division)
set2=c(state.region)
names(set1)=levels(set1)
pareto.chart(table(set1),ylab="Frequency of States",main="Pareto chart for state divisions",col=heat.colors(length(set1)))
names(set2)=levels(set2)
pareto.chart(table(set2),ylab="Frequency of States",main="Pareto chart for state regions",col=heat.colors(length(set2))


 ;Q3

data(state)
detail1 = state.x77
summary(detail1)

s1 <- state.x77[,1]

barplot(
  s1, 
  main = "Graph Of Population", 
  xlab = "States", 
  ylab = "Population", 
  col = rainbow(2), 
  beside = TRUE
)
s2 <- state.x77[,2]

barplot(
  s2, 
  main = "Graph Of Income", 
  xlab = "States", 
  ylab = "Income", 
  col = rainbow(2), 
  beside = TRUE
)


 ;Q4

x77=data.frame(state.x77)
attach(x77)
boxplot(Population)
boxplot(Area)
boxplot(Income)

 ;Q5

filepath=paste(normalizePath(dirname("Assignment2.R")),"\\","file1.csv", sep="")
filepath
write.csv(as.data.frame(state.x77),filepath,row.names=TRUE)
set1=read.csv(filepath)
set2=set1[c(1,2,3,8,9)]
set3=data[order(set2$Income),]
set4=tail(set3,20)
set4

 ;Q6

ddataFrameOfState <- as.data.frame((state.x77))


dataFrameOfState$Division <- state.division


dataFrameOfState$Region <- state.region

mountainData <- dataFrameOfState[dataFrameOfState$Division == "Mountain", ]

mountainData <- mountainData[1:5, ]


pacificData <- dataFrameOfState[dataFrameOfState$Division == "Pacific", ]

pacificData <- pacificData[1:5,]


concateStateNames = paste(rownames(pacificData), 
                       rownames(mountainData))



color1 = c("orange", "purple")
color2 = c("yellow", "red")


barplot(rbind(mountainData$Area,
                pacificData$Area
              ), 
        main = "Division based on Area", 
        names.arg = concateStateNames, 
        xlab = "States", 
        ylab = "Area",
        col = color1,
        beside = TRUE, 
        )

legend("topright", 
       c("Mountain Division", "Pacific Division"), 
       fill = color1
       )


#Based on Income
barplot(rbind(mountainData$Income,
                pacificData$Income
              ), 
        main = "Division based on Income", 
        names.arg = concateStateNames, 
        xlab = "States", 
        ylab = "Income",
        col = color2,
        beside = TRUE, 
        )

legend("topright", 
       c("Mountain Division", "Pacific Division"), 
       fill = color2
       )


 
