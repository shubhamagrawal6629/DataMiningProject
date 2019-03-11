windows(record=TRUE)

# Installing required packages
#install.packages("rggobi")

# Calling necessary libraries
#library(ggplot2)
#library(rggobi)

#---------------------------------------#
# Setting up the directory and database #
#---------------------------------------#
Student.Number <- "101088497"
Initials <- "GK"
ASorLAB <- "Assignment"
Assignment.Number <- "2"
Student.info <- paste(Student.Number, Initials, ASorLAB, Assignment.Number, sep="-")

# "drive", AND "path.up" SHOULD BE THE ONLY PARTS THAT REQUIRE YOUR PROFESSOR 
# OR TA TO BE ABLE TO RUN YOUR CODE
drive="D:"
path.upto <- paste("School", "STAT5703-Data Mining", sep="/" )
code.dir <- paste(drive, path.upto, Student.info, "Code", sep="/")
data.dir <- paste(drive, path.upto, Student.info, "Data", sep="/")
work.dir <- paste(drive, path.upto, Student.info, "Work", sep="/")
setwd(work.dir)

library(ggplot2)
library(DescTools)
library(aod)
library(ROCR)
library(pROC)
library(dplyr)
library(readxl)
library(MASS)
library(Rcpp)
library(sand)
library(igraph)
library(ppcor)
library(corrplot)


#Read file and checking file contents
pubg_stat <- paste(data.dir,"PUBG_Player_Statistics.csv", sep="/")
pubg_statistics <- read.csv(pubg_stat, na.strings="")
summary(pubg_statistics)
head(pubg_statistics)
dim(pubg_statistics)
sum(is.na(pubg_statistics))
sapply(pubg_statistics, function(x) sum(is.na(x)))

#separating out solo,duo and squad data in different variables

solo_data <-pubg_statistics[,1:52]
duo_data <-pubg_statistics[,c(1:2,53:102)]
squad_data<- pubg_statistics[,c(1:2,103:152)]



hist(solo_data$solo_RoundsPlayed, breaks=100, col="grey", main = "Solo ", xlab = "no of games played", ylab = "no of users")
Desc(solo_data1$solo_RoundsPlayed)

hist(duo_data$duo_RoundsPlayed, breaks=100, col="orange", main = "DUO ", xlab = "no of games played", ylab = "no of users")
Desc(solo_data1$solo_RoundsPlayed)

hist(squad_data$squad_RoundsPlayed, breaks=100, col="yellow", main = "Squad ", xlab = "no of games played", ylab = "no of users")
Desc(solo_data1$solo_RoundsPlayed)


#subset datasets according to rounds played by them
solo_data1 <- subset(solo_data, solo_RoundsPlayed >=50)
dim(solo_data1)

duo_data1 <- subset(duo_data, duo_RoundsPlayed >=50)
dim(duo_data1)

squad_data1 <- subset(squad_data, squad_RoundsPlayed >=50)
dim(squad_data1)



hist(solo_data1$solo_RoundsPlayed, breaks=100, col="grey", main = "Solo ", xlab = "no of games played", ylab = "no of users")
hist(duo_data1$duo_RoundsPlayed, breaks=100, col="orange", main = "Duo ", xlab = "no of games played", ylab = "no of users")
hist(squad_data1$squad_RoundsPlayed, breaks=100, col="yellow", main = "Squad ", xlab = "no of games played", ylab = "no of users")

#Analysis of Win Ratio in all 3 modes


hist(solo_data1$solo_WinRatio, breaks=100, col="blue", main = "Solo Win ratio", xlab = "Win %")
Desc(solo_data1$solo_WinRatio)

summary(solo_data1)


hist(duo_data1$duo_WinRatio, breaks=100, col="green", main = "Duo Win ratio", xlab = "Win %")
Desc(duo_data1$duo_WinRatio)

summary(duo_data1)


hist(squad_data1$squad_WinRatio, breaks=100, col="red", main = "Squad Win ratio", xlab = "Win %")
Desc(squad_data1$squad_WinRatio)

summary(squad_data1)



#Analysis of top 10 ratio

hist(solo_data1$solo_Top10Ratio, breaks=100, col="blue", main = "solo top 10 ratio", xlab = "top 10 %")
Desc(solo_data1$solo_Top10Ratio)

summary(solo_data1)


hist(duo_data1$duo_Top10Ratio, breaks=100, col="green", main = "duo Top 10 ratio", xlab = "top 10 %")
Desc(duo_data1$duo_Top10Ratio)

summary(duo_data1)


hist(squad_data1$squad_Top10Ratio, breaks=100, col="red", main = "squad Top 10 ratio", xlab = "top 10 %")
Desc(squad_data1$squad_Top10Ratio)

summary(squad_data1)



#Analysis of Kill to Death Ratio


hist(solo_data1$solo_KillDeathRatio, breaks=100, col="blue", main = "solo Kil Death ratio", xlab = "kill vs death %")
Desc(solo_data1$solo_KillDeathRatio)

summary(solo_data1)


hist(duo_data1$duo_KillDeathRatio, breaks=100, col="green", main = "duo Kill Death ratio", xlab = "kill vs death %")
Desc(duo_data1$duo_KillDeathRatio)

summary(duo_data1)


hist(squad_data1$squad_KillDeathRatio, breaks=100, col="red", main = "squad Kill Death ratio", xlab = "kill vs death %")
Desc(squad_data1$squad_KillDeathRatio)

summary(squad_data1)


#comparing various variables with the WinRatio to see how they affect the winning ratio for solo mode

plot(solo_data1$solo_KillDeathRatio, solo_data1$solo_WinRatio, xlab = "KD ratio",
     ylab = "Win ratio", main = "Win ratio by KD (Solo)", col = "blue", pch = 20)
Desc(solo_data1$solo_WinRatio ~ solo_data1$solo_KillDeathRatio)

plot(solo_data1$solo_MoveDistancePg, solo_data1$solo_WinRatio, xlab = "Distance moved per game (meters)",
     ylab = "Win ratio", main = "Win ratio by Distance moved PG (Solo)", col = "chartreuse1", pch = 20)
Desc(solo_data1$solo_WinRatio ~ solo_data1$solo_MoveDistancePg)

plot(solo_data1$solo_DamagePg, solo_data1$solo_WinRatio, xlab = "Damage dealt per game",
     ylab = "Win ratio", main = "Win ratio by Damage dealt PG (Solo)", col = "red", pch = 20)
Desc(solo_data1$solo_WinRatio ~ solo_data1$solo_DamagePg)


#comparing various variables with the WinRatio to see how they affect the winning ratio for duo mode

plot(duo_data1$duo_KillDeathRatio, duo_data1$duo_WinRatio, xlab = "KD ratio",
     ylab = "Win ratio", main = "Win ratio by KD (duo)", col = "blue", pch = 20)
Desc(duo_data1$duo_WinRatio ~ duo_data1$duo_KillDeathRatio)

plot(duo_data1$duo_MoveDistancePg, duo_data1$duo_WinRatio, xlab = "Distance moved per game (meters)",
     ylab = "Win ratio", main = "Win ratio by Distance moved PG (duo)", col = "chartreuse1", pch = 20)
Desc(duo_data1$duo_WinRatio ~ duo_data1$duo_MoveDistancePg)

plot(duo_data1$duo_DamagePg, duo_data1$duo_WinRatio, xlab = "Damage dealt per game",
     ylab = "Win ratio", main = "Win ratio by Damage dealt PG (duo)", col = "red", pch = 20)
Desc(duo_data1$duo_WinRatio ~ duo_data1$duo_DamagePg)


#comparing various variables with the WinRatio to see how they affect the winning ratio for squad mode

plot(squad_data1$squad_KillDeathRatio, squad_data1$squad_WinRatio, xlab = "KD ratio",
     ylab = "Win ratio", main = "Win ratio by KD (squad)", col = "blue", pch = 20)
Desc(squad_data1$squad_WinRatio ~ squad_data1$squad_KillDeathRatio)

plot(squad_data1$squad_MoveDistancePg, squad_data1$squad_WinRatio, xlab = "Distance moved per game (meters)",
     ylab = "Win ratio", main = "Win ratio by Distance moved PG (squad)", col = "chartreuse1", pch = 20)
Desc(squad_data1$squad_WinRatio ~ squad_data1$squad_MoveDistancePg)

plot(squad_data1$squad_DamagePg, squad_data1$squad_WinRatio, xlab = "Damage dealt per game",
     ylab = "Win ratio", main = "Win ratio by Damage dealt PG (squad)", col = "red", pch = 20)
Desc(squad_data1$squad_WinRatio ~ squad_data1$squad_DamagePg)



#lets visualize the longest kill for all 3 modes 

hist(solo_data1$solo_LongestKill, xlab = 'Distance (meters)', 
     col = "lightblue", main = "Solo Long Distance Kills")

hist(duo_data1$duo_LongestKill, xlab = 'Distance (meters)', 
     col = "lightgreen", main = "Duo Long Distance Kills")

hist(squad_data1$squad_LongestKill, xlab = 'Distance (meters)', 
     col = "orange", main = "Squad Long Distance Kills")

summary(solo_data1$solo_LongestKill)
summary(duo_data1$duo_LongestKill)
summary(squad_data1$squad_LongestKill)

#lets visualize time survived in all 3 modes


hist(solo_data1$solo_TimeSurvivedPg, xlab = 'Time Survived', 
     col = "lightblue", main = "Solo Time Survived")

hist(duo_data1$duo_TimeSurvivedPg, xlab = 'Time survived', 
     col = "lightgreen", main = "Duo Time survived")

hist(squad_data1$squad_TimeSurvivedPg, xlab = 'Time Survived', 
     col = "orange", main = "Squad Time Survived")

summary(solo_data1$solo_TimeSurvivedPg)
summary(duo_data1$duo_TimeSurvivedPg)
summary(squad_data1$squad_TimeSurvivedPg)


#shortening dataset by including only important data from each model

solo_data2<- solo_data1[,c("solo_WinRatio" , "solo_KillDeathRatio" , "solo_TimeSurvivedPg" , "solo_RoundsPlayed" , "solo_DamagePg" , 
                           "solo_HeadshotKillsPg", "solo_KillsPg" , "solo_MoveDistancePg" , "solo_HealsPg" )]

duo_data2<- duo_data1[,c("duo_WinRatio" , "duo_KillDeathRatio" , "duo_TimeSurvivedPg" , "duo_RoundsPlayed" , "duo_DamagePg" , 
                         "duo_HeadshotKillsPg", "duo_KillsPg" , "duo_MoveDistancePg" , "duo_HealsPg" )]

squad_data2<- squad_data1[,c("squad_WinRatio" , "squad_KillDeathRatio" , "squad_TimeSurvivedPg" , "squad_RoundsPlayed" , "squad_DamagePg" , 
                             "squad_HeadshotKillsPg", "squad_KillsPg" , "squad_MoveDistancePg" , "squad_HealsPg" )]


dim(solo_data2)
dim(duo_data2)
dim(squad_data2)

#lets see how closely are these parameters related to each other

solo_data_relation <- cor(solo_data2)
duo_data_relation <- cor(duo_data2)
squad_data_relation <- cor(squad_data2)

corrplot(solo_data_relation, method = "number")
corrplot(duo_data_relation, method = "number")
corrplot(squad_data_relation, method = "number")

pubg_statistics <- na.omit(pubg_statistics)


#___________________________________________
#          Training and Test Dataset
#___________________________________________
get.train <- function (data.sz, train.sz)
{
  set.seed(123)
  # Take subsets of data for training/test samples
  # Return the indices
  train.ind <- sample(data.sz, train.sz)
  test.ind <- (data.sz) %w/o% train.ind
  list(train=train.ind, test=test.ind)
}

pubg_statistics_train <- subset(pubg_statistics, pubg_statistics$tracker_id<=164880)
pubg_statistics_test <- subset(pubg_statistics, pubg_statistics$tracker_id>164880)
dim(pubg_statistics_test)
dim(pubg_statistics_train)

#let's see in which mode a particular person performs well

summary(pubg_statistics$tracker_id)

pubg_statistics$comparison[pubg_statistics$solo_KillDeathRatio>pubg_statistics$duo_KillDeathRatio & pubg_statistics$solo_KillDeathRatio > pubg_statistics$squad_KillDeathRatio] <- '1'
pubg_statistics$comparison[pubg_statistics$duo_KillDeathRatio>pubg_statistics$solo_KillDeathRatio & pubg_statistics$duo_KillDeathRatio > pubg_statistics$squad_KillDeathRatio] <- '2'
pubg_statistics$comparison[pubg_statistics$squad_KillDeathRatio>pubg_statistics$duo_KillDeathRatio & pubg_statistics$squad_KillDeathRatio > pubg_statistics$solo_KillDeathRatio] <- '3'
pubg_statistics$comparison<-as.numeric(as.character(pubg_statistics$comparison))
str(pubg_statistics$comparison)
is.numeric(pubg_statistics$comparison)

pubg_statistics_train$comparison[pubg_statistics_train$solo_KillDeathRatio>pubg_statistics_train$duo_KillDeathRatio & pubg_statistics_train$solo_KillDeathRatio > pubg_statistics_train$squad_KillDeathRatio] <- '1'
pubg_statistics_train$comparison[pubg_statistics_train$duo_KillDeathRatio>pubg_statistics_train$solo_KillDeathRatio & pubg_statistics_train$duo_KillDeathRatio > pubg_statistics_train$squad_KillDeathRatio] <- '2'
pubg_statistics_train$comparison[pubg_statistics_train$squad_KillDeathRatio>pubg_statistics_train$duo_KillDeathRatio & pubg_statistics_train$squad_KillDeathRatio > pubg_statistics_train$solo_KillDeathRatio] <- '3'
pubg_statistics_train$comparison<-as.numeric(as.character(pubg_statistics_train$comparison))
str(pubg_statistics_train$comparison)
is.numeric(pubg_statistics_train$comparison)

pubg_statistics_test$comparison[pubg_statistics_test$solo_KillDeathRatio>pubg_statistics_test$duo_KillDeathRatio & pubg_statistics_test$solo_KillDeathRatio > pubg_statistics_test$squad_KillDeathRatio] <- '1'
pubg_statistics_test$comparison[pubg_statistics_test$duo_KillDeathRatio>pubg_statistics_test$solo_KillDeathRatio & pubg_statistics_test$duo_KillDeathRatio > pubg_statistics_test$squad_KillDeathRatio] <- '2'
pubg_statistics_test$comparison[pubg_statistics_test$squad_KillDeathRatio>pubg_statistics_test$duo_KillDeathRatio & pubg_statistics_test$squad_KillDeathRatio > pubg_statistics_test$solo_KillDeathRatio] <- '3'
pubg_statistics_test$comparison<-as.numeric(as.character(pubg_statistics_test$comparison))
str(pubg_statistics_test$comparison)
is.numeric(pubg_statistics_test$comparison)

head(pubg_statistics$comparison)
pubg_statistics$comparison
head(pubg_statistics_train$comparison)
pubg_statistics_train$comparison
head(pubg_statistics_test$comparison)
pubg_statistics_test$comparison

pubg_statistics <- na.omit(pubg_statistics)
summary(pubg_statistics)
pubg_statistics$comparison

pubg_statistics_train <- na.omit(pubg_statistics_train)
pubg_statistics_train$comparison

pubg_statistics_test <- na.omit(pubg_statistics_test)
pubg_statistics_test$comparison

out = c (nrow(pubg_statistics_train), nrow(pubg_statistics_test))
x.names=c("Training","Test")
barplot(out, main="Data Spliting",xaxt="n")
axis(1,at = 1:2,labels=x.names)

#___________________________________________
#          Standardise Dataset
#___________________________________________

f.data.std <- function(data) {
  data <- as.matrix(data)
  bar <- apply(data, 2, mean)
  s <- apply(data, 2, sd)
  t((t(data) - bar)/s)
}
pubg_statistics.std <- f.data.std(pubg_statistics[-1])
head(pubg_statistics.std)
summary(pubg_statistics.std)


pubg_statistics_train.std <- f.data.std(pubg_statistics_train[-1])
head(pubg_statistics_train.std)
summary(pubg_statistics_train.std)



pubg_statistics_test.std <- f.data.std(pubg_statistics_test[-1])
head(pubg_statistics_test.std)
summary(pubg_statistics_test.std)



pubg_statistics.std[is.nan(pubg_statistics.std)] <- 0
pubg_statistics_train.std[is.nan(pubg_statistics_train.std)] <- 0
pubg_statistics_test.std[is.nan(pubg_statistics_test.std)] <- 0

#___________________________________________
#          Dimension Reduction 
#___________________________________________

#___________________________________________
#          PCA 
#___________________________________________

#Using prcomp to get principal component vectors 

library(stats)
pc.pubg_statistics <- prcomp(pubg_statistics.std)
summary(pc.pubg_statistics)
plot(pc.pubg_statistics, main="Scree Plot: PCA for Raw dataset", col=heat.colors(15))
# First  Principal Components
pc1.pubg_statistics <- data.frame(pc.pubg_statistics$x[,1:3])
head(pc1.pubg_statistics)

pc.pubg_statistics_train <- prcomp(pubg_statistics_train.std)
summary(pc.pubg_statistics_train)
plot(pc.pubg_statistics, main="Scree Plot: PCA for Training dataset", col=heat.colors(15))
# First  Principal Components
pc1.pubg_statistics_train <- data.frame(pc.pubg_statistics_train$x[,1:3])
head(pc1.pubg_statistics_train)

pc.pubg_statistics_test <- prcomp(pubg_statistics_test.std[,])
summary(pc.pubg_statistics_test)
plot(pc.pubg_statistics_test, main="Scree Plot: PCA for Test dataset", col=heat.colors(15))
# First  Principal Components
pc1.pubg_statistics_test <- data.frame(pc.pubg_statistics_test$x[,1:3])
head(pc1.pubg_statistics_test)
#___________________________________________
#          Whitened Dataset Center and Sphere 
#___________________________________________

Sphere.Data <- function(data) {
  data <- as.matrix(data)
  data <- t(t(data) - apply(data, 2, mean))
  data.svd <- svd(var(data))
  sphere.mat <- t(data.svd$v %*% (t(data.svd$u) * (1/sqrt(data.svd$d))))
  return(data %*% sphere.mat)
}

pubg_statistics.white <- Sphere.Data(pubg_statistics[-1])
colnames(pubg_statistics.white) <- colnames(pubg_statistics.std)
apply(pubg_statistics.white, 2, mean)
apply(pubg_statistics.white, 2, sd)
head(pubg_statistics.white)

pubg_statistics_train.white <- Sphere.Data(pubg_statistics_train[-1])
colnames(pubg_statistics_train.white) <- colnames(pubg_statistics_train.std)
apply(pubg_statistics_train.white, 2, mean)
apply(pubg_statistics_train.white, 2, sd)
head(pubg_statistics_train.white)

pubg_statistics_test.white <- Sphere.Data(pubg_statistics_test[-1])
colnames(pubg_statistics_test.white) <- colnames(pubg_statistics_test.std)
apply(pubg_statistics_test.white, 2, mean)
apply(pubg_statistics_test.white, 2, sd)
head(pubg_statistics_test.white)

#___________________________________________
#          Dimension Reduction 
#___________________________________________

#___________________________________________
#          ICA 
#___________________________________________
library(fastICA)
pubg_statistics.white.ica <- fastICA(pubg_statistics.white[,-1], 3, alg.typ = "parallel", fun = "logcosh", alpha = 1,method = "R", 
                                     row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)

pubg_statistics.ica<-pubg_statistics.white.ica$S
head(pubg_statistics.ica)

pubg_statistics_train.white.ica <- fastICA(pubg_statistics_train.white[,-1], 3, alg.typ = "parallel", fun = "logcosh", alpha = 1,method = "R", 
                                           row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)

pubg_statistics_train.ica<-pubg_statistics_train.white.ica$S
head(pubg_statistics_train.ica)

pubg_statistics_test.white.ica <- fastICA(pubg_statistics_test.white[,-1], 3, alg.typ = "parallel", fun = "logcosh", alpha = 1,method = "R", 
                                          row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)

pubg_statistics_test.ica<-pubg_statistics_test.white.ica$S
head(pubg_statistics_test.ica)



#___________________________________________
#          Data Reduction 
#___________________________________________

#*******************************************
#          Davies Bouldin Function
#*******************************************

Davies.Bouldin <- function(A, SS, m) {
  # A  - the centres of the clusters
  # SS - the within sum of squares
  # m  - the sizes of the clusters
  N <- nrow(A)   # number of clusters
  # intercluster distance
  S <- sqrt(SS/m)
  # Get the distances between centres
  M <- as.matrix(dist(A))
  # Get the ratio of intercluster/centre.dist
  R <- matrix(0, N, N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      R[i,j] <- (S[i] + S[j])/M[i,j]
      R[j,i] <- R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}

#*******************************************
#error_cal Function to calculate wrong classifications in cluster
#*******************************************

error_cal <- function(tbl,cluster_size)
{
  wrong_data <- 0
  for(clust in 1:cluster_size)
  {
    wrong_data <- wrong_data + (sum(tbl[,clust])-max(tbl[,clust]))
  }
  return(wrong_data)
}

#Function for Euclidean Clustering
clustering_euclidean <- function(data_set,data_set.orig, limit) 
{
  
  oldpar <- par(mfrow = c(4,4))
  par(mar=c(2,1,2,1))
  
  errs <- rep(0, 7)
  DBI <- rep(0, 7)
  library(cluster)
  library(fpc)
  library(flexclust)
  library(stats)
  for (i in limit)
  {
    min_error <- 250
    min_error_km <- 0
    best.seed <- 0
    #Loop for Seed
    for (j in 2:10)
    {  
      
      set.seed(j)
      #Clustering Using K means
      KM <- kmeans((data_set[,]), i, 25)
      ct.km <- table(KM$cluster, KM$cluster)
      
      #Calculating toal wrong data for each seed         
      error <- error_cal(ct.km,i)
      if(min_error > error)
      {
        #Storing Error count and Kmeans output and best seed for min error
        min_error <- error
        min_error_km <-KM
        best.seed <- j
      }  
      
    }     
    print(paste("Best Seed for the Cluster Size " , i ,"is " , best.seed))
    print(paste("Total Wrong Classification in Cluster Size " , i ,"is " , min_error))
    print(paste("Centroids for the Cluster Size " , i ,"are :"))
    print(min_error_km$centers)
    print(min_error_km)
    
    #Plotting the CLuster
    plotcluster(data_set, col=min_error_km$cluster,min_error_km$cluster, main=paste(i,"Clusters"))
    
    if(length(limit) > 1)
    {  
      #CLuster Analysis
      errs[i-1] <- sum(min_error_km$withinss)
      DBI[i-1] <- Davies.Bouldin(min_error_km$centers, min_error_km$withinss, min_error_km$size)
      
      
    }
    
  }
  if(length(limit) > 1)
  {
    plot(2:10, errs, main = "SSE")
    lines(2:10, errs)
    #
    plot(2:10, DBI, main = "Davies-Bouldin")
    lines(2:10, DBI)
    #
  }
  else
  {  
    return(min_error_km)
  }
  return(errs)
}

cluster_range <- 2:10

library (flexclust)

#*******************************************
#          Clustering: Raw DataSet pubg_statistics
#*******************************************

clust.pubg_statistics <- clustering_euclidean(pubg_statistics[,c(2:4,6,14:18,22,53:54,56,64:68,72,103:104,106,114:118,122,153)], pubg_statistics,cluster_range)

#Optimal Cluster Size is 2

clus.pubg_statistics<- clustering_euclidean(pubg_statistics[,c(2:4,6,14:18,22,53:54,56,64:68,72,103:104,106,114:118,122,153)], pubg_statistics, 2)

group1<-pubg_statistics[clus.pubg_statistics$cluster == 1,]$comparison
group2<-pubg_statistics[clus.pubg_statistics$cluster == 2,]$comparison

#which is highest performance mode group
highest.perf.mode.group7 <- which.max(c(mean(group1),mean(group2))) 
#which is lowest performance mode group
lowest.perf.mode.group7 <- which.min(c(mean(group2),mean(group2))) 


highest.perf.mode.group.raw <- pubg_statistics[clus.pubg_statistics$cluster == highest.perf.mode.group7, c(1,153)]
lowest.perf.mode.group.raw <- pubg_statistics[clus.pubg_statistics$cluster == lowest.perf.mode.group7, c(1,153)]

print(clus.pubg_statistics$size)

print(paste("Raw: Highest performance mode Group is g", highest.perf.mode.group7, sep=""))

print(paste("Raw: Lowest performance mode Group is g", lowest.perf.mode.group7, sep=""))

head(highest.perf.mode.group.raw[order(highest.perf.mode.group.raw$comparison, decreasing=TRUE), ])

tail(lowest.perf.mode.group.raw[order(lowest.perf.mode.group.raw$comparison, decreasing=TRUE), ])

#*******************************************
#          Clustering: Standard DataSet pubg_statistics.std
#*******************************************
clust.pubg_statistics.std <- clustering_euclidean(pubg_statistics.std, pubg_statistics,cluster_range)

#Optimal Cluster Size is 8

clus.pubg_statistics.std <- clustering_euclidean(pubg_statistics.std, pubg_statistics, 8)

group1.std<-pubg_statistics[clus.pubg_statistics.std$cluster == 1,]$comparison
group2.std<-pubg_statistics[clus.pubg_statistics.std$cluster == 2,]$comparison
group3.std<-pubg_statistics[clus.pubg_statistics.std$cluster == 3,]$comparison
group4.std<-pubg_statistics[clus.pubg_statistics.std$cluster == 4,]$comparison
group5.std<-pubg_statistics[clus.pubg_statistics.std$cluster == 5,]$comparison
group6.std<-pubg_statistics[clus.pubg_statistics.std$cluster == 6,]$comparison
group7.std<-pubg_statistics[clus.pubg_statistics.std$cluster == 7,]$comparison
group8.std<-pubg_statistics[clus.pubg_statistics.std$cluster == 8,]$comparison

#which is highest performance mode group
highest.perf.mode.group <- which.max(c(mean(group1.std),mean(group2.std), mean(group3.std),mean(group4.std),mean(group5.std),mean(group6.std), mean(group7.std), mean(group8.std))) 
#which is lowest performance mode group
lowest.perf.mode.group <- which.min(c(mean(group1.std),mean(group2.std), mean(group3.std),mean(group4.std),mean(group5.std),mean(group6.std), mean(group7.std), mean(group8.std))) 


highest.perf.mode.group.std <- pubg_statistics[clus.pubg_statistics.std$cluster == highest.perf.mode.group, c(1,153)]
lowest.perf.mode.group.std <- pubg_statistics[clus.pubg_statistics.std$cluster == lowest.perf.mode.group, c(1,153)]

print(clus.pubg_statistics.std$size)

print(paste("Standard: Highest performance mode Group is g", highest.perf.mode.group, sep=""))

print(paste("Standard: Lowest performance mode Group is g", lowest.perf.mode.group, sep=""))

head(highest.perf.mode.group.std[order(highest.perf.mode.group.std$comparison, decreasing=TRUE), ])

tail(lowest.perf.mode.group.std[order(lowest.perf.mode.group.std$comparison, decreasing=TRUE), ])

#*******************************************
#          Clustering: Whitened DataSet pubg_statistics.white
#*******************************************
cluster_range <- 2:10

library (flexclust)

#Clustering on Whitened Raw DataSet pubg_statistics
clust.pubg_statistics.white <- clustering_euclidean(pubg_statistics.white, pubg_statistics,cluster_range)

#Optimal Cluster Size is 10
clus.pubg_statistics.white <- clustering_euclidean(pubg_statistics.white, pubg_statistics,10)

group1.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 1,]$comparison
group2.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 2,]$comparison
group3.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 3,]$comparison
group4.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 4,]$comparison
group5.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 5,]$comparison
group6.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 6,]$comparison
group7.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 7,]$comparison
group8.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 8,]$comparison
group9.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 9,]$comparison
group10.white<-pubg_statistics[clus.pubg_statistics.white$cluster == 10,]$comparison

#which is highest performance mode group
highest.perf.mode.group <- which.max(c(mean(group1.white),mean(group2.white), mean(group3.white),mean(group4.white),mean(group5.white),mean(group6.white), mean(group7.white), mean(group8.white),mean(group9.white),mean(group10.white))) 
#which is lowest performance mode group
lowest.perf.mode.group <- which.min(c(mean(group1.white),mean(group2.white), mean(group3.white),mean(group4.white),mean(group5.white),mean(group6.white), mean(group7.white), mean(group8.white),mean(group9.white),mean(group10.white))) 


highest.perf.mode.group.white <- pubg_statistics[clus.pubg_statistics.white$cluster == highest.perf.mode.group, c(1,153)]
lowest.perf.mode.group.white <- pubg_statistics[clus.pubg_statistics.white$cluster == lowest.perf.mode.group, c(1,153)]

print(clus.pubg_statistics.white$size)

print(paste("Whitened: Highest performance mode Group is g", highest.perf.mode.group, sep=""))

print(paste("Whitened: Lowest performance mode Group is g", lowest.perf.mode.group, sep=""))

head(highest.perf.mode.group.white[order(highest.perf.mode.group.white$comparison, decreasing=TRUE), ])

tail(lowest.perf.mode.group.white[order(lowest.perf.mode.group.white$comparison, decreasing=TRUE), ])


#*******************************************
#          Data Reduction on Standardized dataset
#*******************************************
## Function Set the indices for the  sets
get.subset <- function (data, size)
{
  set.seed(123)
  data_subset <- sample(data, size)
}

pubg_statistics.std.new <- pubg_statistics
pubg_statistics.std.new$cluster <- clus.pubg_statistics.std$cluster

median1<-median(pubg_statistics.std.new$comparison[pubg_statistics.std.new$cluster == 1])
median2<-median(pubg_statistics.std.new$comparison[pubg_statistics.std.new$cluster == 2])
median3<-median(pubg_statistics.std.new$comparison[pubg_statistics.std.new$cluster == 3])
median4<-median(pubg_statistics.std.new$comparison[pubg_statistics.std.new$cluster == 4])
median5<-median(pubg_statistics.std.new$comparison[pubg_statistics.std.new$cluster == 5])
median6<-median(pubg_statistics.std.new$comparison[pubg_statistics.std.new$cluster == 6])
median7<-median(pubg_statistics.std.new$comparison[pubg_statistics.std.new$cluster == 7])
median8<-median(pubg_statistics.std.new$comparison[pubg_statistics.std.new$cluster == 8])
print(paste("Median Values-> Cluster1 = ", median1,", Cluster2 = ",median2,", Cluster3 = ",median3,",Cluster4 = ",median4,",Cluster5 = ",median3,",Cluster6 = ",median6,",Cluster7 = ",median7,",Cluster8 = ",median8, sep=""))

pub1.std.ind <- which(pubg_statistics.std.new$cluster == 1)
pub2.std.ind <- which(pubg_statistics.std.new$cluster == 2)
pub3.std.ind <- which(pubg_statistics.std.new$cluster == 3)
pub4.std.ind <- which(pubg_statistics.std.new$cluster == 4)
pub5.std.ind <- which(pubg_statistics.std.new$cluster == 5)
pub6.std.ind <- which(pubg_statistics.std.new$cluster == 6)
pub7.std.ind <- which(pubg_statistics.std.new$cluster == 7)
pub8.std.ind <- which(pubg_statistics.std.new$cluster == 8)

pub1.std.size <- round((2*length(pub1.std.ind))/3)
pub2.std.size <- round((2*length(pub2.std.ind))/3)
pub3.std.size <- round((2*length(pub3.std.ind))/3)
pub4.std.size <- round((2*length(pub4.std.ind))/3)
pub5.std.size <- round((2*length(pub5.std.ind))/3)
pub6.std.size <- round((2*length(pub6.std.ind))/3)
pub7.std.size <- round((2*length(pub7.std.ind))/3)
pub8.std.size <- round((2*length(pub8.std.ind))/3)


pub1.std <- get.subset(pub1.std.ind,pub1.std.size)
pub2.std <- get.subset(pub2.std.ind,pub2.std.size)
pub3.std <- get.subset(pub3.std.ind,pub3.std.size)
pub4.std <- get.subset(pub4.std.ind,pub4.std.size)
pub5.std <- get.subset(pub5.std.ind,pub5.std.size)
pub6.std <- get.subset(pub6.std.ind,pub6.std.size)
pub7.std <- get.subset(pub7.std.ind,pub7.std.size)
pub8.std <- get.subset(pub8.std.ind,pub8.std.size)


pubg_statistics.std_reduce_ind <- c(pub1.std,pub2.std,pub3.std,pub4.std,pub5.std,pub6.std,pub7.std,pub8.std)

pubg_statistics.std_reduce <- pubg_statistics[pubg_statistics.std_reduce_ind,]

head(pubg_statistics.std_reduce)

#Dividing the reduced standardised dataset into training and test on the basis of tracker_id and its proper 1/3 test and 2/3 train
nrow(pubg_statistics.std_reduce[pubg_statistics.std_reduce$tracker_id>164880,])
nrow(pubg_statistics.std_reduce[pubg_statistics.std_reduce$tracker_id<=164880,])


#Train Dataset with tracker_id<=164880 and total rows are 43409 (earlier it was 65081)
pubg_statistics.std_reduce_train <- pubg_statistics.std_reduce[pubg_statistics.std_reduce$tracker_id<=164880,]

#Train Dataset with tracker_id<=164880 and total rows are 14436 (earlier it was 21687)
pubg_statistics.std_reduce_test <- pubg_statistics.std_reduce[pubg_statistics.std_reduce$tracker_id>164880,]

out.std_reduce = c(nrow(pubg_statistics.std_reduce),nrow(pubg_statistics.std_reduce_train), nrow(pubg_statistics.std_reduce_test))
x.names=c("Complete","Training","Test")
barplot(out.std_reduce,main="Reduced Standardized Data Spliting",xaxt="n",width=c(1,1,1))
axis(1,at = 1:3,labels=x.names)


#___________________________________________
#          Unsupervised Learning 
#___________________________________________

#*******************************************
#          Clustering: Training DataSet pubg_statistics_train
#*******************************************
cluster_range <- 2:10
pubg_statistics_train[0,]

#Clustering on Raw Train DataSet pubg_statistics_train
clust.pubg_statistics_train <- clustering_euclidean(pubg_statistics_train[,c(2:4,6,14:18,22,53:54,56,64:68,72,103:104,106,114:118,122,153)],pubg_statistics_train,cluster_range)

#Optimal Cluster Size is 2
clus.pubg_statistics_train <- clustering_euclidean(pubg_statistics_train[,c(2:4,6,14:18,22,53:54,56,64:68,72,103:104,106,114:118,122,153)], clust.pubg_statistics_train,2)

group1_train<-pubg_statistics[clus.pubg_statistics_train$cluster == 1,]$comparison
group2_train<-pubg_statistics[clus.pubg_statistics_train$cluster == 2,]$comparison

#which is highest performance mode group
highest.perf.mode.group1 <- which.max(c(mean(group1_train),mean(group2_train))) 
#which is lowest performance mode group
lowest.perf.mode.group1 <- which.min(c(mean(group1_train),mean(group2_train)))


highest.perf.mode.group_train <- pubg_statistics[clus.pubg_statistics_train$cluster == highest.perf.mode.group1, c(1,153)]
lowest.perf.mode.group_train <- pubg_statistics[clus.pubg_statistics_train$cluster == lowest.perf.mode.group1, c(1,153)]

print(clus.pubg_statistics_train$size)

print(paste("Raw Training: Highest performance mode Group is g", highest.perf.mode.group1, sep=""))

print(paste("Raw Training: Lowest performance mode Group is g", lowest.perf.mode.group1, sep=""))

head(highest.perf.mode.group_train[order(highest.perf.mode.group_train$comparison, decreasing=TRUE), ])

tail(lowest.perf.mode.group_train[order(lowest.perf.mode.group_train$comparison, decreasing=TRUE), ])
#*******************************************
#          Clustering: Test DataSet pubg_statistics_test
#*******************************************
cluster_range <- 2:10

#Clustering on Raw test DataSet pubg_statistics_test
clust.pubg_statistics_test <- clustering_euclidean(pubg_statistics_test[,c(2:4,6,14:18,22,53:54,56,64:68,72,103:104,106,114:118,122,153)],pubg_statistics_test,cluster_range)

#Optimal Cluster Size is 2
clus.pubg_statistics_test <- clustering_euclidean(pubg_statistics_test[,c(2:4,6,14:18,22,53:54,56,64:68,72,103:104,106,114:118,122,153)], clust.pubg_statistics_test,2)

group1_test<-pubg_statistics[clus.pubg_statistics_test$cluster == 1,]$comparison
group2_test<-pubg_statistics[clus.pubg_statistics_test$cluster == 2,]$comparison

#which is highest performance mode group
highest.perf.mode.group2 <- which.max(c(mean(group1_test),mean(group2_test))) 
#which is lowest performance mode group
lowest.perf.mode.group2 <- which.min(c(mean(group1_test),mean(group2_test)))


highest.perf.mode.group_test <- pubg_statistics[clus.pubg_statistics_test$cluster == highest.perf.mode.group2, c(1,153)]
lowest.perf.mode.group_test <- pubg_statistics[clus.pubg_statistics_test$cluster == lowest.perf.mode.group2, c(1,153)]

print(clus.pubg_statistics_test$size)
print(paste("Raw Test: Highest performance mode Group is g", highest.perf.mode.group2, sep=""))

print(paste("Raw Test: Lowest performance mode Group is g", lowest.perf.mode.group2, sep=""))

head(highest.perf.mode.group_test[order(highest.perf.mode.group_test$comparison, decreasing=TRUE), ])

tail(lowest.perf.mode.group_test[order(lowest.perf.mode.group_test$comparison, decreasing=TRUE), ])

#*******************************************
#          Clustering: PCA DataSet pc.pubg_statistics
#*******************************************
cluster_range <- 2:10
clust.pc.pubg_statistics <- clustering_euclidean(pc1.pubg_statistics,pubg_statistics,cluster_range)

#Optimal Cluster Size is 5
clus.pc.pubg_statistics <- clustering_euclidean(pc1.pubg_statistics, pubg_statistics,5)

group1.pc<-pubg_statistics[clus.pc.pubg_statistics$cluster == 1,]$comparison
group2.pc<-pubg_statistics[clus.pc.pubg_statistics$cluster == 2,]$comparison
group3.pc<-pubg_statistics[clus.pc.pubg_statistics$cluster == 3,]$comparison
group4.pc<-pubg_statistics[clus.pc.pubg_statistics$cluster == 4,]$comparison
group5.pc<-pubg_statistics[clus.pc.pubg_statistics$cluster == 5,]$comparison
#which is highest performance mode group
highest.perf.mode.group3 <- which.max(c(mean(group1.pc),mean(group2.pc), mean(group3.pc),mean(group4.pc),mean(group5.pc))) 
#which is lowest performance mode group
lowest.perf.mode.group3 <- which.min(c(mean(group1.pc),mean(group2.pc), mean(group3.pc),mean(group4.pc),mean(group5.pc))) 

highest.perf.mode.group.pc <- pubg_statistics[clus.pc.pubg_statistics$cluster == highest.perf.mode.group3, c(1,153)]
lowest.perf.mode.group.pc <- pubg_statistics[clus.pc.pubg_statistics$cluster == lowest.perf.mode.group3, c(1,153)]

print(clus.pc.pubg_statistics$size)

print(paste("PCA on raw dataset: Highest performance mode Group is g", highest.perf.mode.group3, sep=""))

print(paste("PCA on raw dataset: Lowest performance mode Group is g", lowest.perf.mode.group3, sep=""))

head(highest.perf.mode.group.pc[order(highest.perf.mode.group.pc$comparison, decreasing=TRUE), ])

tail(lowest.perf.mode.group.pc[order(lowest.perf.mode.group.pc$comparison, decreasing=TRUE), ])

#*******************************************
#          Clustering: Train DataSet After PCA pc1.pubg_statistics_train
#*******************************************
cluster_range <- 2:10
clust.pc.pubg_statistics_train <- clustering_euclidean(pc1.pubg_statistics_train,pubg_statistics_train,cluster_range)

#Optimal Cluster Size is 7
clus.pc.pubg_statistics_train <- clustering_euclidean(pc1.pubg_statistics_train, pubg_statistics_train,7)

group1.train.pc<-pubg_statistics_train[clus.pc.pubg_statistics_train$cluster == 1,]$comparison
group2.train.pc<-pubg_statistics_train[clus.pc.pubg_statistics_train$cluster == 2,]$comparison
group3.train.pc<-pubg_statistics_train[clus.pc.pubg_statistics_train$cluster == 3,]$comparison
group4.train.pc<-pubg_statistics_train[clus.pc.pubg_statistics_train$cluster == 4,]$comparison
group5.train.pc<-pubg_statistics_train[clus.pc.pubg_statistics_train$cluster == 5,]$comparison
group6.train.pc<-pubg_statistics_train[clus.pc.pubg_statistics_train$cluster == 6,]$comparison
group7.train.pc<-pubg_statistics_train[clus.pc.pubg_statistics_train$cluster == 7,]$comparison
#which is highest performance mode group
highest.perf.mode.group4 <- which.max(c(mean(group1.train.pc),mean(group2.train.pc), mean(group3.train.pc),mean(group4.train.pc),mean(group5.train.pc),mean(group6.train.pc),mean(group7.train.pc))) 
#which is lowest performance mode group
lowest.perf.mode.group4 <- which.min(c(mean(group1.train.pc),mean(group2.train.pc), mean(group3.train.pc),mean(group4.train.pc),mean(group5.train.pc),mean(group6.train.pc),mean(group7.train.pc))) 

highest.perf.mode.group.pc1 <- pubg_statistics_train[clus.pc.pubg_statistics_train$cluster == highest.perf.mode.group4, c(1,153)]
lowest.perf.mode.group.pc1 <- pubg_statistics_train[clus.pc.pubg_statistics_train$cluster == lowest.perf.mode.group4, c(1,153)]

print(clus.pc.pubg_statistics_train$size)

print(paste("Train DataSet After PCA: Highest performance mode Group is g", highest.perf.mode.group4, sep=""))

print(paste("Train DataSet After PCA: Lowest performance mode Group is g", lowest.perf.mode.group4, sep=""))

head(highest.perf.mode.group.pc1[order(highest.perf.mode.group.pc1$comparison, decreasing=TRUE), ])

tail(lowest.perf.mode.group.pc1[order(lowest.perf.mode.group.pc1$comparison, decreasing=TRUE), ])

#*******************************************
#          Clustering: Test DataSet After PCA pc1.pubg_statistics_test
#*******************************************
cluster_range <- 2:10
clust.pc.pubg_statistics_test <- clustering_euclidean(pc1.pubg_statistics_test,pubg_statistics_test,cluster_range)

#Optimal Cluster Size is 5
clus.pc.pubg_statistics_test <- clustering_euclidean(pc1.pubg_statistics_test, pubg_statistics_test,5)

group1.test.pc<-pubg_statistics_test[clus.pc.pubg_statistics_test$cluster == 1,]$comparison
group2.test.pc<-pubg_statistics_test[clus.pc.pubg_statistics_test$cluster == 2,]$comparison
group3.test.pc<-pubg_statistics_test[clus.pc.pubg_statistics_test$cluster == 3,]$comparison
group4.test.pc<-pubg_statistics_test[clus.pc.pubg_statistics_test$cluster == 4,]$comparison
group5.test.pc<-pubg_statistics_test[clus.pc.pubg_statistics_test$cluster == 5,]$comparison

#which is highest performance mode group
highest.perf.mode.group5 <- which.max(c(mean(group1.test.pc),mean(group2.test.pc), mean(group3.test.pc),mean(group4.test.pc),mean(group5.test.pc))) 
#which is lowest performance mode group
lowest.perf.mode.group5 <- which.min(c(mean(group1.test.pc),mean(group2.test.pc), mean(group3.test.pc),mean(group4.test.pc),mean(group5.test.pc))) 

highest.perf.mode.group.pc2 <- pubg_statistics_test[clus.pc.pubg_statistics_test$cluster == highest.perf.mode.group5, c(1,153)]
lowest.perf.mode.group.pc2 <- pubg_statistics_test[clus.pc.pubg_statistics_test$cluster == lowest.perf.mode.group5, c(1,153)]

print(clus.pc.pubg_statistics_test$size)
print(paste("Test dataset after PCA: Highest performance mode Group is g", highest.perf.mode.group5, sep=""))
print(paste("Test dataset after PCA: Lowest performance mode Group is g", lowest.perf.mode.group5, sep=""))

head(highest.perf.mode.group.pc2[order(highest.perf.mode.group.pc2$comparison, decreasing=TRUE), ])
tail(lowest.perf.mode.group.pc2[order(lowest.perf.mode.group.pc2$comparison, decreasing=TRUE), ])

#*******************************************
#          Clustering: ICA DataSet pubg_statistics.ica
#*******************************************
cluster_range <- 2:10
clust.pubg_statistics.ica <- clustering_euclidean(pubg_statistics.ica,pubg_statistics,cluster_range)

#Optimal Cluster Size is 6
clus.pubg_statistics.ica <- clustering_euclidean(pubg_statistics.ica, pubg_statistics,6)

group1.ica<-pubg_statistics[clus.pubg_statistics.ica$cluster == 1,]$comparison
group2.ica<-pubg_statistics[clus.pubg_statistics.ica$cluster == 2,]$comparison
group3.ica<-pubg_statistics[clus.pubg_statistics.ica$cluster == 3,]$comparison
group4.ica<-pubg_statistics[clus.pubg_statistics.ica$cluster == 4,]$comparison
group5.ica<-pubg_statistics[clus.pubg_statistics.ica$cluster == 5,]$comparison
group6.ica<-pubg_statistics[clus.pubg_statistics.ica$cluster == 6,]$comparison
#which is highest performance mode group
highest.perf.mode.group8 <- which.max(c(mean(group1.ica),mean(group2.ica), mean(group3.ica),mean(group4.ica),mean(group5.ica),mean(group6.ica))) 
#which is lowest performance mode group
lowest.perf.mode.group8 <- which.min(c(mean(group1.ica),mean(group2.ica), mean(group3.ica),mean(group4.ica),mean(group5.ica),mean(group6.ica))) 

highest.perf.mode.group.ica <- pubg_statistics[clus.pubg_statistics.ica$cluster == highest.perf.mode.group8, c(1,153)]
lowest.perf.mode.group.ica <- pubg_statistics[clus.pubg_statistics.ica$cluster == lowest.perf.mode.group8, c(1,153)]

print(clus.pubg_statistics.ica$size)

print(paste("ICA on raw dataset: Highest performance mode Group is g", highest.perf.mode.group8, sep=""))

print(paste("ICA on raw dataset: Lowest performance mode Group is g", lowest.perf.mode.group8, sep=""))

head(highest.perf.mode.group.ica[order(highest.perf.mode.group.ica$comparison, decreasing=TRUE), ])

tail(lowest.perf.mode.group.ica[order(lowest.perf.mode.group.ica$comparison, decreasing=TRUE), ])

#*******************************************
#          Clustering: Train DataSet After ICA pubg_statistics_train.ica
#*******************************************
cluster_range <- 2:10
clust.pubg_statistics_train.ica <- clustering_euclidean(pubg_statistics_train.ica,pubg_statistics_train,cluster_range)

#Optimal Cluster Size is 3
clus.pubg_statistics_train.ica <- clustering_euclidean(pubg_statistics_train.ica, pubg_statistics_train,3)

group1.train.ica<-pubg_statistics_train[clus.pubg_statistics_train.ica$cluster == 1,]$comparison
group2.train.ica<-pubg_statistics_train[clus.pubg_statistics_train.ica$cluster == 2,]$comparison
group3.train.ica<-pubg_statistics_train[clus.pubg_statistics_train.ica$cluster == 3,]$comparison

#which is highest performance mode group
highest.perf.mode.group9 <- which.max(c(mean(group1.train.ica),mean(group2.train.ica), mean(group3.train.ica))) 
#which is lowest performance mode group
lowest.perf.mode.group9 <- which.min(c(mean(group1.train.ica),mean(group2.train.ica), mean(group3.train.ica))) 

highest.perf.mode.group.ica1 <- pubg_statistics_train[clus.pubg_statistics_train.ica$cluster == highest.perf.mode.group9, c(1,153)]
lowest.perf.mode.group.ica1 <- pubg_statistics_train[clus.pubg_statistics_train.ica$cluster == lowest.perf.mode.group9, c(1,153)]

print(clus.pubg_statistics_train.ica$size)

print(paste("Train DataSet After ICA: Highest performance mode Group is g", highest.perf.mode.group9, sep=""))

print(paste("Train DataSet After ICA: Lowest performance mode Group is g", lowest.perf.mode.group9, sep=""))

head(highest.perf.mode.group.ica1[order(highest.perf.mode.group.ica1$comparison, decreasing=TRUE), ])

tail(lowest.perf.mode.group.ica1[order(lowest.perf.mode.group.ica1$comparison, decreasing=TRUE), ])

#___________________________________________
#          Supervised Learning 
#___________________________________________
pubg_statistics_test$comparison<-NA

dim(pubg_statistics_test)

pubg_solution <-subset(pubg_statistics, pubg_statistics$tracker_id>164880)

pubg_solution$comparison[pubg_solution$solo_KillDeathRatio>pubg_solution$duo_KillDeathRatio & pubg_solution$solo_KillDeathRatio > pubg_solution$squad_KillDeathRatio] <- '1'

pubg_solution$comparison[pubg_solution$duo_KillDeathRatio>pubg_solution$solo_KillDeathRatio & pubg_solution$duo_KillDeathRatio > pubg_solution$squad_KillDeathRatio] <- '2'

pubg_solution$comparison[pubg_solution$squad_KillDeathRatio>pubg_solution$duo_KillDeathRatio & pubg_solution$squad_KillDeathRatio > pubg_solution$solo_KillDeathRatio] <- '3'


head(pubg_statistics_train$comparison)
str(pubg_statistics_train$comparison)
head(pubg_solution$comparison)
pubg_solution$comparison

pubg_statistics_train <- na.omit(pubg_statistics_train)
pubg_statistics_train$comparison
which(is.na(pubg_statistics_train$comparison))

pubg_solution <- na.omit(pubg_solution)
pubg_solution$comparison
which(is.na(pubg_solution$comparison))
dim(pubg_solution)
library(dplyr)
library(readr)
library(ggthemes)
library(randomForest)
library(rpart)
library(caret)
library(rpart.plot)
library(MASS)
library(DAAG)
library(tree)
set.seed(754)

pubg_statistics_train_model<- randomForest(factor(comparison)~ solo_WinRatio+solo_KillDeathRatio+solo_TimeSurvivedPg+solo_RoundsPlayed+duo_WinRatio+
                                             duo_KillDeathRatio+duo_TimeSurvivedPg+duo_RoundsPlayed+duo_RevivesPg+duo_Revives+duo_TeamKills+
                                             squad_WinRatio+squad_KillDeathRatio+squad_TimeSurvivedPg+squad_RoundsPlayed+squad_RevivesPg+squad_Revives+squad_TeamKills, data = pubg_statistics_train)

pubg_statistics_train_model
str(pubg_statistics_train_model)

#plotting the graph for important variables
importance(pubg_statistics_train_model)
importance_model<- importance(pubg_statistics_train_model)
varImportance <- data.frame(Variables = row.names(importance_model), 
                            Importance = round(importance_model[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()



#providing prediction to test set
pubg_prediction <- predict(pubg_statistics_train_model, data=pubg_statistics_test)
mean(pubg_prediction==pubg_solution$comparison)

summary(pubg_statistics_test)
dim(pubg_statistics_test)
pubg_prediction <- pubg_prediction[1:21974]
pubg_statistics_test<- pubg_statistics_test
pubg_statistics_test$comparison<- pubg_prediction
pubg_statistics_test$comparison


which(is.na(pubg_statistics_test$comparison))
pubg_statistics_test$comparison

dim(pubg_solution)
pubg_solution <- pubg_solution[1:42614,]
pubg_prediction <-pubg_prediction[1:21687]
Actual.Values <- pubg_solution$comparison
Predicted.Values<-as.numeric(pubg_prediction)
table(Predicted.Values,Actual.Values)
confusion(Predicted.Values, Actual.Values)

#plotting the error rate graph
print(pubg_statistics_train_model)
plot(pubg_statistics_train_model, ylim=c(0,0.36))
legend('topright', colnames(pubg_statistics_train_model$err.rate),col = 1:3,fill = 1:3)



randomForest_plot <-rpart(factor(comparison)~solo_WinRatio+solo_KillDeathRatio+solo_TimeSurvivedPg+duo_WinRatio+
                            duo_KillDeathRatio+duo_TimeSurvivedPg+duo_RoundsPlayed+duo_RevivesPg+duo_Revives+duo_TeamKills+
                            squad_WinRatio+squad_KillDeathRatio+squad_TimeSurvivedPg+squad_RoundsPlayed+squad_RevivesPg+squad_Revives+squad_TeamKills,data = pubg_statistics_train,method="class")


plot(randomForest_plot,uniform = TRUE,main="Classifiacation Tree")
text(randomForest_plot,use.n = FALSE, all = TRUE, cex=0.6)
