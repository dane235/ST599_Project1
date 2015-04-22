library(foreign)
library(cluster)
library(LICORS)
library(ggplot2)
library(ggdendro)

# Load Data
# Obviously change these directories if you need to one your own computer. Just
# comment these lines so that I can still use them later.

# # astro.train <- read.arff("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/AstronomyTrainingData.txt")
# astro.test <- read.arff("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/AstronomyTestData.txt")
# astro.lownoise <- read.table("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/LowNoiseData.txt")

# Da Hawk Directory
astro.train <- read.arff("/Users/nicholashockensmith/Desktop/Big Data/Project 1/AstronomyTrainingData.txt")

# ===== Subsetting Data =====
# Since initial efforts proved that clustering the data is challenging, this 
# attempt relies on splitting the data into three types before doing any 
# clustering

# We subset the data into Pulsating, Eruptive, and Multi-Star groups.
# The idea here is to see if the clustering works better in stages.

pulsating <- c("Mira", "Semiregular Pulsating Variable", "RV Tauri",
               "Classical Cepheid", "Population II Cepheid", "Multiple Mode
               Cepheid", "RR Lyrae, Fundamental Mode", "RR Lyrae, First
               Overtone", "RR Lyrae, Double Mode", "Delta Scuti", "Lambda
               Bootis Variable", "Beta Cephei", "Slowly Pulsating B-stars", 
               "Gamma Doradus")

eruptive <- c("Periodically variable supergiants", "Chemically Peculiar Stars",
              "Wolf-Rayet", "T Tauri", "Herbig AE/BE Star", "S Doradus")

multi.star <- c("Ellipsoidal", "Beta Persei", "Beta Lyrae", "W Ursae Majoris")

# Subset data in just pulsating stars
astro.train.pulse <- astro.train[ astro.train$class %in% pulsating, ]

# Subset data into just eruptive stars
astro.train.erupt <- astro.train[ astro.train$class %in% eruptive, ]

# Subet data into just multi-star systems
astro.train.multi <- astro.train[ astro.train$class %in% multi.star, ]

####################################################################
# ===== Begin Special Note ===== #
# Proceed do the very depths of despair for Hierarchical Clustering!
# Otherwise begin data preparation below
# ===== End Special Note ===== #
####################################################################


# # ===== Prepare Data =====
# 
# # Remove Statistical Calculations and Index Number
# n <- which(colnames(astro.train.pulse) == "class")
# astro.train.pulse2 <- astro.train.pulse[c(2:67, n)]
# 
# astro.train.erupt2 <- astro.train.erupt[c(2:67, n)]
# 
# astro.train.multi2 <- astro.train.multi[c(2:67, n)]
# 
# 
# # Remove data with NAs
# astro.train.pulse2 <- astro.train.pulse2[complete.cases(astro.train.pulse2),]
# 
# astro.train.erupt2 <- astro.train.erupt2[complete.cases(astro.train.erupt2),]
# 
# astro.train.multi2 <- astro.train.multi2[complete.cases(astro.train.multi2),]
# 
# 
# # Remove columns of zeros and class column
# astro.train.pulse3 <- astro.train.pulse2[, !sapply(names(astro.train.pulse2), 
#                         function(col) {all(astro.train.pulse2[,col]==0) | 
#                         !is.numeric(astro.train.pulse2[,col]) } ) ]
# 
# astro.train.erupt3 <- astro.train.erupt2[, !sapply(names(astro.train.erupt2), 
#                         function(col) {all(astro.train.erupt2[,col]==0) | 
#                         !is.numeric(astro.train.erupt2[,col]) } ) ]
# 
# astro.train.multi3 <- astro.train.multi2[, !sapply(names(astro.train.multi2), 
#                         function(col) {all(astro.train.multi2[,col]==0) | 
#                         !is.numeric(astro.train.multi2[,col]) } ) ]
# 
# # ===== Begin Special Note ===== #
# # Jump directly to the bottom for the Hierarchical Clustering!
# # o/w Proceed with K-means
# # ===== End Special Note ===== #
# 
# # ===== K-means Clustering =====
# # Note that reducing the data means there are now 12 types in the pulse data
# # 6 types in the erupt data
# # 4 types in the multi data
# 
# fit.pulse <- kmeans(astro.train.pulse3, 12)
# clusplot(astro.train.pulse3, fit.pulse$cluster, color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# 
# fit.erupt <- kmeans(astro.train.erupt3, 6)
# clusplot(astro.train.erupt3, fit.erupt$cluster, color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# 
# fit.multi <- kmeans(astro.train.multi3, 4)
# clusplot(astro.train.multi3, fit.multi$cluster, color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# 
# 
# # ===== K-mediods =====
# pam.pulse <- pam(astro.train.pulse3, 12, metric = "euclidean", stand = TRUE,
#                       keep.diss = FALSE, keep.data = FALSE)
# plot(pam.pulse, data = astro.train.pulse3)
# pam.pulse$clusinfo
# 
# pam.erupt <- pam(astro.train.erupt3, 6, metric = "euclidean", stand = TRUE,
#                  keep.diss = FALSE, keep.data = FALSE)
# plot(pam.erupt, data = astro.train.erupt3)
# pam.erupt$clusinfo
# 
# pam.multi <- pam(astro.train.multi3, 4, metric = "euclidean", stand = TRUE,
#                  keep.diss = FALSE, keep.data = FALSE)
# plot(pam.multi, data = astro.train.multi3)
# pam.multi$clusinfo
# 
# 
# # ===== K-means ++ =====
# fit.pulse2 <- kmeanspp(astro.train.pulse3, 12, start = "random", iter.max = 100,
#                        nstart = 10)
# clusplot(astro.train.pulse3, fit.pulse2$cluster, color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# 
# fit.erupt2 <- kmeanspp(astro.train.erupt3, 12, start = "random", iter.max = 100,
#                        nstart = 10)
# clusplot(astro.train.erupt3, fit.erupt2$cluster, color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# 
# fit.multi2 <- kmeanspp(astro.train.multi3, 12, start = "random", iter.max = 100,
#                        nstart = 10)
# clusplot(astro.train.multi3, fit.multi2$cluster, color=TRUE, shade=TRUE,
#          labels=2, lines=0)


####################################################################
# ===== Begin Special Note ===== #
# Use this data preparation specifically for Hierarchical Clustering!
# Do not pass GO, do not collect $200!
astro.train.all<-na.omit(astro.train)
astro.train.pulse<-na.omit(astro.train.pulse)
astro.train.erupt<-na.omit(astro.train.erupt)
astro.train.multi<-na.omit(astro.train.multi)
# Leave-in or Leave-out
leave.out2<-c(-1,-87)
### Two Different Scaling Methods: Compliments of the Heffe! ###
# # Normalize Scale: 856 forced into Cluster 1
# astro.train.all<-scale(astro.train.all[,leave.out2])
# astro.train.pulse<-scale(astro.train.pulse[,leave.out2])
# astro.train.erupt<-scale(astro.train.erupt[,leave.out2])
# astro.train.multi<-scale(astro.train.multi[,leave.out2])
# 0-1 Scale
scale01 <- function(a){
  (a-min(a))/(max(a)-min(a))
}
astro.train.all<-apply(astro.train.all[,leave.out2],2,scale01)
astro.train.pulse<-apply(astro.train.pulse[,leave.out2],2,scale01)
astro.train.erupt<-apply(astro.train.erupt[,leave.out2],2,scale01)
astro.train.multi<-apply(astro.train.multi[,leave.out2],2,scale01)
# # No Scale: Does not work!
# astro.train.all<-astro.train.all[,leave.out2]
# astro.train.pulse<-astro.train.pulse[,leave.out2]
# astro.train.erupt<-astro.train.erupt[,leave.out2]
# astro.train.multi<-astro.train.multi[,leave.out2]
### End El Heffe ###
leave.out.all<-matrix(NA,length(astro.train.all[1,]),1)
leave.out.pulse<-matrix(NA,length(astro.train.pulse[1,]),1)
leave.out.erupt<-matrix(NA,length(astro.train.erupt[1,]),1)
leave.out.multi<-matrix(NA,length(astro.train.multi[1,]),1)
# Forloopasaurus-Rex
for (i in 1:length(astro.train.all[1,])){
  if(sum(is.na(astro.train.all[,i]))==0){
    leave.out.all[i,] <- 0 
  } else {
    leave.out.all[i,] <- -i
  }
}
for (i in 1:length(astro.train.pulse[1,])){
  if(sum(is.na(astro.train.pulse[,i]))==0){
    leave.out.pulse[i,] <- 0 
  } else {
    leave.out.pulse[i,] <- -i
  }
}
for (i in 1:length(astro.train.erupt[1,])){
  if(sum(is.na(astro.train.erupt[,i]))==0){
    leave.out.erupt[i,] <- 0 
  } else {
    leave.out.erupt[i,] <- -i
  }
}
for (i in 1:length(astro.train.multi[1,])){
  if(sum(is.na(astro.train.multi[,i]))==0){
    leave.out.multi[i,] <- 0 
  } else {
    leave.out.multi[i,] <- -i
  }
}
# Unforloopasaurus-Rex
astro.train.all4<-astro.train.all[,leave.out.all[leave.out.all<0,]]
astro.train.pulse4<-astro.train.pulse[,leave.out.pulse[leave.out.pulse<0,]]
astro.train.erupt4<-astro.train.erupt[,leave.out.erupt[leave.out.erupt<0,]]
astro.train.multi4<-astro.train.multi[,leave.out.multi[leave.out.multi<0,]]
# ===== End Special Note ===== #

# ===== Hierarchical Clustering w/in Subclassification =====
# HC with removal of all statistical indicators!
# Method = "euclidean","manhattan"
# j=0: All, 24 type
# j=1: Pulse, 14 types
# j=2: Erupt, 6 types
# j=3: Multi, 4 types
j=0
# *** see https://stat.ethz.ch/R-manual/R-patched/library/stats/html/dist.html for more options
if(j==0){
  dist.clust <- dist(astro.train.all4,method = "euclidean")
  k <- 24
}else if(j==1){  
  dist.clust <- dist(astro.train.pulse4,method = "euclidean")
  k <- 14
}else if(j==2){
  dist.clust <- dist(astro.train.erupt4,method = "euclidean")
  k <- 6
}else{
  dist.clust <- dist(astro.train.multi4,method = "euclidean")
  k <- 4
}
# Method = "complete","single","average"
# *** see https://stat.ethz.ch/R-manual/R-patched/library/stats/html/hclust.html for more options
hc<-hclust(dist.clust,method = "complete")
plot(hc,xlab="",sub="")
rect.hclust(hc,k,border="red")
# Number of counts within each grouping
hc.groups<-cutree(hc,k)
table(hc.groups)

