# Load foreign and cluster libraries
library(foreign)
library(cluster)
library(LICORS)

# Bring in Data Sets (Training, Test, Low Noise)
# Note that the training set has 25 classes
astro.train <- read.arff("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/AstronomyTrainingData.txt")
astro.test <- read.arff("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/AstronomyTestData.txt")
astro.lownoise <- read.table("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/LowNoiseData.txt")
# Da Hawk Directory
# astro.train <- read.arff("/Users/nicholashockensmith/Desktop/Big Data/Project 1/AstronomyTrainingData.txt")
# ===== Subset Data =====
# Subset training data and remove statistical calculations and id number.
# Update: leave class column to determine number of clusters before clustering
astro.train$class
n <- which(colnames(astro.train) == "class")
ast.train <- astro.train[c(2:67, n)]

# There are NAs present, so make a copy and set NA to 0.  This may be a mistake,
# so we will revisit later.
# ast.train2 <- ast.train
# ast.train2[is.na(ast.train2)] <- 0

# Revisit 2
# Rather than make NAs zero, what happens when we remove them.
ast.train4 <- ast.train[complete.cases(ast.train),]

# UPDATE
# With smaller data set, how many clusters are there now?
str(ast.train4$class)
summary(ast.train4$class)
# Still 25 clusters

# Revisit
# Columns of zeros are creating problems for PCA. Let's remove them.
ast.train3 <- ast.train4[, !sapply(names(ast.train4), function(col) 
          {all(ast.train4[,col]==0) | !is.numeric(ast.train4[,col]) } ) ]
# Data should not have any zero columns at this point.

# ===== K-means Clustering =====
# Try K-means clustering
kmeans(ast.train3, 25)
fit <- kmeans(ast.train3, 25)
clusplot(ast.train3, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Throws an error
# UPDATE: Fixed after removing zero columns
# Plot is impossible to understand

# ===== Clara Function =====
# Try the clara function to see if that works better
?clara
clara.ast.train3 <- clara(ast.train3, 25, metric = "euclidean", stand = TRUE, 
                          samples = 500, sampsize = nrow(ast.train3),
                          keep.data = FALSE)
plot(clara.ast.train3, data = ast.train3)
# Still giving errors.
# UPDATE: Fixed after removing zero columns

# Let's try PCA and see if fewer variables help

# ===== Reducing Data with PCA =====
# Determine PCs
princomp(ast.train3, scale = FALSE)
pca.train <- prcomp(ast.train3, scale = TRUE)
summary(pca.train)
# First 35 components account for 90% of variation

# ===== K-medoids Approach =====
pam.ast.train3 <- pam(ast.train3, 25, metric = "euclidean", stand = TRUE,
                      keep.diss = FALSE, keep.data = FALSE)
plot(pam.ast.train3, data = ast.train3)
pam.ast.train3$clusinfo

# ===== K-means ++ =====
fit2 <- kmeanspp(ast.train3, 25, start = "random", iter.max = 100, nstart = 10)
clusplot(ast.train3, fit2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# ===== Hierarchical Clustering ===== Nick Mod #1 =====
# HC with removal of all statistical indicators!
# Method = "euclidean","manhattan"
# *** see https://stat.ethz.ch/R-manual/R-patched/library/stats/html/dist.html for more options
dist.clust <- dist(ast.train3,method = "euclidean")
# Method = "complete","single","average"
# *** see https://stat.ethz.ch/R-manual/R-patched/library/stats/html/hclust.html for more options
hc<-hclust(dist.clust, method = "complete")
plot(hc)
rect.hclust(hc,k=25,border="red")
# Number of counts within each grouping
hc.groups<-cutree(hc,k=25)
table(hc.groups)

# Omit NA's ===== Nick Mod #2 =====
# Removing statistical indicators bad! 
astro.train.na<-na.omit(astro.train)
leave.out2<-c(-1,-87)
# To Scale or Not to Scale, That is the Question!
# *** Need do scale o/w NA's are produced in the dist function
astro.train.scale<-scale(astro.train.na[,leave.out2])
astro.train.scale<-astro.train.na[,leave.out2]
# Leave out the NA's
leave.out.na<-matrix(NA,length(astro.train.scale[1,]),1)
for (i in 1:length(astro.train.scale[1,])){
  if(sum(is.na(astro.train.scale[,i]))==0){
    leave.out.na[i,] <- 0 
  } else {
    leave.out.na[i,] <- -i
  }
}
astro.train.scale<-astro.train.scale[,leave.out.na[leave.out.na<0,]]
dist.clust.scale <- dist(astro.train.scale,method = "euclidean")
# Method = "complete","single","average"
# *** see https://stat.ethz.ch/R-manual/R-patched/library/stats/html/hclust.html for more options
hc.scale<-hclust(dist.clust.scale, method = "complete")
plot(hc.scale)
rect.hclust(hc.scale,k=25,border="red")
# Number of counts within each grouping
hc.groups.scale<-cutree(hc.scale,k=25)
table(hc.groups.scale)


# ===== NOTES =====
# In the clustering, the majority of data has been subsumed into 1 cluster.
fit2$size
