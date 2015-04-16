# Load foreign and cluster libraries
library(foreign)
library(cluster)
library(LICORS)

# Bring in Data Sets (Training, Test, Low Noise)
# Note that the training set has 25 classes
astro.train <- read.arff("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/AstronomyTrainingData.txt")
astro.test <- read.arff("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/AstronomyTestData.txt")
astro.lownoise <- read.table("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/LowNoiseData.txt")

# ===== Subset Data =====
# Subset training data and remove statistical calculations and id number.
ast.train <- astro.train[c(2:67)]

# There are NAs present, so make a copy and set NA to 0.  This may be a mistake,
# so we will revisit later.
ast.train2 <- ast.train
ast.train2[is.na(ast.train2)] <- 0

# Revisit 2
# Rather than make NAs zero, what happens when we remove them.
ast.train4 <- ast.train[complete.cases(ast.train),]

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

# ===== K-means ++ =====
fit2 <- kmeanspp(ast.train3, 25, start = "random", iter.max = 100, nstart = 10)
clusplot(ast.train3, fit2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

