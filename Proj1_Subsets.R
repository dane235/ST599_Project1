library(foreign)
library(cluster)
library(LICORS)

# Load Data
# Obviously change these directories if you need to one your own computer. Just
# comment these lines so that I can still use them later.

astro.train <- read.arff("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/AstronomyTrainingData.txt")
astro.test <- read.arff("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/AstronomyTestData.txt")
astro.lownoise <- read.table("/home/dane/Google Drive/School/Grad Stuff/2015B Spring/Classes/ST 599/Project 1/Data Files/LowNoiseData.txt")

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

# ===== Prepare Data =====

# Remove Statistical Calculations and Index Number
n <- which(colnames(astro.train.pulse) == "class")
astro.train.pulse2 <- astro.train.pulse[c(2:67, n)]

astro.train.erupt2 <- astro.train.erupt[c(2:67, n)]

astro.train.multi2 <- astro.train.multi[c(2:67, n)]


# Remove data with NAs
astro.train.pulse2 <- astro.train.pulse2[complete.cases(astro.train.pulse2),]

astro.train.erupt2 <- astro.train.erupt2[complete.cases(astro.train.erupt2),]

astro.train.multi2 <- astro.train.multi2[complete.cases(astro.train.multi2),]


# Remove columns of zeros and class column
astro.train.pulse3 <- astro.train.pulse2[, !sapply(names(astro.train.pulse2), 
                        function(col) {all(astro.train.pulse2[,col]==0) | 
                        !is.numeric(astro.train.pulse2[,col]) } ) ]

astro.train.erupt3 <- astro.train.erupt2[, !sapply(names(astro.train.erupt2), 
                        function(col) {all(astro.train.erupt2[,col]==0) | 
                        !is.numeric(astro.train.erupt2[,col]) } ) ]

astro.train.multi3 <- astro.train.multi2[, !sapply(names(astro.train.multi2), 
                        function(col) {all(astro.train.multi2[,col]==0) | 
                        !is.numeric(astro.train.multi2[,col]) } ) ]


# ===== K-means Clustering =====
# Note that reducing the data means there are now 12 types in the pulse data
# 6 types in the erupt data
# 4 types in the multi data

fit.pulse <- kmeans(astro.train.pulse3, 12)
clusplot(astro.train.pulse3, fit.pulse$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

fit.erupt <- kmeans(astro.train.erupt3, 6)
clusplot(astro.train.erupt3, fit.erupt$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

fit.multi <- kmeans(astro.train.multi3, 4)
clusplot(astro.train.multi3, fit.multi$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)


# ===== K-mediods =====
pam.pulse <- pam(astro.train.pulse3, 12, metric = "euclidean", stand = TRUE,
                      keep.diss = FALSE, keep.data = FALSE)
plot(pam.pulse, data = astro.train.pulse3)
pam.pulse$clusinfo

pam.erupt <- pam(astro.train.erupt3, 6, metric = "euclidean", stand = TRUE,
                 keep.diss = FALSE, keep.data = FALSE)
plot(pam.erupt, data = astro.train.erupt3)
pam.erupt$clusinfo

pam.multi <- pam(astro.train.multi3, 4, metric = "euclidean", stand = TRUE,
                 keep.diss = FALSE, keep.data = FALSE)
plot(pam.multi, data = astro.train.multi3)
pam.multi$clusinfo


# ===== K-means ++ =====
fit.pulse2 <- kmeanspp(astro.train.pulse3, 12, start = "random", iter.max = 100,
                       nstart = 10)
clusplot(astro.train.pulse3, fit.pulse2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

fit.erupt2 <- kmeanspp(astro.train.erupt3, 12, start = "random", iter.max = 100,
                       nstart = 10)
clusplot(astro.train.erupt3, fit.erupt2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

fit.multi2 <- kmeanspp(astro.train.multi3, 12, start = "random", iter.max = 100,
                       nstart = 10)
clusplot(astro.train.multi3, fit.multi2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)