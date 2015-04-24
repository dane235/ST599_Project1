# install.packages("ggplot2","dplyr","reshape2")
# packages to make some plots and data manipulation
library(ggplot2)
library(dplyr)
library(reshape2)
# This is used to create multiple plots into a single plot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# install.packages("foreign","cluster","LICORS","kernlab")
# Packages to read in the data and do some cluster analysis
library(foreign)
library(cluster)
library(LICORS)
library(kernlab)

# Read in the data, we only worked on the training set because
# the heavy amount of computational time for some of these methods
astro.train<-read.arff("AstronomyTrainingData.txt")

## Cleaning up the data, here we remove the covariates that are heavily
## Any rows with NA is removed using na.omit function

astro.filter <-data.frame(na.omit(astro.train),row.names=NULL)

## Removing all the columns heavily contained with zeros 
## we need dplyr:: before select if the MASS package is loaded 

astro.train.sub<-dplyr::select(astro.filter,-class,-source_id,-freq_n_alias,-ends_with("phase_0"),-starts_with("eclpoly"))

## Keeping the class locations to make a comparison
## Final data set to make a comparison betwen the different clustering algorithms 
## Here we keep the labels of the rows and what they represent 
trueclass <- astro.filter$class
## Here have the standardized data set of the asto reduced training set 
astro<-data.frame(scale(astro.train.sub,scale=TRUE))
## Removing statistcal properties with this subset
astro<-astro[,-c(63:68)]
##
##########################################################################################
####### Applying some cluster analysis to the start groups using different methods #######
##########################################################################################
# Note: For some of these methods, there is a random start and convergence of the methods#
# will not produce the same results as the previous one.                                 #
##########################################################################################
# ========== K-means ============ #
## Here we have K-means model
fit1 <- kmeans(astro, 25)
clusterlabels.kmeans <-fit$cluster # cluster assignments 
table(clusterlabels.kmeans)
# ======= K-medoids =============  #
fit2 <- pam(astro,k=25,metric = "euclidean", stand = TRUE,keep.diss = FALSE, keep.data = FALSE)
clusterlabels.kmedoids <-fit2$clustering
table(clusterlabels.kmedoids)
# ======= K-means ++ ============ #
fit3 <- kmeanspp(astro, k=25, start = "random", iter.max = 100, nstart = 10)
clusterlabels.kmeanspp <- fit3$cluster
table(clusterlabels.kmeanspp)
# ====== Spectral Clustering ==== #
fit4 <- specc(~.,data=astro,centers=25)
clusterlabels.specc  <- fit4@.Data
table(clusterlabels.specc)
###########################################################################################
## Creating some visuals 
## labels from kmeans, kmedoids, kmeans ++, and spectral clustering3
###########################################################################################
clusterlabels.kmeans <-fit1$cluster
clusterlabels.kmedoids <-fit2$clustering
clusterlabels.kmeanspp <- fit3$cluster
clusterlabels.specc  <- fit4@.Data
## creating a data.frame of the labels to create some visuals with ggplot2
labels <-data.frame(clusterlabels.kmeans,clusterlabels.kmedoids,clusterlabels.kmeanspp,clusterlabels.specc,trueclass)
names(labels) <- c("kmeans","medoids","pp","spec","class")

## individual graphs
g1<-ggplot(labels,aes(x=kmeans,y=class))+
  geom_point()+theme_bw()

g2<-ggplot(labels,aes(x=medoids,y=class))+
  geom_point()+theme_bw()

g3<-ggplot(labels,aes(x=pp,y=class))+
  geom_point()+theme_bw()

g4<-ggplot(labels,aes(x=spec,y=class))+
  geom_point()+theme_bw()
## multiple plots 
multiplot(g1,g2,g3,g4,cols=2)


### Hiearchael Clustering ### 
