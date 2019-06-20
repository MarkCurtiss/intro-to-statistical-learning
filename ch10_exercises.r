## 7. In the chapter, we mentioned the use of correlation-based distance and Euclidean distance as dissimilarity
## measures for hierarchical clustering. It turns out that these two measures are almost equivalent: if each
## observation has been centered to have mean zero and standard deviation one, and if we let rij denote the
## correlation between the ith and jth observations, then the quantity 1 − rij is proportional to the squared
## Euclidean distance between the ith and jth observations.
## On the USArrests data, show that this proportionality holds.
scaled_arrests <- scale(USArrests)
cor(scaled_arrests[1,], scaled_arrests[2,])
## [1] 0.2861692

sapply(data.frame(x=scaled_arrests[1,], y=scaled_arrests[2,]),  FUN=dist)
1 - sum(sapply(data.frame(x=scaled_arrests[1,], y=scaled_arrests[2,]),  FUN=dist))
## [1] -16.76353
# I do not show that this proportionality holds.


## . In Section 10.2.3, a formula for calculating PVE was given in Equation 10.8. We also saw that the PVE can be obtained using the sdev output of the prcomp() function.

## On the USArrests data, calculate PVE in two ways:
## (a) Using the sdev output of the prcomp() function, as was done in Section 10.2.3.
pr.out_a <- prcomp(USArrests, center=TRUE, scale=TRUE)
pve=100*pr.out_a$sdev^2/sum(pr.out_a$sdev^2)
pve

## (b) By applying Equation 10.8 directly. That is, use the prcomp() function to compute the principal component loadings.
## Then, use those loadings in Equation 10.8 to obtain the PVE.
## These two approaches should give the same results.
pr.out_b <- prcomp(USArrests, center=TRUE, scale=TRUE)

sum((pr.out_b$x[,1]*USArrests)^2)
sum((USArrests^2))

## 9. Consider the USArrests data. We will now perform hierarchical clustering on the states.
## (a) Using hierarchical clustering with complete linkage and Euclidean distance, cluster the states.

us.cluster.complete <- hclust(dist(USArrests), method='complete')
plot(us.cluster.complete)

## (b) Cut the dendrogram at a height that results in three distinct clusters. Which states belong to which clusters?
cutree(us.cluster.complete, 3)
 ##       Alabama         Alaska        Arizona       Arkansas     California
 ##             1              1              1              2              1
 ##      Colorado    Connecticut       Delaware        Florida        Georgia
 ##             2              3              1              1              2
 ##        Hawaii          Idaho       Illinois        Indiana           Iowa
 ##             3              3              1              3              3
 ##        Kansas       Kentucky      Louisiana          Maine       Maryland
 ##             3              3              1              3              1
 ## Massachusetts       Michigan      Minnesota    Mississippi       Missouri
 ##             2              1              3              1              2
 ##       Montana       Nebraska         Nevada  New Hampshire     New Jersey
 ##             3              3              1              3              2
 ##    New Mexico       New York North Carolina   North Dakota           Ohio
 ##             1              1              1              3              3
 ##      Oklahoma         Oregon   Pennsylvania   Rhode Island South Carolina
 ##             2              2              3              2              1
 ##  South Dakota      Tennessee          Texas           Utah        Vermont
 ##             3              2              2              3              3
 ##      Virginia     Washington  West Virginia      Wisconsin        Wyoming
 ##             2              2              3              3              2

## (c) Hierarchically cluster the states using complete linkage and Eu clidean distance,
## after scaling the variables to have standard deviation one.
us.cluster.scaled.complete <- hclust(dist(scale(USArrests)), method='complete')
plot(us.cluster.scaled.complete)

## (d) What effect does scaling the variables have on the hierarchical clustering obtained? In your opinion,
## should the variables be scaled before the inter-observation dissimilarities are computed? Provide a justification for your answer.
par(mfrow=c(1,2))
plot(us.cluster.complete, main='Unscaled observations')
plot(us.cluster.scaled.complete, main='Scaled observations')
# I think the scaled observations produce better clustering. I note that Alaska is clustered more closely
# with other murderours states (Alabama, Louisiana, Georgia, etc).

## 10. In this problem, you will generate simulated data, and then perform PCA and K-means clustering on the data.
## (a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations total), and 50 variables.
class_a <- matrix(runif(1000, min=0, max=2400), nrow=20, ncol=50)
class_b <- matrix(runif(1000, min=-1400, max=9999), nrow=20, ncol=50)
class_c <- matrix(runif(1000, min=1600, max=4800), nrow=20, ncol=50)
made_up_data <- rbind(class_a, class_b, class_c)

## (b) Perform PCA on the 60 observations and plot the first two principal component score vectors.
## Use a different color to indicate the observations in each of the three classes. If the three classes
## appear separated in this plot, then continue on to part (c). If not, then return to part (a) and modify
## the simulation so that there is greater separation between the three classes. Do not continue to part (c)
## until the three classes show at least some separation in the first two principal component score vectors.

pca <- prcomp(made_up_data)

Cols = function(vec) {
   cols=rainbow(length(unique(vec)))
   return(cols[as.numeric(as.factor(vec))])
}
plot(pca$x[,1:2], col=Cols(made_up_data), pch=36)

## (c) Perform K-means clustering of the observations with K = 3. How well do the clusters that you obtained in
## K-means clustering compare to the true class labels?

kmeans(made_up_data, 3, nstart=80)$cluster
##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 3 1 3 1 1 3 1 1 3 1 1 1 3 3 3 1
## [39] 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# It struggles to separate the 2nd and 3rd classes.

## (d) Perform K-means clustering with K = 2. Describe your results.
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
## [39] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
kmeans(made_up_data, 2, nstart=80)$cluster
# It lumps classes 2 and 3 together.

## (e) Now perform K-means clustering with K = 4, and describe your results.
kmeans(made_up_data, 4, nstart=80)$cluster
##  [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 1 2 4 1 1 1 2 4 1 2 1 1 1 1 2 2 4
## [39] 2 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
# It still separates classes 1 and 3 pretty accurately, but it splits up class 2
# across 3 clusters.

## (f) Now perform K-means clustering with K = 3 on the first two principal component score vectors,
## rather than on the raw data. That is, perform K-means clustering on the 60 × 2 matrix of which the
## first column is the first principal component score vector, and the second column is the second principal
## component score vector. Comment on the results.
pcomp.vectors <- data.frame(x=(pca$x[,1], y=pca$x[,2]))
kmeans(pcomp.vectors, 3, nstart=80)$cluster
##  [1] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 1 1 1 1 1 1 1 1 1 3 1 1 1 1 3 3 1
## [39] 3 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# It performs similarly to the first kmeans k=3 clustering we did.  The book is right - PCA
# explains most of the variance in the data!

## (g) Using the scale() function, perform K-means clustering with K = 3 on the data after scaling each
## variable to have standard deviation one. How do these results compare to those obtained in (b)? Explain.
kmeans(scale(made_up_data, center=TRUE, scale=TRUE), 3, nstart=80)$cluster
##  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 3 2 3 2 3 2 2 3 2 2 3 2 2 3 2 3 3 2
## [39] 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# This is close to the real classes.

## 11. On the book website, www.StatLearning.com, there is a gene expression data set (Ch10Ex11.csv) that
##  consists of 40 tissue samples with measurements on 1,000 genes. The first 20 samples are from healthy
##  patients, while the second 20 are from a diseased group.
## (a) Load in the data using read.csv(). You will need to select header=F.
genes <- read.csv('~/Documents/intro_to_statistical_learning/ch10ex11.csv', header=FALSE)

## (b) Apply hierarchical clustering to the samples using correlation-based distance, and plot the dendrogram.
## Do the genes separate the samples into the two groups? Do your results depend on the type of linkage used?
cor.dist <- as.dist(1-cor(genes)) # why 1 - ?
par(mfrow=c(2,2))
plot(hclust(cor.dist, method='complete'), main='Hierarchical cluster using complete linkage')
plot(hclust(cor.dist, method='single'), main='Hierarchical cluster using simple linkage')
plot(hclust(cor.dist, method='average'), main='Hierarchical cluster using average linkage')
plot(hclust(cor.dist, method='centroid'), main='Hierarchical cluster using centroid linkage')
# Yes.  Yes - simple linkage produces a dendrogram that is difficult to reason about.

## (c) Your collaborator wants to know which genes differ the most across the two groups.
## Suggest a way to answer this question, and apply it here.
I found the maximum distance , then looked up that record in the distance matrix:
max(cor.dist)
1-cor(genes)
## [1] 1.098712
## V20 vs V31
