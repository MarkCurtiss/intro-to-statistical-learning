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
