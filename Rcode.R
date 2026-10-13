#Title
#An ensemble clustering typology of long-term care policies: evidence from 30 OECD countries 




library(readxl)
library(cluster)  
library(multisom) 
library(ppclust)  
library(clusterCrit) 
library(writexl)
library(clues)
library(PCAmixdata)
library(scales)
library(kohonen)
library(hopkins)
library(ggcorrplot)
library(stats)


#Upload data:

Base_1 <- read_excel("Base_1.xlsx", col_names=FALSE)
Base_1 <- data.frame(Base_1)
Base_1lesscountry <- Base_1[,2:11]
colnames(Base_1lesscountry) <- c('Beds','Workers', 'hhsize', 'Femalelabor', 'Lifeexpectancy', 'Publicexpend', 'Privatexpend', 'Meantesting', 'Giniinequaly', 'Healthstatus')
summary(Base_1)


#Pearson Correlation matrix

Base_1corr <- read_excel("Base_1corr.xlsx", col_names=FALSE)
colnames(Base_1corr) <- c('Beds','Workers', 'hhsize', 'Femalelabor', 'Lifeexpectancy', 'Publicexpend', 'Privatexpend', 'Giniinequaly', 'Healthstatus')
Base_1.cor = cor(Base_1corr, method='pearson')
View(Base_1.cor)
ggcorrplot(Base_1.cor)

#Hopkins:

Base_1scale<- scale(Base_1lesscountry)
set.seed(123)
hopkinsvalue <- hopkins(Base_1scale)
hopkins.pval(hopkinsvalue,30) 

#Colunms name:
colnames(Base_1)
colnames(Base_1) <- c('Country','Beds','Workers', 'hhsize', 'Femalelabor', 'Lifeexpectancy', 'Publicexpend', 'Privatexpend', 'Meantesting', 'Giniinequaly', 'Healthstatus')


################################################################################
# Principal component analysis##################################################
################################################################################
rm(list = ls(all.names = TRUE)) 

#Upload data:
Base_1 <- read_excel("Base_1_v1.xlsx", col_names=FALSE)
#Base_1_v1.xlsx: mean-test variable is qualitative

# Get rid of the Country column:
Base_1lesscountry <- Base_1[,2:11]
head(Base_1lesscountry)

# Apply PCAmix:
split <- splitmix(Base_1lesscountry)
X1 <- split$X.quanti
X1 <- scale(X1)
X2 <- split$X.quali 


#PCA<-PCAmix(quantitatives,qualitatives):
res.pcamix <- PCAmix(X.quanti=X1, X.quali=X2,ndim = 10, rename.level=TRUE,
                     graph=TRUE)
res.pcamix$eig 

#Rotation Varimax:
res.pcarot <- PCArot(res.pcamix,dim=5,graph = TRUE)
res.pcarot$eig #variance of the rotated PCs
sum(res.pcarot$eig[ ,2])


#Squared loadings: 
res.pcamix$sqload
res.pcarot$sqload 
rawLoadings <- res.pcarot$sqload
res.pcarot$scores 
Base_1 <- res.pcarot$scores 

#Component map with factor scores of the numerical columns:
plot(res.pcamix,choice="ind", main="Squared loadings before rotation")
plot(res.pcamix,choice="cor")

#Contributions of the variables before rotation:
plot(res.pcamix,choice="sqload",coloring.var="TRUE")


#Contributions of the variables after rotation:
plot(res.pcarot,choice="sqload",coloring.var=TRUE,
     leg=TRUE,axes=c(1,2),posleg="topright",
     main="Squared loadings after rotation")

plot(res.pcarot,choice="ind",axes=c(1,2))
colnames(Base_1)

#Rename variables with PC's:
colnames(Base_1) <- c('PC1','PC2','PC3', 'PC4', 'PC5')
Base_1 <- data.frame(Base_1)

write_xlsx(x= Base_1, path = "Base1_PCA.xlsx", col_names=TRUE)


################################################################################
# Clustering####################################################################
################################################################################
set.seed(1)


#FCM - by default m=2
fcm_assignments_2 <- fcm(Base_1, centers=2)$cluster
fcm_assignments_3 <- fcm(Base_1, centers=3)$cluster
fcm_assignments_4 <- fcm(Base_1, centers=4)$cluster
fcm_assignments_5 <- fcm(Base_1, centers=5)$cluster
fcm_assignments_6 <- fcm(Base_1, centers=6)$cluster
fcm_assignments_7 <- fcm(Base_1, centers=7)$cluster
fcm_assignments_8 <- fcm(Base_1, centers=8)$cluster
fcm_assignments_9 <- fcm(Base_1, centers=9)$cluster

#FCM - m=2.5 - change to 1.5, 1.001, 1.2
fcm_assignments_2 <- fcm(Base_1, centers=2, m=2.5)$cluster
fcm_assignments_3 <- fcm(Base_1, centers=3, m=2.5)$cluster
fcm_assignments_4 <- fcm(Base_1, centers=4, m=2.5)$cluster
fcm_assignments_5 <- fcm(Base_1, centers=5, m=2.5)$cluster
fcm_assignments_6 <- fcm(Base_1, centers=6, m=2.5)$cluster
fcm_assignments_7 <- fcm(Base_1, centers=7, m=2.5)$cluster
fcm_assignments_8 <- fcm(Base_1, centers=8, m=2.5)$cluster
fcm_assignments_9 <- fcm(Base_1, centers=9, m=2.5)$cluster


#SOM
LTCc_grid_2 <- somgrid(xdim = 2, ydim = 1, topo = "rectangular")
LTCc_grid_3 <- somgrid(xdim = 3, ydim = 1, topo = "rectangular")
LTCc_grid_4 <- somgrid(xdim = 2, ydim = 2, topo = "rectangular")
LTCc_grid_5 <- somgrid(xdim = 5, ydim = 1, topo = "rectangular")
LTCc_grid_6 <- somgrid(xdim = 3, ydim = 2, topo = "rectangular")
LTCc_grid_7 <- somgrid(xdim = 7, ydim = 1, topo = "rectangular")
LTCc_grid_8 <- somgrid(xdim = 4, ydim = 2, topo = "rectangular")
LTCc_grid_9 <- somgrid(xdim = 3, ydim = 3, topo = "rectangular")

#SOM - grid:
LTCc_grid_2 <- somgrid(xdim = 2, ydim = 1, topo = "hexa")
LTCc_grid_3 <- somgrid(xdim = 3, ydim = 1, topo = "hexa")
LTCc_grid_4 <- somgrid(xdim = 2, ydim = 2, topo = "hexa")
LTCc_grid_5 <- somgrid(xdim = 5, ydim = 1, topo = "hexa")
LTCc_grid_6 <- somgrid(xdim = 3, ydim = 2, topo = "hexa")
LTCc_grid_7 <- somgrid(xdim = 7, ydim = 1, topo = "hexa")
LTCc_grid_8 <- somgrid(xdim = 4, ydim = 2, topo = "hexa")
LTCc_grid_9 <- somgrid(xdim = 3, ydim = 3, topo = "hexa")

#SOM BATCHSOM
som_assignments_2 <- as.integer(BatchSOM(as.matrix(Base_1),grid = LTCc_grid_2,min.radius=0.5,max.radius=1,maxit=1000, init="linear", radius.type="gaussian")$classif)
som_assignments_3 <- as.integer(BatchSOM(as.matrix(Base_1),grid = LTCc_grid_3,min.radius=0.5,max.radius=1,maxit=1000, init="linear", radius.type="gaussian")$classif)
som_assignments_4 <- as.integer(BatchSOM(as.matrix(Base_1),grid = LTCc_grid_4,min.radius=0.5,max.radius=1,maxit=1000, init="linear", radius.type="gaussian")$classif)
som_assignments_5 <- as.integer(BatchSOM(as.matrix(Base_1),grid = LTCc_grid_5,min.radius=0.5,max.radius=1,maxit=1000, init="linear", radius.type="gaussian")$classif)
som_assignments_6 <- as.integer(BatchSOM(as.matrix(Base_1),grid = LTCc_grid_6,min.radius=0.5,max.radius=1,maxit=1000, init="linear", radius.type="gaussian")$classif)
som_assignments_7 <- as.integer(BatchSOM(as.matrix(Base_1),grid = LTCc_grid_7,min.radius=0.5,max.radius=1,maxit=1000, init="linear", radius.type="gaussian")$classif)
som_assignments_8 <- as.integer(BatchSOM(as.matrix(Base_1),grid = LTCc_grid_8,min.radius=0.5,max.radius=1,maxit=1000, init="linear", radius.type="gaussian")$classif)
som_assignments_9 <- as.integer(BatchSOM(as.matrix(Base_1),grid = LTCc_grid_9,min.radius=0.5,max.radius=1,maxit=1000, init="linear", radius.type="gaussian")$classif)



#PAM
dist_LTC <- dist(Base_1, method ='euclidean')
kmedoids_assignments_2 <- pam(dist_LTC,k=2,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_3 <- pam(dist_LTC,k=3,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_4 <- pam(dist_LTC,k=4,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_5 <- pam(dist_LTC,k=5,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_6 <- pam(dist_LTC,k=6,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_7 <- pam(dist_LTC,k=7,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_8 <- pam(dist_LTC,k=8,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_9 <- pam(dist_LTC,k=9,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)


#PAM - manhatam distance
dist_LTC <- dist(Base_1, method ='manhattan')
kmedoids_assignments_2 <- pam(dist_LTC,k=2,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_3 <- pam(dist_LTC,k=3,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_4 <- pam(dist_LTC,k=4,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_5 <- pam(dist_LTC,k=5,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_6 <- pam(dist_LTC,k=6,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_7 <- pam(dist_LTC,k=7,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_8 <- pam(dist_LTC,k=8,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)
kmedoids_assignments_9 <- pam(dist_LTC,k=9,cluster.only=TRUE, keep.diss=FALSE, keep.data=FALSE)




#Internal Criteria:

## Average Silhouete Width:

LTCc_s_fcm_2 <- silhouette(fcm_assignments_2, dist_LTC)[,3]
LTCc_s_fcm_3 <- silhouette(fcm_assignments_3, dist_LTC)[,3]
LTCc_s_fcm_4 <- silhouette(fcm_assignments_4, dist_LTC)[,3]
LTCc_s_fcm_5 <- silhouette(fcm_assignments_5, dist_LTC)[,3]
LTCc_s_fcm_6 <- silhouette(fcm_assignments_6, dist_LTC)[,3]
LTCc_s_fcm_7 <- silhouette(fcm_assignments_7, dist_LTC)[,3]
LTCc_s_fcm_8 <- silhouette(fcm_assignments_8, dist_LTC)[,3]
LTCc_s_fcm_9 <- silhouette(fcm_assignments_9, dist_LTC)[,3]


LTCc_s_som_2 <- silhouette(som_assignments_2, dist_LTC)[,3]
LTCc_s_som_3 <- silhouette(som_assignments_3, dist_LTC)[,3]
LTCc_s_som_4 <- silhouette(som_assignments_4, dist_LTC)[,3]
LTCc_s_som_5 <- silhouette(som_assignments_5, dist_LTC)[,3]
LTCc_s_som_6 <- silhouette(som_assignments_6, dist_LTC)[,3]
LTCc_s_som_7 <- silhouette(som_assignments_7, dist_LTC)[,3]
LTCc_s_som_8 <- silhouette(som_assignments_8, dist_LTC)[,3]
LTCc_s_som_9 <- silhouette(som_assignments_9, dist_LTC)[,3]


LTCc_s_kmd_2 <- silhouette(kmedoids_assignments_2, dist_LTC)[,3]
LTCc_s_kmd_3 <- silhouette(kmedoids_assignments_3, dist_LTC)[,3]
LTCc_s_kmd_4 <- silhouette(kmedoids_assignments_4, dist_LTC)[,3]
LTCc_s_kmd_5 <- silhouette(kmedoids_assignments_5, dist_LTC)[,3]
LTCc_s_kmd_6 <- silhouette(kmedoids_assignments_6, dist_LTC)[,3]
LTCc_s_kmd_7 <- silhouette(kmedoids_assignments_7, dist_LTC)[,3]
LTCc_s_kmd_8 <- silhouette(kmedoids_assignments_8, dist_LTC)[,3]
LTCc_s_kmd_9 <- silhouette(kmedoids_assignments_9, dist_LTC)[,3]



## Average Silhouete Width > 0.5:

LTCc_s_05_fcm_2 <- sum((LTCc_s_fcm_2 > 0.5)/30*100)
LTCc_s_05_fcm_3 <- sum((LTCc_s_fcm_3 > 0.5)/30*100)
LTCc_s_05_fcm_4 <- sum((LTCc_s_fcm_4 > 0.5)/30*100)
LTCc_s_05_fcm_5 <- sum((LTCc_s_fcm_5 > 0.5)/30*100)
LTCc_s_05_fcm_6 <- sum((LTCc_s_fcm_6 > 0.5)/30*100)
LTCc_s_05_fcm_7 <- sum((LTCc_s_fcm_7 > 0.5)/30*100)
LTCc_s_05_fcm_8 <- sum((LTCc_s_fcm_8 > 0.5)/30*100)
LTCc_s_05_fcm_9 <- sum((LTCc_s_fcm_9 > 0.5)/30*100)


LTCc_s_05_som_2 <- sum((LTCc_s_som_2 > 0.5)/30*100)
LTCc_s_05_som_3 <- sum((LTCc_s_som_3 > 0.5)/30*100)
LTCc_s_05_som_4 <- sum((LTCc_s_som_4 > 0.5)/30*100)
LTCc_s_05_som_5 <- sum((LTCc_s_som_5 > 0.5)/30*100)
LTCc_s_05_som_6 <- sum((LTCc_s_som_6 > 0.5)/30*100)
LTCc_s_05_som_7 <- sum((LTCc_s_som_7 > 0.5)/30*100)
LTCc_s_05_som_8 <- sum((LTCc_s_som_8 > 0.5)/30*100)
LTCc_s_05_som_9 <- sum((LTCc_s_som_9 > 0.5)/30*100)


LTCc_s_05_kmd_2 <- sum((LTCc_s_kmd_2 > 0.5)/30*100)
LTCc_s_05_kmd_3 <- sum((LTCc_s_kmd_3 > 0.5)/30*100)
LTCc_s_05_kmd_4 <- sum((LTCc_s_kmd_4 > 0.5)/30*100)
LTCc_s_05_kmd_5 <- sum((LTCc_s_kmd_5 > 0.5)/30*100)
LTCc_s_05_kmd_6 <- sum((LTCc_s_kmd_6 > 0.5)/30*100)
LTCc_s_05_kmd_7 <- sum((LTCc_s_kmd_7 > 0.5)/30*100)
LTCc_s_05_kmd_8 <- sum((LTCc_s_kmd_8 > 0.5)/30*100)
LTCc_s_05_kmd_9 <- sum((LTCc_s_kmd_9 > 0.5)/30*100)



## Average Silhouete Width <0:

LTCc_s_0_fcm_2 <- sum((LTCc_s_fcm_2 < 0)/30*100)
LTCc_s_0_fcm_3 <- sum((LTCc_s_fcm_3 < 0)/30*100)
LTCc_s_0_fcm_4 <- sum((LTCc_s_fcm_4 < 0)/30*100)
LTCc_s_0_fcm_5 <- sum((LTCc_s_fcm_5 < 0)/30*100)
LTCc_s_0_fcm_6 <- sum((LTCc_s_fcm_6 < 0)/30*100)
LTCc_s_0_fcm_7 <- sum((LTCc_s_fcm_7 < 0)/30*100)
LTCc_s_0_fcm_8 <- sum((LTCc_s_fcm_8 < 0)/30*100)
LTCc_s_0_fcm_9 <- sum((LTCc_s_fcm_9 < 0)/30*100)


LTCc_s_0_som_2 <- sum((LTCc_s_som_2 < 0)/30*100)
LTCc_s_0_som_3 <- sum((LTCc_s_som_3 < 0)/30*100)
LTCc_s_0_som_4 <- sum((LTCc_s_som_4 < 0)/30*100)
LTCc_s_0_som_5 <- sum((LTCc_s_som_5 < 0)/30*100)
LTCc_s_0_som_6 <- sum((LTCc_s_som_6 < 0)/30*100)
LTCc_s_0_som_7 <- sum((LTCc_s_som_7 < 0)/30*100)
LTCc_s_0_som_8 <- sum((LTCc_s_som_8 < 0)/30*100)
LTCc_s_0_som_9 <- sum((LTCc_s_som_9 < 0)/30*100)


LTCc_s_0_kmd_2 <- sum((LTCc_s_kmd_2 < 0)/30*100)
LTCc_s_0_kmd_3 <- sum((LTCc_s_kmd_3 < 0)/30*100)
LTCc_s_0_kmd_4 <- sum((LTCc_s_kmd_4 < 0)/30*100)
LTCc_s_0_kmd_5 <- sum((LTCc_s_kmd_5 < 0)/30*100)
LTCc_s_0_kmd_6 <- sum((LTCc_s_kmd_6 < 0)/30*100)
LTCc_s_0_kmd_7 <- sum((LTCc_s_kmd_7 < 0)/30*100)
LTCc_s_0_kmd_8 <- sum((LTCc_s_kmd_8 < 0)/30*100)
LTCc_s_0_kmd_9 <- sum((LTCc_s_kmd_9 < 0)/30*100)


## Calinski_Harabasz:


LTCc_ch_fcm_2 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_2,"Calinski_Harabasz"))
LTCc_ch_fcm_3 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_3,"Calinski_Harabasz"))
LTCc_ch_fcm_4 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_4,"Calinski_Harabasz"))
LTCc_ch_fcm_5 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_5,"Calinski_Harabasz"))
LTCc_ch_fcm_6 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_6,"Calinski_Harabasz"))
LTCc_ch_fcm_7 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_7,"Calinski_Harabasz"))
LTCc_ch_fcm_8 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_8,"Calinski_Harabasz"))
LTCc_ch_fcm_9 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_9,"Calinski_Harabasz"))


LTCc_ch_som_2 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_2,"Calinski_Harabasz"))
LTCc_ch_som_3 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_3,"Calinski_Harabasz"))
LTCc_ch_som_4 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_4,"Calinski_Harabasz"))
LTCc_ch_som_5 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_5,"Calinski_Harabasz"))
LTCc_ch_som_6 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_6,"Calinski_Harabasz"))
LTCc_ch_som_7 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_7,"Calinski_Harabasz"))
LTCc_ch_som_8 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_8,"Calinski_Harabasz"))
LTCc_ch_som_9 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_9,"Calinski_Harabasz"))


LTCc_ch_kmd_2 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_2,"Calinski_Harabasz"))
LTCc_ch_kmd_3 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_3,"Calinski_Harabasz"))
LTCc_ch_kmd_4 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_4,"Calinski_Harabasz"))
LTCc_ch_kmd_5 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_5,"Calinski_Harabasz"))
LTCc_ch_kmd_6 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_6,"Calinski_Harabasz"))
LTCc_ch_kmd_7 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_7,"Calinski_Harabasz"))
LTCc_ch_kmd_8 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_8,"Calinski_Harabasz"))
LTCc_ch_kmd_9 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_9,"Calinski_Harabasz"))


## Davies_Bouldin:


LTCc_db_fcm_2 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_2,"Davies_Bouldin"))
LTCc_db_fcm_3 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_3,"Davies_Bouldin"))
LTCc_db_fcm_4 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_4,"Davies_Bouldin"))
LTCc_db_fcm_5 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_5,"Davies_Bouldin"))
LTCc_db_fcm_6 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_6,"Davies_Bouldin"))
LTCc_db_fcm_7 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_7,"Davies_Bouldin"))
LTCc_db_fcm_8 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_8,"Davies_Bouldin"))
LTCc_db_fcm_9 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_9,"Davies_Bouldin"))


LTCc_db_som_2 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_2,"Davies_Bouldin"))
LTCc_db_som_3 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_3,"Davies_Bouldin"))
LTCc_db_som_4 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_4,"Davies_Bouldin"))
LTCc_db_som_5 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_5,"Davies_Bouldin"))
LTCc_db_som_6 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_6,"Davies_Bouldin"))
LTCc_db_som_7 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_7,"Davies_Bouldin"))
LTCc_db_som_8 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_8,"Davies_Bouldin"))
LTCc_db_som_9 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_9,"Davies_Bouldin"))


LTCc_db_kmd_2 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_2,"Davies_Bouldin"))
LTCc_db_kmd_3 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_3,"Davies_Bouldin"))
LTCc_db_kmd_4 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_4,"Davies_Bouldin"))
LTCc_db_kmd_5 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_5,"Davies_Bouldin"))
LTCc_db_kmd_6 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_6,"Davies_Bouldin"))
LTCc_db_kmd_7 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_7,"Davies_Bouldin"))
LTCc_db_kmd_8 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_8,"Davies_Bouldin"))
LTCc_db_kmd_9 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_9,"Davies_Bouldin"))


## Dunn:

LTCc_du_fcm_2 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_2,"Dunn"))
LTCc_du_fcm_3 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_3,"Dunn"))
LTCc_du_fcm_4 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_4,"Dunn"))
LTCc_du_fcm_5 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_5,"Dunn"))
LTCc_du_fcm_6 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_6,"Dunn"))
LTCc_du_fcm_7 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_7,"Dunn"))
LTCc_du_fcm_8 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_8,"Dunn"))
LTCc_du_fcm_9 <- as.numeric(intCriteria(as.matrix(Base_1),fcm_assignments_9,"Dunn"))


LTCc_du_som_2 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_2,"Dunn"))
LTCc_du_som_3 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_3,"Dunn"))
LTCc_du_som_4 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_4,"Dunn"))
LTCc_du_som_5 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_5,"Dunn"))
LTCc_du_som_6 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_6,"Dunn"))
LTCc_du_som_7 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_7,"Dunn"))
LTCc_du_som_8 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_8,"Dunn"))
LTCc_du_som_9 <- as.numeric(intCriteria(as.matrix(Base_1),som_assignments_9,"Dunn"))


LTCc_du_kmd_2 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_2,"Dunn"))
LTCc_du_kmd_3 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_3,"Dunn"))
LTCc_du_kmd_4 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_4,"Dunn"))
LTCc_du_kmd_5 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_5,"Dunn"))
LTCc_du_kmd_6 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_6,"Dunn"))
LTCc_du_kmd_7 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_7,"Dunn"))
LTCc_du_kmd_8 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_8,"Dunn"))
LTCc_du_kmd_9 <- as.numeric(intCriteria(as.matrix(Base_1),kmedoids_assignments_9,"Dunn"))


#Combine results, by algorithm: 

fcm_results <- cbind(Base_1,
                     fcm_assignments_2,
                     fcm_assignments_3,
                     fcm_assignments_4,
                     fcm_assignments_5,
                     fcm_assignments_6,
                     fcm_assignments_7,
                     fcm_assignments_8,
                     fcm_assignments_9,
                     LTCc_s_fcm_2,
                     LTCc_s_fcm_3,
                     LTCc_s_fcm_4,
                     LTCc_s_fcm_5,
                     LTCc_s_fcm_6,
                     LTCc_s_fcm_7,
                     LTCc_s_fcm_8,
                     LTCc_s_fcm_9,
                     LTCc_s_05_fcm_2,
                     LTCc_s_05_fcm_3,
                     LTCc_s_05_fcm_4,
                     LTCc_s_05_fcm_5,
                     LTCc_s_05_fcm_6,
                     LTCc_s_05_fcm_7,
                     LTCc_s_05_fcm_8,
                     LTCc_s_05_fcm_9,
                     LTCc_s_0_fcm_2,
                     LTCc_s_0_fcm_3,
                     LTCc_s_0_fcm_4,
                     LTCc_s_0_fcm_5,
                     LTCc_s_0_fcm_6,
                     LTCc_s_0_fcm_7,
                     LTCc_s_0_fcm_8,
                     LTCc_s_0_fcm_9,
                     LTCc_ch_fcm_2,
                     LTCc_ch_fcm_3,
                     LTCc_ch_fcm_4,
                     LTCc_ch_fcm_5,
                     LTCc_ch_fcm_6,
                     LTCc_ch_fcm_7,
                     LTCc_ch_fcm_8,
                     LTCc_ch_fcm_9,
                     LTCc_db_fcm_2,
                     LTCc_db_fcm_3,
                     LTCc_db_fcm_4,
                     LTCc_db_fcm_5,
                     LTCc_db_fcm_6,
                     LTCc_db_fcm_7,
                     LTCc_db_fcm_8,
                     LTCc_db_fcm_9,
                     LTCc_du_fcm_2,
                     LTCc_du_fcm_3,
                     LTCc_du_fcm_4,
                     LTCc_du_fcm_5,
                     LTCc_du_fcm_6,
                     LTCc_du_fcm_7,
                     LTCc_du_fcm_8,
                     LTCc_du_fcm_9)

som_results <- cbind(Base_1,
                     som_assignments_2,
                     som_assignments_3,
                     som_assignments_4,
                     som_assignments_5,
                     som_assignments_6,
                     som_assignments_7,
                     som_assignments_8,
                     som_assignments_9,
                     LTCc_s_som_2,
                     LTCc_s_som_3,
                     LTCc_s_som_4,
                     LTCc_s_som_5,
                     LTCc_s_som_6,
                     LTCc_s_som_7,
                     LTCc_s_som_8,
                     LTCc_s_som_9,
                     LTCc_s_05_som_2,
                     LTCc_s_05_som_3,
                     LTCc_s_05_som_4,
                     LTCc_s_05_som_5,
                     LTCc_s_05_som_6,
                     LTCc_s_05_som_7,
                     LTCc_s_05_som_8,
                     LTCc_s_05_som_9,
                     LTCc_s_0_som_2,
                     LTCc_s_0_som_3,
                     LTCc_s_0_som_4,
                     LTCc_s_0_som_5,
                     LTCc_s_0_som_6,
                     LTCc_s_0_som_7,
                     LTCc_s_0_som_8,
                     LTCc_s_0_som_9,
                     LTCc_ch_som_2,
                     LTCc_ch_som_3,
                     LTCc_ch_som_4,
                     LTCc_ch_som_5,
                     LTCc_ch_som_6,
                     LTCc_ch_som_7,
                     LTCc_ch_som_8,
                     LTCc_ch_som_9,
                     LTCc_db_som_2,
                     LTCc_db_som_3,
                     LTCc_db_som_4,
                     LTCc_db_som_5,
                     LTCc_db_som_6,
                     LTCc_db_som_7,
                     LTCc_db_som_8,
                     LTCc_db_som_9,
                     LTCc_du_som_2,
                     LTCc_du_som_3,
                     LTCc_du_som_4,
                     LTCc_du_som_5,
                     LTCc_du_som_6,
                     LTCc_du_som_7,
                     LTCc_du_som_8,
                     LTCc_du_som_9)



kmd_results <- cbind(Base_1,
                     kmedoids_assignments_2,
                     kmedoids_assignments_3,
                     kmedoids_assignments_4,
                     kmedoids_assignments_5,
                     kmedoids_assignments_6,
                     kmedoids_assignments_7,
                     kmedoids_assignments_8,
                     kmedoids_assignments_9,
                     LTCc_s_kmd_2,
                     LTCc_s_kmd_3,
                     LTCc_s_kmd_4,
                     LTCc_s_kmd_5,
                     LTCc_s_kmd_6,
                     LTCc_s_kmd_7,
                     LTCc_s_kmd_8,
                     LTCc_s_kmd_9,
                     LTCc_s_05_kmd_2,
                     LTCc_s_05_kmd_3,
                     LTCc_s_05_kmd_4,
                     LTCc_s_05_kmd_5,
                     LTCc_s_05_kmd_6,
                     LTCc_s_05_kmd_7,
                     LTCc_s_05_kmd_8,
                     LTCc_s_05_kmd_9,
                     LTCc_s_0_kmd_2,
                     LTCc_s_0_kmd_3,
                     LTCc_s_0_kmd_4,
                     LTCc_s_0_kmd_5,
                     LTCc_s_0_kmd_6,
                     LTCc_s_0_kmd_7,
                     LTCc_s_0_kmd_8,
                     LTCc_s_0_kmd_9,
                     LTCc_ch_kmd_2,
                     LTCc_ch_kmd_3,
                     LTCc_ch_kmd_4,
                     LTCc_ch_kmd_5,
                     LTCc_ch_kmd_6,
                     LTCc_ch_kmd_7,
                     LTCc_ch_kmd_8,
                     LTCc_ch_kmd_9,
                     LTCc_db_kmd_2,
                     LTCc_db_kmd_3,
                     LTCc_db_kmd_4,
                     LTCc_db_kmd_5,
                     LTCc_db_kmd_6,
                     LTCc_db_kmd_7,
                     LTCc_db_kmd_8,
                     LTCc_db_kmd_9,
                     LTCc_du_kmd_2,
                     LTCc_du_kmd_3,
                     LTCc_du_kmd_4,
                     LTCc_du_kmd_5,
                     LTCc_du_kmd_6,
                     LTCc_du_kmd_7,
                     LTCc_du_kmd_8,
                     LTCc_du_kmd_9)


#Export results:

write_xlsx(fcm_results, "fcm_results.xlsx", col_names=TRUE)
write_xlsx(som_results, "som_results.xlsx", col_names=TRUE)
write_xlsx(kmd_results, "kmd_results.xlsx", col_names=TRUE)



#Optimal number of clusters step:
J <- c(2:9)
SW_fcm <- c(0,0,0,0,0,0,0,0)

SW_fcm[1] <- mean(LTCc_s_fcm_2)
SW_fcm[2] <- mean(LTCc_s_fcm_3)
SW_fcm[3] <- mean(LTCc_s_fcm_4)
SW_fcm[4] <- mean(LTCc_s_fcm_5)
SW_fcm[5] <- mean(LTCc_s_fcm_6)
SW_fcm[6] <- mean(LTCc_s_fcm_7)
SW_fcm[7] <- mean(LTCc_s_fcm_8)
SW_fcm[8] <- mean(LTCc_s_fcm_9)

LTCc_s05_fcm <- c( LTCc_s_05_fcm_2,LTCc_s_05_fcm_3,LTCc_s_05_fcm_4,LTCc_s_05_fcm_5,LTCc_s_05_fcm_6,LTCc_s_05_fcm_7,LTCc_s_05_fcm_8,LTCc_s_05_fcm_9)
LTCc_s0_fcm <- c( LTCc_s_0_fcm_2,LTCc_s_0_fcm_3,LTCc_s_0_fcm_4,LTCc_s_0_fcm_5,LTCc_s_0_fcm_6,LTCc_s_0_fcm_7,LTCc_s_0_fcm_8,LTCc_s_0_fcm_9)
LTCc_ch_fcm <- c( LTCc_ch_fcm_2,LTCc_ch_fcm_3,LTCc_ch_fcm_4,LTCc_ch_fcm_5,LTCc_ch_fcm_6,LTCc_ch_fcm_7,LTCc_ch_fcm_8,LTCc_ch_fcm_9)
LTCc_db_fcm <- c( LTCc_db_fcm_2,LTCc_db_fcm_3,LTCc_db_fcm_4,LTCc_db_fcm_5,LTCc_db_fcm_6,LTCc_db_fcm_7,LTCc_db_fcm_8,LTCc_db_fcm_9)
LTCc_du_fcm <- c( LTCc_du_fcm_2,LTCc_du_fcm_3,LTCc_du_fcm_4,LTCc_du_fcm_5,LTCc_du_fcm_6,LTCc_du_fcm_7,LTCc_du_fcm_8,LTCc_du_fcm_9)
numb_cluster_fcm <- cbind (J,SW_fcm,LTCc_s05_fcm,LTCc_s0_fcm,LTCc_ch_fcm,LTCc_db_fcm, LTCc_du_fcm)

SW_som <- c(0,0,0,0,0,0,0,0)

SW_som[1] <- mean(LTCc_s_som_2)
SW_som[2] <- mean(LTCc_s_som_3)
SW_som[3] <- mean(LTCc_s_som_4)
SW_som[4] <- mean(LTCc_s_som_5)
SW_som[5] <- mean(LTCc_s_som_6)
SW_som[6] <- mean(LTCc_s_som_7)
SW_som[7] <- mean(LTCc_s_som_8)
SW_som[8] <- mean(LTCc_s_som_9)

LTCc_s05_som <- c( LTCc_s_05_som_2,LTCc_s_05_som_3,LTCc_s_05_som_4,LTCc_s_05_som_5,LTCc_s_05_som_6,LTCc_s_05_som_7,LTCc_s_05_som_8,LTCc_s_05_som_9)
LTCc_s0_som <- c( LTCc_s_0_som_2,LTCc_s_0_som_3,LTCc_s_0_som_4,LTCc_s_0_som_5,LTCc_s_0_som_6,LTCc_s_0_som_7,LTCc_s_0_som_8,LTCc_s_0_som_9)
LTCc_ch_som <- c( LTCc_ch_som_2,LTCc_ch_som_3,LTCc_ch_som_4,LTCc_ch_som_5,LTCc_ch_som_6,LTCc_ch_som_7,LTCc_ch_som_8,LTCc_ch_som_9)
LTCc_db_som <- c( LTCc_db_som_2,LTCc_db_som_3,LTCc_db_som_4,LTCc_db_som_5,LTCc_db_som_6,LTCc_db_som_7,LTCc_db_som_8,LTCc_db_som_9)
LTCc_du_som <- c( LTCc_du_som_2,LTCc_du_som_3,LTCc_du_som_4,LTCc_du_som_5,LTCc_du_som_6,LTCc_du_som_7,LTCc_du_som_8,LTCc_du_som_9)
numb_cluster_som <- cbind (J,SW_som,LTCc_s05_som,LTCc_s0_som,LTCc_ch_som,LTCc_db_som, LTCc_du_som)

SW_kmd <- c(0,0,0,0,0,0,0,0)

SW_kmd[1] <- mean(LTCc_s_kmd_2)
SW_kmd[2] <- mean(LTCc_s_kmd_3)
SW_kmd[3] <- mean(LTCc_s_kmd_4)
SW_kmd[4] <- mean(LTCc_s_kmd_5)
SW_kmd[5] <- mean(LTCc_s_kmd_6)
SW_kmd[6] <- mean(LTCc_s_kmd_7)
SW_kmd[7] <- mean(LTCc_s_kmd_8)
SW_kmd[8] <- mean(LTCc_s_kmd_9)

LTCc_s05_kmd <- c( LTCc_s_05_kmd_2,LTCc_s_05_kmd_3,LTCc_s_05_kmd_4,LTCc_s_05_kmd_5,LTCc_s_05_kmd_6,LTCc_s_05_kmd_7,LTCc_s_05_kmd_8,LTCc_s_05_kmd_9)
LTCc_s0_kmd <- c( LTCc_s_0_kmd_2,LTCc_s_0_kmd_3,LTCc_s_0_kmd_4,LTCc_s_0_kmd_5,LTCc_s_0_kmd_6,LTCc_s_0_kmd_7,LTCc_s_0_kmd_8,LTCc_s_0_kmd_9)
LTCc_ch_kmd <- c( LTCc_ch_kmd_2,LTCc_ch_kmd_3,LTCc_ch_kmd_4,LTCc_ch_kmd_5,LTCc_ch_kmd_6,LTCc_ch_kmd_7,LTCc_ch_kmd_8,LTCc_ch_kmd_9)
LTCc_db_kmd <- c( LTCc_db_kmd_2,LTCc_db_kmd_3,LTCc_db_kmd_4,LTCc_db_kmd_5,LTCc_db_kmd_6,LTCc_db_kmd_7,LTCc_db_kmd_8,LTCc_db_kmd_9)
LTCc_du_kmd <- c( LTCc_du_kmd_2,LTCc_du_kmd_3,LTCc_du_kmd_4,LTCc_du_kmd_5,LTCc_du_kmd_6,LTCc_du_kmd_7,LTCc_du_kmd_8,LTCc_du_kmd_9)
numb_cluster_kmd <- cbind (J,SW_kmd,LTCc_s05_kmd,LTCc_s0_kmd,LTCc_ch_kmd,LTCc_db_kmd, LTCc_du_kmd)

numb_cluster_fcm <- data.frame(numb_cluster_fcm)
numb_cluster_som <- data.frame(numb_cluster_som)
numb_cluster_kmd <- data.frame(numb_cluster_kmd)
write_xlsx(numb_cluster_fcm, "numb_cluster_fcm.xlsx", col_names=TRUE)
write_xlsx(numb_cluster_som, "numb_cluster_som.xlsx", col_names=TRUE)
write_xlsx(numb_cluster_kmd, "numb_cluster_kmd.xlsx", col_names=TRUE)

# To analyse results and re-labelling 


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################


library(readxl)
library(dplyr)

Base_1_Original <- read_excel("Base_1.xlsx", col_names=FALSE)

colnames(Base_1_Original)

colnames(Base_1_Original) <- c('Country','Beds','Workers', 'hhsize', 'Femalelabor', 'Lifeexpectancy', 'Publicexpend', 'Privatexpend', 'Meantesting', 'Giniinequaly', 'Healthstatus')


#External criteria: 

path <- ("numb_cluster_analysis.xlsm")

ensemble <- pull(read_excel(path, sheet = 3, range = "X2:X35"))
FCM <- pull(read_excel(path, sheet = 3, range = "C2:C35"))
SOM <- pull(read_excel(path, sheet = 3, range = "D2:D35"))
KMD <- pull(read_excel(path, sheet = 3, range = "E2:E35"))


ensemble <- ensemble[ ensemble != 0 ]
FCM <- FCM[ FCM != 0 ]
SOM <- SOM[ SOM != 0 ]
KMD <- KMD[ KMD != 0 ]



library(clues)
adjustedRand_fcm <- adjustedRand(ensemble, FCM, randMethod = c("Rand", "HA", "Jaccard"))
adjustedRand_som <- adjustedRand(ensemble, SOM, randMethod = c("Rand", "HA", "Jaccard"))
adjustedRand_kmd <- adjustedRand(ensemble, KMD, randMethod = c("Rand", "HA", "Jaccard"))
print(adjustedRand_fcm)
print(adjustedRand_som)
print(adjustedRand_kmd)

###############################################################################

# Pearson's chi-square independence test:
library (gmodels)
CrossTable(ensemble, FCM, chisq=TRUE)
CrossTable(ensemble, SOM, chisq=TRUE)
CrossTable(ensemble, KMD, chisq=TRUE)


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################


BaseFinal <- cbind(Base_1_Original, ensemble)

BaseF_ensemble1 <- subset(BaseFinal,ensemble=='1' )
BaseF_ensemble2 <- subset(BaseFinal,ensemble=='2' )
BaseF_ensemble3 <- subset(BaseFinal,ensemble=='3' )
BaseF_ensemble4 <- subset(BaseFinal,ensemble=='4' )
BaseF_ensemble5 <- subset(BaseFinal,ensemble=='5' )
BaseF_ensemble6 <- subset(BaseFinal,ensemble=='6' )
BaseF_ensemble7 <- subset(BaseFinal,ensemble=='7' )

# To calculate the mean and SD: 
BaseF_ensemble1_sd <- sapply(BaseF_ensemble1[ ,c(2:ncol(BaseF_ensemble1))], sd)
BaseF_ensemble1_mean <- sapply(BaseF_ensemble1[ ,c(2:ncol(BaseF_ensemble1))], mean)

BaseF_ensemble2_sd <- sapply(BaseF_ensemble2[ ,c(2:ncol(BaseF_ensemble1))], sd)
BaseF_ensemble2_mean <- sapply(BaseF_ensemble2[ ,c(2:ncol(BaseF_ensemble1))], mean)

BaseF_ensemble3_sd <- sapply(BaseF_ensemble3[ ,c(2:ncol(BaseF_ensemble1))], sd)
BaseF_ensemble3_mean <- sapply(BaseF_ensemble3[ ,c(2:ncol(BaseF_ensemble1))], mean)

BaseF_ensemble4_sd <- sapply(BaseF_ensemble4[ ,c(2:ncol(BaseF_ensemble1))], sd)
BaseF_ensemble4_mean <- sapply(BaseF_ensemble4[ ,c(2:ncol(BaseF_ensemble1))], mean)

BaseF_ensemble5_sd <- sapply(BaseF_ensemble5[ ,c(2:ncol(BaseF_ensemble1))], sd)
BaseF_ensemble5_mean <- sapply(BaseF_ensemble5[ ,c(2:ncol(BaseF_ensemble1))], mean)

BaseF_ensemble6_sd <- sapply(BaseF_ensemble6[ ,c(2:ncol(BaseF_ensemble1))], sd)
BaseF_ensemble6_mean <- sapply(BaseF_ensemble6[ ,c(2:ncol(BaseF_ensemble1))], mean)

BaseF_ensemble7_sd <- sapply(BaseF_ensemble7[ ,c(2:ncol(BaseF_ensemble1))], sd)
BaseF_ensemble7_mean <- sapply(BaseF_ensemble7[ ,c(2:ncol(BaseF_ensemble1))], mean)

BaseF_ensemble_meansd <- cbind(BaseF_ensemble1_mean, BaseF_ensemble1_sd, 
                               BaseF_ensemble2_mean, BaseF_ensemble2_sd,
                               BaseF_ensemble3_mean, BaseF_ensemble3_sd,
                               BaseF_ensemble4_mean, BaseF_ensemble4_sd,
                               BaseF_ensemble5_mean, BaseF_ensemble5_sd,
                               BaseF_ensemble6_mean, BaseF_ensemble6_sd,
                               BaseF_ensemble7_mean, BaseF_ensemble7_sd)

# Data with meand and SD for each cluster:
BaseF_ensemble_meansd <- data.frame(BaseF_ensemble_meansd)




#To include the number of countries: 
nr_countries <- c(nrow(BaseF_ensemble1),0,nrow(BaseF_ensemble2),0,nrow(BaseF_ensemble3),0,
                  nrow(BaseF_ensemble4),0,nrow(BaseF_ensemble5),0,nrow(BaseF_ensemble6),0,
                  nrow(BaseF_ensemble7),0)
BaseF_ensemble_meansd <- rbind(nr_countries, BaseF_ensemble_meansd)
row.names(BaseF_ensemble_meansd)[1] <- "Nr Countries"

#To include which countries in cluster: 

library("qpcR") 
ListcountriesFinal <- qpcR:::cbind.na(BaseF_ensemble1[1], BaseF_ensemble2[1], BaseF_ensemble3[1],
                                      BaseF_ensemble4[1], BaseF_ensemble5[1], BaseF_ensemble6[1],
                                      BaseF_ensemble7[1])
colnames(ListcountriesFinal) <- c('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Cluster 5','Cluster 6','Cluster 7')
rownames(ListcountriesFinal)<-NULL

library(writexl)

BaseF_ensemble_meansd <- cbind(" "= rownames(BaseF_ensemble_meansd), BaseF_ensemble_meansd)
list_of_datasets <- list(BaseF_ensemble_meansd, ListcountriesFinal)

write_xlsx(list_of_datasets, "Ouput_Final.xlsx")

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

#Tuckey test:


library(readxl)
library(stats)

Base.aov.ensemble <- cbind(ensemble, Base_1_Original)


#FCM
Base.aov.FCM <- cbind(FCM, Base_1_Original)

#SOM
Base.aov.SOM <- cbind(SOM, Base_1_Original)

#KMD
Base.aov.KMD <- cbind(KMD, Base_1_Original)

Base.aov.ensemble$ensemble <- as.factor(Base.aov.ensemble$ensemble)
FCM <- as.factor(FCM)
SOM <- as.factor(SOM)
KMD <- as.factor(KMD)

Base.aov.FCM$FCM <- as.factor(Base.aov.FCM$FCM)
Base.aov.SOM$SOM <- as.factor(Base.aov.SOM$SOM)
Base.aov.KMD$KMD <- as.factor(Base.aov.KMD$KMD)

colnames(Base.aov.ensemble)



#Ensemble  
aov_ensemble_Beds   <- aov(Beds ~ ensemble, data = Base.aov.ensemble )
aov_ensemble_Workers   <- aov(Workers ~ ensemble, data = Base.aov.ensemble )
aov_ensemble_hhsize  <- aov(hhsize ~ ensemble, data = Base.aov.ensemble )
aov_ensemble_Femalelabor   <- aov(Femalelabor ~ ensemble, data = Base.aov.ensemble )
aov_ensemble_Lifeexpectancy   <- aov(Lifeexpectancy ~ ensemble, data = Base.aov.ensemble )
aov_ensemble_Publicexpend  <- aov(Publicexpend ~ ensemble, data = Base.aov.ensemble )
aov_ensemble_Privatexpend   <- aov(Privatexpend ~ ensemble, data = Base.aov.ensemble )
#aov_ensemble_Meantesting   <- aov(Meantesting ~ ensemble, data = Base.aov.ensemble )
aov_ensemble_Giniinequaly   <- aov(Giniinequaly ~ ensemble, data = Base.aov.ensemble )
aov_ensemble_Healthstatus   <- aov(Healthstatus ~ ensemble, data = Base.aov.ensemble )

# Summary of the analysis_ Compute one-way ANOVA test:
summary(aov_ensemble_Beds)
summary(aov_ensemble_Workers)
summary(aov_ensemble_hhsize)
summary(aov_ensemble_Femalelabor)
summary(aov_ensemble_Lifeexpectancy)
summary(aov_ensemble_Publicexpend)
summary(aov_ensemble_Privatexpend)
#summary(aov_ensemble_Giniinequaly)
summary(aov_ensemble_Healthstatus)



# Tukey to mean-tested:

model3 <- glm(Base_1_Original$Meantesting ~ factor(ensemble), data=Base.aov.ensemble, family = binomial(link = "logit"))
library(emmeans)
model.emmeans <- emmeans(model3, pairwise ~ ensemble)
pairs(model.emmeans)

TukeyHSD(aov(model3))


#FCM
aov_FCM_Beds   <- aov(Beds ~ FCM, data = Base.aov.FCM )
aov_FCM_Workers   <- aov(Workers ~ FCM, data = Base.aov.FCM )
aov_FCM_hhsize  <- aov(hhsize ~ FCM, data = Base.aov.FCM )
aov_FCM_Femalelabor   <- aov(Femalelabor ~ FCM, data = Base.aov.FCM )
aov_FCM_Lifeexpectancy   <- aov(Lifeexpectancy ~ FCM, data = Base.aov.FCM )
aov_FCM_Publicexpend   <- aov(Publicexpend ~ FCM, data = Base.aov.FCM )
aov_FCM_Privatexpend   <- aov(Privatexpend ~ FCM, data = Base.aov.FCM )
#aov_FCM_Meantesting   <- aov(Meantesting ~ FCM, data = Base.aov.FCM )
aov_FCM_Giniinequaly   <- aov(Giniinequaly ~ FCM, data = Base.aov.FCM )
aov_FCM_Healthstatus   <- aov(Healthstatus ~ FCM, data = Base.aov.FCM )


#SOM
aov_SOM_Beds   <- aov(Beds ~ SOM, data = Base.aov.SOM )
aov_SOM_Workers   <- aov(Workers ~ SOM, data = Base.aov.SOM )
aov_SOM_hhsize  <- aov(hhsize ~ SOM, data = Base.aov.SOM )
aov_SOM_Femalelabor   <- aov(Femalelabor ~ SOM, data = Base.aov.SOM )
aov_SOM_Lifeexpectancy   <- aov(Lifeexpectancy ~ SOM, data = Base.aov.SOM )
aov_SOM_Publicexpend   <- aov(Publicexpend ~ SOM, data = Base.aov.SOM )
aov_SOM_Privatexpend   <- aov(Privatexpend ~ SOM, data = Base.aov.SOM )
#aov_SOM_Meantesting   <- aov(Meantesting ~ SOM, data = Base.aov.SOM )
aov_SOM_Giniinequaly   <- aov(Giniinequaly ~ SOM, data = Base.aov.SOM )
aov_SOM_Healthstatus   <- aov(Healthstatus ~ SOM, data = Base.aov.SOM )


#KMD
aov_KMD_Beds   <- aov(Beds ~ KMD, data = Base.aov.KMD )
aov_KMD_Workers   <- aov(Workers ~ KMD, data = Base.aov.KMD )
aov_KMD_hhsize  <- aov(hhsize ~ KMD, data = Base.aov.KMD )
aov_KMD_Femalelabor   <- aov(Femalelabor ~ KMD, data = Base.aov.KMD )
aov_KMD_Lifeexpectancy   <- aov(Lifeexpectancy ~ KMD, data = Base.aov.KMD )
aov_KMD_Publicexpend   <- aov(Publicexpend ~ KMD, data = Base.aov.KMD )
aov_KMD_Privatexpend   <- aov(Privatexpend ~ KMD, data = Base.aov.KMD )
#aov_KMD_Meantesting   <- aov(Meantesting ~ KMD, data = Base.aov.KMD )
aov_KMD_Giniinequaly   <- aov(Giniinequaly ~ KMD, data = Base.aov.KMD )
aov_KMD_Healthstatus   <- aov(Healthstatus ~ KMD, data = Base.aov.KMD )

#TUCKEY HSD Test:###############################################################
#Ensemble:
tuc_ensemble_Beds <- TukeyHSD(aov_ensemble_Beds)
tuc_ensemble_Workers <- TukeyHSD(aov_ensemble_Workers)
tuc_ensemble_hhsize <- TukeyHSD(aov_ensemble_hhsize)
tuc_ensemble_Femalelabor <- TukeyHSD(aov_ensemble_Femalelabor)
tuc_ensemble_Lifeexpectancy <- TukeyHSD(aov_ensemble_Lifeexpectancy)
tuc_ensemble_Publicexpend <- TukeyHSD(aov_ensemble_Publicexpend)
tuc_ensemble_Privatexpend <- TukeyHSD(aov_ensemble_Privatexpend)
#tuc_ensemble_Meantesting <- TukeyHSD(aov_ensemble_Meantesting)
tuc_ensemble_Giniinequaly <- TukeyHSD(aov_ensemble_Giniinequaly)
tuc_ensemble_Healthstatus <- TukeyHSD(aov_ensemble_Healthstatus)

#FCM:
tuc_FCM_Beds <- TukeyHSD(aov_FCM_Beds)
tuc_FCM_Workers <- TukeyHSD(aov_FCM_Workers)
tuc_FCM_hhsize <- TukeyHSD(aov_FCM_hhsize)
tuc_FCM_Femalelabor <- TukeyHSD(aov_FCM_Femalelabor)
tuc_FCM_Lifeexpectancy <- TukeyHSD(aov_FCM_Lifeexpectancy)
tuc_FCM_Publicexpend <- TukeyHSD(aov_FCM_Publicexpend)
tuc_FCM_Privatexpend <- TukeyHSD(aov_FCM_Privatexpend)
#tuc_FCM_Meantesting <- TukeyHSD(aov_FCM_Meantesting)
tuc_FCM_Giniinequaly <- TukeyHSD(aov_FCM_Giniinequaly)
tuc_FCM_Healthstatus <- TukeyHSD(aov_FCM_Healthstatus)


#SOM:
tuc_SOM_Beds <- TukeyHSD(aov_SOM_Beds)
tuc_SOM_Workers <- TukeyHSD(aov_SOM_Workers)
tuc_SOM_hhsize <- TukeyHSD(aov_SOM_hhsize)
tuc_SOM_Femalelabor <- TukeyHSD(aov_SOM_Femalelabor)
tuc_SOM_Lifeexpectancy <- TukeyHSD(aov_SOM_Lifeexpectancy)
tuc_SOM_Publicexpend <- TukeyHSD(aov_SOM_Publicexpend)
tuc_SOM_Privatexpend <- TukeyHSD(aov_SOM_Privatexpend)
#tuc_SOM_Meantesting <- TukeyHSD(aov_SOM_Meantesting)
tuc_SOM_Giniinequaly <- TukeyHSD(aov_SOM_Giniinequaly)
tuc_SOM_Healthstatus <- TukeyHSD(aov_SOM_Healthstatus)


#KMD:
tuc_KMD_Beds <- TukeyHSD(aov_KMD_Beds)
tuc_KMD_Workers <- TukeyHSD(aov_KMD_Workers)
tuc_KMD_hhsize <- TukeyHSD(aov_KMD_hhsize)
tuc_KMD_Femalelabor <- TukeyHSD(aov_KMD_Femalelabor)
tuc_KMD_Lifeexpectancy <- TukeyHSD(aov_KMD_Lifeexpectancy)
tuc_KMD_Publicexpend <- TukeyHSD(aov_KMD_Publicexpend)
tuc_KMD_Privatexpend <- TukeyHSD(aov_KMD_Privatexpend)
#tuc_KMD_Meantesting <- TukeyHSD(aov_KMD_Meantesting)
tuc_KMD_Giniinequaly <- TukeyHSD(aov_KMD_Giniinequaly)
tuc_KMD_Healthstatus <- TukeyHSD(aov_KMD_Healthstatus)

#Export excel with Tuckey K=7:


col<- rbind("2-1","3-1","4-1","5-1","6-1","7-1",
            "3-2","4-2","5-2","6-2","7-2",
            "4-3","5-3","6-3","7-3",
            "5-4","6-4","7-4",
            "6-5","7-5",
            "7-6")

library(xlsx)

#ensemble
write.xlsx(cbind(col,as.data.frame(tuc_ensemble_Beds$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="Beds", row.names=FALSE)
write.xlsx(cbind(col,as.data.frame(tuc_ensemble_Workers$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="Workers", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_ensemble_hhsize$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="hhsize", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_ensemble_Femalelabor$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="Femalelabor", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_ensemble_Lifeexpectancy$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="Lifeexpectancy", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_ensemble_Publicexpend$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="Publicexpend", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_ensemble_Privatexpend$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="Privatexpend", row.names=FALSE, append=TRUE)
#write.xlsx(cbind(col,as.data.frame(tuc_ensemble_Meantesting$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="Meantesting", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_ensemble_Giniinequaly$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="Giniinequaly", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_ensemble_Healthstatus$ensemble)), file="TUC_ensemble_Output.xlsx", sheetName="Healthstatus", row.names=FALSE, append=TRUE)

#FCM
write.xlsx(cbind(col,as.data.frame(tuc_FCM_Beds$FCM)), file="TUC_FCM_Output.xlsx", sheetName="Beds", row.names=FALSE)
write.xlsx(cbind(col,as.data.frame(tuc_FCM_Workers$FCM)), file="TUC_FCM_Output.xlsx", sheetName="Workers", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_FCM_hhsize$FCM)), file="TUC_FCM_Output.xlsx", sheetName="hhsize", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_FCM_Femalelabor$FCM)), file="TUC_FCM_Output.xlsx", sheetName="Femalelabor", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_FCM_Lifeexpectancy$FCM)), file="TUC_FCM_Output.xlsx", sheetName="Lifeexpectancy", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_FCM_Publicexpend$FCM)), file="TUC_FCM_Output.xlsx", sheetName="Publicexpend", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_FCM_Privatexpend$FCM)), file="TUC_FCM_Output.xlsx", sheetName="Privatexpend", row.names=FALSE, append=TRUE)
#write.xlsx(cbind(col,as.data.frame(tuc_FCM_Meantesting$FCM)), file="TUC_FCM_Output.xlsx", sheetName="Meantesting", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_FCM_Giniinequaly$FCM)), file="TUC_FCM_Output.xlsx", sheetName="Giniinequaly", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_FCM_Healthstatus$FCM)), file="TUC_FCM_Output.xlsx", sheetName="Healthstatus", row.names=FALSE, append=TRUE)


#SOM
write.xlsx(cbind(col,as.data.frame(tuc_SOM_Beds$SOM)), file="TUC_SOM_Output.xlsx", sheetName="Beds", row.names=FALSE)
write.xlsx(cbind(col,as.data.frame(tuc_SOM_Workers$SOM)), file="TUC_SOM_Output.xlsx", sheetName="Workers", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_SOM_hhsize$SOM)), file="TUC_SOM_Output.xlsx", sheetName="hhsize", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_SOM_Femalelabor$SOM)), file="TUC_SOM_Output.xlsx", sheetName="Femalelabor", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_SOM_Lifeexpectancy$SOM)), file="TUC_SOM_Output.xlsx", sheetName="Lifeexpectancy", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_SOM_Publicexpend$SOM)), file="TUC_SOM_Output.xlsx", sheetName="Publicexpend", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_SOM_Privatexpend$SOM)), file="TUC_SOM_Output.xlsx", sheetName="Privatexpend", row.names=FALSE, append=TRUE)
#write.xlsx(cbind(col,as.data.frame(tuc_SOM_Meantesting$SOM)), file="TUC_SOM_Output.xlsx", sheetName="Meantesting", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_SOM_Giniinequaly$SOM)), file="TUC_SOM_Output.xlsx", sheetName="Giniinequaly", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_SOM_Healthstatus$SOM)), file="TUC_SOM_Output.xlsx", sheetName="Healthstatus", row.names=FALSE, append=TRUE)


#KMD
write.xlsx(cbind(col,as.data.frame(tuc_KMD_Beds$KMD)), file="TUC_KMD_Output.xlsx", sheetName="Beds", row.names=FALSE)
write.xlsx(cbind(col,as.data.frame(tuc_KMD_Workers$KMD)), file="TUC_KMD_Output.xlsx", sheetName="Workers", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_KMD_hhsize$KMD)), file="TUC_KMD_Output.xlsx", sheetName="hhsize", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_KMD_Femalelabor$KMD)), file="TUC_KMD_Output.xlsx", sheetName="Femalelabor", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_KMD_Lifeexpectancy$KMD)), file="TUC_KMD_Output.xlsx", sheetName="Lifeexpectancy", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_KMD_Publicexpend$KMD)), file="TUC_KMD_Output.xlsx", sheetName="Publicexpend", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_KMD_Privatexpend$KMD)), file="TUC_KMD_Output.xlsx", sheetName="Privatexpend", row.names=FALSE, append=TRUE)
#write.xlsx(cbind(col,as.data.frame(tuc_KMD_Meantesting$KMD)), file="TUC_KMD_Output.xlsx", sheetName="Meantesting", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_KMD_Giniinequaly$KMD)), file="TUC_KMD_Output.xlsx", sheetName="Giniinequaly", row.names=FALSE, append=TRUE)
write.xlsx(cbind(col,as.data.frame(tuc_KMD_Healthstatus$KMD)), file="TUC_KMD_Output.xlsx", sheetName="Healthstatus", row.names=FALSE, append=TRUE)


##################
tuc_KMD_Beds <- cbind(col,as.data.frame(tuc_KMD_Beds$KMD))
tuc_KMD_Workers <- cbind(col,as.data.frame(tuc_KMD_Workers$KMD))
tuc_KMD_hhsize <- cbind(col,as.data.frame(tuc_KMD_hhsize$KMD))
tuc_KMD_Femalelabor <- cbind(col,as.data.frame(tuc_KMD_Femalelabor$KMD))
tuc_KMD_Lifeexpectancy <- cbind(col,as.data.frame(tuc_KMD_Lifeexpectancy$KMD))
tuc_KMD_Publicexpend <- cbind(col,as.data.frame(tuc_KMD_Publicexpend$KMD))
tuc_KMD_Privatexpend <- cbind(col,as.data.frame(tuc_KMD_Privatexpend$KMD))
#tuc_KMD_Meantesting <- cbind(col,as.data.frame(tuc_KMD_Meantesting$KMD))
tuc_KMD_Giniinequaly <- cbind(col,as.data.frame(tuc_KMD_Giniinequaly$KMD))
tuc_KMD_Healthstatus <- cbind(col,as.data.frame(tuc_KMD_Healthstatus$KMD))

tuc_kmd_output <- list(tuc_KMD_Beds, tuc_KMD_Workers, tuc_KMD_hhsize, tuc_KMD_Femalelabor, tuc_KMD_Lifeexpectancy, tuc_KMD_Publicexpend,
                       tuc_KMD_Privatexpend, tuc_KMD_Giniinequaly, tuc_KMD_Healthstatus)

write_xlsx(tuc_kmd_output, "TUC_KMD_Output.xlsx")

tuc_SOM_Beds <- cbind(col,as.data.frame(tuc_SOM_Beds$SOM))
tuc_SOM_Workers <- cbind(col,as.data.frame(tuc_SOM_Workers$SOM))
tuc_SOM_hhsize <- cbind(col,as.data.frame(tuc_SOM_hhsize$SOM))
tuc_SOM_Femalelabor <- cbind(col,as.data.frame(tuc_SOM_Femalelabor$SOM))
tuc_SOM_Lifeexpectancy <- cbind(col,as.data.frame(tuc_SOM_Lifeexpectancy$SOM))
tuc_SOM_Publicexpend <- cbind(col,as.data.frame(tuc_SOM_Publicexpend$SOM))
tuc_SOM_Privatexpend <- cbind(col,as.data.frame(tuc_SOM_Privatexpend$SOM))
#tuc_SOM_Meantesting <- cbind(col,as.data.frame(tuc_SOM_Meantesting$SOM))
tuc_SOM_Giniinequaly <- cbind(col,as.data.frame(tuc_SOM_Giniinequaly$SOM))
tuc_SOM_Healthstatus <- cbind(col,as.data.frame(tuc_SOM_Healthstatus$SOM))

tuc_SOM_output <- list(tuc_SOM_Beds, tuc_SOM_Workers, tuc_SOM_hhsize, tuc_SOM_Femalelabor, tuc_SOM_Lifeexpectancy, tuc_SOM_Publicexpend,
                       tuc_SOM_Privatexpend, tuc_SOM_Giniinequaly, tuc_SOM_Healthstatus)

write_xlsx(tuc_SOM_output, "TUC_SOM_Output.xlsx")



tuc_FCM_Beds <- cbind(col,as.data.frame(tuc_FCM_Beds$FCM))
tuc_FCM_Workers <- cbind(col,as.data.frame(tuc_FCM_Workers$FCM))
tuc_FCM_hhsize <- cbind(col,as.data.frame(tuc_FCM_hhsize$FCM))
tuc_FCM_Femalelabor <- cbind(col,as.data.frame(tuc_FCM_Femalelabor$FCM))
tuc_FCM_Lifeexpectancy <- cbind(col,as.data.frame(tuc_FCM_Lifeexpectancy$FCM))
tuc_FCM_Publicexpend <- cbind(col,as.data.frame(tuc_FCM_Publicexpend$FCM))
tuc_FCM_Privatexpend <- cbind(col,as.data.frame(tuc_FCM_Privatexpend$FCM))
#tuc_FCM_Meantesting <- cbind(col,as.data.frame(tuc_FCM_Meantesting$FCM))
tuc_FCM_Giniinequaly <- cbind(col,as.data.frame(tuc_FCM_Giniinequaly$FCM))
tuc_FCM_Healthstatus <- cbind(col,as.data.frame(tuc_FCM_Healthstatus$FCM))

tuc_FCM_output <- list(tuc_FCM_Beds, tuc_FCM_Workers, tuc_FCM_hhsize, tuc_FCM_Femalelabor, tuc_FCM_Lifeexpectancy, tuc_FCM_Publicexpend,
                       tuc_FCM_Privatexpend, tuc_FCM_Giniinequaly, tuc_FCM_Healthstatus)

write_xlsx(tuc_FCM_output, "TUC_FCM_Output.xlsx")



tuc_ensemble_Beds <- cbind(col,as.data.frame(tuc_ensemble_Beds$ensemble))
tuc_ensemble_Workers <- cbind(col,as.data.frame(tuc_ensemble_Workers$ensemble))
tuc_ensemble_hhsize <- cbind(col,as.data.frame(tuc_ensemble_hhsize$ensemble))
tuc_ensemble_Femalelabor <- cbind(col,as.data.frame(tuc_ensemble_Femalelabor$ensemble))
tuc_ensemble_Lifeexpectancy <- cbind(col,as.data.frame(tuc_ensemble_Lifeexpectancy$ensemble))
tuc_ensemble_Publicexpend <- cbind(col,as.data.frame(tuc_ensemble_Publicexpend$ensemble))
tuc_ensemble_Privatexpend <- cbind(col,as.data.frame(tuc_ensemble_Privatexpend$ensemble))
#tuc_ensemble_Meantesting <- cbind(col,as.data.frame(tuc_ensemble_Meantesting$ensemble))
tuc_ensemble_Giniinequaly <- cbind(col,as.data.frame(tuc_ensemble_Giniinequaly$ensemble))
tuc_ensemble_Healthstatus <- cbind(col,as.data.frame(tuc_ensemble_Healthstatus$ensemble))

tuc_ensemble_output <- list(tuc_ensemble_Beds, tuc_ensemble_Workers, tuc_ensemble_hhsize, tuc_ensemble_Femalelabor, tuc_ensemble_Lifeexpectancy, tuc_ensemble_Publicexpend,
                            tuc_ensemble_Privatexpend, tuc_ensemble_Giniinequaly, tuc_ensemble_Healthstatus)

write_xlsx(tuc_ensemble_output, "TUC_ensemble_Output.xlsx")


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################


#To calculate the internal criteria – cases:

rm(list = ls(all.names = TRUE)) 
################################################################################
library(readxl)
library(dplyr)
library(clusterCrit)
Base_1 <- read_excel("Base1_PCA.xlsx", col_names=FALSE)
colnames(Base_1)
colnames(Base_1) <- c('Country','PC1','PC2','PC3', 'PC4', 'PC5')
path <- ("clusters.xlsx")

#K=7:
ensemble7My <- pull(read_excel(path, sheet = 9, range = "B1:B31"))
ensemble7My <- ensemble7My [ ensemble7My!= 0 ]
ensemble7My  <-  as.integer(ensemble7My)


BaseFinal <- cbind(Base_1, ensemble7My)

BaseFinal2 <- BaseFinal[, 2:6]
intCriteria(as.matrix(BaseFinal2), ensemble7My,"Calinski_Harabasz")
intCriteria(as.matrix(BaseFinal2), ensemble7My,"Davies_Bouldin")
intCriteria(as.matrix(BaseFinal2), ensemble7My,"Dunn")

library(cluster)
Base_1lesscountry <- Base_1[,2:6]
dist.matrix <- dist(Base_1lesscountry)
sil <- silhouette(ensemble7My, dist.matrix)
mean(sil[, 3])    # average silhouette width



#m=2,5:
ensemble7m25 <- pull(read_excel(path, sheet = 10, range = "B1:B31"))
ensemble7m25 <- ensemble7m25[ ensemble7m25 != 0 ]
ensemble7m25 <-  as.integer(ensemble7m25)

BaseFinal <- cbind(Base_1, ensemble7m25)

BaseFinal2 <- BaseFinal[, 2:6]
intCriteria(as.matrix(BaseFinal2),ensemble7m25,"Calinski_Harabasz")
intCriteria(as.matrix(BaseFinal2),ensemble7m25,"Davies_Bouldin")
intCriteria(as.matrix(BaseFinal2),ensemble7m25,"Dunn")

sil <- silhouette(ensemble7m25, dist.matrix)
mean(sil[, 3])    # average silhouette width



# m=1,5:
ensemble7m15 <- pull(read_excel(path, sheet = 11, range = "B1:B31"))
ensemble7m15 <- ensemble7m15[ ensemble7m15 != 0 ]
ensemble7m15 <-  as.integer(ensemble7m15)

BaseFinal <- cbind(Base_1, ensemble7m15)

BaseFinal2 <- BaseFinal[, 2:6]
intCriteria(as.matrix(BaseFinal2),ensemble7m15,"Calinski_Harabasz")
intCriteria(as.matrix(BaseFinal2),ensemble7m15,"Davies_Bouldin")
intCriteria(as.matrix(BaseFinal2),ensemble7m15,"Dunn")

sil <- silhouette(ensemble7m15, dist.matrix)
mean(sil[, 3])    # average silhouette width



# m=1,001:
ensemble7m1 <- pull(read_excel(path, sheet = 12, range = "B1:B31"))
ensemble7m1 <- ensemble7m1[ ensemble7m1 != 0 ]
ensemble7m1 <-  as.integer(ensemble7m1)

BaseFinal <- cbind(Base_1, ensemble7m1)

BaseFinal2 <- BaseFinal[, 2:6]
intCriteria(as.matrix(BaseFinal2),ensemble7m1,"Calinski_Harabasz")
intCriteria(as.matrix(BaseFinal2),ensemble7m1,"Davies_Bouldin")
intCriteria(as.matrix(BaseFinal2),ensemble7m1,"Dunn")

sil <- silhouette(ensemble7m1, dist.matrix)
mean(sil[, 3])    # average silhouette width



# m=1,2:
ensemble7m12 <- pull(read_excel(path, sheet = 15, range = "B1:B31"))
ensemble7m12 <- ensemble7m12[ ensemble7m12 != 0 ]
ensemble7m12 <-  as.integer(ensemble7m12)

BaseFinal <- cbind(Base_1, ensemble7m12)

BaseFinal2 <- BaseFinal[, 2:6]
intCriteria(as.matrix(BaseFinal2),ensemble7m12,"Calinski_Harabasz")
intCriteria(as.matrix(BaseFinal2),ensemble7m12,"Davies_Bouldin")
intCriteria(as.matrix(BaseFinal2),ensemble7m12,"Dunn")

sil <- silhouette(ensemble7m12, dist.matrix)
mean(sil[, 3])    # average silhouette width



#SOM rectangular:
ensemble7SOMREC <- pull(read_excel(path, sheet = 13, range = "B1:B31"))
ensemble7SOMREC <- ensemble7SOMREC[ ensemble7SOMREC != 0 ]
ensemble7SOMREC <-  as.integer(ensemble7SOMREC)


BaseFinal <- cbind(Base_1, ensemble7SOMREC)

BaseFinal2 <- BaseFinal[, 2:6]
intCriteria(as.matrix(BaseFinal2),ensemble7SOMREC,"Calinski_Harabasz")
intCriteria(as.matrix(BaseFinal2),ensemble7SOMREC,"Davies_Bouldin")
intCriteria(as.matrix(BaseFinal2),ensemble7SOMREC,"Dunn")

sil <- silhouette(ensemble7SOMREC, dist.matrix)
mean(sil[, 3])    # average silhouette width




#PAM manhatam:
ensemble7pamman <- pull(read_excel(path, sheet = 14, range = "B1:B31"))
ensemble7pamman <- ensemble7pamman[ ensemble7pamman != 0 ]
ensemble7pamman <-  as.integer(ensemble7pamman)


BaseFinal <- cbind(Base_1, ensemble7pamman)

BaseFinal2 <- BaseFinal[, 2:6]
intCriteria(as.matrix(BaseFinal2),ensemble7pamman,"Calinski_Harabasz")
intCriteria(as.matrix(BaseFinal2),ensemble7pamman,"Davies_Bouldin")
intCriteria(as.matrix(BaseFinal2),ensemble7pamman,"Dunn")

sil <- silhouette(ensemble7pamman, dist.matrix)
mean(sil[, 3])    # average silhouette width




# SOM K=5:
ensemble5som <- pull(read_excel(path, sheet = 16, range = "B1:B31"))
ensemble5som <- ensemble5som[ ensemble5som != 0 ]
ensemble5som <-  as.integer(ensemble5som)


BaseFinal <- cbind(Base_1, ensemble5som)

BaseFinal2 <- BaseFinal[, 2:6]
intCriteria(as.matrix(BaseFinal2),ensemble5som,"Calinski_Harabasz")
intCriteria(as.matrix(BaseFinal2),ensemble5som,"Davies_Bouldin")
intCriteria(as.matrix(BaseFinal2),ensemble5som,"Dunn")

sil <- silhouette(ensemble5som, dist.matrix)
mean(sil[, 3])    # average silhouette width



##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################


#To get the plots of ASW and Heatmaps:
  
#ASW plot:
  
library(cluster)

dist.matrix <- dist(Base_1)

sil <- silhouette(ensemble, dist.matrix)
mean(sil[, 3])    # average silhouette width
plot(sil,
     border = NA,
     main = "Silhouette plot (K = 7)")

library(factoextra)

fviz_silhouette(sil)



#Heatmap:
library(dplyr)

scaled.data <- Base_1_Original
scaled.data[,2:11] <- scale(scaled.data[,2:11])
scaled.data$Cluster <- factor(ensemble)
cluster.means <-
  scaled.data %>%
  group_by(Cluster) %>%
  summarise(across(Beds:Healthstatus, mean))

library(tidyr)

heat <- pivot_longer(cluster.means,
                     cols=-Cluster,
                     names_to="Variable",
                     values_to="Mean")


ggplot(heat,
       aes(Variable,
           Cluster,
           fill=Mean))+
  geom_tile(color="white")+
  scale_fill_gradient2(low="blue",
                       mid="white",
                       high="red",
                       midpoint=0)+
  theme_minimal()+
  theme(axis.text.x =
          element_text(angle=45,
                       hjust=1))


