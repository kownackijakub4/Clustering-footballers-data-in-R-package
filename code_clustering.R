library(factoextra)
library(corrplot)
library(FactoMineR)
library(cluster)
library(fpc)
library(clValid)
library(dendextend)
library(NbClust)
library(car)
library(gplots)
library(mclust)
library(fpc)
library(dbscan)
library(dplyr)



###WPROWADZENIE DANYCH###

dane <- read.csv(file = "footballers2wyczyszczone.csv", header = TRUE, sep=";", dec=".", row.names = 'name')
set.seed(123)
dane <- na.omit(dane)  #czyszczenie z NA
dane <- scale(dane)    #znormalizowanie kolumn

bb <- head(na.omit(dane))



###USTALANIE LICZBY SKUPIEN### 

#metoda lokcia dla metody K-średnich

fviz_nbclust(dane, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method for K-means method")

#metoda lokcia dla metody PAM

fviz_nbclust(dane, pam, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method for k-medoids method")

nb <- NbClust(dane, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)



###GRUPOWANIE###

#KMEANS - klasteruje dane metoda kmeans z 4 klastrami i 10 iteracjami###

dane_kmeans <- kmeans(dane, 4, iter.max = 10, nstart = 25) 
print(dane_kmeans)

dane_kmeans$cluster
dane_kmeans$centers
dane_kmeans$size


fviz_cluster(dane_kmeans, data = dane,
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = FALSE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal(),
             labelsize=12)

#PAM


dane_pam <- pam(dane,4)
print(dane_pam)

dane_pam$clustering
dane_pam$medoids
dane_pam$clusinfo


fviz_cluster(dane_pam,
             star_plot = TRUE,
             ellipse.type = 'euclid',
             repel = TRUE,
             ggtheme = theme_classic())


###METODA SYLWETKI####



sil_pam <- silhouette(dane_pam)
plot(sil_pam,col = c("red", "green", "blue", "purple") )
fviz_silhouette(sil_pam)


D <- daisy(dane)
sil_kmeans <- silhouette(dane_kmeans$cluster,D)
plot(sil_kmeans,col = c("red", "green", "blue", "purple") )
fviz_silhouette(sil_kmeans)



