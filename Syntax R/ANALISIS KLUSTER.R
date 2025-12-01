data <- read.csv(file.choose(),header=T,sep=";",dec=",")
data

data_kluster <- data[,3:8];data_kluster
str(data_kluster)

dataku <- scale(data_kluster)
dataku

d <- dist(dataku, method="euclidean")
d

library(cluster)
set.seed(123)
gap_stat <- clusGap(dataku, FUN = hcut, K.max = 10, B = 100)
fviz_gap_stat(gap_stat)


fit <- hclust(d,method="single")
plot(fit)
rect.hclust(fit,k=3,border="red")

fit2 <- hclust(d,method="complete")
plot(fit2)
rect.hclust(fit2,k=3,border="red")

fit3 <- hclust(d,method="average")
plot(fit3)
rect.hclust(fit3,k=3,border="red")

fit4 <- hclust(d,method="ward.D2")
plot(fit4)
rect.hclust(fit3,k=3,border="red")

# 3. Cophenetic distance
coph_single <- cophenetic(fit);coph_single
coph_complete <- cophenetic(fit2)
coph_average <- cophenetic(fit3)
coph_ward <- cophenetic(fit4)

# 4. Koefisien cophenetic (korelasi antara dist asli dan hasil dendrogram)
coef_single <- cor(d, coph_single)
coef_complete <- cor(d, coph_complete)
coef_average <- cor(d, coph_average)
coef_ward <- cor(d, coph_ward)

# 5. Buat tabel perbandingan
cophenetic_table <- data.frame(
  Method = c("Single", "Complete", "Average","Ward"),
  Cophenetic_Correlation = c(coef_single, coef_complete, coef_average, coef_ward)
)

# 6. Tampilkan tabel
print(cophenetic_table)

library(cluster)
cluster <- cutree(fit3, k = 3)  # misalnya k = 3
sil <- silhouette(cluster, dist(dataku))
plot(sil)
mean(sil[, 3])  # rata-rata silhouette



##KMEANS CLUSTERING

# Install jika belum ada
install.packages(c("cluster", "factoextra", "NbClust", "tidyverse"))

# Load library
library(cluster)
library(factoextra)
library(NbClust)
library(tidyverse)

fviz_nbclust(dataku, kmeans, method = "wss") +
  labs(title = "Elbow Method - Total Within Sum of Squares")

fviz_nbclust(dataku, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method")

set.seed(123)
nb <- NbClust(dataku, min.nc = 2, max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

set.seed(123)
kmeans_result <- kmeans(dataku, centers = 6, nstart = 25)  # ubah jumlah klaster sesuai hasil evaluasi

summary(kmeans_result)


# Tambahkan hasil klaster ke data
data$cluster <- as.factor(kmeans_result$cluster)

fviz_cluster(kmeans_result, data = dataku,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#EVALUASI KMEANS
kmeans_result$tot.withinss  # Semakin kecil semakin baik

kmeans_result$betweenss / kmeans_result$totss  # Semakin mendekati 1, semakin baik

library(cluster)
sil <- silhouette(kmeans_result$cluster, dist(dataku))
mean(sil[, 3])  # Nilai rata-rata silhouette (semakin mendekati 1, semakin baik)

