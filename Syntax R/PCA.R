data<-read.csv(file.choose(),header=T,sep=",",dec=".")
str(data)

data

#Standardisasi Data
X <- as.matrix(scale(data[,2:6]))
head(X)

#Menghitung matriks korelasi
b<-cor(X)
b

#Menghitung eigenvalue dan eigenvector
eigen(b)

#Menganalisis Komponen Utama
fit_pca<-princomp(X,cor=T)
fit_pca

summary(fit_pca)

#Visualisasi
plot(fit_pca,type="barplot",main="Bar Plot PCA",col="blue")
plot(fit_pca,type="lines",main="Scree Plot PCA",col="blue")

