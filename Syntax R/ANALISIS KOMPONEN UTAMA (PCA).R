data<-read.csv(file.choose(),header=T,sep=";",dec=",")
str(data)

x1 = data$Produksi.Beras
x2 = data$Luas.Panen.Padi
x3 = data$Produksi.Telur.Unggas...Ayam.Buras.kg...Kg.
x4 = data$Produk.Domestik.Regional.Bruto..PDRB.
x5 = data$Rata.rata.Pengeluaran.per.Kapita.Sebulan.di.Perkotaan.dan.Perdesaan...Jumlah
x6 = data$Laju.Pertumbuhan.Penduduk
data2 <- data.frame(cbind(x1,x2,x3,x4,x5,x6));data2

#Standardisasi Data
X <- as.matrix(scale(data2[,1:6]))
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

