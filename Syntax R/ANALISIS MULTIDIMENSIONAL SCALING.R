data <- read.csv(file.choose(),header=T,sep=";",dec=",")
data
str(data)

# Data numerik saja
dataku <- data[, 2:5]

# Normalisasi
dataku_scaled <- scale(dataku)

# Matriks jarak Euclidean
d <- dist(dataku_scaled)
d

fit<-cmdscale(d, eig=TRUE,k=2)
fit

x <- fit$points[,1]
y <- fit$points[,2]
plot(x,y,xlab="x",ylab = "y",main="Classical MDS",type="p",pch=20)
text(x,y,labels=data$Kabupaten.Kota,cex=0.7,pos=3)
