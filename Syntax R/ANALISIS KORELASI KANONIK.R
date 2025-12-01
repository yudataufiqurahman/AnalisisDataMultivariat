install.packages("CCA")
install.packages("CCP")
library(CCA)
library(CCP)
library(MVN)
library(biotools)
library(heplots)

###input data
data <- read.csv(file.choose(), sep=";", dec=",", header=T)
str(data)



y1=as.numeric(data$y1)
y2=as.numeric(data$y2)
x1=as.numeric(data$x1)
x2=as.numeric(data$x2)
x3=as.numeric(data$x3)

data1 = data.frame(cbind(y1,y2,x1,x2,x3))

X <- cbind(x1,x2,x3)
head(X)

Y <- cbind(y1,y2)
head(Y)

str(data1)

# Uji normalitas multivariat pada Y
mardia_result <- mvn(data = data1, mvnTest = "mardia")
print(mardia_result)

# Hitung median
median_Y1 <- median(data$y1, na.rm = TRUE)

# Buat kolom kelompok berdasarkan median
data$y1_group <- ifelse(data$y1 <= median_Y1, "Low", "High")

# Ubah jadi faktor
data$y1_group <- factor(data$y1_group, levels = c("Low", "High"))

# Pastikan grouping berupa faktor, misalnya dari Happiness_group
group <- as.factor(data$y1_group)

# Uji homogenitas kovarian
boxM_result <- boxM(X, group = group)

print(boxM_result)

library(lmtest)
library(car)

model1 <- lm(y2 ~ x1 + x2 +x3, data = data1)
resettest(model1)

correl <- matcor(X,Y)
correl

img.matcor(correl, type = 2)

###PEMBENTUKAN FUNGSI KANONIK
library(CCA)
cc=cc(X, Y)
cc$cor

#lihat dimana korelasi yang paling tinggi
cc$ycoef
cc$xcoef

library(CCP)
rho=cc$cor
N=dim(X)[1] #jumlah baris dari var x
P=dim(X)[2] #banyak var x
Q=dim(Y)[2] #banyak var y

#Menggunakan statistik uji Wilks
p.asym(rho,N,P,Q,tstat="Wilks")

#Menggunakan statistik uji Hotelling
p.asym(rho,N,P,Q,tstat="Hotelling")

#Menggunakan statistik uji Pillai
p.asym(rho,N,P,Q,tstat="Pillai")

