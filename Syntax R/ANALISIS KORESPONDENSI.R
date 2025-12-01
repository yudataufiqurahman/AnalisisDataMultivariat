data <- read.csv(file.choose(),header=TRUE,sep=";",dec=",")
data

tabel1 <- xtabs(~data$Domisili+data$Tempat.Bekerja,data=data)
tabel1

prop.table(tabel1,1) #Proporsi tiap baris
prop.table(tabel1,2) #Proporsi tiap kolom

library(ca)
fit <- ca(tabel1)
fit

summary(fit)

plot(fit, mass=TRUE,contrib="absolute",map="rowgreen")

