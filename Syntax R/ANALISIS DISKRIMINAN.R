# 1. Baca ulang data
data <- read.csv(file.choose(),header=T,sep=";",dec=",")

# Hapus baris dengan NA (jika ada)
data <- na.omit(data)
str(data)
# Ubah kolom target jadi faktor (kolom 'num' adalah target)
data$Class <- as.factor(data$Class)

nrow(data)
library(biotools)

# Asumsi: 'num' adalah variabel kategori (Y), sisanya X
box_m_result <- boxM(data[, -which(names(data) == "Class")], grouping = data$Class)

# Lihat hasil
print(box_m_result)

library(MVN)

# Uji normalitas Mardia (untuk seluruh data tanpa grup)
mardia_result <- mvn(data[, -which(names(data) == "Class")], mvnTest = "mardia")

# Lihat hasil
print(mardia_result)

library(Hotelling)

# Pisahkan dua grup
group1 <- subset(data, Class == "Cammeo")[, -which(names(data) == "Class")]
group2 <- subset(data, Class == "Osmancik")[, -which(names(data) == "Class")]

# Jalankan Hotelling's T² test
tes<-hotelling.test(group1, group2)
tes


# Pastikan kolom X adalah semua kecuali 'num'
X_vars <- names(data)[names(data) != "Class"]

# Buat formula
formula <- as.formula(paste("Class ~", paste(X_vars, collapse = " + ")))

# LDA
library(MASS)
lda_model <- lda(data$Class~data$Area+data$Perimeter+data$Convex_Area,data=data)

# Prediksi
lda_pred <- predict(lda_model)

# Tabel klasifikasi
ct<-table(Predicted = lda_pred$class, Actual = data$Class)
ct
mean(lda_pred$class == data$Class)
diag(prop.table(ct,1))
sum(diag(prop.table(ct)))

