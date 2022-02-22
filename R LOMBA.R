# KODING R UNTUK LOMBA ISOTERM

# Import library
library("ggpubr") # untuk plot sederhana dan uji korelasi
library("readxl") # untuk membuka .xlsx
library("nortest") # untuk uji normalitas data (untuk bisa pakai pearson)

# Import data
data_Indonesia = read_excel("D:/Lomba Esai/002/DATA LOMBA.xlsx", sheet = "Indonesia")
data_Indonesia
data_Dunia = read_excel("D:/Lomba Esai/002/DATA LOMBA.xlsx", sheet = "Dunia")
data_Dunia
data_Additional = read_excel("D:/Lomba Esai/002/DATA LOMBA.xlsx", sheet = "Additional")
data_Additional

# PENGOLAHAN DATA KORELASI PERKAPITA TERHADAP AFFLUENCE

# Indonesia
x_Indonesia = data_Indonesia$`A (GDP Perkapita)`
x_Indonesia
y_Indonesia = data_Indonesia$`Tapak ekologi Konsumsi Perkapita`
y_Indonesia

# uji normal
# H0 : Normal
# H1 : Tidak
# Tolak H0 saat p-val < alpha = 0.05
# Anderson-Darling
ad.test(x_Indonesia)
ad.test(y_Indonesia)
# Kolmogorov-Smirnov
lillie.test(x_Indonesia)
lillie.test(y_Indonesia)
# Shapiro
shapiro.test(x_Indonesia)
shapiro.test(y_Indonesia)
# Hasil : Data tidak berdistribusi normal kecuali dengan KG-Smirnov
# Akibatnya korelasi Pearson tidak dianjurkan untuk dipakai

# korelasi
# plotting dan melihat koefisien korelasi spearman
ggscatter(data_Indonesia, x = "A (GDP Perkapita)", y = "Tapak ekologi Konsumsi Perkapita",
          add = "reg.line", cor.coef = "TRUE", cor.method = "spearman",
          xlab = "GDP Per Kapita", ylab = "Tapak Ekologi")
# koefisien korelasi
cor.test(x_Indonesia,y_Indonesia, method = "spearman")
cor.test(x_Indonesia,y_Indonesia, method = "kendall")
cor.test(x_Indonesia,y_Indonesia, method = "pearson") #tidak dianjurkan
# Hasil : Nilai koefisien masing-masing jauh melebihi 0.5.
# Jadi TERDAPAT korelasi antara GDP dengan tapak ekologi
# dimana semakin besar GDP, semakin besar tapak ekologi

# Dunia
x_Dunia = data_Dunia$`A (GDP Perkapita)`
x_Dunia
y_Dunia = data_Dunia$`Tapak ekologi Konsumsi Perkapita`
y_Dunia

# uji normal
# H0 : Normal
# H1 : Tidak
# Tolak H0 saat p-val < alpha = 0.05
# Anderson-Darling
ad.test(x_Dunia)
ad.test(y_Dunia)
# Kolmogorov-Smirnov
lillie.test(x_Dunia)
lillie.test(y_Dunia)
# Shapiro
shapiro.test(x_Dunia)
shapiro.test(y_Dunia)
# Hasil : Data tidak berdistribusi normal di semua uji
# Akibatnya tidak dianjurkan menggunakan koefisien korelasi Pearson

# korelasi
# plotting dan melihat koefisien korelasi spearman
ggscatter(data_Dunia, x = "A (GDP Perkapita)", y = "Tapak ekologi Konsumsi Perkapita",
          add = "reg.line", cor.coef = "TRUE", cor.method = "spearman",
          xlab = "GDP Per Kapita", ylab = "Tapak Ekologi")
# koefisien korelasi
cor.test(x_Dunia,y_Dunia, method = "spearman")
cor.test(x_Dunia,y_Dunia, method = "kendall")
cor.test(x_Dunia,y_Dunia, method = "pearson") # tidak dianjurkan
# Hasil : Menunjukkan adanya korelasi dari GDP dengan Tapak ekologi
# tetapi korelasinya lebih lemah dari di Indonesia
# Jadi bahkan tren dunia dari waktu ke waktu
# menyatakan bahwa GDP mempengaruhi tapak ekologi
# walau pengaruhnya lebih lemah dari pengaruh di Indonesia.

# Additional (untuk melihat trend negara di dunia tahun 2016)
x_Additional = data_Additional$`A (GDP Perkapita)`
x_Additional
y_Additional = data_Additional$`Tapak ekologi Konsumsi Perkapita`
y_Additional

# uji normal
# H0 : Normal
# H1 : Tidak
# Tolak H0 saat p-val < alpha = 0.05
# Anderson-Darling
ad.test(x_Additional)
ad.test(y_Additional)
# Kolmogorov-Smirnov
lillie.test(x_Additional)
lillie.test(y_Additional)
# Shapiro
shapiro.test(x_Additional)
shapiro.test(y_Additional)
# Hasil : p-value sangat kecil sehingga H0 dapat ditolak dan akibatnya
# data tidak berdistribusi normal
# Akibatnya tidak dianjurkan menggunakan koefisien korelasi Pearson

# korelasi
# plotting dan melihat koefisien korelasi spearman
ggscatter(data_Additional, x = "A (GDP Perkapita)", y = "Tapak ekologi Konsumsi Perkapita",
          add = "reg.line", cor.coef = "TRUE", cor.method = "spearman",
          xlab = "GDP Per Kapita", ylab = "Tapak Ekologi")
# koefisien korelasi
cor.test(x_Additional,y_Additional, method = "spearman")
cor.test(x_Additional,y_Additional, method = "kendall")
cor.test(x_Additional,y_Additional, method = "pearson")
# Hasil : Nilai koefisien korelasi cukup tinggi
# Akibatnya dapat disimpulkan bahwa pada tren dunia di tahun 2016
# GDP perkapita masih mempengaruhi tapak ekologi perkapita

# KESIMPULAN: GDP MEMPENGARUHI BESARAN TAPAK EKOLOGI
# KARENA TAPAK EKOLOGI MEMPENGARUHI DAMPAK TERHADAP LINGKUNGAN
# BERDASARKAN MODEL I=PAT, MAKA GDP MEMPENGARUHI KERUSAKAN LINGKUNGAN


# REGRESI DAN EKSTRAPOLASI UNTUK GDP~TAPAK EKOLOGI 2045 DI INDONESIA

# Percobaan 1: Model regresi linear
ggscatter(data_Indonesia, x = "A (GDP Perkapita)", y = "Tapak ekologi Konsumsi Perkapita",
          add = "reg.line", xlab = "GDP Per Kapita Indonesia",
          add.params = list(color = "red", linetype="dashed"),
          ylab = "Tapak Ekologi Per Kapita Indonesia")
model = lm(y_Indonesia ~ x_Indonesia)
summary(model)
# Model regresinya bagus karena p-value untuk regresinya kecil
# jadi model signifikan serta nilai dari R-squared yaitu 0.8496
# yang menyatakan bahwa model ini bagus untuk dipakai untuk
# merepresentasikan hubungan GDP dengan tapak ekologi (perkapita)
# di Indonesia.
# Model regresinya linearnya,
# y = 0.00009049x + 1.277
# R-squared : 0.8496


# MELIHAT MASA DEPAN (2045)
# Model regresi,
# y = 0.00009049x + 1.277
# Target pemerintah GDP perkapita Indonesia 2045: 23199
# Tapak ekologi perkapita (hasil regresi): 3.37627751
# Proyeksi populasi indo 2045 (berdasarkan un): 325705348
# I = PAT (asumsi T konstan)
# Impact tanpa pengendalian                    : 1099671641
# Impact tapak sama dengan 2016 (populasi naik): 550438939
# Impact populasi sama dengan 2016 (tapak naik): 883086926.8
# Kesimpulan:
# SANGAT PENTING UNTUK MENJAGA AGAR TAPAK EKOLOGI TIDAK NAIK
# SAAT GDP NAIK.