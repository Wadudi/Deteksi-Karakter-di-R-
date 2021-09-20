

data_heb <- read.csv("fonts-heb.csv", header = T)
#View(data_heb)
datalatih_heb <- data_heb[1:14,1:63]
#View(datalatih_heb)

datauji_heb <- data_heb[15:21,1:63]

#buat target 
target_heb_datalatih <- c(-1,1,-1,1,1,-1,1,-1,1,-1,1,1,-1,-1)

#buat target uji
target_heb_datauji <- c(-1,1,-1,1,1,-1,1)

#awal
bobot_heb <- as.vector(rep(0, 63))
bias_heb <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)

fungsi_aktivasi <- as.vector(rep(0, 14))
fungsi_aktivasiuji <- as.vector(rep(0, 7))


n <- nrow(datalatih_heb)
final_target_datalatih <- as.vector(rep(0, n))


for(obyek in 1:n){
  
  bias_heb[obyek] <- bias_heb[obyek] + target_heb_datalatih[obyek]
  
  for(w in 1:63){
    bobot_heb[w] <- bobot_heb[w] + (datalatih_heb[obyek,w] * target_heb_datalatih[obyek])
  }
   
}

#dapatkan fungsi aktivasi datalatih.
for(obyek in 1:n){
  for(w in 1:63){
    fungsi_aktivasi[obyek] <- fungsi_aktivasi[obyek] + (datalatih_heb[obyek,w] * bobot_heb[w])
  }
  fungsi_aktivasi[obyek] <- fungsi_aktivasi[obyek] + bias_heb[14]
}
print(fungsi_aktivasi)


# testing data uji

for(obyek in 1:7){
  for(w in 1:63){
    fungsi_aktivasiuji[obyek] <- fungsi_aktivasiuji[obyek] + (datauji_heb[obyek,w] * bobot_heb[w])
  }
  fungsi_aktivasiuji[obyek] <- fungsi_aktivasiuji[obyek] + bias_heb[14]
}
print(fungsi_aktivasiuji)

#karna pakai bipolar, maka jika hasil > 0 = 1, jika hasil < 0 = -1.
for(obyek in 1:7){
  if(fungsi_aktivasiuji[obyek] > 0){
    fungsi_aktivasiuji[obyek]=1 
  }else{
    fungsi_aktivasiuji[obyek]=-1 
  }
}


#Result
#> print(fungsi_aktivasiuji)
#[1]  -30  126  -22  122  102 -142   78

#Target data uji
#target_heb_datauji <- c(-1,1,-1,1,1,-1,1)

# lihat hasil prediksi
data.frame(
  "Predicted" = round(fungsi_aktivasiuji, 3), 
  "Actual" = target_heb_datauji
)

#Predicted Actual
#1        -1     -1
#2         1      1
#3        -1     -1
#4         1      1
#5         1      1
#6        -1     -1
#7         1      1

#Akurasi 100%

