

data = read.csv("fonts.csv", header = T)
#View(data_heb)
datalatih_heb <- data_heb[1:14,1:63]
#View(datalatih_heb)

datauji_heb <- data_heb[15:21,1:63]

#buat target 
target_heb_datalatih <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)

#awal
bobot_heb <- as.vector(rep(0, 63))
bias_heb <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)

fungsi_aktivasi <- as.vector(rep(0, 14))

# update bobot baru
# w1 = w1old + (x1* t)

# update bias baru
# b1 = blama + t


n <- nrow(datalatih_heb)
final_target_datalatih <- as.vector(rep(0, n))


for(obyek in 1:n){
  
  bias_heb[obyek] <- bias_heb[obyek] + target_heb_datalatih[obyek]
  
  for(w in 1:63){
    bobot_heb[w] <- bobot_heb[w] + (datalatih_heb[obyek,w] * target_heb_datalatih[obyek])
  }
   
}

Y_hat <- c()
for (letter in 1:7) { # untuk setiap huruf A, B, C, D, E, J, K
  temp <- c()
  for (i in 1:7) { # untuk setiap output Y[1] sampai Y[7]
    temp <- append(temp, sum(X[letter,] * my_hebb$weights[i,]))
  }
  # print(temp)
  Y_hat <- append(Y_hat, temp)
}

# testing data uji menggunakan bobot yg sudah diupdate
for(obyek in 1:n){
  for(w in 1:63){
    bobot_heb[w] <- bobot_heb[w] + (datauji_heb[obyek,w] * target_heb_datalatih[obyek])
  }
  
}



#for(obyek in 1:n){
#  for(w in 1:63){
#    fungsi_aktivasi[obyek] <- fungsi_aktivasi[obyek] + (datalatih_heb[obyek,w] * bobot_heb[w])
#  }
#  fungsi_aktivasi[obyek] + fungsi_aktivasi[obyek] + bias_heb[14]
#}

#print(fungsi_aktivasi)

