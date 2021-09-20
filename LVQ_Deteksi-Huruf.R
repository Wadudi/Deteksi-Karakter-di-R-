#LVQ

data <- read.csv("C:/Users/DSITD-Wadudi/Documents/data dudi/dokumen/S2 ILKOM/Semester 1/Kecerdasan Komputasional/Kuliah/fonts.csv", header = T)

datalatih_lvq <- data[1:14,1:63]

bobotlvq <- data[15:21,1:63] #menggunakan datauji utk bobotlvq

#menggunakan data random utk bobot lvq
#rand_vector <- runif(63 * 7)
#bobotlvq <- cbind.data.frame(split(rand_vector, rep(1:63, times=length(rand_vector)/63)), stringsAsFactors=F)

datauji_lvq <- data[15:21,1:63]

rownames(bobotlvq)<- c("weight1", "weight2","weight3", "weight4","weight5", "weight6","weight7")

#buat vector target
target_lvq_datalatih <- c(1,2,3,4,5,6,7,1,2,3,4,5,6,7)
target_lvq_datalatih = matrix(target_lvq_datalatih)

target_lvq_datauji <- c('A','B','C','D','E','J','K')
target_lvq_datauji = matrix(target_lvq_datauji)


n <- nrow(datalatih_lvq)
n1 <- nrow(datauji_lvq)
nepoch <- 10
alpha <- as.vector(rep(0, nepoch))
final_target_datalatih <- as.vector(rep(0, n))
final_target_datauji <- as.vector(rep(0, 7))

for(t in 1:nepoch){
  
  alpha[t] <- 0.1
  
  for(obyek in 1:n){
    
    jarak <- as.vector(rep(0, 7))
    
    #menghitung jarak
    for(j in 1:7){
      jarak[j] <- sum((datalatih_lvq[obyek,]-bobotlvq[j,])^2)
    }
    #dapat nilai minimum
    jarak_min <- min(jarak)
    
    #carijarak minimum ada di mana
    p = 0
    for(jm in 1:7){
      if(p == 0){
        if(jarak[jm] == jarak_min){
          
          if(jm == target_lvq_datalatih[obyek]){
            bobotlvq[jm,] <- bobotlvq[jm,] + alpha[t]*(datalatih_lvq[obyek,]-bobotlvq[jm,])
          }else{
            bobotlvq[jm,] <- bobotlvq[jm,] - alpha[t]*(datalatih_lvq[obyek,]-bobotlvq[jm,])
          }
          p<-1
          if(jm == 1){
            final_target_datalatih[obyek] = 'A'
          }else if(jm == 2){
            final_target_datalatih[obyek] = 'B'
          }else if(jm == 3){
            final_target_datalatih[obyek] = 'C'
          }else if(jm == 4){
            final_target_datalatih[obyek] = 'D'
          }else if(jm == 5){
            final_target_datalatih[obyek] = 'E'
          }else if(jm == 6){
            final_target_datalatih[obyek] = 'J'
          }else if(jm == 7){
            final_target_datalatih[obyek] = 'K'
          }
        } 
      }
    }
  }
}

#mentest datauji dengan bobot akhir yang sudah dilakukan training sebanyak 10 iterasi.
for(obyek in 1:n1){
  jarak <- as.vector(rep(0, 7))
  for(j in 1:7){
    jarak[j] <- sum((datauji_lvq[obyek,]-bobotlvq[j,])^2)
  }
  #dapat nilai minimum
  jarak_min <- min(jarak)
  
  #carijarak minimum ada di mana
  p = 0
  for(jm in 1:7){
    if(p == 0){
      if(jarak[jm] == jarak_min){
        p<-1
        if(jm == 1){
          final_target_datauji[obyek] = 'A'
        }else if(jm == 2){
          final_target_datauji[obyek] = 'B'
        }else if(jm == 3){
          final_target_datauji[obyek] = 'C'
        }else if(jm == 4){
          final_target_datauji[obyek] = 'D'
        }else if(jm == 5){
          final_target_datauji[obyek] = 'E'
        }else if(jm == 6){
          final_target_datauji[obyek] = 'J'
        }else if(jm == 7){
          final_target_datauji[obyek] = 'K'
        }
      }
    }
  }
}


print(final_target_datauji)
#[1] "A" "B" "C" "D" "E" "J" "K"
#[1] 1 2 3 4 5 6 7

predicted_value <- final_target_datauji
expected_value <- target_lvq_datauji

example <- confusionMatrix(factor(predicted_value, levels = c('A','B','C','D','E','J','K')), factor(expected_value, levels = c('A','B','C','D','E','J','K')))


print(example$overall)

#Accuracy     
1.000000e+00   #mendapatkan akurasi 100%.


print(final_target_datalatih)
#[1] "A" "B" "C" "D" "E" "J" "K" "A" "B" "C" "D" "E" "J" "E"
#[1] 1 2 3 4 5 6 7 1 2 3 4 5 6 5

print(bobotlvq)
