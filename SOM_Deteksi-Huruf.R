#Nama : Wadudi Muthahari
#NIM  : G6501211053

#kohonen SOM
#Insatll required packages
#install.packages('caret')

#Import required library
#library(caret)

data = read.csv("fonts.csv", header = T)

datalatih <- data[1:14,1:63]
datauji <- data[15:21,1:63]

bobotsom <- data[15:21,1:63] #menggunakan datauji utk bobotsom

#menggunakan data random utk bobot som
#rand_vector <- runif(63 * 7)
#bobotsom <- cbind.data.frame(split(rand_vector, rep(1:63, times=length(rand_vector)/63)), stringsAsFactors=F)


rownames(bobotsom)<- c("clusterA", "clusterB","clusterC", "clusterD","clusterE", "clusterJ","clusterK")


# Dilanjutkan untuk n=4, dan nepoch=8
n <- nrow(datalatih)
n1 <- nrow(datauji)
nepoch <- 10
alpha <- as.vector(rep(0, nepoch))
final_cluster_datalatih <- as.vector(rep(0, n))
final_cluster_datauji <- as.vector(rep(0, 7))

for(t in 1:nepoch){
  
  if(t >= 0 && t <= 5){
    alpha[t] <- 0.5
  }else{
    alpha[t] <- 0.5*0.4
  }
  
  for(obyek in 1:n){
    
    jarak <- as.vector(rep(0, 7))
    
    #menghitung jarak
    for(j in 1:7){
      jarak[j] <- sum((datalatih[obyek,]-bobotsom[j,])^2)
    }
    #dapat nilai minimum
    jarak_min <- min(jarak)
    
    #carijarak minimum ada di cluster mana
    p = 0
    for(jm in 1:7){
      if(p == 0){
        if(jarak[jm] == jarak_min){
          bobotsom[jm,] <- bobotsom[jm,]+alpha[t]*(datalatih[obyek,]-bobotsom[jm,])
          p<-1
          if(jm == 1){
            final_cluster_datalatih[obyek] = 'A'
          }else if(jm == 2){
            final_cluster_datalatih[obyek] = 'B'
          }else if(jm == 3){
            final_cluster_datalatih[obyek] = 'C'
          }else if(jm == 4){
            final_cluster_datalatih[obyek] = 'D'
          }else if(jm == 5){
            final_cluster_datalatih[obyek] = 'E'
          }else if(jm == 6){
            final_cluster_datalatih[obyek] = 'J'
          }else if(jm == 7){
            final_cluster_datalatih[obyek] = 'K'
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
    jarak[j] <- sum((datauji[obyek,]-bobotsom[j,])^2)
  }
  #dapat nilai minimum
  jarak_min <- min(jarak)
  
  #carijarak minimum ada di cluster mana
  p = 0
  for(jm in 1:7){
    if(p == 0){
      if(jarak[jm] == jarak_min){
        p<-1
        if(jm == 1){
          final_cluster_datauji[obyek] = 'A'
        }else if(jm == 2){
          final_cluster_datauji[obyek] = 'B'
        }else if(jm == 3){
          final_cluster_datauji[obyek] = 'C'
        }else if(jm == 4){
          final_cluster_datauji[obyek] = 'D'
        }else if(jm == 5){
          final_cluster_datauji[obyek] = 'E'
        }else if(jm == 6){
          final_cluster_datauji[obyek] = 'J'
        }else if(jm == 7){
          final_cluster_datauji[obyek] = 'K'
        }
        
      }
    }
  }
}

print(final_cluster_datauji)
#[1] "A" "D" "C" "D" "E" "J" "K"
#[1] 1 4 3 4 5 6 7

predicted_value <- final_cluster_datauji
expected_value <- c('A','B','C','D','E','J','K')

example <- confusionMatrix(factor(predicted_value, levels = c('A','B','C','D','E','J','K')), factor(expected_value, levels = c('A','B','C','D','E','J','K')))

print(example$overall)

#Accuracy
#8.571429e-01 = 86%

print(final_cluster_datalatih)
print(bobotsom)



#> print(final_cluster_datalatih)
#[1] "A" "D" "C" "D" "E" "J" "K" "A" "B" "C" "C" "B" "J" "B"
#[1] 1 4 3 4 5 6 7 1 2 3 3 2 6 2

