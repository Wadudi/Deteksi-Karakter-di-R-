
data = read.csv("fonts.csv", header = T)

data$label = as.character(data$label)
uniqlabel = unique(data$label)
head(data)
x = as.matrix( subset(data, select=x1:x63) )
x = x[1:14,]
glabel = c(
  "A", "B", "C", "D", "E", "J", "K"
)
gl = data$label
data[data == "A"] = 1
data[data == "B"] = 2
data[data == "C"] = 3
data[data == "D"] = 4
data[data == "E"] = 5
data[data == "J"] = 6
data[data == "K"] = 7
data$label = as.numeric(data$label)
data$labelreal = gl
y = as.matrix( subset(data, select=label) )
y = y[1:14,]

xtest = as.matrix( subset(data[15:21,], select=x1:x63) )
ytest = as.matrix( subset(data[15:21,], select=label) )


createOneHot = function(labels){
  uniq = unique(labels)
  m1 = matrix(
    rep(-1, length(uniq)*length(labels)),
    nrow=length(labels),
    byrow=T
  )
  colnames(m1) = uniq
  
  for(i in seq(1:nrow(m1))){
    for(e in seq(1:length(uniq))){
      if(labels[i] == e){
        m1[i, e] = 1
      }
    }
  }
  m1
}

transformOneHot = function(oh, label){
  r = c()
  for(i in seq(1:nrow(oh))){
    p = ""
    for(j in seq(1:ncol(oh))){
      if(oh[i,j] == 1){
        p = label[j]
        r = append(r, p)
        break
      }
    }
    if(j == ncol(oh) && p == ""){
      r = append(r, "")
    }
  }
  r
}

createNN = function(x, y, class=3, oh=F, label=c()){
  rand_vector = runif(ncol(x) * class)
  # rand_vector = rep(0, ncol(x) * class)
  # print(rand_vector)
  rand_matrix = matrix(
    rand_vector,
    nrow = class,
    byrow = T
  )
  
  w2 = matrix(
    runif(class*class), 
    ncol = class,
    byrow=T
  )
  nn = list(
    alpha = 0.1,
    input = x,
    label = label,
    weights1 = rand_matrix,
    bias1 = 1.0,
    weights2 = w2,
    bias2 = 1.0,
    y = y,
    output = matrix(
      rep(0, times=length(y)),
      ncol = 1
    ),
    test=F,
    debug=F,
    epoch=0
  )
  
  if(oh){
    nn$isOh = T
    nn$oh = createOneHot(y)
  }
  
  nn
}

# Loss Function
loss_function <- function(nn) {
  sum((nn$y - nn$output) ^ 2)
}

sigmoid = function(x){
  1.0 / (1.0 + exp(-x))
}


sigmoid_derivative <- function(x) {
  sigmoid(x) * (1.0 - sigmoid(x))
}

act1 = function(x){
  ifelse(x>0,1,-1)
}

activate = function(x, theta=1){
  if(is.vector(x) || is.matrix(x)){
    x[x > theta] = 1
    x[x >=0 && x <= theta] = 0
    x[x <= theta] = -1
    x
  }else{
    if (x > theta){
      1
    }else if (x >= 0 && x <= theta){
      0
    }else if (x <= -theta){
      -1
    }
  }
}

softmax = function(x){
  expx = exp(x)
  expx / sum(exp(x))
}



feedforward = function(nn){
  zin = (nn$input %*% t(nn$weights1))
  nn$layer1 = sigmoid(zin+nn$bias1)
  yin = (nn$layer1 %*% nn$weights2)
  nn$output = act1(yin+nn$bias2)
  if(nn$test){
    hr("nn$output")
    nn$output = nn$output[0:nrow(nn$x), ]
  }
  
  nn
}

backprop <- function(nn) {
  alpha = nn$alpha
  cost = nn$oh - nn$output
  doy = ((cost) * sigmoid_derivative(nn$output))
  d_weights2 <- (
    t(nn$layer1)
    %*%
      doy
  )
  d_weights1 <- (
    t(nn$input) %*% (
      ((cost) * sigmoid_derivative(nn$output))
      %*%
        t(nn$weights2)* sigmoid_derivative(nn$layer1)
    )
  )
  
  nn$weights1 <- nn$weights1 + alpha*t(d_weights1)
  nn$weights2 <- nn$weights2 + alpha*d_weights2
  
  dbg = debug
  if(dbg){
    print(paste("epoch ====== ", nn$epoch))
    print(nn$weights1)
    print(nn$weights1)
  }
  
  nn
}

train = function(nn, n=1500, learnType="fb", backprop=T){
  nn$loss_df <- data.frame(
    iteration = 1:n,
    loss = vector("numeric", length = n)
  )
  for (i in 1:n) {
    progress("e", i, n/10)
    if(learnType=="fb"){
      
      nn <- feedforward(nn)
      if(nn$test){
        backprop = F
      }
      if(backprop){
        nn <- backprop(nn)
        
      }
      
    }
    
    tryCatch({
      nn$loss_df$loss[i] <- loss_function(nn)
    }, warning = function(warning_condition) {
      # print("w")
    }, error = function(error_condition) {
      # print("e")
    }, finally={
    })
    nn$epoch = i
  }
  
  nn
}

epoch = 1



doFnnBnn = function(epoch=1, bp=T, baseNN=NULL, x=NULL, y=NULL){
  if(is.null(baseNN)){
    baseNN = createNN(x, y, 7, oh=T)
  }
  nnObj = train(baseNN, epoch, "fb", bp)
  
  library(ggplot2)
  ggplot(data = nnObj$loss_df, aes(x = iteration, y = loss)) + geom_line()
  
  nnObj
}

dotrain = function(t, nn=NULL, epoch=100){
  dowhat=t
  # epoch=100
  print(t)
  if(dowhat=="bnn"){
    nn = doFnnBnn(epoch, T, nn, x, y)
  }
  print(paste("result",t))
  print(nn$output)
  nn
}

accuracy = function(a1, a2){
  arr = a1 == a2
  sum(arr)/length(arr)
}

epoch = 100

gtruth = data[15:21,]$labelreal

b1 = dotrain("bnn", epoch=100)
hr("test bnn")
b1$output
b1$x = xtest
b1$y = ytest
b1$test = T
b1$output = NULL
b1test = dotrain("bnn", b1, epoch=1)
print(b1test$output)

b1result = transformOneHot(b1test$output, gtruth)
print(b1result)
print(data[15:21,]$labelreal)
value = accuracy(b1result, data[15:21,]$labelreal)
print(paste("Akurasi", value*100, "%"))


# "predict"
#1  2  3  4  5  6  7
#1  1 -1 -1 -1 -1 -1 -1
#2 -1  1 -1 -1 -1 -1 -1
#3 -1 -1  1 -1 -1 -1 -1
#4 -1 -1 -1  1 -1 -1 -1
#5 -1 -1 -1 -1  1 -1 -1
#6 -1 -1 -1 -1 -1  1 -1
#7 -1 -1 -1 -1 -1 -1  1
#[1] "actual"
#1  2  3  4  5  6  7
#[1,]  1 -1 -1 -1 -1 -1 -1
#[2,] -1  1 -1 -1 -1 -1 -1
#[3,] -1 -1  1 -1 -1 -1 -1
#[4,] -1 -1 -1  1 -1 -1 -1
#[5,] -1 -1 -1 -1  1 -1 -1
#[6,] -1 -1 -1 -1 -1  1 -1
#[7,] -1 -1 -1 -1 -1 -1  1
#[8,]  1 -1 -1 -1 -1 -1 -1
#[9,] -1  1 -1 -1 -1 -1 -1
#[10,] -1 -1  1 -1 -1 -1 -1
#[11,] -1 -1 -1  1 -1 -1 -1
#[12,] -1 -1 -1 -1  1 -1 -1
#[13,] -1 -1 -1 -1 -1  1 -1
#[14,] -1 -1 -1 -1 -1 -1  1
#[1] "result"
#1  2  3  4  5  6  7
#1  1 -1 -1 -1 -1 -1 -1
#2 -1  1 -1 -1 -1 -1 -1
#3 -1 -1  1 -1 -1 -1 -1
#4 -1 -1 -1  1 -1 -1 -1
#5 -1 -1 -1 -1  1 -1 -1
#6 -1 -1 -1 -1 -1  1 -1
#7 -1 -1 -1 -1 -1 -1  1
#> print(b1test$output)
#1  2  3  4  5  6  7
#1  1 -1 -1 -1 -1 -1 -1
#2 -1  1 -1 -1 -1 -1 -1
#3 -1 -1  1 -1 -1 -1 -1
#4 -1 -1 -1  1 -1 -1 -1
#5 -1 -1 -1 -1  1 -1 -1
#6 -1 -1 -1 -1 -1  1 -1
#7 -1 -1 -1 -1 -1 -1  1
#> 
#> print(b1result)
#[1] "a" "b" "c" "d" "e" "j" "k"
#> print(data[15:21,]$labelreal)
#[1] "a" "b" "c" "d" "e" "j" "k"
#> value = accuracy(b1result, data[15:21,]$labelreal)
#> print(paste("Akurasi", value*100, "%"))
#[1] "Akurasi 100 %"
