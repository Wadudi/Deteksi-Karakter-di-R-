
data = read.csv("fonts.csv", header = T)

data$label = as.character(data$label)
uniqlabel = unique(data$label)
head(data)
x = as.matrix( subset(data, select=x1:x63) )
x = x[1:14,]
ylabel = c("a", "b", "c", "d","e", "j", "k")

gl = data$label
data[data == "a"] = 1
data[data == "b"] = 2
data[data == "c"] = 3
data[data == "d"] = 4
data[data == "e"] = 5
data[data == "j"] = 6
data[data == "k"] = 7
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
createPerceptron = function(x,y, class=3){
  nn = createNN(x, y, label=uniqlabel, oh=T)
  
  nn$weights1 = matrix(
    runif(class * ncol(nn$input)), 
    nrow = class,
    byrow = T
  )
  
  nn$output = rep(0,7)
  nn
}

loss_function <- function(nn) {
  sum((nn$y - nn$output) ^ 2)
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

perceptron = function(nn){
  alfa = 0.1
  theta = 0.5
  
  nn$input = activate(nn$input)
  yin = nn$weights1 %*% t(nn$input) + nn$bias1
  output = act1(yin)
  nn$output = output
  print(nn$output)
  
  for(o in seq(1:ncol(nn$output))){
    print(output[,o][nn$y[o]])
    out = ifelse(is.na(nn$y[o]), -1, 0)
  }
  
  nn
}

train = function(nn, n=1500, learnType="fb", backprop=T){
  nn$loss_df <- data.frame(
    iteration = 1:n,
    loss = vector("numeric", length = n)
  )
  # Lakukan pelatihan sebanya n iterasi kemudian simpan nilai loss
  for (i in 1:n) {
    progress("e", i, n/10)
    if(learnType=="perceptron"){
      nn = perceptron(nn)
    }
    tryCatch({
      nn$loss_df$loss[i] <- loss_function(nn)
    }, warning = function(warning_condition) {
    }, error = function(error_condition) {
    }, finally={
    })
    nn$epoch = i
  }
  
  nn
}

epoch = 1


doPerceptron = function(epoch=1, baseNN=NULL, x=NULL, y=NULL){
  if(is.null(baseNN)){
    baseNN = createPerceptron(x, y, 7)
  }
  idx = 2
  bw1 = baseNN$weights1
  nnObj = train(baseNN, epoch, "perceptron")
  bw2 = nnObj$weights1
  logs(nnObj$output)
  logs(bw1 == bw2)
  
  nnObj
}

dotrain = function(t, nn=NULL, epoch=100){
  dowhat=t
  # epoch=100
  print(t)
  if(dowhat=="perceptron"){
    nn = doPerceptron(epoch, nn, x, y)
  }else{
    print("nothing")
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

p1 = dotrain("perceptron", epoch=100)
p1$test = T
p1$x = xtest
p1$y = ytest
p1test = dotrain("perceptron", p1, epoch=1)
p1result = transformOneHot(p1test$output, ylabel)

gtruth = data[15:21,]$labelreal

value = accuracy(p1result, data[15:21,]$labelreal)
print(paste("Akurasi ", value*100, "%"))