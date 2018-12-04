library(MASS)
library(ggplot2)
data<-diamonds
summary(data[c("price","carat")])
plot(data$carat,data$price)

##################################################################################
  #problem 1
##################################################################################

test_error<-c()
sample_size<- c(10,20,50,70,100,200,350)
for (i in sample_size){
  rand1 = sample(1:nrow(data),2*i)
  data1 = data[rand1, c("carat","price") ]
  setwd("F:\\R")
  set.seed(40)
  rand = sample(1:nrow(data1),i)
  train = data1[rand, c("carat","price") ]
  test = data1[-rand, c("carat","price") ]
  #MOdel of Order 7
  
    
  m1 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5) 
             + I(carat^6) + I(carat^7), train)
  
  #PLOTTING THE MODEL OVER THE DATA
  graph_name <- "polynomial of degree 7"
  additional<- ", sample size "
  format<-".jpg"
  name<-paste(graph_name,additional,i,format)
  jpeg(name)
  
  plot(train$carat,train$price, pch=19, cex=0.5,main = paste(graph_name,additional,i),xlab = "CARAT",ylab = "PRICE" )
  lines(sort(train$carat), fitted(m1)[order(train$carat)], col='red', type='l',pch=20) 
  
  dev.off()
  
  #TRAIN AND TEST ACCURACY
  sum(m1$residuals^2)
  pred = predict(m1, newdata=test)
  error_test<-sum((pred-test$price)^2)
  test_error<-append(test_error,error_test)
}
setwd("F:\\R")
jpeg("Error Vs Sample.jpg")
plot(sample_size,test_error, pch=19, cex=0.5,main = paste("Error Vs Sample Size, ","Polynomial of order 7"),xlab = "Sample Size",ylab = "Test Error" )
lines(sample_size,test_error, col='red', type='l',pch=20)
dev.off()



##################################################################################
  #problem 2
##################################################################################

#Deviding the data into 4 parts and saving in a vector
complexity<-c(1,2,7,8,9,10)
data_parts = c("data_q1","data_q2","data_q3","data_q4")
sample_size<-c(20,100)
rand = sample(1:nrow(data),nrow(data)/2)
data_h1 = data[rand, c("carat","price") ]
data_h2 = data[-rand, c("carat","price") ]

rand1 = sample(1:nrow(data_h1),nrow(data_h1)/2)
rand2 = sample(1:nrow(data_h2),nrow(data_h2)/2)

data_q1 = data_h1[rand1, c("carat","price") ]
data_q2 = data_h1[-rand1, c("carat","price") ]
data_q3 = data_h2[rand2, c("carat","price") ]
data_q4 = data_h2[-rand2, c("carat","price") ]

assign(data_parts[1],data_q1)
assign(data_parts[2],data_q2)
assign(data_parts[3],data_q3)
assign(data_parts[4],data_q4)

#Color for line graph
color=c("red","NA","NA","NA","NA","Black","Green","Grey","Blue","Magenta")
TrError20<-c()
TrError100<-c()
RMSE20<-c()
RMSE100<-c()  


for (samplesz in sample_size){
  #Accesing each of the four parts for model
  for (dataname in data_parts){
  set.seed(samplesz)
  rand0 = sample(1:nrow(get(dataname)),2*samplesz)
  datanw = get(dataname)[rand0, c("carat","price") ]
  randnw = sample(1:nrow(datanw),samplesz)
  #for every part taking sample of pre defined sie in train and test using random numbers
  train = datanw[randnw, c("carat","price") ]
  test = datanw[-randnw, c("carat","price") ]
  
  
  graph_name1 <- "polynomial of degree (1,2,7,8,9,10)"
  additional1<- ", sample size "
  format<-".jpg"
  
  jpeg(paste(graph_name1,additional1,samplesz,", ",dataname,format))
  plot(train$carat,train$price, pch=19, cex=0.5,
  main = paste("Sample size ",samplesz,", polynomial of orders (1,2,7,8,9,10)"),
  xlab = "CARAT",ylab = "PRICE" )
  model = "price~carat"
  poly_mod0<-lm(as.formula(model),train)

  #TRAIN AND TEST ACCURACY
  tr_er<-sum(poly_mod0$residuals^2)
  pred = predict(poly_mod0, newdata=test)
  error_test<-sum((pred-test$price)^2)
  rmse<-sqrt(error_test/samplesz)
  
  #Appending train and test error in a vector
  if (samplesz == 20){
  TrError20<-append(TrError20,tr_er)
  RMSE20<-append(RMSE20,rmse)
  } else if (samplesz == 100) {
  TrError100<-append(TrError100,tr_er)
  RMSE100<-append(RMSE100,rmse)
  }

  lines(sort(train$carat), fitted(poly_mod0)[order(train$carat)],
        col='red', type='l',pch=20)
  
  #building Polynomial model of orders 1,2,7,8,9,10 using loops
  #and graph plott for each of the model
  for (i in c(2,7:10)){
    model1=""
  for (j in c(2:i)){
    model1<-paste(model1," + I(carat^",j,")",sep = "")
  }
  x <- paste(model,model1,sep = "")
  poly_mod<-lm(as.formula(x),train)
  
  #TRAIN AND TEST ACCURACY
  tr_er<-sum(poly_mod$residuals^2)
  pred = predict(poly_mod, newdata=test)
  error_test<-sum((pred-test$price)^2)
  rmse<-sqrt(error_test/samplesz)
  
  #Appending train and test error in a vector
  if (samplesz == 20){
  TrError20<-append(TrError20,tr_er)
  RMSE20<-append(RMSE20,rmse)
  } else if (samplesz == 100) {
  TrError100<-append(TrError100,tr_er)
  RMSE100<-append(RMSE100,rmse)
  }
  
  lines(sort(train$carat), fitted(poly_mod)[order(train$carat)],
        col = color[i], type='l',pch=20)
  }
  legend("topleft", legend=c("Order 1","Order 2","Order 7","Order 8","Order 9","Order 10"),
       col=c("red","Black","Green","Grey","Blue","Magenta"),lty =c(1,2,7:10),cex = 0.7 )
  dev.off()
  }
  
}

########################################################
  #Plotting Training and Test Error
########################################################

  jpeg("RMSE VS complexity, sample size 20.jpg")
  plot(complexity,RMSE20[1:6], pch=19, cex=0.5,
  main = paste("RMSE VS complexity, sample 20", ", polynomials(1,2,7,8,9,10)"),
  xlab = "Order of polynomial",ylab = "RMSE" )
  lines(complexity,RMSE20[1:6], col='red', type='l',pch=20)
  dev.off()

  jpeg("RMSE VS complexity, sample size 100.jpg")
  plot(complexity,RMSE100[1:6], pch=19, cex=0.5,
  main = paste("RMSE VS complexity, sample 100", ", polynomials(1,2,7,8,9,10)"),
  xlab = "Order of polynomial",ylab = "RMSE" )
  lines(complexity,RMSE100[1:6], col='red', type='l',pch=20)
  dev.off()
  
  jpeg("RSS VS model complexity, sample size 20.jpg")
  plot(complexity,TrError20[1:6], pch=19, cex=0.5,
  main = paste("RSS VS complexity, sample 20",", polynomials(1,2,7,8,9,10)"),
  xlab = "Order of polynomial",ylab = "RSS" )
  lines(complexity,TrError20[1:6], col='red', type='l',pch=20)
  dev.off()
  
  jpeg("RSS VS model complexity, sample size 100.jpg")
  plot(complexity,TrError100[1:6], pch=19, cex=0.5,
  main = paste("RSS VS complexity, sample 100",", polynomials(1,2,7,8,9,10)"),
  xlab = "Order of polynomial",ylab = "RSS" )
  lines(complexity,TrError100[1:6], col='red', type='l',pch=20)
  dev.off()

