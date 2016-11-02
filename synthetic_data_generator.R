generate_many_synthetic = function(num=100){
  synthetic_data = list()
  for(i in 1:num)
    synthetic_data[[i]] = synthetic2()
  save(synthetic_data,file='synthetic_data.RData')
}

single_synthetic = function(){
  
  x  = matrix(runif(100*350)*10,ncol=100)
  w  = runif(100)*5
  y  = x%*%w+rnorm(350)
  
  w1 = w + rnorm(100)*sqrt(2)
  w2 = w + rnorm(100)*sqrt(0.5)
  
  y[1:80]    = x[1:80,]%*%w1
  y[81:179]  = x[81:179,]%*%w2
  y[180:200] = x[180:200,]%*%w2
  
  return(cbind(x,y))
}