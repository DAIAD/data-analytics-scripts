require(sgd)
require(MASS)
require(cpm)

make_all_dataset_truth = function(user,end=360){
  X = t(matrix(user$cons[1:(end*24)],nrow=24))
  d = make_dates(as.factor(weekdays(user$dates)[1:end]))
  y = rowSums(t(matrix(user$cons[25:((end+1)*24)],nrow=24)))
  X = cbind(1,X,d)
  return(list(X,y))
}

make_dates = function(dates){
  date_matrix = matrix(0,length(dates),7)
  for(i in 1:length(dates))
    date_matrix[i,dates[i]]=1
  return(date_matrix)
}

make_all_users_dataset = function(users_in){
  users_out = list()
  for(user in users_in){
    buf = make_all_dataset_truth(user)
    users_out[[length(users_out)+1]] = list()
    users_out[[length(users_out)]]$X = buf[[1]]
    users_out[[length(users_out)]]$y = buf[[2]]
  }
  return(users_out)
}

initGradParam = function(X,y,i1,i2,lambda){
  
  X = X[i1:i2,]
  y = y[i1:i2] 
  m = ncol(X)
  w = 1:m*0
  
  #A,B,C,N,w,lambda,D...
  params = matrix(0,6+m,m)
  params[1, ] = colSums( as.vector(X%*%w)*X )
  params[2, ] = colSums( y*X )
  params[3, ] = 2*lambda*w
  params[3,1] = 0
  params[4, ] = nrow(X)
  params[5, ] = w
  params[6, ] = lambda
  
  for(i in 1:m)
    params[6+i, ] = colSums(X*X[,i])
  
  return(params)
  
}

getGrad = function(params){
  return( 2 * (params[1,]-params[2,])/params[4,] + params[3,] )
}

updateGradX = function(params,X,y,i1,i2){
  
  m = ncol(X)
  
  X = X[i1:i2,]
  y = y[i1:i2] 
  
  params[1,] = params[1,] + colSums(as.vector(X%*%params[5,])*X)
  params[2,] = params[2,] + colSums(X*y)
  params[4,] = params[4,] + nrow(X)
  
  for(i in 1:m)
    params[6+i,] = params[6+i,] + colSums(X*X[,i])
  
  return(params)
  
}

updateGradW = function(params,w){
  
  delta = w - params[5,]
  buf = 0
  m = length(w)
  
  for(j in 1:m)
    params[1,j] = params[1,j]+delta%*%params[6+j,]
  
  params[3, ] = params[3,]+2*params[6,]*delta
  params[3,1] = 0
  params[5, ] = w
  
  return(params)
  
} 

matrixGrad1 = function(X,y,w,lambda){
  m = ncol(X)
  n = nrow(X)
  grad = c()
  grad[1] = 2/n * sum( ( X %*% w - y ) * X[,1] )
  grad[2:m] = 2/n * colSums( as.vector( X %*% w - y ) * X[,2:m] ) + 2 * lambda * w[2:m]
  return(grad)
}

matrixGrad2 = function(w,X,y,lambda){
  if(is.null(nrow(X)))
    return(as.vector(2*X*(X%*%w-y) + 2*c(0,w[-1])*lambda))
  else
    return(as.vector(2/nrow(X)*t(X)%*%(X%*%w-y) + 2*c(0,w[-1])*lambda))
}

stochGradDesc = function(X,y,w,lam1,k,lam2=0.01,step=0.00001){
  
  grad = matrixGrad2(w,X,y,lam1) 
  step = step/(1+step*lam2*k)**(3/4)
  w = w-step*grad
  
  return(w)
  
}


gradDescParam = function(gradParams,X,y,convThresh=1, maxIter=1000){
  
  w = gradParams[5,]
  lambda = gradParams[6,1]
  beta = 0.01
  ers = c()
  impv = -Inf
  iter = 0
  
  X_val = X[1:50,]
  y_val = y[1:50]
  
  while(abs(impv)>convThresh && iter<maxIter){
  #while(iter<maxIter){
    
    step = 1
    grad = matrixGrad1(X,y,w,lambda) 
  # grad = getGrad(gradParams)
  # plot(grad)
    while( er(X_val,y_val,w-step*grad,lambda) > er(X_val,y_val,w,lambda) - step/2*sqrt(sum(grad**2)) )
      step = step*beta
    
    w = w-step*grad
   # gradParams = updateGradW(gradParams,w)
    
    ers = c(ers,er(X,y,w,lambda))
    
    iter = iter+1
    
    if(iter>=2)
      impv = ers[iter]-ers[iter-1]
   # if(impv>0)
  #    step = step/100
      
    
  }
 # plot(ers)
  return(list(w,gradParams))
  
}

er = function(w,X,y,lambda){
  n = nrow(X)
  m = ncol(X)
  return( 1/n * sum( ( X %*% w - y )**2 ) + lambda /2 * w[2:m] %*% w[2:m] )
}

v_seq_grad = function(X,y,i0,val_size,nmin,step,lambda){
  
  w = NULL
  ws = matrix(0,ncol=ncol(X),nrow=seq(i0-nmin,1,-step))
  index = 0
  errors = c()
  for(i in seq(i0-nmin,1,-step)){
    res = perform_v_grad(X[i:i0,],y[i:i0],val_size,w,lambda,step)
    errors = c(errors,res[[1]])
    w = res[[2]]
    index = index+1
    ws[index,]=w
  }
  return(list(t(matrix(errors,nrow=val_size)),ws))
  
}

perform_v_grad = function(X,y,val_size,w,lam,step_size){
  
  len = nrow(X)
  val_start = len-val_size+1
  X_test = X[val_start:len,]
  y_test = y[val_start:len]
  if(is.null(w)){
    X_train = X[1:val_start-1,]
    y_train = y[1:val_start-1]
    w = rnorm(ncol(X))
  }
  else{
    X_train = X[1:step_size,]
    y_train = y[1:step_size]
  }
  w=stochGradDesc(X_train,y_train,w,lam,length(y)-length(y_test))
  pred = X_test %*% w  
  ers = abs(pred-y_test)
  return(list(ers,w))
  
}

v_seq_mat = function(X,y,i0,val_size,nmin,step,lambda){
  
  errors= c()
  for(i in seq(i0-nmin,1,-step)){
    # print(i)
    res = perform_v_mat(X[i:i0,],y[i:i0],val_size,lambda)
    errors = c(errors,res[[1]])
  }
  return(t(matrix(errors,nrow=val_size)))
  
}

my_ridge = function(X,y,lambda){
  lambda_mat = diag(ncol(X))*lambda
  lambda_mat[1,1] = 0
  return(solve(t(X)%*%X+lambda_mat)%*%t(X)%*%y)
}

perform_v_mat = function(X,y,val_size,lambda){
  
  len = nrow(X)
  val_start = len-val_size+1
  X_train = X[1:val_start-1,]
  y_train = y[1:val_start-1]
  X_test = X[val_start:len,]
  y_test = y[val_start:len]
  
  d_train = data.frame(cbind(X_train,y_train))
  d_test = data.frame(cbind(X_test,y_test))
  #lam  = lambda_cv(x[start:(test_start-1),],y[start:(test_start-1)])
  #mdl  = glmnet(X_train,y_train,lambda =c(0.1),alpha=0.5)
  #mdl = lm(y~.,data=d[start:(test_start-1),])  
  #ae = abs(y_test-predict(mdl,X_test))
  w = my_ridge(X_train,y_train,lambda)
  pred = X_test%*%w
  ers = abs(pred-y_test)
  
  return(list(ers,w))
  
}

clock_methods = function(users,n=100000,m=100,years=5){
  
  X = matrix(rnorm(n*m),nrow=n,ncol=m)
  y = rnorm(n)
 
#  X = c()
#  y = c()
#  for(i in 1:years){
#    X = rbind(X, users[[i]]$X)
#    y = c(y, users[[i]]$y)
#  }
#   n = nrow(X)
     
  start_grad <- Sys.time()
  v_seq_grad(X,y,n,10,20,5)
  
  end_grad <- Sys.time()
  
  start_mat <- Sys.time()
  v_seq_mat(X,y,n,10,20,5)
  end_mat <- Sys.time()
  
  return(c(end_grad-start_grad, end_mat-start_mat))

}

clock_grid = function(){
  
  times_grad = c()
  times_mat = c()
  
  n_vals = (1:20)*500
  m_vals = seq(10,200,10)
  
  for(n in n_vals)
    for(m in m_vals){
      print(c(n,m))
      res = clock_methods(NULL,n,m)
      times_grad = c(times_grad,res[1])
      times_mat = c(times_mat,res[2])
      print(times_grad)
      print(times_mat)
    }
    
  return(list(matrix(times_grad,10,10),matrix(times_mat,10,10)))  
  
}

train_optimal_model_mat = function(X,y,i0,nmin=20,val_size=10,step=5,alpha=0.05,lambda,w){
  
  start = Sys.time()
  ers = v_seq_mat(X,y,i0,val_size,nmin,step,lambda)
  end = Sys.time()
  w_opt = find_optimal_window_length(ers,alpha,w)
  abline(v=w_opt)
  ind_opt = i0-nmin-(w_opt-1)*step
  print(ind_opt)
  X_train = X[ind_opt:i0,]
  y_train = y[ind_opt:i0]
  w = my_ridge(X_train,y_train,lambda)
  return(list(w,difftime(end,start,'sec')))
  
}

train_optimal_model_grad = function(X,y,i0,nmin=20,val_size=10,step=5,alpha=0.05,lambda,w){
  
  start = Sys.time()
  res = v_seq_grad(X,y,i0,val_size,nmin,step,lambda)
  end = Sys.time()
  ers = res[[1]]
  w_opt = find_optimal_window_length(ers,alpha,w)
  abline(v=w_opt)
  #w = res[[2]][w_opt,]

  ind_opt = i0-nmin-(w_opt-1)*step
  print(ind_opt)
  X_train = X[ind_opt:i0,]
  y_train = y[ind_opt:i0]
  w = sgd(y~.,data = data.frame(X=X_train,y=y_train),model='lm',model.control = list(lambda2=lambda),sgd.control= list(npasses=1))$coefficients
  w[2] = w[1]+w[2]
  w = w[-1]
  return(list(w,difftime(end,start,'sec')))
  
}

train_optimal_model_cp = function(X,y,i0,nmin=20,lambda){
    
    start = Sys.time()
    cp = run_cpd(y[1:i0])
    end = Sys.time()
    
    if(length(cp[[1]])<1 )
      ind_opt = 1
    else
      ind_opt = cp[[1]][length(cp[[1]])]
    if(ind_opt>i0-nmin)
      ind_opt = i0-nmin
    
    print(ind_opt)
    X_train = X[ind_opt:i0,]
    y_train = y[ind_opt:i0]
    w = sgd(y~.,data = data.frame(X=X_train,y=y_train),model='lm',model.control = list(lambda2=lambda),sgd.control= list(npasses=1))$coefficients
    w[2] = w[1]+w[2]
    w = w[-1]
    return(list(w,difftime(end,start,'sec')))
    
}


find_optimal_window_length = function(ers,alpha,w){
  
  min = Inf
  index = nrow(ers)
  #index = -1
  vector = rowMeans(ers,na.rm=TRUE)
  plot(vector)
  for(i in 1:(length(vector)-w)){
    med = median(vector[i:(i+w)])
    if(med<min){
      min = med
      index = i + (which(vector[i:(i+w)]==med)[[1]]-1)
    }
  }
  
  min_ers = ers[index,!is.na(ers[index,])]
  null_ers = ers[nrow(ers),!is.na(ers[nrow(ers),])]
  
  if(t.test(min_ers,null_ers)$p.value<alpha && mean(abs(min_ers)) < mean(abs(null_ers)))
    sel_ind = index
  else
    sel_ind = nrow(ers)
  
  #plot(mer)
  #plot(pvals)
  #print(c(sel_ind))
  
  return(sel_ind)
  
}

run_cpd = function(x,ind=0){
  res = processStream(x,cpmType="Mann-Whitney",ARL0=500)
#  plot(x,xlab="Day",ylab="Consumption (Litres)")
#  abline(v=res$changePoints,col='red',lty=2)
  # abline(v=res$detectionTimes,col='red',lty=2)
  # title(ind)
  return(list(res$changePoints,res$detectionTimes,x))
}

get_optimal_forecast = function(X,y,i0,i1=10,nmin=20,val_size=10,step=5,alpha=0.1,lambda=100,w=4){
  
  #X = X[,-1]
  
  X_test = X[(i0+1):(i0+i1),]
  y_test = y[(i0+1):(i0+i1)]
  
  w0 = my_ridge(X[1:i0,],y[1:i0],lambda)
  ypred0 = X_test%*%w0
  er0 = sqrt(mean((y_test-ypred0)**2))
  
  res1 = train_optimal_model_mat(X,y,i0,nmin,val_size,step,alpha,lambda,w)
  w1 = res1[[1]]
  time1 = res1[[2]]
  ypred1 = X_test%*%w1
  er1 = sqrt(mean((y_test-ypred1)**2))
  
  res2 = train_optimal_model_grad(X,y,i0,nmin,val_size,step,alpha,lambda,w)
  w2 = res2[[1]]
  time2 = res2[[2]]
  ypred2 = X_test%*%w2
  er2 = sqrt(mean((y_test-ypred2)**2))
  
  res3 = train_optimal_model_cp(X,y,i0,nmin,lambda)
  w3 = res3[[1]]
  time3 = res3[[2]]
  ypred3 = X_test%*%w3
  er3 = sqrt(mean((y_test-ypred3)**2))
  
  print(c(er0,er1,er2,er3,time1,time2,time3))
  
  return(c(er0,er1,er2,er3,time1,time2,time3))
  
}

run_experiments = function(users){
  
  res = c()

  #for(n1 in c(1000,1500,2000))
  n1 = 4000
     for(us in users[1:200]){
      user = make_synthetic_user(us,n1,1051,0,2) 
      buf = try({get_optimal_forecast(user$X,user$y,n1+1000,i1=50,nmin=50,val_size=25,step=1,alpha=0.05,lambda=10,w=10)})
      if(is.numeric(buf)){
         res = c(res,buf)
         print(t(matrix(res,nrow=7)))
         print(colMeans(t(matrix(res,nrow=7))))
         print(length(res))
      }
    }
  
  return(t(matrix(res,nrow=7)))

}

make_synthetic_user = function(user,n1,n2,a,b){
  
  X = user$X
  y = user$y
  
  mu = colMeans(X)
  X_cov = cov(X)
  X_sam1 = mvrnorm(n1,mu[1:24],X_cov[1:24,1:24])
  X_sam2 = mvrnorm(n2,mu[1:24],X_cov[1:24,1:24])
  
  days = c()
  for(i in 1:ceiling((n1+n2)/7))
    days = rbind(days,diag(7))
  
  X_sam1 = cbind(1,X_sam1,days[1:nrow(X_sam1),])
  X_sam2 = cbind(1,X_sam2,days[1:nrow(X_sam2),])
  
  w1 = my_ridge(X,y,10**-5)
  er_sig = sd(y-X%*%w1)
  w2 = w1 + (rnorm(length(w))*a+b)
  
  y_sam1 = X_sam1%*%w1 + rnorm(n1)*er_sig
  y_sam2 = X_sam2%*%w2 + rnorm(n2)*er_sig
  
  X_sam = rbind(X_sam1,X_sam2)
  y_sam = c(y_sam1,y_sam2)
  
  return(list(X=X_sam,y=y_sam))
  
}

