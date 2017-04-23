require(glmnet)
source('~/temp.R')

make_all_dataset_truth = function(user,hour,end=360){
  x = t(matrix(user$cons[1:(end*24)],nrow=24))
  d = make_dates(as.factor(weekdays(user$dates)[1:end]))
  y = rowSums(t(matrix(user$cons[25:((end+1)*24)],nrow=24)))
  f = data.frame(cbind(x,d,y))
  #f = cbind(x,y)
  colnames(f) = c(make.names(1:31),"y")
  return(f)
}

make_dates = function(dates){
  date_matrix = matrix(0,length(dates),7)
  for(i in 1:length(dates))
     date_matrix[i,dates[i]]=1
  return(date_matrix)
}

train_test_24 = function(d,start,end,test_points=15,perm=FALSE){
  if(perm!=FALSE)
    d = d[perm,]
  x = d[,1:24]
  y = d[,25] 
  mses = c()
  test_start = end-test_points
  lam  = cv.glmnet(x[start:(test_start-1),],y[start:(test_start-1)],nfolds=4)$lambda.min
  mdl  = glmnet(x[start:(test_start-1),],y[start:(test_start-1)],lambda =c(lam))
  #mdl = lm(y~.,data=d[start:(test_start-1),])  
  er = sqrt(mean( (y[test_start:end]-predict(mdl,x[test_start:end,]))**2 ))
  return(er)
}

seq_tt_24_d = function(d,t0,min_size,test_size,perm=FALSE){
  ers = c()
  points = seq(t0-min_size,1,-1)
  for(i in points){
    ers = c(ers, train_test_24(d,i,t0,test_size,perm)[[1]])
  }
  return(list(ers,points))
}

seq_tt_24 = function(user,h2,t0,min_size,test_size,perm=FALSE){
  d = data.matrix(make_all_dataset_truth(user,h2))
  return(seq_tt_24_d(d,t0,min_size,test_size,perm))
}

get_train_res = function(user,h2,t0){
  d = data.matrix(make_all_dataset_truth(user,h2))
  x = d[,1:31]
  y = d[,32] 
  lam0 = cv.glmnet(x[1:t0,],y[1:t0])$lambda.min
  mdl0 = glmnet(x[1:t0,],y[1:t0],lambda=c(lam0))
  pred0 = predict(mdl0,x[1:t0,])
  res = y[1:t0] - pred0
  return(res)
}

select_start_24 = function(user,h2,t0,min_size=40,test_size=15,thresh=0.05,window=20){
 
 #thresh = sort(test_var(user,h2,t0,min_size-test_size,test_size))[25]
  ers = seq_tt_24(user,h2,t0,min_size,test_size,FALSE)
  fin_er = ers[[1]][length(ers[[1]])]
  min_mid = min_moving_median(ers[[1]],window)
  
 if(((fin_er - min_mid[[2]]) / fin_er  > thresh))
    sel1 = ers[[2]][min_mid[[1]]]
  else
    sel1 = 1

  d1 = get_train_res(user,h2,t0)
  cp = run_cpd(d1)
  
  if(length(cp[[1]])<1 )
    sel2 = 1
  else
    sel2 = cp[[1]][length(cp[[1]])]
  if(t0-sel2<25)
    sel2 = 1
  
  d2 = make_all_dataset_truth(user)[1:t0,h2]
  cp = run_cpd(d2)
  
  if(length(cp[[1]])<1 )
    sel3 = 1
  else
    sel3 = cp[[1]][length(cp[[1]])]
  if(t0-sel3<10)
    sel3 = 1
  
  plot(d1)
  abline(v=sel1,col='red')
  abline(v=sel2,col='green')
  abline(v=sel3,col='blue')

  plot(d2)
  abline(v=sel1,col='red')
  abline(v=sel2,col='green')
  abline(v=sel3,col='blue')

  plot(ers[[2]],ers[[1]])
  abline(v=sel1,col='red')
  abline(v=sel2,col='green')
  abline(v=sel3,col='blue')
  
  return(list(sel1,sel2,sel3))
}

test_var = function(user,h2,t0,s1,s2){
  d = data.matrix(make_all_dataset_truth(user,h2))
  x = d[,1:24]
  y = d[,25] 
  set1 = 1:(t0-s2)
  set2 = (t0-s2+1):t0
  ers = c()
  for(i in 1:500){
    print(i)
    inds1 = block_sample(set1,5,s1)
    lam  = cv.glmnet(x[inds1,],y[inds1],nfolds=4)$lambda.min
    mdl  = glmnet(x[inds1,],y[inds1],lambda =c(lam))
 #   mdl = lm(y~.,data=d[inds1,])
    er = sqrt(mean( (y[set2]-predict(mdl,x[set2,]))**2 ))
    ers = c(ers,er)
  }
  hist(ers,25)
  abline(v=sort(ers)[25],col='red')
  return(ers)
}

block_sample = function(set,block_size,bs_size){
  m=bs_size/block_size
  set = set[1:(length(set)-block_size)]
  sel = c()
  for(i in 1:m){
    sam = sample(set,1)
    sel = c(sel, sam:(sam+(block_size-1)))
  }
  return(sel)
}

compare_methods = function(user,h2,t0,ahead=10){
  d = data.matrix(make_all_dataset_truth(user,h2))
  x = d[,1:31]
  y = d[,32] 
  plot(y)
  abline(v=t0)
  if(any(y>10000)||length(y)>10000)
    return(-1)
  lam0 = cv.glmnet(x[1:t0,],y[1:t0])$lambda.min
  mdl0 = glmnet(x[1:t0,],y[1:t0],lambda=c(lam0))
  pred0 = predict(mdl0,x[(t0+1):(t0+ahead),])
  er0 = sqrt(mean((y[(t0+1):(t0+ahead)] - pred0)**2))

  sel   = select_start_24(user,h2,t0,min_size=40,test_size=15,window=20)
  
  if(sel[[1]]==1)
    lam1=lam0
  else
    lam1  = cv.glmnet(x[sel[[1]]:t0,],y[sel[[1]]:t0])$lambda.min
  mdl1  = glmnet(x[sel[[1]]:t0,],y[sel[[1]]:t0],lambda=c(lam1))
  pred1 = predict(mdl1,x[(t0+1):(t0+ahead),])
  er1   = sqrt(mean((y[(t0+1):(t0+ahead)] - pred1)**2))
  
  if(sel[[2]]==1)
    lam2 = lam0
  else
    lam2 = cv.glmnet(x[sel[[2]]:t0,],y[sel[[2]]:t0])$lambda.min
  mdl2 = glmnet(x[sel[[2]]:t0,],y[sel[[2]]:t0],lambda=c(lam2))
  pred2 = predict(mdl2,x[(t0+1):(t0+ahead),])
  er2 = sqrt(mean((y[(t0+1):(t0+ahead)] - pred2)**2))
  
  if(sel[[3]]==1)
    lam3 = lam0
  else
    lam3 = cv.glmnet(x[sel[[3]]:t0,],y[sel[[3]]:t0])$lambda.min
  mdl3 = glmnet(x[sel[[3]]:t0,],y[sel[[3]]:t0],lambda=c(lam3))
  pred3 = predict(mdl3,x[(t0+1):(t0+ahead),])
  er3 = sqrt(mean((y[(t0+1):(t0+ahead)] - pred3)**2))
  
  pred4 = mean(y[sel[[3]]:t0])
  er4 = sqrt(mean((y[(t0+1):(t0+ahead)] - pred4)**2))
  
  return(c(er0,er1,er2,er3,er4))
}

compare_many = function(users){
  tried = c()
  ers   = c()
  N     = length(users)
  for(u in 1:N){
    for(h in c(5,6,7,8)){
      for( t in c(200,240,280,320)){
        
        part = c()
        #h = sample(6:24,1)
        
        print(paste(u,h,t,sep=" "))
        res = tryCatch(compare_methods(users[[u]],h,t),error=function(err){
          print(err)
          return(-1)
        })
    
        if(res==-1)
        next
    
        tried = c(tried,u,h,t)
        ers   = c(ers,res)
        print(res)
      }
    }
  }
    
  return(list(matrix(ers,nrow=5),matrix(tried,nrow=3)))
}

seq_bootstrap = function(user,h2,t0){
  test_size = 15
  min_size  = 31
  ahead     = 15 
  res = list()
  for(i in 1:100){
    print(i)
    perm = block_sample(1:t0,5,t0)
    res[[i]]=seq_tt_24(user,h2,t0,min_size,test_size,perm)
    plot(res[[i]][[2]],res[[i]][[1]])
  }
  return(res)
}

seq_mc = function(){
  t0=200
  v =100
  x = matrix(runif(t0*24),ncol=24)
  w = rnorm(24)*10+5
  b = rnorm(1)*10
  e = rnorm(t0)*v
  y = x%*%w+b+e
  d = cbind(x,y)
  res = seq_tt_24_d(d,t0,35,30)
  plot(res[[2]],res[[1]])
  return(res)
}

min_moving_median = function(vector,w){
  min = Inf
  index = -1
  for(i in 1:(length(vector)-w)){
    med = median(vector[i:(i+w)])
    if(med<min){
      min = med
      index = i + (which(vector[i:(i+w)]==med))
    }
  }
  return(list(index,min))
}

parse_results = function(res,h2,t0){
  hours = c()
  ids = c()
  for(i in 1:dim(res[[1]])[2]){
    if(res[[2]][2,i]==h2 && res[[2]][3,i]==t0){
      hours = c(hours,res[[1]][,i])
      ids = c(ids,res[[2]][,i] )
    }
  }
  ers = matrix(hours,nrow=5)
  ids = matrix(ids, nrow=3)
  r = c()
  r = c(r,mean((ers[1,]-ers[2,])/ers[1,]))
  r = c(r,mean((ers[1,]-ers[3,])/ers[1,]))
  r = c(r,mean((ers[1,]-ers[4,])/ers[1,]))
  r = c(r,mean((ers[1,]-ers[5,])/ers[1,]))
  
  return(r)
}

parse_all = function(res){
  ers = array(dim=c(6,4,4))
  hours = c(5,6,7,8,9,10)
  t0 = c(200,240,280,320)
  for( i in hours )
    for( j in t0  )
      ers[which(hours==i),which(t0==j),] = parse_results(res,i,j)
  return(ers)  
}

remove_period = function(user,h2,t0,t1,t2,ahead=10){
  dt = data.matrix(make_all_dataset_truth(user,h2))
  d = rbind(dt[1:t1,],dt[t2:t0,])
  x = d[,1:24]
  y = d[,25] 
  if(any(y>200)||length(y)>10000)
    return(-1)
  lam0 = cv.glmnet(x,y)$lambda.min
  mdl0 = glmnet(x,y,lambda=c(lam0))
  pred0 = predict(mdl0,dt[(t0+1):(t0+ahead),1:24])
  er0 = sqrt(mean((dt[(t0+1):(t0+ahead),25] - pred0)**2))
  print(er0)
}

gradients = function(user,h2,t1,t0){
  d = data.matrix(make_all_dataset_truth(user,h2))
  x = d[,1:31]
  y = d[,32] 
  if(any(y>10000)||length(y)>10000)
    return(-1)
  lam0 = cv.glmnet(x[t1:t0,],y[t1:t0])$lambda.min
  mdl0 = glmnet(x[t1:t0,],y[t1:t0],lambda=c(lam0))
  
  grads = matrix(0,t0-t1,32)
  preds = predict(mdl0,x[t1:t0,])
  
  for(i in 1:(t0-t1)){
    for( j in 1:31)
      grads[i,j] = -(preds[i]-y[i])*x[i,j]
    grads[i,32] = -(preds[i]-y[i])
  }
  
  for(i in 1:31){
      plot(grads[,i],sub=i)
      title(paste(mdl0$beta[i],mean(x[t1:t0,i]),sep=" "))
        
  #  points(y**2,col='red')
  }
  plot(grads[,32],sub="a0")
  title(mdl0$a0)
  return(grads)
}


lambda_cv = function(x,y,k=10){
  len       = length(y)
  fold_size =  floor(len/k)
  ers = c()
  lam = glmnet(x,y,nlambda=20)$lambda
  for(i in 0:(k-1)){
    test_ind = (i*fold_size+1):(i*fold_size+fold_size)
    train_x  = x[-test_ind,]
    train_y = y[-test_ind]
    test_x = x[test_ind,]
    test_y = y[test_ind]
    mdl = glmnet(train_x,train_y,lambda=lam)
    pred = predict(mdl,test_x,nlambda=lam)
    ers = c(ers,sqrt(colSums((pred-test_y)**2)/fold_size))
  }
  er_mat = matrix(ers,nrow=length(lam))
  sums = rowSums(er_mat)
  return(mdl$lambda[which(sums==min(sums))])
}

