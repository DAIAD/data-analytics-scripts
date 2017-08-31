means=c(50,80,8,56,25)
stds=c(20,5,1,10,7)
b=c(10,5)

time_exp = read.table('/home/pant/Desktop/disaggregation/time_exp',header=TRUE)
time_exp = as.matrix(time_exp/rowSums(time_exp))

val_exp = as.matrix(read.table('/home/pant/Desktop/disaggregation/val_exp',header=TRUE))

vals_from_survey = function(user,surv){
  us_surv = surv[surv$SWM.ID==user$id,]
  val_exp = matrix(0,5,11)
  val_exp[1,] = make_val_distr(us_surv$Number.of.showers.taken.in.your.household.per.week./7,1.5)
  val_exp[2,] = make_val_distr(us_surv$Number.of.baths.taken.in.your.household.per.month./30,1.5)
  val_exp[3,] = make_val_distr(us_surv$Toilet.flushes.in.your.household.per.day.,2)
  val_exp[4,] = make_val_distr(us_surv$How.many.times.is.the.washing.machine.used.per.week./7,0.8)
  val_exp[5,] = make_val_distr(us_surv$How.many.times.is.the.dishwasher.used.per.week./7,0.8)
  return(val_exp)
}

make_val_distr = function(val,sigm=2){
  row = c()
  for(i in 0:10)
    row[i+1]=exp(-(i-val)**2/(2*sigm**2))
  return(row/sum(row))
}

add_all_noise = function(r=0.2){
  
  means=c(50,80,8,56,25)
  stds=c(20,5,1,10,7)
  b=c(10,5)
  
  time_exp = read.table('/home/pant/Desktop/disaggregation/time_exp',header=TRUE)
  time_exp = as.matrix(time_exp/rowSums(time_exp))
  val_exp = as.matrix(read.table('/home/pant/Desktop/disaggregation/val_exp',header=TRUE))
  
  means<<-add_noise_vert(means,r)
  stds<<-add_noise_vert(stds,r)
  b<<-add_noise_vert(b,r)
  fix_vert_noise()
   
  #time_exp <<- matrix(1/24,5,24)
  #val_exp <<- matrix(1/8,5,8)
  val_exp<<-add_noise_hor(val_exp,r)
  time_exp<<-add_noise_hor(time_exp,r)
  
}

fix_vert_noise = function(){
  for(i in 1:length(stds)){
    if(stds[i]<=0)
      stds[i]=3
    if(means[i]-2*stds[i]<0)
      means[i] = 2*stds[i]
  }
  if(b[2]<0)
    b[2]=3
  if(b[1]-2*b[2]<0)
    b[1] = 2*b[2]
}

add_noise_vert = function(vals,r){
    
  for(i in 1:length(vals))
    vals[i] = vals[i] + rnorm(1)*r*vals[i]
  
  return(vals)
  
}

add_noise_hor = function(vals,r){
  
  for(i in 1:dim(vals)[1])
    for(j in 1:dim(vals)[2])
      vals[i,j] = vals[i,j] + rnorm(1)*r*(vals[i,j]+0.01)
  
  for(i in 1:dim(vals)[1]){
    if(min(vals[i,])<0)
      vals[i,] = vals[i,] - min(vals[i,])
    vals[i,] = vals[i,]/sum(vals[i,])  
  }
  
  return(vals)
  
}

cond_like = function(cons,vals,means,stds,b,loglike=FALSE){
  std_sum = sqrt( (sum(vals*stds)+b[2])**2 )
  mean_sum = sum(vals*means)+b[1]
  return(dnorm((cons-mean_sum)/std_sum,log=loglike))
}

prior = function(vals,time_occ,val_exp,time_exp,prevs){
  pts = rowSums(time_exp[,time_occ,drop=FALSE])
  prob=1
   for(i in 1:length(vals))
     prob=prob*single_dim_prior(vals[i],val_exp[i,],prevs[i],pts[i])
   return(prob)
}

vi_cond_vtvp = function(val,val_tot,val_prev,pt){
  if(val_tot<val_prev)
    return(0)
  else
    return(dbinom(val,val_tot-val_prev,pt))
}

single_dim_prior = function(val,tot_val_probs,val_prev,time_prob){
  prob = 0
  for(i in val_prev:(length(tot_val_probs)-1))
    prob=prob+v_cond_vp(i,tot_val_probs,val_prev)*vi_cond_vtvp(val,i,val_prev,time_prob)
  return(prob)
}

v_cond_vp = function(i,all_v_probs,vp){
  norm = sum(all_v_probs[(vp+1):length(all_v_probs)])
  return(all_v_probs[i+1]/norm)
}

norm_factor = function(cons,all_events,means,stds,b,time,val_exp,time_exp,prevs){
  sum=0
  for(i in 1:dim(all_events)[1])
    sum = sum+cond_like(cons,all_events[i,],means,stds,b)*prior(all_events[i,],time,val_exp,time_exp,prevs)
  return(sum)
}

get_prevs = function(data,i,n){
  if (i==1)
    return(1:n*0)
  else{
    event_sum=0
    for(j in 1:(i-1))
      event_sum=event_sum+data[[j]]$event
    }
  return(event_sum)
}

argmax = function(all_events,cons,time,means,stds,b,val_exp,time_exp,prevs=1:dim(all_events)[2]*0){
  probs = c()
  norm = 0
  for(i in 1:dim(all_events)[1]){
    num = cond_like(cons,all_events[i,],means,stds,b)*prior(all_events[i,],time,val_exp,time_exp,prevs)
    probs = c(probs,num)
    norm = norm+num
  }
  probs = probs / norm
  return(all_events[which(probs==max(probs))[1],])
}

greedy_opt = function(day,all_events,means,stds,b,val_exp,time_exp){
  n=5
  for(i in 1:length(day)){
    day[[i]]$events = argmax(all_events,day[[i]]$cons,day[[i]]$time,means,stds,b,val_exp,time_exp,get_prevs(day,i,n))
  }
  return(day)
}

disaggregate_user = function(user,all_events,means,stds,b,val_exp,time_exp){
  days = user$days
  for(i in 1:length(days))
    days[[i]]=greedy_opt(days[[i]],all_events,means,stds,b,val_exp,time_exp)
  user$days=days
  return(user)
}

calc_probs = function(day,time_exp){
  probs = c()
  for(event in day){
    probs = c(probs, sum(time_exp[event$time]))
  }
  probs = c(probs,1-sum(probs))
  return(probs)
}

all_probs = function(all_events,cons,time,means,stds,b,val_exp,time_exp,prevs=1:dim(all_events)[2]*0){
  probs = c()
  norm = 0
  for(i in 1:dim(all_events)[1]){
    num = cond_like(cons,all_events[i,],means,stds,b)*prior(all_events[i,],time,val_exp,time_exp,prevs)
    probs = c(probs,num)
    norm = norm+num
  }
  probs = probs / norm
  return(all_events[which(probs==max(probs))[1],])
}

joint_like=function(x,day,means,stds,b,val_exp,time_exp){
  like = 0
  for(j in 1:length(day))
    like = like + cond_like(day[[j]]$cons,x[,j],means,stds,b,log=TRUE) 
  
  for(i in 1:dim(x)[1]){
    vdi = sum(x[i,])
    if(vdi<=10)
      like = like + log(val_exp[i,vdi+1])
    else
      like = like + log(0)
    pt = calc_probs(day,time_exp)
    like = like + dmultinom(x=c(x[i,],0),prob=pt,log=TRUE)
  }
  return(like)
}

prune_cond_like = function(thresh,cons,all_events,means,stds,b){
  good = list()
  li = 1
  for(i in 1:dim(all_events)[1]){
    like = cond_like(cons,all_events[i,],means,stds,b,log=TRUE)
    if(like>thresh){
      good[[li]] = all_events[i,]
      li = li+1
    }
  }
  return(good)
}

put_events = function(x,day){
  for(j in 1:length(day))
    day[[j]]$events = x[,j]
  return(day)
}

find_thresh = function(day,means,stds,b,val_exp,time_exp){
  x = c()
  for(e in day)
    x = cbind(x,e$events)
  return(joint_like(x,day,means,stds,b,val_exp,time_exp))
}

events_too_many = function(good){
  prod = 1
  for(g in good)
    prod=prod*length(g)
  if(prod>10000)
    return(TRUE)
  else
    return(FALSE)
}

day_events_to_x = function(day){
  x = c()
  for(e in day)
    x = cbind(x,e$events)
  return(x)
}

prune_search_day = function(day,all_events,means,stds,b, thresh){
  
  good = list()
  for(i in 1:length(day))
    good[[i]] = prune_cond_like(thresh, day[[i]]$cons,all_events , means ,stds, b )
  if(events_too_many(good))
    return(list(day_events_to_x(day),thresh,0))
  pruned_events = as.matrix(expand.grid(good))
  gain = 1-dim(pruned_events)[1]/(48**length(day))
  max_like = -Inf
  max_event = 0
  for(i in 1:dim(pruned_events)[1]){
    x = matrix(unlist(pruned_events[i,]),nrow=5)
    like = joint_like(x ,day,means ,stds,b,val_exp,time_exp)
    if (like>max_like){
      max_like = like 
      max_event = x
    }
  }
  return(list(max_event,max_like,gain))
  
}

prune_disaggregate = function(user,all_events,means,stds,b,val_exp,time_exp){
  gains = c()
  i=1
  for(i in 1:length(user$days)){
   # print(i)
    thresh = find_thresh(user$days[[i]],means,stds,b,val_exp,time_exp)
    res = prune_search_day(user$days[[i]],all_events,means,stds,b,thresh)
    user$days[[i]] = put_events(res[[1]],user$days[[i]])
    gains = c(gains,res[[3]])
    i=i+1
  }
  print(mean(gains))
  return(user)
}

get_patterns = function(user,thresh=10){
  events = list()
  e_index = 1
  event_sum = 0
  active_pat = FALSE
  cons = user$cons
  dates = user$dates
  for(i in 2:length(cons)){
    if(cons[i]>=thresh & !active_pat){
      active_pat = TRUE
      pat_start = get_hour(dates[i])
      event_sum = cons[i]
      events[[e_index]] = list()
      events[[e_index]]$start_date = dates[i-1]
      events[[e_index]]$start_index = i
    }
    else if(cons[i]<thresh & active_pat){
      active_pat = FALSE
      pat_end = get_hour(dates[i-1])
      events[[e_index]]$time = unwarp_time(pat_start,pat_end)
      events[[e_index]]$cons = event_sum
      event_sum = 0
      events[[e_index]]$end_date = dates[i-1]
      events[[e_index]]$end_index = i-1
      e_index = e_index + 1
    }
    else if(active_pat)
      event_sum = event_sum + cons[i]
    
  }
  if(active_pat){
    active_pat = FALSE
    pat_end = get_hour(dates[i-1])
    events[[e_index]]$time = unwarp_time(pat_start,pat_end)
    events[[e_index]]$cons = event_sum
    event_sum = 0
    events[[e_index]]$end_date = dates[i-1]
    e_index = e_index + 1
  }
  return(make_days(events))
}

is.weekday = function(date){
  if(date$wday %in% 1:5)
    return(TRUE)
  else
    return(FALSE)
}

get_hour = function(date){
  time = as.numeric(format(date,tz='CET',format='%H'))
  if(time==0)
    time=24
  return(time)
}

pattern_stats  = function(events){
  cons = c()
  for(event in events)
    cons = c(cons, event$cons)
  return(cons)
}

unwarp_time = function(start,end){
  index = start;
  hours = c(start)
  while(index!=end){
    if(index!=24)
      index=index+1
    else
      index=1
    hours=c(hours,index)
  }
  return(hours)
}

make_days = function(patterns){
  days = list()
  days[[1]] = list()
  days[[1]][[1]] = patterns[[1]]
  index = 1
  for(i in 2:length(patterns)){
    if(is_very_large(patterns[[i]]))
      next
    if(is.sameday(patterns[[i]],days[[index]][[1]]) ){
      days[[index]][[length(days[[index]])+1]]=patterns[[i]]
    }
    else{
      index=index+1
      days[[index]] = list(patterns[[i]])
    }
  }
  return(days)
}

is_very_large = function(pat,thresh=8){
  if( difftime(pat$end_date,pat$start_date,units='hours') > thresh)
    return(TRUE)
  else
    return(FALSE)
}

is.sameday = function(pat1,pat2){
  if(as.Date(pat1$start_date,tz='CET')==as.Date(pat2$start_date,tz='CET'))
    return(TRUE)
  else
    return(FALSE)
}

gather_event_stats = function(days){
  
  times = matrix(0,5,24)
  
  for(day in days)
    for(pat in day)
      for(j in pat$time)
        for(i in 1:5){
          if(is.na(pat$events[i]))
            next
          times[i,j] = times[i,j]+pat$events[i]
        }
  
  return(times)

}

extract_showers = function(sh){
  sh = sh[as.Date(sh$Date.Time,tz='CET') < as.Date('2016-06-08',tz='CET'),]
  sh = sh[sh$History=='NO',]
  sh = sh[sh$Volume>10,]
  return(sh)
}

get_shower_time = function(shower){
  end = as.POSIXct(shower$local.datetime,format='%d/%m/%Y %H:%M:%S',tz='CET')
  start = end-shower$duration
  time = list()
  time$start = start
  time$end = end
  return(time)
}

find_shower_loc = function(days,shower){

  for(i in 1:length(days))
    for(j in 1:length(days[[i]]))
      if(is_inside(days[[i]][[j]],shower))
       return(c(i,j))

}

is_inside = function(pat,sh){
  pat_start = pat$start_date
  pat_end = pat$end_date
  sh_time = get_shower_time(sh)
  sh_start = sh_time$start
  sh_end = sh_time$end
  
  if(pat_start<=sh_start & sh_start<=pat_end | pat_start<=sh_end & sh_end<=pat_end)
    return(TRUE)
  else
    return(FALSE)
}

get_shower_locations = function(days,sh){
  sh = sh[sh$history=='false' & sh$volume>10,]
  loc = c()
  for(i in 1:dim(sh)[1]){
   loc = c(loc,find_shower_loc(days,sh[i,]))
  }
  return(loc)
}

pat_stats = function(pats){
  cons = c()
  len = c()
  
  for(pat in pats){
    cons = c(cons, pat$cons)
    len = c(len,pat$len)
  }
  return(list(cons,len))
}

amph_stats = function(amph){
  ids = unique(amph$device.key)
  lot1 = c()
  lot2 = c()
  lot3 = c()
  date_thres = as.Date('2016-06-08')
  for(id in ids){
    dev = amph[amph$device.key==id,]
    have = length(dev$session.id)
    good = sum(dev$volume>10)
    rate = good/have
    all = max(dev$session.id)
    all_good = round(all*rate)
    good_real = sum(dev$history=='false' & dev$volume>10)
    good_real_date = sum(dev$history=='false' & dev$volume>10 & as.Date(dev$local.datetime,format= '%d/%m/%Y %H:%M:%S')<date_thres)
    perc = good_real/all_good
    if(good_real>10){
      lot1 = c(lot1, good_real)
      lot2 = c(lot2, perc)
      lot3 = c(lot3, good_real_date)
    }
  }
  return(list(lot1,lot2,lot3))
}

take_user = function(csv,id){
  user = csv[csv[[1]]==id,]
  user[[2]]=as.POSIXct(user[[2]],format='%d/%m/%Y %H:%M:%S',tz='CET')
  user = user[!duplicated(user[[2]]),]
  user = user[order(user[[2]]),]
  user_out = list()
  user_out$cons = diff(user[[3]])
  user_out$dates = user[[2]][-1]
  user_out$id = id
  return(user_out)
}

make_users=function(csv,id_match,showers,surv){
  users = list()
  ids = surv$SWM.ID
  u_index=1
  for(i in 1:length(ids)){
    print(i)
    user = take_user(csv,trial_ids[i])
    if(length(user$cons)==0)
      next
    user$days = get_patterns(user)
    user$showers = put_showers_on_user(user,id_match,showers,surv)
   if(dim(user$showers)[1]==0 | sum(user$showers$history=='false' & user$showers$volume>10)<10 )
     next
   user$shower_loc = get_shower_locations(user$days,user$showers)
   users[[u_index]]=user
   u_index=u_index+1
  }
  return(users)
}

put_showers_on_user = function(user,id_match,showers,surv){
  mail = surv$Email[surv$SWM.ID==user$id]
  key = id_match$user.key[as.character(id_match$user.name)==as.character(mail)]
  return(showers[as.character(showers$user.key)==as.character(key),])
}

put_days_on_user = function(user){
  user$days = get_patterns(user)
  return(user)
}

recall = function(user,r){
  start = round(length(user$days)*r)
  showers = user$shower_loc
  got = 0
  all = 0
  sh_lengths = c()
  pr_lengths = c()
  for(i in seq(1,length(showers),2)){
    if(showers[i]<start)
      next
    events = user$days[[showers[i]]][[showers[i+1]]]$events
    if(events[1]>0){
      got=got+1
      events[1]=events[1]-1
      user$days[[showers[i]]][[showers[i+1]]]$events = events
      pr_lengths = c(pr_lengths,length(user$days[[showers[i]]][[showers[i+1]]]$time))
    }
    sh_lengths = c(sh_lengths,length(user$days[[showers[i]]][[showers[i+1]]]$time))
    all = all+1
  }
  return(c(all,got,all_pat_lengths(user),mean(sh_lengths),mean(pr_lengths)))
}

all_pat_lengths = function(user){
  lens = c()
  for(day in user$days)
      for(pat in day)
        lens = c(lens, length(pat$time))
    return(mean(lens))
}

expected_survey = function(user,surv){
  week_exp = surv[as.character(surv$SWM.ID)==user$id,]$Number.of.showers.taken.in.your.household.per.week.
  swm_start = user$dates[1]
  swm_end = user$dates[length(user$dates)]
  swm_period = as.numeric(difftime(swm_end,swm_start,units='weeks'))
  return(week_exp*swm_period)
}

precision = function(user,surv,r){
  pred = 0
  all = 0
  dupl = 0
  for(i in round(length(user$days)*r):length(user$days))
    for(pat in user$days[[i]]){
      if(!is.na(pat$events[1])){
        if(as.numeric(pat$events[1])>0)
          pred=pred+1
        dupl = dupl + as.numeric(pat$events[1])
      }
      all=all+1
    }
  exp = expected_showers(user)*(1-r)
  act = sum(user$showers$volume>10)
  exp_surv = expected_survey(user,surv)
  return(c(act,exp,exp_surv,pred,dupl,all))
}

expected_showers = function(user){
  id_exp = id_expect(user$showers)[2]
  swm_start = user$dates[1]
  swm_end = user$dates[length(user$dates)]
  swm_period = as.numeric(difftime(swm_end,swm_start,units='weeks'))
  swm_exp = swm_period*id_exp
  return(swm_exp)
}

id_expect = function(sh){
  devs = unique(sh$device.key)
  per = 0
  tot = 0
  for(i in 1:length(devs)){
    res = id_expect_dev(get_device(sh,i))
    ids = res[1]
    good_rcv = res[2]
    weeks = sh_weeks_dev(get_device(sh,i))
    tot = tot + ids
    if(good_rcv>10 & weeks>2)
      per = per + ids/weeks
  }
  return(c(tot,per))
}

get_device=function(buf,ind){
  keys = unique(buf$device.key)
  return(buf[buf$device.key==keys[ind],])
}

id_expect_dev = function(sh){
  rcv = dim(sh)[1]
  good_rcv = sum(sh$volume>10)
  ids = sh$session.id[length(sh$session.id)]
  all_good = ids*good_rcv/rcv
  return(c(all_good,good_rcv))
}

sh_weeks_dev = function(sh){
  start = as.POSIXct(as.character(sh$local.datetime[1]),format='%d/%m/%Y %H:%M:%S',tz='CET')
  end = as.POSIXct(as.character(sh$local.datetime[length(sh$local.datetime)]),format='%d/%m/%Y %H:%M:%S',tz='CET')
  return(as.numeric(difftime(end,start,units='weeks')))
}

run_experiments = function(users,surv,r=0.5){
  results = c()
  results_b = c()
  results_p = c()
  results_mc = c()
  for(i in 1:length(users)){
    print(i)
    if(length(users[[i]]$shower_loc)<10)
       next
  #  user = disaggregate_user(users[[i]],all_events,means,stds,b,val_exp,time_exp)
  #  results = c(results , precision(user,surv,r),recall(user,r))
  #  print(t(matrix(results,nrow=11)))
    
    user_b = baseline_disag(users[[i]])
    results_b = c(results_b,precision(user_b,surv,r),recall(user_b,r))
    print(t(matrix(results_b,nrow=11)))
    
  #  user_pr = prune_disaggregate(user,all_events,means,stds,b,val_exp,time_exp)
  #  results_p = c(results_p , precision(user_pr,surv,r),recall(user_pr,r))
  #  print(t(matrix(results_p,nrow=11)))
    
  #  user_mc = mcmc_disaggregate_user(user,all_events,means,stds,b,val_exp,time_exp)
  #  results_mc = c(results_mc , precision(user_mc,surv,r),recall(user_mc,r))
  #  print(t(matrix(results_mc,nrow=11)))
  }
  return(list(t(matrix(results_b,nrow=11))))
  #return(list(t(matrix(results,nrow=11)),t(matrix(results_b,nrow=11))))
  #return(list(t(matrix(results,nrow=11)),t(matrix(results_p,nrow=11)),t(matrix(results_mc,nrow=11)),t(matrix(results_b,nrow=11))))
}

exp_accuracy = function(res){
  
  t1 = res[2]
  
  tpr = res[8]/res[7]
  fnr = 1-tpr
  p1  = res[4]
  p0  = res[6] - res[4]
  
  if(t1*tpr<p1)
    tp = t1*tpr
  else
    tp = p1
  
  if(t1*fnr<p0)
    fn = t1*fnr
  else
    fn = p0
  
  tn = p0-fn
  fp = p1-tp

  acc = (tp+tn)/(tp+tn+fp+fn)
  
  return(acc)

}

exp_acc = function(res,tr1=0.5){
  acc = c()
  for(i in 1:dim(res)[1])
    acc = c(acc, exp_accuracy(res[i,],tr1))
  return(acc)
}

acc_curve = function(res){
  accs = c()
  tr1s = seq(0,1,0.05)
  for(tr1 in tr1s)
    accs = c(accs, mean(exp_acc(res,tr1)))
  plot(tr1s,accs)
  return(accs)
}

measurment_stats = function(user){
  dates = user$dates
  durs = c()
  for(i in 2:length(dates)){
    dt = as.numeric(difftime(dates[i],dates[i-1],units='hours'))
    durs = c(durs, dt)
  }
  return(durs)
}

eval_res = function(res){
  pr  = res[,3]/res[,4]
  rec = res[,6]/res[,5]
  return(c(mean(pr),mean(rec)))
}

get_showers = function(email,showers,id_match){
  return(showers[as.character(showers$user.key)==as.character(id_match[id_match[,2]==email,1]),])
}

get_surv = function(id,surv,id_match){
  return(surv[as.character(surv$Email)==as.character(id_match[as.character(id_match[,1])==as.character(sh$user.key[1]),2]),])
}

which_user = function(email,users,surv){
  id = surv$SWM.ID[surv$Email==email]
  for(i in 1:length(users))
    if(users[[i]]$id==id)
      return(i)
}

pattern_stats = function(user){
  showers = user$shower_loc
  lens = c()
  for(i in seq(1,length(showers),2)){
    pat = user$days[[showers[i]]][[showers[i+1]]]
    lens = c(lens,length(pat$time),pat$end_index-pat$start_index+1,(pat$end_index-pat$start_index+1)/length(pat$time))
  }
  return(t(matrix(lens,nrow=3)))
}

shower_stats = function(sh){
  print(id_expect(sh))
  sh = sh[sh$volume>10 & sh$history=='false',]
  hours = as.numeric(format(as.POSIXct(sh$local.datetime,format='%d/%m/%Y %H:%M:%S',tz='CET'),format='%H'))
  hist(hours,20)
  hist(sh$volume,20)
  print(mean(sh$volume))
  print(sd(sh$volume))
}

scores = function(res){
  #res = res[[1]]
  res = res[res[,7]>=5,]
  scores = matrix(0,nrow=dim(res)[1],4)
  scores[,1] = res[,8]/res[,7]
  scores[,2] = res[,4]/res[,6]
  scores[,3] = res[,5]/res[,2]
  #scores[,3] = abs(1-res[,5]/res[,2])
  scores[,4] = res[,10]
#  print(mean(scores[,1]))
#  print(mean(scores[,2]))
#  print(mean(scores[,3]))
#  print(mean(scores[,4]))
  return(scores)
#  return(c(mean(scores[,1]),mean(scores[,2]),mean(scores[,3]),mean(scores[,4])))
}

get_others = function(day,j){
  prevs = 0
  for(i in 1:length(day))
    prevs = prevs + day[[i]]$events
  prevs = prevs - day[[j]]$events
  return(prevs)
}

calc_all_cond_probs = function(day,j,all_events,means,stds,b,val_exp,time_exp){
  probs = c()
  norm = 0
  prevs = get_others(day,j)
  cons = day[[j]]$cons
  time = day[[j]]$time
  for(i in 1:dim(all_events)[1]){
    num = cond_like(cons,all_events[i,],means,stds,b)*prior(all_events[i,],time,val_exp,time_exp,prevs)
    probs = c(probs,num)
    norm = norm+num
  }
  probs = probs / norm
  return(probs)
}

cond_sample = function(day,j,all_events,means,stds,b,val_exp,time_exp){
  all_probs = calc_all_cond_probs(day,j,all_events,means,stds,b,val_exp,time_exp)
  return(all_events[which(cumsum(all_probs)>runif(1))[1],])
}

copy_events = function(day){
  day_events = c()
  for(pat in day)
    day_events = c(day_events,pat$events)
  return(day_events)
}

day_mcmc = function(day,k=110,all_events,means,stds,b,val_exp,time_exp){
  m = length(day)
  n = length(day[[1]]$events)
  lot = matrix(0,k,n*m)
  for(i in 1:k){
    for(j in 1:m)
      day[[j]]$events=cond_sample(day,j,all_events,means,stds,b,val_exp,time_exp)
    lot[i,] = copy_events(day)
  }
  return(lot)
}

max_prob = function(lot,burn){
  ind = table(apply(lot[-(1:burn),], 1, paste, collapse = "/"))
  ind = which.max(ind)[1]
  return(as.numeric(strsplit(names(ind), "/")[[1]])) 
}

mcmc_disaggregate_day = function(day,all_events,means,stds,b,val_exp,time_exp){
  m = length(day)
  n = length(day[[1]]$events)
  lot = day_mcmc(day,k=60,all_events,means,stds,b,val_exp,time_exp)
  max = max_prob(lot,burn=10)
  for (j in 1:m){
    day[[j]]$events = max[((j-1)*n+1):(j*n)]
  }
  return(day)
}

mcmc_disaggregate_user = function(user,all_events,means,stds,b,val_exp,time_exp){
  for(i in 1:length(user$day)){
    print(i)
    user$days[[i]] = mcmc_disaggregate_day(user$days[[i]],all_events,means,stds,b,val_exp,time_exp)
  }
  return(user)
}

surv_experiments = function(users,surv,r=0.5){
  results = c()
  results_p = c()
  results_mc = c()
  for(i in 1:length(users)){
    print(i)
    if(length(users[[i]]$shower_loc)<10)
      next
    val_exp <<- vals_from_survey(user,surv)
    user = disaggregate_user(users[[i]],all_events,means,stds,b,val_exp,time_exp)
    results = c(results , precision(user,surv,r),recall(user,r))
    print(t(matrix(results,nrow=11)))
    
    #user_pr = prune_disaggregate(user,all_events,means,stds,b,val_exp,time_exp)
    #results_p = c(results_p , precision(user_pr,surv),recall(user_pr))
    #print(t(matrix(results_p,nrow=11)))
    
    #user_mc = mcmc_disaggregate_user(user,all_events,means,stds,b,val_exp,time_exp)
    #results_mc = c(results_mc , precision(user_mc,surv),recall(user_mc))
    #print(t(matrix(results_mc,nrow=11)))
    
  }
  return(t(matrix(results,nrow=11)))
  #return(list(t(matrix(results,nrow=11)),t(matrix(results_p,nrow=11)),t(matrix(results_mc,nrow=11))))
}

noisy_experiments = function(users,surv,r=0.1){
  all_res = list()
  for(i in 1:100){
    print('iter')
    print(i)
    add_all_noise(r)
    all_res[[i]] = run_experiments(users,surv,0.5)
  }
  return(all_res)
}

mean_noise_res = function(noise_res){
  sc = scores(noise_res[[1]])
  for(i in 2:length(noise_res))
    sc = rbind(sc,colMeans(scores(noise_res[[i]]),na.rm=TRUE))
  return(colMeans(sc,na.rm=TRUE))
}

disaggregate_user_base = function(user){
  days = user$days
  for(i in 1:length(days))
    days[[i]]=disag_day_base(days[[i]])
  user$days=days
  return(user)
}

pat_feats = function(user,i,j){
  pat=user$days[[i]][[j]]
  sh = t(matrix(user$shower_loc,nrow=2))
  shi=sum(apply(sh,1,function(x) all.equal(x,c(i,j)))==TRUE)
  return(c(as.numeric(format(pat$start_date,'%H')),length(pat$time),pat$cons,shi))
}

unwarp_patterns = function(user,r){
  lot = c()
  for(i in 1:(length(user$days)*r))
    for(j in 1:length(user$days[[i]]))
      lot = c(lot, pat_feats(user,i,j))
  d = t(matrix(lot,nrow=4))
  norm = t(matrix(nrow=2,c(mean(d[,1]),sd(d[,1]),mean(d[,2]),sd(d[,2]),mean(d[,3]),sd(d[,3]))))
  d[,1] = (d[,1]-norm[1,1])/norm[1,2]
  d[,2] = (d[,2]-norm[2,1])/norm[2,2]
  d[,3] = (d[,3]-norm[3,1])/norm[3,2]
  return(list(d,norm))
}

show_in_clus = function(d,clus){
  sho = c()
  all = c()
  for(i in 1:length(unique(clus$cluster))){
    all = c(all, sum(clus$cluster==i) )
    sho = c(sho, sum(d[clus$cluster==i,4]))
  }
  return(cbind(sho,all))
}

select_clusters = function(sc,user,r){
  n = expected_showers(user)*r
  m = sum(sc[,2])
  p = 1/m
  exp_pats = (1-(1-p)**n)*m
  ord = rev(order(sc[,1]))
  good_cl = c()
  tot = 0
  for(cl in ord){
    if(tot>=exp_pats)
      break
    else{
     tot=tot+sc[cl,2]
     good_cl = c(good_cl,cl)
    }
  }
  return(good_cl)
}

classify_pattern = function(point,clus,good_cl){
  cl = as.numeric( which.min( apply(clus$centers,1,function(x) x %*% point) ) )
  if(cl %in% good_cl)
    return(3)
  else
    return(0)
}

pat_feats_norm = function(user,i,j,norm){
  pat=user$days[[i]][[j]]
  sh = t(matrix(user$shower_loc,nrow=2))
  if (any(apply(sh,1,function(x) all.equal(x,c(i,j)))==TRUE))
    shi = TRUE
  else
    shi = FALSE
  res = c(as.numeric(format(pat$start_date,'%H')),length(pat$time),pat$cons)
  res[1] = (res[1] - norm[1,1])/norm[1,2]
  res[2] = (res[2] - norm[2,1])/norm[2,2]
  res[3] = (res[3] - norm[3,1])/norm[3,2]
  return(res)
}

baseline_disag = function(user,r=0.5,k=12){
  
  res  = unwarp_patterns(user,r)
  clus = kmeans(res[[1]][,1:3],k)
  norm = res[[2]]
  sc = show_in_clus(res[[1]],clus)
  good_cl = select_clusters(sc,user,r)
  
  for(i in round(length(user$days)*r):(length(user$days)))
    for(j in 1:length(user$days[[i]])){
      point = pat_feats_norm(user,i,j,norm)
      ev = classify_pattern(point,clus,good_cl)
      user$days[[i]][[j]]$events = c(ev,0,0,0,0)
    }
    
  return(user)
        
}

pats_per_day = function(user){
  lot = c()
  for(day in user$days)
    lot = c(lot, length(pat$time))
    #for(pat in day)
    #  lot = c(lot, length(pat$time))
  return(lot)
}

mean_pats_user = function(users){

  lot = c()
  for(user in users)
    lot = c(lot,mean(pats_per_day(user)))
  return(lot)
  
}
