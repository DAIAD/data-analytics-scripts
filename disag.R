means=c(50,80,8,56,25)
stds=c(20,5,1,10,7)
b=c(10,5)

time_exp = read.table('/home/pant/Desktop/disaggregation/time_exp',header=TRUE)
time_exp = as.matrix(time_exp/rowSums(time_exp))
#time_exp = matrix(1/24,5,24)
val_exp = as.matrix(read.table('/home/pant/Desktop/disaggregation/val_exp',header=TRUE))

cond_like = function(cons,vals,means,stds,b){
  std_sum = sqrt( (sum(vals*stds)+b[2])**2 )
  mean_sum = sum(vals*means)+b[1]
  return(dnorm((cons-mean_sum)/std_sum))
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

joint_prob=function(data,all_events,means,stds,b,val_exp,time_exp){
  prob = 1
  for(i in 1:length(data)){
    prevs = get_prevs(data,i,length(data[[1]]$events))
    prob = prob * cond_like(data[[i]]$cons,data[[i]]$event,means,stds,b) * prior(data[[i]]$event,data[[i]]$time,val_exp,time_exp,prevs) / norm_factor(data[[i]]$cons,all_events,means,stds,b,data[[i]]$time,val_exp,time_exp,prevs)
  }
  return(prob)
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

all_probs = function(all_events,cons,time,means,stds,b,val_exp,time_exp,prevs){
  probs = c()
  norm = 0
  for(i in 1:dim(all_events)[1]){
    num = cond_like(cons,all_events[i,],means,stds,b)*prior(all_events[i,],time,val_exp,time_exp,prevs)
    probs = c(probs,num)
    norm = norm+num
  }
  return(probs)
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

is_very_large = function(pat){
  if( difftime(pat$end_date,pat$start_date,units='hours') > 8)
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

recall = function(user){
  showers = user$shower_loc
  got = 0
  all = 0
  sh_lengths = c()
  pr_lengths = c()
  for(i in seq(1,length(showers),2)){
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

precision = function(user,surv){
  pred = 0
  all = 0
  dupl = 0
  for(day in user$days)
    for(pat in day){
      if(as.numeric(pat$events[1])>0)
        pred=pred+1
      dupl = dupl + as.numeric(pat$events[1])
      all=all+1
    }
  exp = expected_showers(user)
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

run_experiments = function(users,surv){
  results = c()
  for(i in 1:length(users)){
    print(i)
    if(length(users[[i]]$shower_loc)<10)
       next
    user = disaggregate_user(users[[i]],all_events,means,stds,b,val_exp,time_exp)
    results = c(results , precision(user,surv),recall(user))
    print(t(matrix(results,nrow=11)))
  }
  return(t(matrix(results,nrow=11)))
}

exp_accuracy = function(res,tr1=0.5){
  
  t1 = tr1*res[4]
  
  tpr = res[6]/res[5]
  fnr = 1-tpr
  p1  = res[3]
  p0  = res[4] - res[3]
  
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
  scores = matrix(0,nrow=dim(res)[1],4)
  scores[,1] = res[,8]/res[,7]
  scores[,2] = res[,4]/res[,6]
  scores[,3] = res[,5]/res[,2]
  scores[,4] = res[,11]
  print(mean(scores[,1]))
  print(mean(scores[,2]))
  print(mean(scores[,3]))
  print(mean(scores[,4]))
  return(scores)
}

