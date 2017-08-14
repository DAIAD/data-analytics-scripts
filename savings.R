make_users = function(swm,surv=NULL){
  
  users = list()
  user = list()
  #  ids = intersect(swm[[1]],surv$SWM.ID)
  ids = unique(swm[[1]])
  
  for(i in 1:length(ids)){
    
    print(i)
    user = list()
    user$id=ids[i]
    
    #    surv_row = surv[surv$SWM.ID==ids[i],]
    #    user$mail = surv_row$Email
    #    user$members = as.numeric(as.character(surv_row$How.many.members.are.there.in.your.household..1))
    #    user$showers_no = as.numeric(as.character(surv_row$How.many.hand.showers.are.there.in.your.home.))
    #    user$apartment_size = parse_apartment_size(surv_row$What.is.the.size.of.your.appartment.house.in.square.meters.feet.)
    #    user$child = as.numeric(as.character(surv_row$Is.there.any.minor.in.your.home.))
    #    user$male_rate = (as.numeric(as.character(surv_row$How.many.of.your.household.members.are.males.))-user$child)/user$members
    #    user$income = parse_income(surv_row$What.is.approximately.your.yearly.household.income.before.tax.)
    #    user$own_house = parse_rent(surv_row$Do.you.own.or.lease.your.residence.)
    #    user$consider = parse_consider(surv_row$I.consider.my.household.s.water.consumption.to.be.)
    
    #    user$cons = diff( rev(swm[[3]][swm[[1]]==user$id]) )
    #    user$cons = swm[[4]][swm[[1]]==user$id]
    #    user$dates = as.POSIXct(rev(swm[[2]][swm[[1]]==user$id])[-1],format="%d/%m/%Y %H:%M:%S",tz="CET")
    #    user$days = split_days(user$dates)
    
    swm_slice = swm[swm[[1]]==user$id,]
    user$aggr_cons = rev(swm_slice[[3]])
    user$dates = as.POSIXct(rev(swm_slice[[2]]),format="%d/%m/%Y %H:%M:%S",tz="CET")
    user$hour_cons = rev(swm_slice[[4]])
    # user$days = split_days(user$dates)
    
    users[[i]] = user
    
  }
  
  return(users)
  
}

make_good_csv = function(users,period,slack){
  #1
  write.table(c(),file='/home/pant/Desktop/out1.csv',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=';')
  for(i in 1:length(users)){
    print(i)
    good_weeks = take_good_period(users[[i]],period,slack)
    if(is.null(good_weeks))
      next
    cons  = as.numeric(good_weeks[1,])
    week_ids = good_weeks[2,]
    buf = as.data.frame(cbind(as.character(users[[i]]$id),week_ids,cons))
    write.table(buf,file='/home/pant/Desktop/out1.csv',append=TRUE,quote=FALSE,col.names=FALSE,row.names=FALSE,sep=';')
  }
  #2
  csv = read.csv('/home/pant/Desktop/out1.csv',header=FALSE,sep=';')
  daily_cons = diff(csv[[3]])
  id_diff = diff(csv[[2]])
  id_diff[id_diff!=1] = 0
  new_csv = cbind(as.character(csv[[1]][-1]),csv[[2]][-1]-1,csv[[3]][-1],daily_cons,id_diff)
  write.table(new_csv,file='/home/pant/Desktop/out2.csv',quote=FALSE,col.names=FALSE,row.names=FALSE,sep=';')
}

take_good_period = function(user,period,slack){
  
  dates = user$dates
  aggr_cons = user$aggr_cons
  good_values = c()
  all_times = seq(as.POSIXct('2015-01-01 3:00',tz='CET'), as.POSIXct("2017-5-15 3:00",tz='CET'), by=paste(period,'days'))
  if(period == 30)
    all_times = seq(as.POSIXct('2015-01-01 3:00',tz='CET'), as.POSIXct("2017-5-15 3:00",tz='CET'), by=paste('months'))
  
  for(i in 1:length(all_times)){
    
    #opt
    difs = abs(as.numeric(difftime(all_times[i],dates,units='hours')))
    
    if(min(difs)<=slack){
      ind = which.min(difs)
      good_values = c(good_values,aggr_cons[ind],i)
    }
    
  }
  
  if(!is.null(good_values))
    return(matrix(good_values,nrow=2))
  else
    return(NULL)
  
}

is_valid2 = function(vec,thresh=0.8){
  len = length(vec)
  mid = round(len/2)
  if(sum(is.na(vec[1:mid]))>mid*thresh)
    return(FALSE)
  else if (sum(is.na(vec[mid:len]))>mid*thresh)
    return(FALSE)
  else
    return(TRUE)
}

make_vec = function(csv,n){
  time = csv[,2]
  cons = csv[,4]
  valid = csv[,5]
  out = 1:n
  for(i in 1:n){
    if(any(time==i) && valid[time==i])
      out[i] = cons[time==i]
    else
      out[i] = NA
  }
  return(out)
}

is_valid = function(vec,len,perc=0.6){
  if(length(vec)==0)
    return(FALSE)
  else if(sum(is.na(vec))>length(vec)*perc)
    return(FALSE)
  else if(mean(vec,na.rm=TRUE)<len*80)
    return(FALSE)
  else
    return(TRUE)
}

is_valid2 = function(vec,thresh=0.8){
  len = length(vec)
  mid = round(len/2)
  if(sum(is.na(vec[1:mid]))>mid*thresh)
    return(FALSE)
  else if (sum(is.na(vec[mid:len]))>mid*thresh)
    return(FALSE)
  else
    return(TRUE)
}

clean = function(vec){
  m = mean(vec,na.rm=TRUE)
  vec[vec<0.3*m | vec>2*m] = NA
  return(vec)
}


make_feats = function(csv,all_ids,meas_l,meas_n){
  feats = c()
  valid_ids = c()
  for(id in all_ids){
    slice = csv[csv[[1]]==id,]
    vec = make_vec(slice,meas_n)
    if(is_valid(vec,meas_l)){
      vec = clean(vec)
      if(is_valid2(vec)){
        valid_ids = c(valid_ids,id)
        feats = rbind(feats,clean(vec))
      }
    }
  }
  return(list(feats,valid_ids))
}

make_feats_raw = function(csv,all_ids,meas_n){
  feats = c()
  valid_ids = c()
  for(id in all_ids){
    slice = csv[csv[[1]]==id,]
    vec = make_vec(slice,meas_n)
    valid_ids = c(valid_ids,id)
    feats = rbind(feats,clean(vec))
  }
  return(list(feats,valid_ids))
}

get_day_id = function(date,type){
  all_weeks = seq(as.Date('2015-01-01'), as.Date("2017-1-20"), by="days")
  dt = difftime(all_weeks,as.Date(date,format = '%d/%m/%Y %H:%M:%S'),units='days')
  left = which(dt<0)[sum(dt<0)]
  right = which(dt>0)[1]
  if(type=='start')
    return(right)
  if(type=='end')
    return(left)
}

get_phases = function(id, phases_csv){
  
  row = phases_csv[phases_csv$meter.serial == id,]
  if(dim(row)[1]==0)
    return(NA)
  
  ph1_start = as.POSIXct(row$BASELINE.start,format = '%d/%m/%Y %H:%M:%S')
  ph1_end = as.POSIXct(row$BASELINE.end,format = '%d/%m/%Y %H:%M:%S')
  
  ph2_start = as.POSIXct(row$Phase.1.start,format = '%d/%m/%Y %H:%M:%S')
  ph2_end = as.POSIXct(row$Phase.1.end,format = '%d/%m/%Y %H:%M:%S')
  
  if(row$Phase.1 == 'MOBILE_ON')
    type = 1
  else
    type = 2
  
  ph3_start = as.POSIXct(row$Phase.2.start,format = '%d/%m/%Y %H:%M:%S')
  ph3_end = as.POSIXct('2016/12/22 00:00:00')
  
  ph4_start = ph3_end
  type2 = 1 
  if( row$Phase.3.start=='' || as.POSIXct(row$Phase.3.start, format = '%d/%m/%Y %H:%M:%S') > as.POSIXct('2017/1/1 00:00:00') )
    type2 = 2
  ph4_end = as.POSIXct('2017/2/1 00:00:00')
  
  ph5_start = ph4_end
  ph5_end = as.POSIXct('2017/2/28 23:59:59')
  
  ph6_start = ph5_end
  ph6_end = as.POSIXct('2017/4/30 23:59:59')
  
  #ph5_start = as.POSIXct(row$Phase.4.start,format = '%d/%m/%Y %H:%M:%S')
  #ph5_end = as.POSIXct(row$Phase.4.end,format = '%d/%m/%Y %H:%M:%S')
  
  return(list(ph1_start,ph1_end,ph2_start,ph2_end,ph3_start,ph3_end,ph4_start,ph4_end,ph5_start,ph5_end,ph6_start,ph6_end,type,type2))
  
}

interpolate_missing = function(vec){
  
  bckp = vec
  
  for(i in 1:length(vec)){
    
    if(is.na(vec[i])){
      
      buf = which(!is.na(vec[1:i]))
      if(length(buf)==0)
        start=NA
      else
        start=buf[length(buf)]
      
      buf = which(!is.na(vec[i:length(vec)]))
      if(length(buf)==0)
        end=NA
      else
        end=i-1+buf[1]
      
      if( !is.na(start) && !is.na(end) ) {
        step = (vec[end] - vec[start]) / (end-start)
        dist = i - start
        vec[i] = vec[start] + step*dist
      }
      else if( is.na(start) && !is.na(end) )
        vec[i] = mean(bckp,na.rm=TRUE)
      else if( !is.na(start) && is.na(end) )
        vec[i] = mean(bckp,na.rm=TRUE)
      
    }
    
  }
  
  return(vec)
  
}

get_period_consumption = function(cons,time1,time2,feats,ret='sum'){
  
  if(feats == 'weeks')
    all_times = seq(as.POSIXct('2015-01-01 3:00',tz='CET'), as.POSIXct("2017-5-30 3:00",tz='CET'), by="weeks")[1:(length(cons)+1)]
  if(feats == 'd15')
    all_times = seq(as.POSIXct('2015-01-01 3:00',tz='CET'), as.POSIXct("2017-5-30 3:00",tz='CET'), by="15 days")[1:(length(cons)+1)]
  if(feats == 'months')
    all_times = seq(as.POSIXct('2015-01-01 3:00',tz='CET'), as.POSIXct("2017-5-30 3:00",tz='CET'), by="months")[1:(length(cons)+1)]
  
  if(time1 < all_times[1] || time2 > all_times[length(cons)+1])
    return(NA)
  
  facs = as.numeric(all_times > time1 & c(0,all_times[-length(all_times)]) >= time1 & all_times<= time2)
  
  buf = which(all_times>time1)
  first = buf[1]
  if(all_times[first]<time2)
    facs[first] = as.numeric(difftime(all_times[first],time1,units='days'))/as.numeric(difftime(all_times[first],all_times[first-1],units='days'))
  else
    facs[first] = as.numeric(difftime(time2,time1,units='days'))/as.numeric(difftime(all_times[first],all_times[first-1],units='days'))
  
  buf = which(all_times>time2)
  last = buf[1]
  if(all_times[last-1]>time1)
    facs[last] = as.numeric(difftime(time2,all_times[last-1],units='days'))/as.numeric(difftime(all_times[last],all_times[last-1],units='days'))
  else
    facs[last] = as.numeric(difftime(time2,time1,units='days'))/as.numeric(difftime(all_times[last],all_times[last-1],units='days'))
  
  facs = facs[-1]
  if(ret=='sum')
    return(sum(facs*cons,na.rm=TRUE))
  else if(ret=='vec')
    return((facs*cons)[facs>0])
  
}

dist = function(v1,v2){
  
  if(sum(is.na(v1+v2)) > 0.8 *length(v1))
    return(NA)
  
  if(type=='dtw'){
    if(any(!is.na(v1+v2)))
      return(dtw(v1[!is.na(v1) & !is.na(v2)],v2[!is.na(v1) & !is.na(v2)])$distance)
    else
      return(NA)
  }
  
}

find_nns = function(feats,ids,target_id,other_ids,k,train_start,train_end){
  dists = c()
  target_feat = feats[ids==target_id,train_start:train_end]
  valid_ids = c()
  for(id in other_ids){
    selected_feat = feats[ids==id,train_start:train_end]
    res = dist(target_feat,selected_feat)
    if(!is.na(res)){
      dists = c(dists,res)
      valid_ids = c(valid_ids,id)
    }
  }
  if(length(dists)<1)
    return(NA)
  else
    return(valid_ids[order(dists)[1:k]])
}

find_seas_dif = function(basel,phases,feats){
  
  year = 60*60*24*366
  
  basel = interpolate_missing(basel)
  
  per15p1 = get_period_consumption(basel,phases[[1]]-year,phases[[2]]-year,feats)
  per16p1 = get_period_consumption(basel,phases[[1]],phases[[2]],feats)
  per15p2 = get_period_consumption(basel,phases[[3]]-year,phases[[4]]-year,feats)
  per16p2 = get_period_consumption(basel,phases[[3]],phases[[4]],feats)
  per15p3 = get_period_consumption(basel,phases[[5]]-year,phases[[6]]-year,feats)
  per16p3 = get_period_consumption(basel,phases[[5]],phases[[6]],feats)
  per15p4 = get_period_consumption(basel,phases[[7]]-year,phases[[8]]-year,feats)
  per16p4 = get_period_consumption(basel,phases[[7]],phases[[8]],feats)
  per15p5 = get_period_consumption(basel,phases[[9]]-year,phases[[10]]-year,feats)
  per16p5 = get_period_consumption(basel,phases[[9]],phases[[10]],feats)
  per15p6 = get_period_consumption(basel,phases[[11]]-year,phases[[12]]-year,feats)
  per16p6 = get_period_consumption(basel,phases[[11]],phases[[12]],feats)
  
  #return(c(per16p1-per15p1,per16p2-per15p2,per16p3-per15p3,per16p4-per15p4))
  return(c(per16p1-per15p1,per16p2-per15p2,per16p3-per15p3,per16p4-per15p4,per16p5-per15p5,per16p6-per15p6))
  
}

apply_new_cleaning = function(feats,ft){
  
  b=8
  
  for(i in 1:dim(feats)[1]){
    
    buf = feats[i,]
    for(j in (b+1):length(buf)){
      
      mu = 0
      for(k in b:(j-1)){
        mu = mean(buf[(j-k):(j-1)],na.rm=TRUE)
        if (!is.na(mu))
          break
      }
      
      crit1 = 1.3 * mu
      crit2 = 0.7 * mu
      
      if(!is.na(crit1) & !is.na(buf[j]) & buf[j]>crit1)
        buf[j] = crit1
      if(!is.na(crit2) & !is.na(buf[j]) & buf[j]<crit2)
        buf[j] = crit2
      
    }
    
    feats[i,]=buf
    
  }
  
  return(feats)
  
}

calc_train_ind = function(){
  return(c(21,64))
}

calc_all_savings_conf = function(feats,ids,phases_csv,trial_ids,non_trial_ids){
  
  res = list()
  conf = c()
  
  feats = apply_new_cleaning(feats,ft)
                
  trn_ind = calc_train_ind(ft,trn)
                
  res = calc_all_savings(feats,ids,trial_ids,non_trial_ids,trn_ind,phases_csv)
  
  return(res)
  
}

calc_all_savings = function(feats,ids,trial_ids,non_trial_ids,phases_csv,trn_ind){
  
  proc_ids = c()
  disc_ids = c()
  type1 = c()
  type2 = c()
  saves = c()
  all_nns = c()
  yd = c()
  seass = c()
  data_out = list()
  
  non_trial_ids = intersect(non_trial_ids,ids)
  trial_ids = intersect(trial_ids,ids)
  
  #while(TRUE){ #1
  for( id in trial_ids){
    # print(which(trial_ids==id))
    nns = find_nns(feats,ids,id,non_trial_ids,15,trn_ind[1],trn_ind[2])
    phases = get_phases(id,phases_csv)
    
    if(!is.na(phases) && any(!is.na(nns))){
      
      proc_ids = c(proc_ids,id)
      all_nns = c(all_nns,nns[1:k])
      #   non_trial_ids = setdiff(non_trial_ids,all_nns) #2
      type1 = c(type1,phases[[11]])
      type2 = c(type2,phases[[12]])
      
      basel = get_baseline(feats,ids,nns,k)
      seas = find_seas_dif(basel,phases)
      seass = c(seas,seass)
      
      targ = clean_feats[ids==id,]
      
      data_out[[length(data_out)+1]] = list(targ,basel,phases,seas,feat_type,phases[[13]],phases[[14]],id,nns)
      
    }
  }
  
  return(data_out)
  
}

find_nns = function(feats,ids,target_id,other_ids,k,train_start,train_end){
  dists = c()
  target_feat = feats[ids==target_id,train_start:train_end]
  valid_ids = c()
  for(id in other_ids){
    selected_feat = feats[ids==id,train_start:train_end]
    res = dist(target_feat,selected_feat)
    if(!is.na(res)){
      dists = c(dists,res)
      valid_ids = c(valid_ids,id)
    }
  }
  if(length(dists)<1)
    return(NA)
  else
    return(valid_ids[order(dists)[1:k]])
}

find_seas_dif = function(basel,phases,feats){
  
  year = 60*60*24*366
  
  basel = interpolate_missing(basel)
  
  per15p1 = get_period_consumption(basel,phases[[1]]-year,phases[[2]]-year,feats)
  per16p1 = get_period_consumption(basel,phases[[1]],phases[[2]],feats)
  per15p2 = get_period_consumption(basel,phases[[3]]-year,phases[[4]]-year,feats)
  per16p2 = get_period_consumption(basel,phases[[3]],phases[[4]],feats)
  per15p3 = get_period_consumption(basel,phases[[5]]-year,phases[[6]]-year,feats)
  per16p3 = get_period_consumption(basel,phases[[5]],phases[[6]],feats)
  per15p4 = get_period_consumption(basel,phases[[7]]-year,phases[[8]]-year,feats)
  per16p4 = get_period_consumption(basel,phases[[7]],phases[[8]],feats)
  per15p5 = get_period_consumption(basel,phases[[9]]-year,phases[[10]]-year,feats)
  per16p5 = get_period_consumption(basel,phases[[9]],phases[[10]],feats)
  per15p6 = get_period_consumption(basel,phases[[11]]-year,phases[[12]]-year,feats)
  per16p6 = get_period_consumption(basel,phases[[11]],phases[[12]],feats)
  
  return(c(per16p1-per15p1,per16p2-per15p2,per16p3-per15p3,per16p4-per15p4,per16p5-per15p5,per16p6-per15p6))
  
}

get_baseline = function(feats,ids,nns,k){
  
 return(basel,feats[ids==nns[1],])
  
}

find_seas_dif = function(basel,phases,feats){
  
  year = 60*60*24*366
  
  basel = interpolate_missing(basel)
  
  per15p1 = get_period_consumption(basel,phases[[1]]-year,phases[[2]]-year,feats)
  per16p1 = get_period_consumption(basel,phases[[1]],phases[[2]],feats)
  per15p2 = get_period_consumption(basel,phases[[3]]-year,phases[[4]]-year,feats)
  per16p2 = get_period_consumption(basel,phases[[3]],phases[[4]],feats)
  per15p3 = get_period_consumption(basel,phases[[5]]-year,phases[[6]]-year,feats)
  per16p3 = get_period_consumption(basel,phases[[5]],phases[[6]],feats)
  per15p4 = get_period_consumption(basel,phases[[7]]-year,phases[[8]]-year,feats)
  per16p4 = get_period_consumption(basel,phases[[7]],phases[[8]],feats)
  per15p5 = get_period_consumption(basel,phases[[9]]-year,phases[[10]]-year,feats)
  per16p5 = get_period_consumption(basel,phases[[9]],phases[[10]],feats)
  per15p6 = get_period_consumption(basel,phases[[11]]-year,phases[[12]]-year,feats)
  per16p6 = get_period_consumption(basel,phases[[11]],phases[[12]],feats)
  
  #return(c(per16p1-per15p1,per16p2-per15p2,per16p3-per15p3,per16p4-per15p4))
  return(c(per16p1-per15p1,per16p2-per15p2,per16p3-per15p3,per16p4-per15p4,per16p5-per15p5,per16p6-per15p6))
  
}

single_sel_basel = function(sel,type){
  
  buf = matrix(0,length(sel),12)
  
  ind = sample(1:length(sel),replace = TRUE)
  
  type1 = c()
  type2 = c()
  swm = c()
  for(j in 1:length(sel)){
    us = sel[[j]]
    buf[j,] = single_basel2(us[[1]],us[[3]],us[[4]],us[[5]])
    type1 = c(type1,us[[6]])
    type2 = c(type2,us[[7]])
    swm = c(swm,us[[8]])
  }
  
  cs = colSums(buf)
  sav = c()
  ci = c()
  sav56 = c()
  ci56 = c()
  for(i in 1:6){
    sav = c(sav,(cs[6+i]-cs[i])/cs[6+i]) 
    ci = cbind(ci,parametric_mc(buf[,6+i],buf[,i],type))
  }
  sav56 = c(sav56,((cs[11]+cs[12])-(cs[5]+cs[6]))/(cs[11]+cs[12]))
  ci56 = cbind(ci56,parametric_mc(buf[,11]+buf[,12],buf[,5]+buf[,6],type))
  
  buf11 = buf[type1==1,]
  cs11 = colSums(buf11)
  sav = c(sav,(cs11[6+2]-cs11[2])/cs11[6+2])
  ci = cbind(ci,parametric_mc(buf11[,6+2],buf11[,2],type))
  sav56 = c(sav56,((cs11[11]+cs11[12])-(cs11[5]+cs11[6]))/(cs11[11]+cs11[12]))
  ci56 = cbind(ci56,parametric_mc(buf11[,11]+buf11[,12],buf11[,5]+buf11[,6],type))
  
  buf12 = buf[type1==2,]
  cs12 = colSums(buf12)
  sav = c(sav,(cs12[6+2]-cs12[2])/cs12[6+2])
  ci = cbind(ci,parametric_mc(buf12[,6+2],buf12[,2],type))
  sav56 = c(sav56,((cs12[11]+cs12[12])-(cs12[5]+cs12[6]))/(cs12[11]+cs12[12]))
  ci56 = cbind(ci56,parametric_mc(buf12[,11]+buf12[,12],buf12[,5]+buf12[,6],type))
  
  buf21 = buf[type2==1,]
  cs21 = colSums(buf21)
  sav = c(sav,(cs21[6+4]-cs21[4])/cs21[6+4])
  ci = cbind(ci,parametric_mc(buf21[,6+4],buf21[,4],type))
  sav56 = c(sav56,((cs21[11]+cs21[12])-(cs21[5]+cs21[6]))/(cs21[11]+cs21[12]))
  ci56 = cbind(ci56,parametric_mc(buf21[,11]+buf21[,12],buf21[,5]+buf21[,6],type))
  
  buf22 = buf[type2==2,]
  cs22 = colSums(buf22)
  sav = c(sav,(cs22[6+4]-cs22[4])/cs22[6+4])
  ci = cbind(ci,parametric_mc(buf22[,6+4],buf22[,4],type))
  sav56 = c(sav56,((cs22[11]+cs22[12])-(cs22[5]+cs22[6]))/(cs22[11]+cs22[12]))
  ci56 = cbind(ci56,parametric_mc(buf22[,11]+buf22[,12],buf22[,5]+buf22[,6],type))
  
  return(list(sav,ci,sav56,ci56,rowSums(buf[,c(5:6)]),rowSums(buf[,c(11:12)]),rowSums(buf[,c(11:12)])-rowSums(buf[,c(5:6)]),swm))
  
}

parametric_mc = function(v1,v2,type='norm',r=1000){
  
  res = c()
  
  m1 = mean(v1)
  m2 = mean(v2)
  s1 = sd(v1)
  s2 = sd(v2)
  
  b1 = min(v1)
  a1 = max(v1-b1)
  b2 = min(v2)
  a2 = max(v2-b2)
  
  v1t = (v1 - b1)/a1
  v2t = (v2 - b2)/a2
  
  dst1 = fitdist(v1t,'beta','mme')
  dst2 = fitdist(v2t,'beta','mme')
  
  n = length(v1)
  
  for(i in 1:r){
    if(type=='norm'){
      b = rnorm(n)*s1+m1
      y = rnorm(n)*s2+m2
    }
    else if(type=='beta'){
      b = rbeta(n,dst1[[1]][[1]],dst1[[1]][[2]])
      b = b*a1+b1
      y = rbeta(n,dst2[[1]][[1]],dst2[[1]][[2]])
      y = y*a2+b2
    }
    else if(type=='non-param'){
      ind = sample(1:n,replace=TRUE)
      b = v1[ind]
      y = v2[ind]
    }
    res = c(res,(sum(b)-sum(y))/sum(b))
  }
  
  res = sort(res)
  
  return(c(res[0.025*r],res[0.975*r]))
  
}

single_basel2 = function(targ,phases,seas,feat_type){
  
  targ_int = interpolate_missing(targ)
  
  targ_p1 = get_period_consumption(targ_int,phases[[1]],phases[[2]],feat_type,'sum')
  targ_p2 = get_period_consumption(targ_int,phases[[3]],phases[[4]],feat_type,'sum')
  targ_p3 = get_period_consumption(targ_int,phases[[5]],phases[[6]],feat_type,'sum')
  targ_p4 = get_period_consumption(targ_int,phases[[7]],phases[[8]],feat_type,'sum')
  targ_p5 = get_period_consumption(targ_int,phases[[9]],phases[[10]],feat_type,'sum')
  targ_p6 = get_period_consumption(targ_int,phases[[11]],phases[[12]],feat_type,'sum')
  
  year = 60*60*24*366
  
  basel2_p1_s = get_period_consumption(targ_int,phases[[1]]-year,phases[[2]]-year,feat_type,'sum') + seas[1]
  if(basel2_p1_s<0)
    basel2_p1_s=0
  
  basel2_p2_s = get_period_consumption(targ_int,phases[[3]]-year,phases[[4]]-year,feat_type,'sum') + seas[2]
  if(basel2_p2_s<0)
    basel2_p2_s=0
  
  basel2_p3_s = get_period_consumption(targ_int,phases[[5]]-year,phases[[6]]-year,feat_type,'sum') + seas[3]
  if(basel2_p3_s<0)
    basel2_p3_s=0
  
  basel2_p4_s = get_period_consumption(targ_int,phases[[7]]-year,phases[[8]]-year,feat_type,'sum') + seas[4]
  if(basel2_p4_s<0)
    basel2_p4_s=0
  
  basel2_p5_s = get_period_consumption(targ_int,phases[[9]]-year,phases[[10]]-year,feat_type,'sum') + seas[5]
  if(basel2_p5_s<0)
    basel2_p5_s=0
  
  basel2_p6_s = get_period_consumption(targ_int,phases[[11]]-year,phases[[12]]-year,feat_type,'sum') + seas[6]
  if(basel2_p6_s<0)
    basel2_p6_s=0
  
  return(c(targ_p1,targ_p2,targ_p3,targ_p4,targ_p5,targ_p6,basel2_p1_s,basel2_p2_s,basel2_p3_s,basel2_p4_s,basel2_p5_s,basel2_p6_s))
  
}
