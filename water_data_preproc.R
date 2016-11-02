check_country=function(ts,time,cons){
  end=round(length(ts)/720)*720
  months = sort(colSums(matrix(ts[1:end],nrow=720)))
  start = round((1-time)*length(months)+1)
  end = length(months)
  if(sum(months[start:end])>sum(ts)*cons)
    return(TRUE)
  else
    return(FALSE)
}

create_days_ts = function(ts,dates){
  days = list()
  days_c = c()
  index = 1
  lindex = 1
  for(i in 1:365){
    if(i==119){
      days[[lindex]]=ts[index:(index+24)]
      index=index+25
    }
    else if(i==273){
      days[[lindex]] = ts[index:(index+22)]
      index=index+23
    }
    else{
      days[[lindex]] = ts[index:(index+23)]
      days_c = c(days_c,as.numeric(ts[index:(index+23)]))
      index=index+24
    }
    lindex=lindex+1
  }
  return(t(matrix(days_c,nrow=24)))
}

create_days = function(ts){
  days = c()
  index = 1
  for(i in 1:365){
    if(i==119){
      days=c(days, sum(ts[index:(index+24)]))
      index=index+25
    }
    else if(i==273){
      days = c(days, sum(ts[index:(index+22)]))
      index=index+23
    }
    else{
      days = c(days, sum(ts[index:(index+23)]))
      index=index+24
    }
  }
  return(days)
}

create_weeks = function(days){
  end = floor(length(days)/7)*7
  return(colSums(matrix(days[1:end],nrow=7)))
}

create_months = function(days){
  steps = c(31,31,30,31,30,31,31,28,31,30,31,30)
  index=1
  months = c()
  for(i in 1:12){
    months = c(months,sum(days[index:(index+steps[i]-1)])/steps[i]*30)
    index=index+steps[i]
  }
  return(months)
}

find_trend = function(months){
  d = data.frame(cbind(months,1:12))
  return(as.numeric(lm(months~.,data=d)$coefficients[2]))
}

plot_events = function(vec){
  m = mean(vec)
  s = sd(vec)
  hist(vec,20)
  abline(v=m+0.5*m,lty=2)
  abline(v=m+1.6*s,lty=1)
  plot(vec)
  abline(h=m+0.5*m,lty=2)
  abline(h=m+1.6*s,lty=1)
}

find_cors = function(ts,start,end){
  dts = create_days_ts(ts)
  x = dts[start:(end-1),]
  y = rowSums(dts[(start+1):end,])
  run_cpd(y)
  d = cbind(x,y)
  cors = cor(d)[1:24,25]
  ci = cor_ci(cors,length(y))
  plot(cors,ylim=c(-1,1))
  points(ci[1,],pch='-',col=2)
  points(ci[2,],pch='-',col=2)
  abline(h=0,lty=2)
  return(cors)
}

rtoz = function(r){
  return(1/2*log((1+r)/(1-r)))
}

ztor = function(z){
  return( ( exp(2*z)-1 ) / ( exp(2*z)+1 ) )
}

# names_in = unique(csv_in[[1]])
# names_out = unique(csv_out[[1]])
# 
# filt = setdiff(names_in,names_out)
# 
# i=0
# wrong = c()
# for(id in names_in){
#   i=i+1
#   print(i)
#   print(id)
#   ts = csv_in[[3]][csv_in[[1]]==id]
#   if(any(ts<(-5) ) || any(ts>1000)){
#     print('aaa')
#     wrong = c(wrong,id)
#   }
# }



# days_ts = list()
# days = list()
# all_days = c()
# weeks = list()
# all_weeks = c()
# months = list()
# all_months = c()
# trends = c()
# path='/home/pant/Desktop/event_plots/all plots/'
# for(i in 1:length(tss)){
#   print(i)
#   days_ts[[i]] = create_days_ts(tss[[i]])
#   days[[i]] = create_days(tss[[i]])
#   all_days=c(all_days,days[[i]])
#   weeks[[i]] = create_weeks(days[[i]])
#   all_weeks=c(all_weeks,weeks[[i]])
#   months[[i]] = create_months(days[[i]])
#   all_months = c(all_months, months[[i]])
#   trends = c(trends, find_trend(months[[i]]))
  # png(filename=paste(path,i,'-a-days.png',sep=''),800,600)
  # plot(days[[i]],xlab='time (days)',ylab='consumption (litres)')
  # title(i)
  # dev.off()
  # png(filename=paste(path,i,'-b-weeks.png',sep=''),800,600)
  # plot(weeks[[i]],xlab='time (weeks)',ylab='consumption (litres)')
  # title(i)
  # dev.off()
  # png(filename=paste(path,i,'-c-months.png',sep=''),800,600)
  # plot(months[[i]],xlab='time (months)',ylab='consumption (litres)')
  # title(i)
  # dev.off()

# }

# png('/home/pant/Desktop/event_plots/all_days.png',800,600)
# hist(all_days,100,xlab='co Ep;ishnsumption (litres)')
# dev.off()
# png('/home/pant/Desktop/event_plots/all_weeks.png',800,600)
# hist(all_weeks,100,xlab='consumption (litres)')
# dev.off()
# png('/home/pant/Desktop/event_plots/all_months.png',800,600)
# hist(all_months,100,xlab='consumption (litres)')
# dev.off()

all_dates = function(alal){
  ids=unique(alal[[1]])
  times = c()
  count = 1
  for(id in ids){
    print(count)
    count=count+1
    dates=as.character(sort(as.Date(alal[[2]][alal[[1]]==id],"%d/%m/%Y")))
    times = c(times, id, dates[1], dates[length(dates)])
  }
  return(matrix(times,nrow=3));
}

mc_dens = function(points,range,test_range,test_points,iter=10000){
  ocur = c()
  for(i in 1:iter){
    samp = runif(points)*range
    ocur = c(ocur,scan_dens(samp,range,test_range,test_points))
  }
  return(sum(ocur)/iter)
}

scan_dens = function(samp,range,test_range,test_points){
  samp = sort(samp)
  sums = c()
  for(i in 1:length(samp)){
     if(samp[i]+test_range>max(samp))
       break
     sums=c(sums, sum(samp>=samp[i] & samp<=samp[i]+test_range))
  }
    
  if(any(sums<test_points))
    check=1
  else
    check=0
  
  return(check)
}

tai = function(d){
  d = as.matrix(d)
  dv = d[d[,3]>12,]
  dvh = dv[dv[,8]=='NO',]
  return(as.numeric(strftime(strptime(dvh[,2],format="%Y-%m-%d %H:%M:%S",tz=""),format="%H")))
}

get_patterns_day=function(day,thresh){
  pats = list()
  check=FALSE
  for(i in 1:24){
    if(day[i]>=thresh & !check){
      start=i
      check=TRUE
    }
    if(day[i]<thresh & check){
      end=i
      check=FALSE
      pats[[length(pats)+1]]=start:(end-1)
    }
  }
  return(pats)
}

merge_util = function(folder='/home/pant/Desktop/trial swm/users/'){
  
  users = list.files(path=folder)
  count = 1
  lengths = c()
  
  for (user in users) {
    
    print(count)
    
    wb = loadWorkbook(paste(folder,user,sep=''))
    sheets = getSheets(wb)
  
    for (sheet in names(sheets)) {
      if (!grepl("amphiro", sheet)) {
        swm = read.xlsx(paste(folder,user,sep=''), sheet)
      }
    }
    
    print(dim(swm))
    
   if(dim(swm)[1]==0)
     next
    
    email = gsub(".xlsx", "", user)
    
    if(count==1)
      out=cbind(email,swm)
    else
      out=rbind(out,cbind(email,swm))
    
    count=count+1
    
  }

  return(out)

  }

make_users = function(swm,surv){
  
  users = list()
  user = list()
  ids = intersect(swm[[1]],surv$SWM.ID)
 
   for(i in 1:length(ids)){
    
    print(i)
    user = list()
    user$id=ids[i]
   
    surv_row = surv[surv$SWM.ID==ids[i],]
    user$mail = surv_row$Email
    user$members = as.numeric(as.character(surv_row$How.many.members.are.there.in.your.household..1))
    user$showers_no = as.numeric(as.character(surv_row$How.many.hand.showers.are.there.in.your.home.))
    user$apartment_size = parse_apartment_size(surv_row$What.is.the.size.of.your.appartment.house.in.square.meters.feet.)
    user$child = as.numeric(as.character(surv_row$Is.there.any.minor.in.your.home.))
    user$male_rate = (as.numeric(as.character(surv_row$How.many.of.your.household.members.are.males.))-user$child)/user$members
    user$income = parse_income(surv_row$What.is.approximately.your.yearly.household.income.before.tax.)
    user$own_house = parse_rent(surv_row$Do.you.own.or.lease.your.residence.)
    user$consider = parse_consider(surv_row$I.consider.my.household.s.water.consumption.to.be.)
    
    user$cons = diff( rev(swm[[3]][swm[[1]]==user$id]) )
    user$dates = as.POSIXct(rev(swm[[2]][swm[[1]]==user$id])[-1],format="%d/%m/%Y %H:%M:%S",tz="UTC")
    user$days = split_days(user$dates)
    
    users[[i]] = user
  
  }
  
  return(users)

}

make_data = function(users,type='all'){
  
  x=c()
  
  for(user in users){
  
    if(mean(get_daily_sums(user)>10)<0.5 | missing_labels(user)>0.5)
      next
    
    res1 = count_days(user,type)
    
    x = c(x,res1$mean_cons,res1$zero_rate,res1$mean_max)#,res1$zero_rate,res1$mean_max,res2$zero_rate,res2$mean_max)
    x = c(x,user$members,user$showers_no,user$apartment_size,user$male_rate,user$child,user$income,user$own_house,user$consider)    
   
  }
  
  table = t(matrix(x,nrow=11))
  colnames(table)=c("mean_cons","zero_rate","mean_max","members","showrs_no","apart_size","male_rate","child","income","own_house","consider")
  
  return(table)

}

split_days = function(dates){
  days = list()
  index = 1
  day_set = c(1)
  for(i in 2:length(dates)){
    if(as.Date(format(dates[i],tz="CET"))==as.Date(format(dates[i-1],tz="CET")))
      day_set = c(day_set , i)
    else{
      days[[index]]=day_set
      index=index+1
      day_set=c(i)
    }
  }
  return(days)
}

count_days = function(user,type='all'){

  dates = user$dates
  cons = user$cons
  days = user$days
  daily_cons = get_daily_sums(user)
  zero_counts = 0
  all_counts = 0
  
  maxes = c()
  max_counts=0
  
  whole_cons = 0
  days_count = 0
  
  for(i in 2:length(days)){
    
    wday = as.POSIXlt(dates[days[[i]]][1])$wday
    
    if((type=='all' | (type=='weekdays' & wday %in% 1:5) | (type=='weekends' & wday %in% c(0,6))) && daily_cons[i]>10){

      tdiff = difftime(dates[days[[i]]],dates[days[[i]]-1],units='hours')
      zero_counts = zero_counts + sum(tdiff>0.75 & tdiff<1.15 & cons[days[[i]]]<5)
      all_counts = all_counts+sum(tdiff>0.75 & tdiff<1.15)
    
      clear_cons = cons[days[[i]]][tdiff>0.75 & tdiff<1.15]
      
      if(length(clear_cons)>=5) {
        maxes = c(maxes, sort(clear_cons,decreasing=TRUE)[1:3])
        max_counts = max_counts+1
      }
      
      whole_cons = whole_cons + sum(user$cons[days[[i]]])
      days_count = days_count+1
      
    }
    
  }
  
  res=list()
  res$zero_rate=zero_counts/all_counts
  res$mean_max=sum(maxes)/max_counts
  res$mean_cons=whole_cons/days_count
  
  if(is.nan(res$mean_cons))
    print(user$id)
  
  return(res)

}

get_daily_sums = function(user){
  sums = c()
  for(day in user$days)
    sums = c(sums,sum(user$cons[day]))
  return(sums)
}

parse_apartment_size = function(apart_size){
  if(apart_size=="de 31 a 60 metros cuadrados")
    return(45)
  else if (apart_size=="de 61 a 80 metros cuadrados")
    return(70)
  else if(apart_size=="de 81 a 110 metros cuadrados")
    return(95)
  else if(grepl('111',apart_size))
    return(125)
  else
    return(NA)
}

parse_income = function(inc){
  if(inc=="15.000€ - 20.000€")
    return(17500)
  else if(inc=="20.000€ - 25.000€")
    return(22500)
  else if(inc=="25.000€ - 30.000€")
    return(27500)
  else if(inc=="30.000€ - 35.000€")
    return(32500)
  else if(inc=="35.000€ - 40.000€")
    return(37500)
  else if(inc=="40.000€ - 50.000€")
    return(45000)
  else if(inc=="50.000€ - 60.000€")
    return(55000)
  else if(grepl('de 60.000€',inc))
    return(65000)
  else if(inc=="Menos de 15.000€")
    return(12500)
  else
    return(NA)
}

parse_rent = function(rent){
  if(rent=="Propietario")
    return(1)
  else
    return(0)
}

parse_consider = function(consider){
  if(consider=="Extremadamente bajo")
    return(0)
  else if(consider=="Bajo")
    return(1)
  else if(consider=="Normal")
    return(2)
  else if(consider=="Alto")
    return(3)
  else
    return(NA)
}

cor_ci = function(r,n){
  z = rtoz(r)
  up = z+1.96*sqrt(1/(n-3))
  down = z-1.96*sqrt(1/(n-3))
  up = ztor(up);
  down = ztor(down);
  return(rbind(up,down))
}

all_cis = function(x,cr){
  cri = matrix(0,11,11)
  for(i in 1:dim(cr)[1]){
    for(j in 1:dim(cr)[2]){
      if(i==j)
        next
      n_  = sum(is.finite(x[,i]) & is.finite(x[,j]))
      c_  = cr[i,j]
      print(c_)
      print(n_)
      ci = cor_ci(c_,n_)
      cri[i,j] = paste(as.character(c_),"  (",as.character(ci[2])," ",as.character(ci[1]),")",sep='')
    }
  }
  return(cri)
}

day_stats = function(user){
  counts = c()
  for(d in user$days_raw)
    counts = c(counts , length(d))
  return(counts)
}

all_day_stats = function(users){
  counts = c()
  for(user in users){
    counts = c(counts, mean(day_stats(user))) 
  }
  return(counts)
}

missing_distr = function(user){
  counts = 1:24*0
  all_hours = 1:24
  for(day in user$days){
    hours = get_hours(user,day)
    counts[all_hours[-(hours+1)]] = counts[all_hours[-(hours+1)]]+1
  }
  total_days = as.numeric(strsplit(format(user$dates[length(user$dates)]  - user$dates[1],format='%d'),' ')[[1]][1])
  return(counts/total_days)
}

get_hours = function(user,day){
  dates = user$dates[day]
  hours = c()
  for(i in 1:length(dates)){
    date = dates[i]
    hours = c(hours, as.numeric(format(date,format="%H",tz='CET')))
  }
  return(hours)
}

get_trial_ids = function(csv,trial_ids){
  trial_csv = csv[csv[[1]]==trial_ids[1],]
  trial_csv[[2]] = strptime(trial_csv[[2]],format="%d/%m/%Y %H:%M:%S",tz="UTC")
  trial_csv = trial_csv[order(trial_csv[[2]]),]
  d = diff(trial_csv[[3]])
  trial_csv = trial_csv[2:dim(trial_csv)[1],]
  trial_csv[[3]]=d
  hist_ids = unique(csv[[1]])
  for(i in 2:length(trial_ids)){
    print(i)
    if(! trial_ids[i] %in% hist_ids)
      next
    buf = csv[csv[[1]]==trial_ids[i],]
    buf[[2]] = strptime(buf[[2]],format="%d/%m/%Y %H:%M:%S",tz="UTC")
    buf = buf[order(buf[[2]]),]
    d = diff(buf[[3]])
    buf = buf[2:dim(buf)[1],]
    buf[[3]]=d
    trial_csv = rbind(trial_csv,buf)
  }
  return(trial_csv)
}

missing_labels = function(user,expected_days=100){
  return(1-length(user$cons)/(24*expected_days))
}

all_total_cons = function(csv){
  names = unique(csv[[1]])
  lot = c()
  for(name in names){
    cons = csv[[3]][csv[[1]]==name]
    lot = c(lot,cons[1]-cons[length(cons)])
  }
  return(lot)
}

create_days_ts = function(ts,dates){
  ts = ts[-(1:23)]
  dates = dates[-(1:23)]
  days_c = c()
  index = 24
  lindex = 1
  dates_out = list()
  for(i in 2:100){
    if(format(dates[index+5],'%Y-%m-%d')=='2013-10-27'){
      index=index+25
    }
    else if(format(dates[index+5],'%Y-%m-%d')=='2016-03-27'){
      index=index+23
    }
    else{
      days[[lindex]] = ts[index:(index+23)]
      days_c = c(days_c,as.numeric(ts[index:(index+23)]))
      dates_out[[lindex]]=dates[index+5]
      index=index+24
      lindex=lindex+1
    }
  }
  return(list(dates_out,t(matrix(days_c,nrow=24))))
}

get_weekdays_m = function(days,dates){
  out = c()
  for(i in 1:length(dates))
    if(dates[[i]]$wday %in% 1:5)
      out = rbind(out,days[i,])
    return(out)
}

get_weekends_m = function(days,dates){
  out = c()
  for(i in 1:length(dates))
    if(dates[[i]]$wday %in% c(0,6))
      out = rbind(out,days[i,])
    return(out)
}

get_days = function(user,day){
  out = c()
  dates = user$days_d
  days = user$days_m
  for(i in 1:length(dates))
    if(dates[[i]]$wday == day)
      out = rbind(out,days[i,])
    return(rowSums(out))
}

make_days_matrix = function(csv){
  
  ids = unique(csv[[1]])
  users = list()

  for(i in 1:length(ids)){
    print(i)
    users[[i]] = list()
    users[[i]]$id = ids[i]
    #users[[i]]$dates = as.POSIXlt(csv[[2]][csv[[1]]==ids[i]],format='%Y-%m-%d %H:%M',tz= 'UTC')
    users[[i]]$dates = rev(as.POSIXlt(csv[[2]][csv[[1]]==ids[i]],format='%d/%m/%Y %H:%M',tz= 'UTC'))[-1]
    users[[i]]$swm = diff(rev(csv[[3]][csv[[1]]==ids[i]]))
   # buf = create_days_ts( users[[i]]$swm, users[[i]]$dates )
  #  users[[i]]$days_m =  buf[[2]]
  #  users[[i]]$days_d = buf[[1]]
  #  users[[i]]$weekdays = get_weekdays_m(users[[i]]$days_m,users[[i]]$days_d)
  #  users[[i]]$weekends = get_weekends_m(users[[i]]$days_m,users[[i]]$days_d)
    
  }
  
  return(users)
  
}

perm_test = function(s1,s2,r=5000){
  
  diff = mean(s1) - mean(s2)
  null_diff = c()
  l = length(s1)
  
  for(i in 1:r){
    s = sample(c(s1,s2),replace=FALSE)
    null_diff = c(null_diff,mean(s[1:l])-mean(s[(l+1):length(s)]))
  }
  #plot1
  hist(null_diff,20,freq=FALSE)
  snd = sort(null_diff)
  abline(v=c(snd[round(r*0.025)],snd[round(r*0.975)]),col='blue')
  abline(v=c(snd[round(r*0.005)],snd[round(r*0.995)]),col='blue',lty=2)
  
  abline(v=diff,col='red')
  
  return(c(sum(as.numeric(abs(null_diff)>=abs(diff)))/r,t.test(s1,s2)$p.value))
}

plot_old_days = function(old_days){
  all_days = c()
  path = '/home/pant/Desktop/messages_plots/all days/'
  for(u in old_days){
    days = rowSums(u$days_m)
    png(paste(path,u$id,'.png',sep=''),1024,768)
    hist(days,30,xlab='daily consumption',main=paste('mean=',as.character(mean(days)),'sd=',as.character(sd(days)),sep=' '))
    dev.off()
    all_days = c(all_days,days)
  }
  png(paste(path,'all_days','.png',sep=''),1024,768)
  hist(all_days,600,xlim=c(-10,1500),xlab='consumption of day',main=paste('mean=',as.character(mean(all_days)),'sd=',as.character(sd(all_days)),sep=' '))
  dev.off()
}

zeros_cons_distr = function(mat){
  plot(colSums(mat>3),type='o')
}

bootstrap_thresh = function(dat,quant=0.99,reps=10000){
  lot = c()
  quant_index = quant*length(dat)
  for( i in 1:reps){
    dat_s = sample(dat,replace=TRUE)
    lot = c(lot,sort(dat_s)[quant_index])
    #lot = c(lot,mean(dat_s))
    #lot = c(lot,sd(dat_s))
  }
  hist(lot,20,freq=FALSE)
  sorted_lot = sort(lot)
  #abline(v=mean(dat),lty=2)
  #abline(v=sd(dat),lty=2)
  #abline(v=sort(dat)[quant_index],lty=2)
  abline(v=c(sorted_lot[0.025*reps],sorted_lot[0.975*reps]),lty=3)
  print(sorted_lot[0.975*reps]-sorted_lot[0.025*reps])
  print(sorted_lot[0.975*reps]-sorted_lot[0.025*reps]/sd(dat))
  return(lot)
}

get_shower_time = function(shower){
  end = as.POSIXlt(shower$Date.Time,tz='CET')
  start = end-shower$Duration
  return(list(start,end))
}

plot_file = function(data,path){
  png(path)
  plot(data)
  dev.off
}
