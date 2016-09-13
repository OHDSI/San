runSan<-function(hosp1, hosp2){
  strHosp1<-c(0,0,0)
  strHosp2<-c(0,0,0)
  
  mu_a1<-mean(hosp1[,3])
  mu_a2<-mean(hosp2[,3])
  
  for(i in 0:8){  
    if(i==8){
      a<-hosp1[hosp1[,1]>=(i*10),] #select specific age group of data source 1
      Male<-a[a[,2]=="M",] #select specific gender group (male) of data source 1
      Female<-a[a[,2]=="F",] #select specific gender group (female) of data source 1
      strHosp1<-rbind(strHosp1, c(nrow(Male), mean(Male[,3]), mean((Male[,3]-mu_a1)^2))) #save average and mean of deviations of result values per each age and gender group (data source 1)
      strHosp1<-rbind(strHosp1, c(nrow(Female), mean(Female[,3]), mean((Female[,3]-mu_a1)^2))) #save average and mean of deviations of result values per each age and gender group (data source 1)
      
      a<-hosp2[hosp2[,1]>=(i*10),] #select specific age group of data source 2
      Male<-a[a[,2]=="M",] #select specific gender group (male) of data source 2
      Female<-a[a[,2]=="F",] #select specific gender group (female) of data source 2
      strHosp2<-rbind(strHosp2, c(nrow(Male), mean(Male[,3]), mean((Male[,3]-mu_a2)^2))) #save average and mean of deviations of result values per each age and gender group (data source 2)
      strHosp2<-rbind(strHosp2, c(nrow(Female), mean(Female[,3]), mean((Female[,3]-mu_a2)^2))) #save average and mean of deviations of result values per each age and gender group (data source 2)
    }
    else {
      a<-hosp1[hosp1[,1]>=(i*10)&hosp1[,1]<((i+1)*10),] #select specific age group of data source 1
      Male<-a[a[,2]=="M",] #select specific gender group (male) of data source 1
      Female<-a[a[,2]=="F",] #select specific gender group (female) of data source 1
      strHosp1<-rbind(strHosp1, c(nrow(Male), mean(Male[,3]), mean((Male[,3]-mu_a1)^2))) #save average and mean of deviations of result values per each age and gender group (data source 1)
      strHosp1<-rbind(strHosp1, c(nrow(Female), mean(Female[,3]), mean((Female[,3]-mu_a1)^2))) #save average and mean of deviations of result values per each age and gender group (data source 1)
      
      a<-hosp2[hosp2[,1]>=(i*10)&hosp2[,1]<((i+1)*10),] #select specific age group of data source 2
      Male<-a[a[,2]=="M",] #select specific gender group (male) of data source 2
      Female<-a[a[,2]=="F",] #select specific gender group (female) of data source 2
      strHosp2<-rbind(strHosp2, c(nrow(Male), mean(Male[,3]), mean((Male[,3]-mu_a2)^2))) #save average and mean of deviations of result values per each age and gender group (data source 2)
      strHosp2<-rbind(strHosp2, c(nrow(Female), mean(Female[,3]), mean((Female[,3]-mu_a2)^2))) #save average and mean of deviations of result values per each age and gender group (data source 2)
    }  
  }
  
  strHosp1<-strHosp1[-1,] #remove the first row
  strHosp2<-strHosp2[-1,] #remove the first row
  
  #calculation of adjusted mean
  sw<-data.frame(strHosp2[,1],strHosp1[,2], strHosp1[,3])
  colnames(sw)<-c("count", "mean", "deviation")
  sw$groupsum<-sw$count*sw$mean 
  sw$groupdev<-sw$count*sw$deviation
  
  adjusted_Mean<-sum(sw$groupsum)/sum(sw$count)
  adjusted_SD<-sqrt(sum(sw$groupdev)/sum(sw$count))
  
  Ma<- adjusted_Mean 
  SDa<- adjusted_SD
  
  Mb<-mean(hosp2[,3])
  SDb<-sd(hosp2[,3])
  
  whosp1<-hosp1
  whosp2<-hosp2
  
  standardization<-function(x, Ma, SDa, Mb, SDb) {
    ((x-Mb)/SDb)*SDa+Ma
  }
  
  whosp1$SAN<-hosp1[,3] 
  whosp2$SAN<-standardization(hosp2[,3],Ma, SDa, Mb, SDb) #insert normalized laboratory test result values
  
  list(whosp1,whosp2)
}