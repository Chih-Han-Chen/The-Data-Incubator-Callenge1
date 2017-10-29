

for(i_cate1 in 1:4){
  W<-c(1,1,1,1,1)
  
  i_cate<-1
  
  ###################################################SET UPS
  OF_data<-readRDS("/home/hans/Desktop/GAproject/OF_ready_data.rds")
  #USDA_data<-readRDS("/home/hans/Desktop/GAproject/USDA_ready_data.rds")
  #Tesco_data<-readRDS("/home/hans/Desktop/GAproject/Tesco_ready_data.rds")
  OF_data[[1]][OF_data[[1]][,1]==i_cate1,]
  i_cate<-1
  ####behaviour gausian
  
  x <- seq(0, 100, length=100)
  y <- dnorm(x, mean=50, sd=10)
  y<-y*(50/max(y))
  plot(x, y, type="l", lwd=2)
  ################organize data
  OF_cate<-OF_data[[i_cate]][,1]
  OF_name<-OF_data[[i_cate]][,2]
  OF_F1<-OF_data[[i_cate]][,3]
  OF_F2<-OF_data[[i_cate]][,4]
  OF_F3<-OF_data[[i_cate]][,5]
  OF_F4<-OF_data[[i_cate]][,6]
  OF_F5<-OF_data[[i_cate]][,7]
  
  
  #remove na data
  
  loc_na<-is.na(as.numeric(OF_F1))|is.na(as.numeric(OF_F2))|is.na(as.numeric(OF_F3))|is.na(as.numeric(OF_F4))|is.na(as.numeric(OF_F5))
  
  OF_cate<-OF_cate[!loc_na]
  OF_name<-OF_name[!loc_na]
  
  OF_F1<-OF_F1[!loc_na]
  OF_F2<-OF_F2[!loc_na]
  OF_F3<-OF_F3[!loc_na]
  OF_F4<-OF_F4[!loc_na]
  OF_F5<-OF_F5[!loc_na]
  
  ####Cut Off rendom generated
  OF_FF<-list(OF_F1,OF_F2,OF_F3,OF_F4,OF_F5)
  
  
  
  
  library(GA)
f <- function(z){
  W<-c(1,1,1,1,1)
  ###################################################SET UPS
  OF_data<-readRDS("/home/hans/Desktop/GAproject/OF_ready_data.rds")
  #USDA_data<-readRDS("/home/hans/Desktop/GAproject/USDA_ready_data.rds")
  #Tesco_data<-readRDS("/home/hans/Desktop/GAproject/Tesco_ready_data.rds")
  OF_data[[1]][OF_data[[1]][,1]==i_cate1,]
  i_cate<-1
  ####behaviour gausian
  
  x <- seq(0, 100, length=100)
  y <- dnorm(x, mean=50, sd=10)
  y<-y*(50/max(y))
  plot(x, y, type="l", lwd=2)
  ################organize data
  OF_cate<-OF_data[[i_cate]][,1]
  OF_name<-OF_data[[i_cate]][,2]
  OF_F1<-OF_data[[i_cate]][,3]
  OF_F2<-OF_data[[i_cate]][,4]
  OF_F3<-OF_data[[i_cate]][,5]
  OF_F4<-OF_data[[i_cate]][,6]
  OF_F5<-OF_data[[i_cate]][,7]
  
  
  #remove na data
  
  loc_na<-is.na(as.numeric(OF_F1))|is.na(as.numeric(OF_F2))|is.na(as.numeric(OF_F3))|is.na(as.numeric(OF_F4))|is.na(as.numeric(OF_F5))
  
  OF_cate<-OF_cate[!loc_na]
  OF_name<-OF_name[!loc_na]
  
  OF_F1<-OF_F1[!loc_na]
  OF_F2<-OF_F2[!loc_na]
  OF_F3<-OF_F3[!loc_na]
  OF_F4<-OF_F4[!loc_na]
  OF_F5<-OF_F5[!loc_na]
  
  ####Cut Off rendom generated
  OF_FF<-list(OF_F1,OF_F2,OF_F3,OF_F4,OF_F5)
  
  ###################################################SET UPS
  
  
  
  
  
  #z<-c(rep(10,5),rep(5,5),rep(1,5))####ex
  zh<-z[1:5]
  zm<-z[6:10]
  zl<-z[11:15]
  ####reorganize sort
  #i<-1
  for(i in 1:5){
  zt<-sort(c(zh[i],zm[i],zl[i]))
  zh[i]<-zt[3]
  zm[i]<-zt[2]
  zl[i]<-zt[1]
  }
  #assume input = z=(1:100 15)
  #i<-1
  for(i in 1:5){
  if(i==1){
    step_unit<-(summary(as.numeric(OF_FF[[i]]))[[6]]-summary(as.numeric(OF_FF[[i]]))[[1]])/100
    oth<-(as.numeric(step_unit)*zh[i])+summary(as.numeric(OF_FF[[i]]))[[1]]
    otm<-(as.numeric(step_unit)*zm[i])+summary(as.numeric(OF_FF[[i]]))[[1]]
    otl<-(as.numeric(step_unit)*zl[i])+summary(as.numeric(OF_FF[[i]]))[[1]]
    }else{
      step_unit<-(summary(as.numeric(OF_FF[[i]]))[[6]]-summary(as.numeric(OF_FF[[i]]))[[1]])/100
      oth<-c(oth,(as.numeric(step_unit)*zh[i])+summary(as.numeric(OF_FF[[i]]))[[1]])
      otm<-c(otm,(as.numeric(step_unit)*zm[i])+summary(as.numeric(OF_FF[[i]]))[[1]])
      otl<-c(otl,(as.numeric(step_unit)*zl[i])+summary(as.numeric(OF_FF[[i]]))[[1]])
      
  }
  }
  ot<-rbind(oth,otm,otl)
  #ot[1:3,1:5]
  if(sum(sum(!oth>otm),sum(!otm>otl))==0){### follow rules
  ### filter products and evaluate
    flag<-1
    ir1<-1#1~3
    for(ir1 in 1:3){
    ir2<-1#1~3
    for(ir2 in 1:3){
    ir3<-1#1~3
    for(ir3 in 1:3){
      
    ir4<-1#1~3
    for(ir4 in 1:3){
      
    ir5<-1#1~3
    for(ir5 in 1:3){
      
    loc_filtered<-(as.numeric(OF_FF[[1]])<ot[ir1,1])&(as.numeric(OF_FF[[2]])<ot[ir2,2])&(as.numeric(OF_FF[[3]])<ot[ir3,3])&(as.numeric(OF_FF[[4]])<ot[ir4,4])&(as.numeric(OF_FF[[5]])<ot[ir5,5])
    recom_pro_count<-sum(loc_filtered)
    recom_pro<-list(OF_FF[[1]][loc_filtered],OF_FF[[2]][loc_filtered],OF_FF[[3]][loc_filtered],OF_FF[[4]][loc_filtered],OF_FF[[5]][loc_filtered])
    ###prob of following
    prob_f<-(y[round(recom_pro_count/length(loc_filtered)*100)]/100)
    for(iii in 1:5){
    if(iii==1){
      Scores<-W[iii]*((mean(as.numeric(OF_FF[[iii]]))-((mean(as.numeric(recom_pro[[iii]]))*prob_f)+(mean(as.numeric(OF_FF[[iii]]))*(1-prob_f))))/mean(as.numeric(OF_FF[[iii]])))
    }else{
      Scores<-c(Scores,W[iii]*((mean(as.numeric(OF_FF[[iii]]))-((mean(as.numeric(recom_pro[[iii]]))*prob_f)+(mean(as.numeric(OF_FF[[iii]]))*(1-prob_f))))/mean(as.numeric(OF_FF[[iii]]))))
      
    }
      
    }
    
    if(flag==1){
    indi_scores<-mean(Scores)
    flag<-0
    }else{
      indi_scores<-c(indi_scores,mean(Scores))
    }
    }}}}}
    avg_Score<-mean(indi_scores)
  }else{## doesn't follow rules
    avg_Score<-0
  }
  
  return(avg_Score)
}
result <- ga(type = "real-valued",fitness=f,
             min=rep(1,15),max = rep(100,15),maxiter = 50)#,names=paste0("factor",1:15)


aa<-summary(result)$solution
if(length(aa)==15){
  z<-aa
  zh<-z[1:5]
  zm<-z[6:10]
  zl<-z[11:15]
  for(i in 1:5){
    zt<-sort(c(zh[i],zm[i],zl[i]))
    zh[i]<-zt[3]
    zm[i]<-zt[2]
    zl[i]<-zt[1]
  }
  z<-rbind(zh,zm,zl)
  
  
  
}else{
  z<-aa[,1]
  zh<-z[1:5]
  zm<-z[6:10]
  zl<-z[11:15]
  for(i in 1:5){
    zt<-sort(c(zh[i],zm[i],zl[i]))
    zh[i]<-zt[3]
    zm[i]<-zt[2]
    zl[i]<-zt[1]
  }
  z<-rbind(zh,zm,zl)
}



###################################################


for(i in 1:5){
  if(i==1){
    step_unit<-(summary(as.numeric(OF_FF[[i]]))[[6]]-summary(as.numeric(OF_FF[[i]]))[[1]])/100
    oth<-(as.numeric(step_unit)*zh[i])+summary(as.numeric(OF_FF[[i]]))[[1]]
    otm<-(as.numeric(step_unit)*zm[i])+summary(as.numeric(OF_FF[[i]]))[[1]]
    otl<-(as.numeric(step_unit)*zl[i])+summary(as.numeric(OF_FF[[i]]))[[1]]
  }else{
    step_unit<-(summary(as.numeric(OF_FF[[i]]))[[6]]-summary(as.numeric(OF_FF[[i]]))[[1]])/100
    oth<-c(oth,(as.numeric(step_unit)*zh[i])+summary(as.numeric(OF_FF[[i]]))[[1]])
    otm<-c(otm,(as.numeric(step_unit)*zm[i])+summary(as.numeric(OF_FF[[i]]))[[1]])
    otl<-c(otl,(as.numeric(step_unit)*zl[i])+summary(as.numeric(OF_FF[[i]]))[[1]])
    
  }
}
ot<-rbind(oth,otm,otl)
flag<-1
ir1<-1#1~3
for(ir1 in 1:3){
  ir2<-1#1~3
  for(ir2 in 1:3){
    ir3<-1#1~3
    for(ir3 in 1:3){
      
      ir4<-1#1~3
      for(ir4 in 1:3){
        
        ir5<-1#1~3
        for(ir5 in 1:3){
          
          loc_filtered<-(as.numeric(OF_FF[[1]])<ot[ir1,1])&(as.numeric(OF_FF[[2]])<ot[ir2,2])&(as.numeric(OF_FF[[3]])<ot[ir3,3])&(as.numeric(OF_FF[[4]])<ot[ir4,4])&(as.numeric(OF_FF[[5]])<ot[ir5,5])
          recom_pro_count<-sum(loc_filtered)
          recom_pro<-list(OF_FF[[1]][loc_filtered],OF_FF[[2]][loc_filtered],OF_FF[[3]][loc_filtered],OF_FF[[4]][loc_filtered],OF_FF[[5]][loc_filtered])
          ###prob of following
          prob_f<-(y[round(recom_pro_count/length(loc_filtered)*100)]/100)
          for(iii in 1:5){
            if(iii==1){
              Scores<-W[iii]*((mean(as.numeric(OF_FF[[iii]]))-((mean(as.numeric(recom_pro[[iii]]))*prob_f)+(mean(as.numeric(OF_FF[[iii]]))*(1-prob_f)))))
            }else{
              Scores<-c(Scores,W[iii]*((mean(as.numeric(OF_FF[[iii]]))-((mean(as.numeric(recom_pro[[iii]]))*prob_f)+(mean(as.numeric(OF_FF[[iii]]))*(1-prob_f))))))
              
            }
            
          }
          
          if(flag==1){
            indi_scores<-(Scores)
            flag<-0
            counter<-2
          }else{
            #indi_scores[[counter]]<-((Scores))
            indi_scores<-rbind(indi_scores,Scores)
            counter<-counter+1
            }
        }}}}}
improvement<-indi_scores
improvement_avg<-c((mean(improvement[,1])),mean(improvement[,2]),mean(improvement[3]),mean(improvement[,4]),mean(improvement[,5]))
OF_GAresult<-list(summary(result)$fitness,improvement_avg,improvement,z,ot,result)
links<-"/home/hans/Desktop/GAproject/OF_GAresult"
w_links<-paste(links,paste(i_cate1,".rds",sep = ""),sep = "")
saveRDS(OF_GAresult,w_links)
aa<-readRDS(w_links)
}
