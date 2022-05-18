load("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/data/pass_model_5speeds.Rdata")
load("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/data/pass_summary.Rdata")

#perform quantile normalization
all_dat <- pass_speed[[1]]
combine_dat <-cbind("speed"=1,"id"=2,pass_speed[[1]][[2]])
for(j in 1:5){
  if(j!=1){
    combine_dat <- rbind(combine_dat, cbind("speed"=j,"id"=i,pass_speed[[j]][[2]]))
  }
  for(i in 3:length(all_dat)){
    if(length(all_dat[[i]])>0){
      combine_dat <- rbind(combine_dat, cbind("speed"=j,"id"=i,pass_speed[[j]][[i]]))
    }
  }
}

norm_dat <- sapply(combine_dat[,c()], function(x) quantile(x,na.rm=TRUE, probs = seq(0, 1, 1/nrow(combine_dat))))

which(norm_dat[,1]==combine_dat$all_ctrl[1]) 

all_rank = apply(combine_dat[,c(11,15,17)],2,rank)
quant_rank = all_rank/nrow(combine_dat)


#to adjust c("adj_pass_value","location_pass_value", "expected_pass_value")
