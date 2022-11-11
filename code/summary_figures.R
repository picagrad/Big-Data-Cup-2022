load.libraries = c("rjson", "jsonlite", "tidyverse", "gganimate", "ggpmisc", "ggnewscale", "viridis", "tictoc", "scales", "ggforce", "cowplot", 'here')
install.lib = load.libraries[!load.libraries %in% installed.packages()]
for (libs in install.lib) {install.packages(libs, dependencies = TRUE)}
sapply(load.libraries, require, character = TRUE)

load(here('code','pass_model_4speeds.Rdata'))
load(here("code", "pass_summary.Rdata"))
# Calculate summary statistics over various speeds and angles

#0.05*10*180/pi gives us a range of about 30 degrees around the intended target
#change the 0.25 below 
theta_range = 0.1
theta_scale = 0.05
for(track_pick in 1:nrow(current_event)){
  ang_shot = current_event$ang_init[track_pick]
  
  #speed 45
  track_pass = pass_speed[[1]][[track_pick]]
  
  if(!is.null(track_pass)){
    current_event$passer_pass_value[track_pick] = track_pass[which(track_pass$t==0),"pass_value"]
    target_ang = track_pass[which.min(abs(track_pass$theta-ang_shot)),]
    all_within_ang = track_pass[!is.na(match(round(track_pass$theta,4),round(target_ang$theta+seq(-theta_range,theta_range,by=theta_scale),4))),]
    current_event$max_best_case_within_45[track_pick] = max(all_within_ang$best_case_pass_value)
    current_event$ang_best_case_overall_45[track_pick] = track_pass[which.max(track_pass$best_case_pass_value),"theta"]
    current_event$max_best_case_overall_45[track_pick] = track_pass[which.max(track_pass$best_case_pass_value),"best_case_pass_value"]
    current_event$max_keep_possession_within_45[track_pick] = max(all_within_ang$keep_possesion_prob)
    current_event$ang_keep_possession_overall_45[track_pick] = track_pass[which.max(track_pass$keep_possesion_prob),"theta"]
    current_event$max_keep_possession_overall_45[track_pick] = track_pass[which.max(track_pass$keep_possesion_prob),"keep_possesion_prob"]
    current_event$max_expected_pass_value_within_45[track_pick] = max(all_within_ang$expected_pass_value)
    current_event$ang_expected_pass_value_overall_45[track_pick] = track_pass[which.max(track_pass$expected_pass_value),"theta"]
    current_event$max_expected_pass_value_overall_45[track_pick] =track_pass[which.max(track_pass$expected_pass_value),"expected_pass_value"]
    
    #speed 65
    track_pass = pass_speed[[2]][[track_pick]]
    target_ang = track_pass[which.min(abs(track_pass$theta-ang_shot)),]
    all_within_ang = track_pass[!is.na(match(round(track_pass$theta,4),round(target_ang$theta+seq(-theta_range,theta_range,by=theta_scale),4))),]
    current_event$max_best_case_within_65[track_pick] = max(all_within_ang$best_case_pass_value)
    current_event$ang_best_case_overall_65[track_pick] = track_pass[which.max(track_pass$best_case_pass_value),"theta"]
    current_event$max_best_case_overall_65[track_pick] =track_pass[which.max(track_pass$best_case_pass_value),"best_case_pass_value"]
    current_event$max_keep_possession_within_65[track_pick] = max(all_within_ang$keep_possesion_prob)
    current_event$ang_keep_possession_overall_65[track_pick] = track_pass[which.max(track_pass$keep_possesion_prob),"theta"]
    current_event$max_keep_possession_overall_65[track_pick] = track_pass[which.max(track_pass$keep_possesion_prob),"keep_possesion_prob"]
    current_event$max_expected_pass_value_within_65[track_pick] = max(all_within_ang$expected_pass_value)
    current_event$ang_expected_pass_value_overall_65[track_pick] = track_pass[which.max(track_pass$expected_pass_value),"theta"]
    current_event$max_expected_pass_value_overall_65[track_pick] = track_pass[which.max(track_pass$expected_pass_value),"expected_pass_value"]
    
    #speed 85
    track_pass = pass_speed[[3]][[track_pick]]
    target_ang = track_pass[which.min(abs(track_pass$theta-ang_shot)),]
    all_within_ang = track_pass[!is.na(match(round(track_pass$theta,4),round(target_ang$theta+seq(-theta_range,theta_range,by=theta_scale),4))),]
    current_event$max_best_case_within_85[track_pick] = max(all_within_ang$best_case_pass_value)
    current_event$ang_best_case_overall_85[track_pick] = track_pass[which.max(track_pass$best_case_pass_value),"theta"]
    current_event$max_best_case_overall_85[track_pick] = track_pass[which.max(track_pass$best_case_pass_value),"best_case_pass_value"]
    current_event$max_keep_possession_within_85[track_pick] = max(all_within_ang$keep_possesion_prob)
    current_event$ang_keep_possession_overall_85[track_pick] = track_pass[which.max(track_pass$keep_possesion_prob),"theta"]
    current_event$max_keep_possession_overall_85[track_pick] = track_pass[which.max(track_pass$keep_possesion_prob),"keep_possesion_prob"]
    current_event$max_expected_pass_value_within_85[track_pick] = max(all_within_ang$expected_pass_value)
    current_event$ang_expected_pass_value_overall_85[track_pick] = track_pass[which.max(track_pass$expected_pass_value),"theta"]
    current_event$max_expected_pass_value_overall_85[track_pick] = track_pass[which.max(track_pass$expected_pass_value),"expected_pass_value"]
    
    #Original speed which is labeled "speed Vel_init"
    track_pass = pass_speed[[4]][[track_pick]]
    target_ang = track_pass[which.min(abs(track_pass$theta-ang_shot)),]
    all_within_ang = track_pass[!is.na(match(round(track_pass$theta,4),round(target_ang$theta+seq(-theta_range,theta_range,by=theta_scale),4))),]
    current_event$max_best_case_within_Vel_init[track_pick] = max(all_within_ang$best_case_pass_value)
    current_event$ang_best_case_overall_Vel_init[track_pick] = track_pass[which.max(track_pass$best_case_pass_value),"theta"]
    current_event$max_best_case_overall_Vel_init[track_pick] = track_pass[which.max(track_pass$best_case_pass_value),"best_case_pass_value"] 
    current_event$max_keep_possession_within_Vel_init[track_pick] = max(all_within_ang$keep_possesion_prob)
    current_event$ang_keep_possession_overall_Vel_init[track_pick] = track_pass[which.max(track_pass$keep_possesion_prob),"theta"]
    current_event$max_keep_possession_overall_Vel_init[track_pick] = track_pass[which.max(track_pass$keep_possesion_prob),"keep_possesion_prob"]
    current_event$max_expected_pass_value_within_Vel_init[track_pick] = max(all_within_ang$expected_pass_value)
    current_event$ang_expected_pass_value_overall_Vel_init[track_pick] = track_pass[which.max(track_pass$expected_pass_value),"theta"]
    current_event$max_expected_pass_value_overall_Vel_init[track_pick] =track_pass[which.max(track_pass$expected_pass_value),"expected_pass_value"]
  }
}

current_event %>% filter(!is.na(event_successful)) %>% filter(!is.na(max_keep_possession_within_Vel_init)) %>%
ggplot(aes(x=event_successful, y=max_keep_possession_within_Vel_init,fill=event_successful))+ 
  geom_boxplot(alpha=0.8,
             outlier.size=2, notch=FALSE)+
  theme_bw() + 
  scale_fill_brewer(palette="BuPu")+
  labs(x='',y='Successful Pass Probability',title='Outcome vs. Successful Pass Probability')

t = current_event %>% filter(!is.na(event_successful)) %>% filter(!is.na(max_keep_possession_within_Vel_init))
t.test(t$max_keep_possession_within_Vel_init~t$event_successful)

  

current_event %>% filter(!is.na(time_to_shot)) %>% filter(!is.na(max_expected_pass_value_within_Vel_init)) %>%
ggplot(aes(y=(as.numeric(time_to_shot)), x=max_expected_pass_value_within_Vel_init))+
  labs(x='Expected Pass Value', y='Time till Shot on Goal',title='Expected Pass Value vs. Time to Shot')+
  geom_point()+
  theme_bw() + 
  geom_smooth(method = "lm", formula= (y ~ log(x)), se = F, colour = "gold")
  

simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

library(pROC)
rare_success <- t$event_successful=="t"
guess_not <- t$max_keep_possession_within_Vel_init
var1=roc(rare_success, guess_not)
var1$specificities = 1-var$specificities

plot(x=var$specificities, y=var$sensitivities, xlim=c(0,1), ylim=c(0,1),pty='s', type='l', ylab='True Positive Rate', xlab='False Positive Rate', main = 'ROC Curve - "Successful Pass Probability"')
abline(a=0,b=1,lty=2, ylim=c(0,1), xlim=c(0,1), col='green')
text(x=0.2,y=0.85, labels=paste("AUC=",round(var$auc[1],3)))
## 
## Call:
## roc.default(response = rare_success, predictor = guess_not)
## 
## Data: guess_not in 1978 controls (rare_success FALSE) < 22 cases (rare_success TRUE).
## Area under the curve: 0.5
simp_roc <- simple_roc(rare_success, guess_not)
with(simp_roc, lines(1 - FPR, TPR, col="blue", lty=2, xlim=c(1,0), ylim=c(0,1)))

