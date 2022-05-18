load.libraries = c("rjson", "jsonlite", "tidyverse", "gganimate","ggpmisc","ggnewscale","viridis","tictoc","scales","gifski")
install.lib = load.libraries[!load.libraries %in% installed.packages()]
for (libs in install.lib) {install.packages(libs, dependencies = TRUE)}
sapply(load.libraries, require, character = TRUE)

#time_step = seq(0.01,10,0.01) #extra fine scale
time_step = seq(0.05,10,0.05) #regular scale
#theta_scale = 0.01 #extra fine scale
theta_scale = 0.05 #regular scale
#speed_puck = 90 #ft/s
speed_puck = 55 #ft/s
#speed_puck = 40 #ft/s

#source("C:/Users/thepi/OneDrive/Documents/Python/Hockey/Big-Data-Cup-2022-Private/code/visualization/hockey_pipeline.R")
#setwd("C:/Users/thepi/OneDrive/Documents/Python/Hockey/Big-Data-Cup-2022-Private")
source("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/code/visualization/hockey_pipeline.R") 
setwd("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private")
#json_file <- "data/BDC_2022_all_data.json"
json_file <- "data/BDC_2022_all_direct_wFrames.json"
#json_file <- "data/BDC_2022_passOnly.json"
dat <- fromJSON(json_file)

# Set up the event data to be in a data frame. 
json_events <- lapply(dat[1:28], function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})
current_event <- as.data.frame(do.call("cbind", json_events))
current_event$team_name[current_event$team_name=="Olympic (Women) - Canada"]="Canada"
current_event$team_name[current_event$team_name=="Olympic (Women) - United States"]="Switzerland"
current_event$team_name[current_event$team_name=="Olympic (Women) - Finland"]="Finland"
current_event$team_name[current_event$team_name=="Olympic (Women) - Olympic Athletes from Russia"]="ROC"
current_event$team_name[current_event$team_name=="Olympic (Women) - Switzerland"]="Switzerland"

current_event$x_coord = current_event$x_coord %>% as.double()
current_event$y_coord = current_event$y_coord %>% as.double()
current_event$x_coord_2 = current_event$x_coord_2 %>% as.double()
current_event$y_coord_2 = current_event$y_coord_2 %>% as.double()

current_event$x_coord[current_event$x_coord < 100] = 200-current_event$x_coord[current_event$x_coord < 100]
current_event$x_coord_2[which(current_event$x_coord_2 < 100)] = 200-current_event$x_coord_2[which(current_event$x_coord_2 < 100)]

current_event$y_coord = 85-current_event$y_coord
current_event$y_coord_2 = 85-current_event$y_coord_2

vel_ang = calc_vmag_ang(current_event)
current_event$vel_init = vel_ang[,1]
current_event$ang_init = vel_ang[,2]

t_track=393 # 11, 393, 405 is right before a goal, 221 passer not in view
#69 is the first images I shared

line1 <- paste('dat$tracks$\'',(t_track-1),'\'',sep='')
line2 <- eval(parse(text=line1))
if(length(line2$frame_id)!=0){
  json_tracks <- lapply(line2, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  current_track <- as.data.frame(do.call("cbind", json_tracks))
}
current_track$frame_id = current_track$frame_id %>% as.integer()
current_track$period = current_track$period %>% as.integer()
current_track$track_id = current_track$track_id %>% as.integer()
current_track$jersey_number = current_track$jersey_number %>% as.integer()
current_track$x_ft = current_track$x_ft %>% as.double()
current_track$y_ft = current_track$y_ft %>% as.double()
current_track$vel_x = current_track$vel_x %>% as.double()
current_track$vel_y = current_track$vel_y %>% as.double()

current_track$vel_x[which(current_track$vel_x==0)]=0.05
current_track$vel_y[which(current_track$vel_y==0)]=0.05
current_track$goalie = ifelse(current_track$goalie =='TRUE',T,F)

if(current_track$x_ft[1]<100){
  current_track$x_ft = 200 - current_track$x_ft
  current_track$vel_x = -current_track$vel_x
}

all_frames = current_track$frame_id %>% unique()

save_data <- matrix(ncol=21,nrow=0)
colnames(save_data)<- c("frame_id","x_puck","y_puck","column_label","theta","x","y","t","all_ctrl","score_prob","pass_value", "adj_pass_value","off_prob", "def_prob",
                        "none_prob","location_pass_value","keep_possesion_prob", "expected_pass_value","max_pass_value", "best_case_pass_value",
                        "successful_pass_prob")
for(pick_frame in all_frames){
  new_track = current_track[current_track$frame_id==pick_frame,]
  
  event_info <- current_event[t_track,] %>% select(team_name,Player_1_num,Player_2_num)
  off <- event_info$team_name
  puck = new_track[new_track$jersey_number==event_info$Player_1_num,] %>% select(team_name,x_ft,y_ft)
  puck = puck[puck$team_name==off,]
  
  n_players <- table(new_track$team_name)
  n_off <- sum(n_players[names(n_players)==off])
  n_def <- sum(n_players[names(n_players)!=off])
  
  x_p = puck$x_ft #current_event[t_track,"x_coord"]
  y_p = puck$y_ft #current_event[t_track,"y_coord"]
  
  if(abs(y_p-puck$y_ft)>20){
    y_p = 85-y_p
  }
  
  theta = seq(-pi,pi,by=theta_scale)
  passes = data.frame(angle=c(),x=c(),y=c(), t=c())
  for(angle in theta){
    passes = rbind(passes,cbind(angle,puck_motion_model2(x_p,y_p,angle)))
  }
  new_pass <- clean_pass(passes)
  calc_pass <- new_pass %>% group_by(angle) %>% top_n(1, t)
  
  new_pass <- rbind(new_pass,c(new_pass$angle[1],x_p,y_p,0))
  
  xyt <- new_pass %>% select(x,y,t)
  loc_vel <- new_track %>% mutate(team_label = ifelse(team_name == off,1,-1)) %>%
    select(x_ft,y_ft,vel_x,vel_y, team_label, goalie) 
  # options(warn=-1)
  new_pass <- new_pass %>% 
    mutate(all_ctrl = teamwise_ice_ctrl_xyt(loc_vel, xyt), score_prob = apply(xyt,1,score_prob)) %>%
    mutate(pass_value = score_prob * ((all_ctrl+1)/2)^1)
  #fix tracks!!!
  all_point_val <- apply(calc_pass,1,probs_to_point,x_puck=x_p,y_puck=y_p,all_ang=new_pass,tracks1=current_track,offence=off,want_plot=FALSE)
  pass_potential <- bind_rows(all_point_val, .id = "column_label")
  pass2 <- cbind(pick_frame,x_p,y_p,pass_potential)
  colnames(pass2) <- c("frame_id","x_puck","y_puck","column_label","theta","x","y","t","all_ctrl","score_prob","pass_value", "adj_pass_value","off_prob", "def_prob",
                          "none_prob","location_pass_value","keep_possesion_prob", "expected_pass_value","max_pass_value", "best_case_pass_value",
                          "successful_pass_prob")
  save_data <- rbind(save_data,pass2)
  print(pick_frame)
}

colnames(save_data)<- c("frame_id","x_puck","y_puck","column_label","theta","x","y","t","all_ctrl","score_prob","pass_value", "adj_pass_value","off_prob", "def_prob",
  "none_prob","location_pass_value","keep_possesion_prob", "expected_pass_value","max_pass_value", "best_case_pass_value",
  "successful_pass_prob")



  ## ANIMATED PLOTS ##
  
  # Set the specs for the gif we want to create (lower res to make it run quicker)
  options(gganimate.dev_args = list(width = 10, height = 6, units = 'in', res = 320))
  
  p = ggplot(data = save_data,aes(x = x, y = y)) + 
    geom_point(aes(color = best_case_pass_value),size=2, shape=16) +
    scale_colour_gradient2(na.value="white",low = muted("green"), mid = "white", high = muted("purple"))+#,midpoint = 0.5
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #geom_point(aes(x = x, y = y), size = 1, shape = 4)+
    geom_text(aes(x = 130, y = 40, label = frame_id), size = 5) +
    new_scale_fill() +
    new_scale_color() +
    geom_point(data=current_track,aes(x = x_ft, y = y_ft, fill = team_name), size = 5, shape = 21) +
    geom_text(data=current_track,aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
    #geom_segment(aes(x = x_puck, y = y_puck, xend = puck$x_ft, yend = puck$y_ft),size=1.5, colour='brown')+
    #geom_point(aes(x = x_puck, y = y_puck), size = 2, shape = 16, colour='black') +
    scale_colour_manual(values = c("Switzerland" = "black", "Finland" = "white")) +
    scale_fill_manual(values = c("Switzerland" = "yellow", "Finland" = "blue")) +
    geom_segment(data=current_track,aes(x = x_ft, y = y_ft, xend = x_ft+vel_x, yend = y_ft+vel_y), #/sqrt(vel_x^2+vel_y^2) to get r=1
                 arrow = arrow(length = unit(0.2, "cm")),size=1, colour='cyan') +
    transition_time(frame_id) + 
    labs(fill = "Team") +
    guides(colour = "none")
  p2 = plot_half_rink(p)
  
  
  # Get the maximum and minimum frame number (so we know how long the gif should be)
  max_frame = current_track$frame_id %>% max()
  min_frame = current_track$frame_id %>% min() 
  
    #Save as gif
  #anim_save("all_ctrl.gif", p2, fps = 30, duration = (max_frame - min_frame+1)/30)
  animate(p2, duration = (max_frame - min_frame+1)/30, fps = 30, renderer = gifski_renderer())
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/best_case.gif")

  p3 = animate(p2, renderer = ffmpeg_renderer(), fps = 30, duration = (max_frame - min_frame+1)/30)#((max_frame - min_frame)/30 + 1))
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/best_case.mp4", p3)
  
  p = ggplot(data = save_data,aes(x = x, y = y)) + 
    geom_point(aes(color = keep_possesion_prob),size=2, shape=16) +
    scale_colour_gradient2(na.value="white",low = muted("green"), mid = "white", high = muted("purple"))+#,midpoint = 0.5
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #geom_point(aes(x = x, y = y), size = 1, shape = 4)+
    geom_text(aes(x = 130, y = 40, label = frame_id), size = 5) +
    new_scale_fill() +
    new_scale_color() +
    geom_point(data=current_track,aes(x = x_ft, y = y_ft, fill = team_name), size = 5, shape = 21) +
    geom_text(data=current_track,aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
    #geom_segment(aes(x = x_puck, y = y_puck, xend = puck$x_ft, yend = puck$y_ft),size=1.5, colour='brown')+
    #geom_point(aes(x = x_puck, y = y_puck), size = 2, shape = 16, colour='black') +
    scale_colour_manual(values = c("Switzerland" = "black", "Finland" = "white")) +
    scale_fill_manual(values = c("Switzerland" = "yellow", "Finland" = "blue")) +
    geom_segment(data=current_track,aes(x = x_ft, y = y_ft, xend = x_ft+vel_x, yend = y_ft+vel_y), #/sqrt(vel_x^2+vel_y^2) to get r=1
                 arrow = arrow(length = unit(0.2, "cm")),size=1, colour='cyan') +
    transition_time(frame_id) + 
    labs(fill = "Team") +
    guides(colour = "none")
  p2 = plot_half_rink(p)
  
  
  # Get the maximum and minimum frame number (so we know how long the gif should be)
  max_frame = current_track$frame_id %>% max()
  min_frame = current_track$frame_id %>% min() 
  
  #Save as gif
  #anim_save("all_ctrl.gif", p2, fps = 30, duration = (max_frame - min_frame+1)/30)
  animate(p2, duration = (max_frame - min_frame+1)/30, fps = 30, renderer = gifski_renderer())
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/keep_possesion_prob.gif")
  
  p3 = animate(p2, renderer = ffmpeg_renderer(), fps = 30, duration = (max_frame - min_frame+1)/30)#((max_frame - min_frame)/30 + 1))
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/keep_possesion_prob.mp4", p3)
  
  p = ggplot(data = save_data,aes(x = x, y = y)) + 
    geom_point(aes(color = expected_pass_value),size=2, shape=16) +
    scale_colour_gradient2(na.value="white",low = muted("green"), mid = "white", high = muted("purple"))+#,midpoint = 0.5
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #geom_point(aes(x = x, y = y), size = 1, shape = 4)+
    geom_text(aes(x = 130, y = 40, label = frame_id), size = 5) +
    new_scale_fill() +
    new_scale_color() +
    geom_point(data=current_track,aes(x = x_ft, y = y_ft, fill = team_name), size = 5, shape = 21) +
    geom_text(data=current_track,aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
    #geom_segment(aes(x = x_puck, y = y_puck, xend = puck$x_ft, yend = puck$y_ft),size=1.5, colour='brown')+
    #geom_point(aes(x = x_puck, y = y_puck), size = 2, shape = 16, colour='black') +
    scale_colour_manual(values = c("Switzerland" = "black", "Finland" = "white")) +
    scale_fill_manual(values = c("Switzerland" = "yellow", "Finland" = "blue")) +
    geom_segment(data=current_track,aes(x = x_ft, y = y_ft, xend = x_ft+vel_x, yend = y_ft+vel_y), #/sqrt(vel_x^2+vel_y^2) to get r=1
                 arrow = arrow(length = unit(0.2, "cm")),size=1, colour='cyan') +
    transition_time(frame_id) + 
    labs(fill = "Team") +
    guides(colour = "none")
  p2 = plot_half_rink(p)
  
  
  # Get the maximum and minimum frame number (so we know how long the gif should be)
  max_frame = current_track$frame_id %>% max()
  min_frame = current_track$frame_id %>% min() 
  
  #Save as gif
  #anim_save("all_ctrl.gif", p2, fps = 30, duration = (max_frame - min_frame+1)/30)
  animate(p2, duration = (max_frame - min_frame+1)/30, fps = 30, renderer = gifski_renderer())
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/expected_pass_value.gif")
  
  p3 = animate(p2, renderer = ffmpeg_renderer(), fps = 30, duration = (max_frame - min_frame+1)/30)#((max_frame - min_frame)/30 + 1))
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/expected_pass_value.mp4", p3)

  p = ggplot(data = save_data,aes(x = x, y = y)) + 
    geom_point(aes(color = off_prob),size=2, shape=16) +
    scale_colour_gradient2(na.value="white",low = muted("green"), mid = "white", high = muted("purple"))+#,midpoint = 0.5
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #geom_point(aes(x = x, y = y), size = 1, shape = 4)+
    geom_text(aes(x = 130, y = 40, label = frame_id), size = 5) +
    new_scale_fill() +
    new_scale_color() +
    geom_point(data=current_track,aes(x = x_ft, y = y_ft, fill = team_name), size = 5, shape = 21) +
    geom_text(data=current_track,aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
    #geom_segment(aes(x = x_puck, y = y_puck, xend = puck$x_ft, yend = puck$y_ft),size=1.5, colour='brown')+
    #geom_point(aes(x = x_puck, y = y_puck), size = 2, shape = 16, colour='black') +
    scale_colour_manual(values = c("Switzerland" = "black", "Finland" = "white")) +
    scale_fill_manual(values = c("Switzerland" = "yellow", "Finland" = "blue")) +
    geom_segment(data=current_track,aes(x = x_ft, y = y_ft, xend = x_ft+vel_x, yend = y_ft+vel_y), #/sqrt(vel_x^2+vel_y^2) to get r=1
                 arrow = arrow(length = unit(0.2, "cm")),size=1, colour='cyan') +
    transition_time(frame_id) + 
    labs(fill = "Team") +
    guides(colour = "none")
  p2 = plot_half_rink(p)
  
  
  # Get the maximum and minimum frame number (so we know how long the gif should be)
  max_frame = current_track$frame_id %>% max()
  min_frame = current_track$frame_id %>% min() 
  
  #Save as gif
  #anim_save("all_ctrl.gif", p2, fps = 30, duration = (max_frame - min_frame+1)/30)
  animate(p2, duration = (max_frame - min_frame+1)/30, fps = 30, renderer = gifski_renderer())
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/off_prob.gif")
  
  p3 = animate(p2, renderer = ffmpeg_renderer(), fps = 30, duration = (max_frame - min_frame+1)/30)#((max_frame - min_frame)/30 + 1))
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/off_prob.mp4", p3)
  
  p = ggplot(data = save_data,aes(x = x, y = y)) + 
    geom_point(aes(color = adj_pass_value),size=2, shape=16) +
    scale_colour_gradient2(na.value="white",low = muted("green"), mid = "white", high = muted("purple"))+#,midpoint = 0.5
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #geom_point(aes(x = x, y = y), size = 1, shape = 4)+
    geom_text(aes(x = 130, y = 40, label = frame_id), size = 5) +
    new_scale_fill() +
    new_scale_color() +
    geom_point(data=current_track,aes(x = x_ft, y = y_ft, fill = team_name), size = 5, shape = 21) +
    geom_text(data=current_track,aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
    #geom_segment(aes(x = x_puck, y = y_puck, xend = puck$x_ft, yend = puck$y_ft),size=1.5, colour='brown')+
    #geom_point(aes(x = x_puck, y = y_puck), size = 2, shape = 16, colour='black') +
    scale_colour_manual(values = c("Switzerland" = "black", "Finland" = "white")) +
    scale_fill_manual(values = c("Switzerland" = "yellow", "Finland" = "blue")) +
    geom_segment(data=current_track,aes(x = x_ft, y = y_ft, xend = x_ft+vel_x, yend = y_ft+vel_y), #/sqrt(vel_x^2+vel_y^2) to get r=1
                 arrow = arrow(length = unit(0.2, "cm")),size=1, colour='cyan') +
    transition_time(frame_id) + 
    labs(fill = "Team") +
    guides(colour = "none")
  p2 = plot_half_rink(p)
  
  
  # Get the maximum and minimum frame number (so we know how long the gif should be)
  max_frame = current_track$frame_id %>% max()
  min_frame = current_track$frame_id %>% min() 
  
  #Save as gif
  #anim_save("all_ctrl.gif", p2, fps = 30, duration = (max_frame - min_frame+1)/30)
  animate(p2, duration = (max_frame - min_frame+1)/30, fps = 30, renderer = gifski_renderer())
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/adj_pass_value.gif")
  
  p3 = animate(p2, renderer = ffmpeg_renderer(), fps = 30, duration = (max_frame - min_frame+1)/30)#((max_frame - min_frame)/30 + 1))
  anim_save("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/images/adj_pass_value.mp4", p3)