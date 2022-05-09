
probs_to_point <- function(x_puck,y_puck, points1,all_ang, offence,want_plot=FALSE){
  points1=rbind(data.frame('theta'=c(),'x'=c(),'y'=c(),'t'=c()),points1)
  names(points1)=c('theta','x','y','t')
  points=rbind(data.frame('theta'=c(),'x'=c(),'y'=c(),'t'=c()),all_ang[which(all_ang$angle==points1$theta),])
  names(points)=c('theta','x','y','t')
  tracks=tracking_data
  tracks = tracks[-which.min((tracks$x_ft-x_puck)^2+(tracks$y_ft-y_puck)^2),]
  #give slight movement to avoid dividing by zero
  tracks$vel_x[which(tracks$vel_x==0)]=0.5
  tracks$vel_y[which(tracks$vel_y==0)]=0.5
  
  teams = tracks$team_name %>% unique()
  defence = teams[-which(teams==offence)]
  
  off_tracks = tracks %>% filter(team_name==offence)
  def_tracks = tracks %>% filter(team_name==defence)
  
  
  #point_val_off = 0
  #point_val_def = 0
  
  dist_to_points = matrix(nrow=nrow(points), ncol=nrow(tracks))
  for(player in 1:nrow(tracks)){
    dist_to_points[,player]=apply(points,1,dist_to_xyt,x0=tracks$x_ft[player],y0=tracks$y_ft[player],vx=tracks$vel_x[player],vy=tracks$vel_y[player])
  }
  #distance from point of interests for each player
  
  pickup_probs = abs(dnorm((dist_to_points+stick)/stick)-dnorm((dist_to_points-stick)/stick))
  new_data2 = cbind(points,pickup_probs)
  
  off_lines = which(tracks$team_name==offence)
  def_lines = which(tracks$team_name!=offence)
  off_probs = new_data2[,off_lines+4]
  def_probs = new_data2[,def_lines+4]
  
  off_mat = t(replicate(nrow(new_data2),tracks$team_name==offence))
  
  all_rank = t(apply(-pickup_probs,1,rank))
  rix <-  as.vector(t(replicate(ncol(pickup_probs),seq(1,nrow(pickup_probs),1))))
  
  ranked_probs <-  pickup_probs * 0
  ranked_probs[cbind(rix,as.vector(t(all_rank)))] <- as.vector(t(pickup_probs))
  
  ranked_probs[,2] = ranked_probs[,2]*(1-ranked_probs[,1])
  for (c in 3:ncol(ranked_probs)){
    ranked_probs[,c] = ranked_probs[,c]*(1-ranked_probs[,c-1])
  }
  
  ranked_off_mat <- off_mat * 0
  ranked_off_mat[cbind(rix,as.vector(t(all_rank)))] <- as.vector(t(off_mat))
  
  shot_probs <- data.frame(off = rowSums(ranked_probs * ranked_off_mat),
                           def = rowSums(ranked_probs * (1-ranked_off_mat))) %>% 
    mutate(None = 1 - off - def)
  
  # shot_probs2 <- shot_probs
  
  for (r in 2:nrow(shot_probs)){
    shot_probs[r,] = shot_probs[r,] * shot_probs$None[r-1]
  }
  
  shot_good = cumsum(shot_probs[,1])/(cumsum(shot_probs[,1])+cumsum(shot_probs[,2]))#-shot_probs[,3]
  
  if(want_plot){
    plot_pass=plot_half_rink(ggplot(tracks)) +
      geom_point(aes(x = x_ft, y = y_ft, fill = team_name), size = 5, shape = 21) +
      geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
      geom_segment(aes(x = x_ft, y = y_ft, xend = x_ft+vel_x, yend = y_ft+vel_y), #/sqrt(vel_x^2+vel_y^2) to get r=1
                   arrow = arrow(length = unit(0.2, "cm")),size=1, colour='cyan') + 
      geom_point(aes(x = x_puck+2, y = y_puck-0.5), size = 2, shape = 16, colour='black') + 
      geom_point(data = new_data2, aes(x = x, y = y), colour='dark grey',size = 1, shape = 16) +
      scale_colour_manual(values = c("USA" = "white", "Canada" = "white")) +
      scale_fill_manual(values = c("USA" = "blue", "Canada" = "red")) +
      geom_segment(data = points,aes(x = x_puck, y = y_puck, xend = x, yend = y),linetype=2)+
      geom_point(data = points, aes(x = x, y = y), size = 2, shape = 4, colour='dark grey') + 
      labs(fill = "Team") +
      guides(colour = "none") 
    return(list(cbind(new_data2,shot_probs,prob=shot_good),plot_pass))
  }else{
    return(cbind(new_data2,shot_probs,prob=shot_good))#list(diff_p,100*off_p/(off_p+def_p),off_probs,def_probs,off_max,def_max))
  }
}


# Create rink plot function
plot_rink = function(p_object){
  
  require(ggforce)
  require(cowplot)
  require(tidyverse)
  
  upper_outline = data.frame(
    x = c(
      115,
      172 + 28*sin(seq(0,pi/2,length=20)),
      172 + 28*sin(seq(pi/2,0,length=20)),
      115
    ),
    y = c(
      0, 
      0 + 28 - 28*cos(seq(0,pi/2,length=20)),
      85 - 28 + 28*cos(seq(pi/2,0,length=20)),
      85
    )
  )
  
  lower_outline = data.frame(
    x = c(
      115,
      100-72 - 28*sin(seq(0,pi/2,length=20)),
      100-72 - 28*sin(seq(pi/2,0,length=20)),
      115
    ),
    y = c(
      0, 
      0 + 28 - 28*cos(seq(0,pi/2,length=20)),
      85 - 28 + 28*cos(seq(pi/2,0,length=20)),
      85
    )
  )
  
  p = p_object +
    ## FACEOFF CIRCLES ##
    geom_circle(data = data.frame(x0 = 100, y0 = 42.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "gray50", inherit.aes = FALSE) +
    geom_circle(data = data.frame(x0 = 169, y0 = 20.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "gray50", inherit.aes = FALSE) +
    geom_circle(data = data.frame(x0 = 169, y0 = 64.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "gray50", inherit.aes = FALSE) +
    geom_circle(data = data.frame(x0 = 31, y0 = 64.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "gray50", inherit.aes = FALSE) +
    geom_circle(data = data.frame(x0 = 31, y0 = 20.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "gray50", inherit.aes = FALSE) +
    ## FACEOFF DOTS ##
    geom_point(inherit.aes = FALSE, aes(y = 42.5, x = 100), col = "gray50", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 20.5, x = 169), col = "gray50", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 64.5, x = 169), col = "gray50", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 20.5, x = 120), col = "gray50", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 64.5, x = 120), col = "gray50", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 20.5, x = 31), col = "gray50", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 64.5, x = 31), col = "gray50", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 20.5, x = 80), col = "gray50", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 64.5, x = 80), col = "gray50", size = 1) +
    ## BLUE AND RED LINES ##
    annotate("segment", col = "gray50",  x = 75, xend = 75, y = 0, yend = 85, lwd = 0.5) +
    annotate("segment", col = "gray50", x = 100, xend = 100, y = 0, yend = 85, lwd = 0.5) +
    annotate("segment", col = "gray50",  x = 125, xend = 125, y = 0, yend = 85, lwd = 0.5) +
    ## NET AND GOAL LINE ##
    geom_segment(col = "gray50", inherit.aes = FALSE, lwd = 0.5, aes(y = 79.25, x = 11, yend = 5.75, xend = 11)) +
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 7.5, yend = 45.5, xend = 7.5)) + 
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 7.5, yend = 39.5, xend = 11)) +  
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 45.5, x = 7.5, yend = 45.5, xend = 11)) +
    geom_segment(col = "gray50", inherit.aes = FALSE, lwd = 0.5, aes(y = 5.75, x = 189, yend = 79.25, xend = 189)) +
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 192.5, yend = 45.5, xend = 192.5)) + 
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 192.5, yend = 39.5, xend = 189)) +  
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 45.5, x = 192.5, yend = 45.5, xend = 189)) +
    ## OUTLINE ##
    geom_path(data = upper_outline, aes(x = x, y = y), colour = "gray80", inherit.aes = FALSE, lwd = 0.5) +
    geom_path(data = lower_outline, aes(x = x, y = y), colour = "gray80", inherit.aes = FALSE, lwd = 0.5) +
    ## ADDITIONAL SPECS ##
    scale_x_continuous(expand = c(0, 0), limits = c(0,200)) + scale_y_continuous(expand = c(0,0), limits = c(0,85)) +
    coord_fixed() +
    theme_void()
  
  return(p)
}

plot_half_rink = function(p_object){
  
  require(ggforce)
  require(cowplot)
  require(tidyverse)
  
  upper_outline = data.frame(
    x = c(
      115,
      172 + 28*sin(seq(0,pi/2,length=20)),
      172 + 28*sin(seq(pi/2,0,length=20)),
      115
    ),
    y = c(
      0, 
      0 + 28 - 28*cos(seq(0,pi/2,length=20)),
      85 - 28 + 28*cos(seq(pi/2,0,length=20)),
      85
    )
  )
  
  lower_outline = data.frame(
    x = c(
      115,
      100-72 - 28*sin(seq(0,pi/2,length=20)),
      100-72 - 28*sin(seq(pi/2,0,length=20)),
      115
    ),
    y = c(
      0, 
      0 + 28 - 28*cos(seq(0,pi/2,length=20)),
      85 - 28 + 28*cos(seq(pi/2,0,length=20)),
      85
    )
  )
  
  p = p_object +
    ## FACEOFF CIRCLES ##
    geom_circle(data = data.frame(x0 = 169, y0 = 20.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "gray50", inherit.aes = FALSE) +
    geom_circle(data = data.frame(x0 = 169, y0 = 64.5, r = 15), aes(x0 = x0, y0 = y0, r = r), lwd = 0.5, col = "gray50", inherit.aes = FALSE) +
    ## FACEOFF DOTS ##
    geom_point(inherit.aes = FALSE, aes(y = 20.5, x = 169), col = "gray50", size = 1) +
    geom_point(inherit.aes = FALSE, aes(y = 64.5, x = 169), col = "gray50", size = 1) +
    ## BLUE AND RED LINES ##
    annotate("segment", col = "gray50",  x = 125, xend = 125, y = 0, yend = 85, lwd = 0.5) +
    ## NET AND GOAL LINE ##
    geom_segment(col = "gray50", inherit.aes = FALSE, lwd = 0.5, aes(y = 5.75, x = 189, yend = 79.25, xend = 189)) +
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 192.5, yend = 45.5, xend = 192.5)) + 
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 39.5, x = 192.5, yend = 39.5, xend = 189)) +  
    geom_segment(col = "indianred", inherit.aes = FALSE, lwd = 0.5, aes(y = 45.5, x = 192.5, yend = 45.5, xend = 189)) +
    ## OUTLINE ##
    geom_path(data = upper_outline, aes(x = x, y = y), colour = "gray80", inherit.aes = FALSE, lwd = 0.5) +
    ## ADDITIONAL SPECS ##
    scale_x_continuous(expand = c(0, 0), limits = c(115,200)) + scale_y_continuous(expand = c(0,0), limits = c(0,85)) +
    coord_fixed() +
    theme_void()
  
  return(p)
}

# Function to save the ggplot of interest to mp4, can be changed to gif if desired
save_play <- function(data, r, t_start, type){
  line1 <- paste('data$relevant_events$\'',r,'\'$tracks$\'',t_start,'\'',sep='')
  line2 <- eval(parse(text=line1))
  if(length(line2$frame_id)!=0){
    json_tracks <- lapply(line2, function(x) {
      x[sapply(x, is.null)] <- NA
      unlist(x)
    })
    tracking_data <- as.data.frame(do.call("cbind", json_tracks))
    tracking_data <- tracking_data[,c('frame_id','period','track_id','team_id','team_name','jersey_number','x_ft','y_ft')]
    rownames(tracking_data) = NULL
    teams <- unique(tracking_data$team_name)
    team1 <- teams[1]
    team2 <- teams[2]
    title = paste("PP", r, "_", team1, "_", team2, "_SS", t_start, sep = "")
    
    tracking_data$frame_id = tracking_data$frame_id %>% as.integer()
    #tracking_data$frame_id = tracking_data$frame_id - min(tracking_data$frame_id)+1
    tracking_data$period = tracking_data$period %>% as.integer()
    tracking_data$track_id = tracking_data$track_id %>% as.integer()
    tracking_data$team_id = tracking_data$team_id %>% as.character()
    tracking_data$team_name = tracking_data$team_name %>% as.character()
    tracking_data$jersey_number = tracking_data$jersey_number %>% as.integer()
    tracking_data$x_ft = tracking_data$x_ft %>% as.double()
    tracking_data$y_ft = tracking_data$y_ft %>% as.double()

    ## ANIMATED PLOTS ##
    
    # Set the specs for the gif we want to create (lower res to make it run quicker)
    options(gganimate.dev_args = list(width = 10, height = 6, units = 'in', res = 320))
    
    # Source in the plot_rink function
    #source("C:/Users/Paula/Desktop/Big-Data-Cup-2022-Private/OTTHAC_Tutorial/Code/plot_rink.R")
    #source("/Volumes/BRICK_HOUSE/Hockey/Big-Data-Cup-2022-Private-main/OTTHAC_Tutorial/Code/plot_rink.R")
    
    # Create a gif of this play
    p = plot_rink(ggplot(tracking_data)) +
      geom_point(aes(x = x_ft, y = y_ft, fill = team_name), shape = 21, size = 6) +
      geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
      scale_colour_manual(values = c("USA" = "white", "Canada" = "white")) +
      scale_fill_manual(values = c("USA" = "blue", "Canada" = "red")) +
      # Additional specs
      geom_text(aes(x = 25, y = 40, label = frame_id), size = 5) +
      transition_time(frame_id) + 
      labs(fill = "Team") +
      guides(colour = "none")
    
    
    # Get the maximum and minimum frame number (so we know how long the gif should be)
    max_frame = tracking_data$frame_id %>% max()
    min_frame = tracking_data$frame_id %>% min() 
    
    # Render the animation
    if(type == "mp4"){
    # Save as mp4
      p2 = animate(p, renderer = ffmpeg_renderer(), fps = 30, duration = (max_frame - min_frame+1)/30)#((max_frame - min_frame)/30 + 1))
      anim_save(paste(title,".mp4",sep=""), p2)
    #
    }else{
    #Save as gif
      anim_save(paste(title,".gif",sep=""), p, fps = 30, duration = (max_frame - min_frame+1)/30)
    }
  }
}


# This function gives the distance a player would be from a target point
# given starting location and velocity and a target time t 
dist_to_xyt <- function(xyt,x0,y0,vx,vy, vmax = 30, alpha = 1.3, t_r = 0.5){
  
  # xyt - triplet of x y t of desired location and time
  # x0,y0 - current location
  # vx,vy - current speeD
  # vmax - maximum speed
  # alpha - decay coefficient (related to acceleration)
  # t_r - reaction time
  tx <- xyt['x']
  ty <- xyt['y']
  t <- xyt['t']
  
  
  # If time is smaller than reaction time, skater keeps going at initial speed
  if (t<t_r){
    c_x = x0 +vx * t
    c_y = y0 + vy * t
    r = 0
    }
  #first accounting for reaction time
  else{
    x0 = x0 + t_r * vx
    y0 = y0 + t_r * vy
    t = t - t_r
  
  # Now building the motion model for the remaining time
  
    c_x = x0 + vx * (1-exp(-alpha * t))/alpha
    c_y = y0 + vy * (1-exp(-alpha * t))/alpha
    r = vmax * (t - (1-exp(-alpha * t))/alpha)
  }
  
  remaining_dist = ((tx-c_x)^2 + (ty-c_y)^2)^0.5-r
  return(max(remaining_dist,0))
  
}

time_center_radius <- function(x0,y0,vx,vy, vmax = 30, alpha = 1.3, t_r = 0.5, tmax = 15, tres = 1/30){
  ti <- seq(0,t_r,tres) # initial time - before reaction
  tr <- seq(0,10-t_r,tres)# reamining time after reaction
  
  c_xi <- x0 + ti * vx
  c_yi <- x0 + ti *vy
  r_i <- 0 * ti
  
  x0 = x0 + t_r * vx
  y0 = y0 + t_r * vy
  
  # Now building the motion model for the remaining time
  
  c_xr = x0 + vx * (1-exp(-alpha * tr))/alpha
  c_yr = y0 + vy * (1-exp(-alpha * tr))/alpha
  r_r = vmax * (tr - (1-exp(-alpha * tr))/alpha)
  
  c_xf <- c(c_xi,c_xr)
  c_yf <- c(c_yi,c_yr)
  r_f <- c(r_i,r_r)
  
  t <- c(ti, tr+t_r)
  
  return(data.frame(t = t, cx = c_xf, cy = c_yf, r = r_f))
  
}

t_reach <- function(loc, t_c_r){
  tx = loc[1]
  ty = loc[2]
  remaining_dist = ((tx-t_c_r$cx)^2 + (ty-t_c_r$cy)^2)^0.5-t_c_r$r
  ix  <- max(which(remaining_dist>0))
  return(ifelse(ix == -Inf, 0, t_c_r$t[ix+1]))
}

player_arrival_times <- function(x0,y0,vx,vy,
                                 grid = expand.grid(x = seq(0,200,0.5), y = seq(0,85,0.5)),
                                 vmax = 30,
                                 alpha = 1.3, 
                                 t_r = 0.5, 
                                 tmax = 15, 
                                 tres = 1/30
                                 ){
  t_c_r <- time_center_radius(x0, y0, vx, vy, vmax = vmax, alpha = alpha, t_r = t_r, tmax = tmax, tres = tres)
  times <- apply(grid,1,t_reach,t_c_r)
  return(cbind(grid, arr_times = times))
}


puck_motion_model <- function(x0,y0,vx,vy, t = seq(0,15,0.2), mu = 0.1, beta = 0.1322, g = 32.174){
  
  vmag <-  sqrt(vx^2 + vy^2)
  
  x <-  x0 + (vx + mu*g * vx/vmag/beta) * (1 - exp(-beta * t))/beta - (mu*g * vx/vmag)/beta * t
  y <-  y0 + (vy + mu*g * vy/vmag/beta) * (1 - exp(-beta * t))/beta - (mu*g * vy/vmag)/beta * t
  
  return(data.frame(x = x, y = y, t = t))
}

puck_motion_model2 <- function(x0,y0,angle,vmag=speed_puck, t = seq(0.05,5,0.05), mu = 0.1, beta = 0.1322, g = 32.174){
  vx = vmag*sin(angle)
  vy = vmag*cos(angle)
  
  x <-  x0 + (vx + mu*g * vx/vmag/beta) * (1 - exp(-beta * t))/beta - (mu*g * vx/vmag)/beta * t
  y <-  y0 + (vy + mu*g * vy/vmag/beta) * (1 - exp(-beta * t))/beta - (mu*g * vy/vmag)/beta * t
  
  return(data.frame(x = x, y = y, t = t))
}

clean_pass <- function(passes, xmin=125, xmax=200, ymin=0, ymax=85){
  passes1 <- passes %>% filter(xmin<x) %>% filter(ymin<y) %>% filter(x<xmax) %>% filter(y<ymax)
  
  return(passes1)
}













#Use get_to_point values to make heatmap. May need to smooth for points between discrete spots. 

#Delete points that will get intercepted prior to arrival.

#Identify which points have a high probability of resulting in a successful pass or shot on goal. Or places close to the puck carrier, they could move to.

#Use these probabilities to predict the next move. With some type of model.

#Rank players who make the correct decisions.

#Other metrics
  #Calculate minimum arrival time to carrier
  #Size of convex hull for each team
  #Defender angles between shooter, defender and goal edges.

stick = 6.5 #feet, stick plus arm length
speed_puck = 120 #120 ft/s puck speed
frame_rate = 1/30

#Old stuff
assess_value_old <- function(pass,offence,d_min,o_min,int_d,int_o){
  pickup_min=rbind(d_min,o_min)
  int_od=rbind(int_d,int_o)
  #Interceptions
  #Keep only int_o and int_d where player_2_p < puck_2_p
  #Each intercepter will get a probability of intercepting
  #Who has smaller sqrt((x_int-x_puck)^2+(y_int-y_puck)^2) will intercept the puck first and 
  int_od$h_int = sqrt(int_od$x_int^2+int_od$y_int^2)
  int_od$h_mean = int_od$h_int+int_od$h_vel*int_od$puck_2_p
  h_angle = asin(int_od$vel_x/int_od$h_vel)
  stick = 2 #meters, stick plus arm length
  x_s = sin(h_angle)*stick
  y_s = cos(h_angle)*stick
  h_s = sqrt(x_s^2+y_s^2)
  int_od$prob_int = dnorm(int_od$h_int+h_s, mean = int_od$h_mean, sd = stick, log = FALSE)-dnorm(int_od$h_int-h_s, mean = int_od$h_mean, log = FALSE)
  
  pickup_min$h_int = sqrt(pass$x_coor^2+pass$y_coor^2)
  pickup_min$h_vel = sqrt(pickup_min$vel_x^2+pickup_min$vel_y^2)
  pickup_angle = asin(pickup_min$vel_x/pickup_min$h_vel)
  x_angle = sin(pickup_angle)*stick
  y_angle = cos(pickup_angle)*stick
  h_angle = sqrt(x_angle^2+y_angle^2)
  pickup_min$h_mean = pickup_min$h_int+pickup_min$h_vel*pickup_min$puck_2_p
  pickup_min$prob_int = dnorm(pickup_min$h_int+h_angle, mean = pickup_min$h_mean, sd = stick, log = FALSE)-dnorm(pickup_min$h_int-h_angle, mean = pickup_min$h_mean, log = FALSE)
  
  point_value = 0
  off_intercept = int_od$prob_int[which(int_od$team_name==offence)]
  if(length(off_intercept)>0){
    point_value=point_value+mean(off_intercept)
  }
  def_intercept = int_od$prob_int[-which(int_od$team_name==offence)]
  if(length(def_intercept)>0){
    point_value=point_value-mean(def_intercept)
  }
  point_value = (point_value + mean(pickup_min$prob_int[which(pickup_min$team_name==offence)])- mean(pickup_min$prob_int[-which(pickup_min$team_name==offence)]))*100/2
  return(point_value)
}


assess_value <- function(pass,offence,d_min,o_min,int_d,int_o){
  point_val_off = 0
  point_val_def = 0
  stick = 6.5 #meters, stick plus arm length
  
  pickup_min=rbind(d_min,o_min)
  pickup_min$dist_to_int = NULL
  for(j in 1:nrow(pickup_min)){
    pickup_min$dist_to_int[j]=dist_to_xyt(pass$x_coor,pass$y_coor,pickup_min$x_ft[j],pickup_min$y_ft[j],
                                          pickup_min$vel_x[j],pickup_min$vel_y[j],pickup_min$puck_2_p[j])
    #print(pickup_min$dist_to_int[j])
  }
  
  int_od=rbind(int_d,int_o)
  int_od$dist_to_int = NULL
  if(nrow(int_od)>0){
    for(i in 1:nrow(int_od)){
      int_od$dist_to_int[i]=dist_to_xyt(int_od$x_int[i],int_od$y_int[i],int_od$x_ft[i],int_od$y_ft[i],
                                        int_od$vel_x[i],int_od$vel_y[i],int_od$puck_2_p[i])
      #print(int_od$dist_to_int[i])
    }
    
    #Interceptions
    #Keep only int_o and int_d where player_2_p < puck_2_p
    #Each intercepter will get a probability of intercepting
    #Who has smaller sqrt((x_int-x_puck)^2+(y_int-y_puck)^2) will intercept the puck first and 
    
    h_int = sqrt(int_od$x_int^2+int_od$y_int^2)
    h_angle = atan(int_od$vel_y/int_od$vel_x)
    
    x_mean = int_od$x_int+cos(h_angle)*int_od$dist_to_int
    y_mean = int_od$y_int+sin(h_angle)*int_od$dist_to_int
    h_mean = sqrt(x_mean^2+y_mean^2)
    int_od$prob_int = abs(dnorm((h_int+stick-h_mean)/stick)-dnorm((h_int-stick-h_mean)/stick))
    
    off_intercept = int_od$prob_int[which(int_od$team_name==offence)]
    if(length(off_intercept)>0){
      point_val_off=point_val_off+mean(off_intercept)
    }
    def_intercept = int_od$prob_int[-which(int_od$team_name==offence)]
    if(length(def_intercept)>0){
      point_val_def=point_val_def+mean(def_intercept)
    }
  }
  
  
  h_int_puck = sqrt(pass$x_coor^2+pass$y_coor^2)
  pickup_angle = atan(pickup_min$vel_y/pickup_min$vel_x)
  x_mean_puck = pass$x_coor+cos(pickup_angle)*pickup_min$dist_to_int
  y_mean_puck = pass$y_coor+sin(pickup_angle)*pickup_min$dist_to_int
  h_mean_puck = sqrt(x_mean_puck^2+y_mean_puck^2)
  pickup_min$prob_int = abs(dnorm((h_int_puck+stick-h_mean_puck)/stick)-dnorm((h_int_puck-stick-h_mean_puck)/stick))
  
  
  point_val_off = (point_val_def+mean(pickup_min$prob_int[-which(pickup_min$team_name==offence)]))*100
  point_val_def = (point_val_def+mean(pickup_min$prob_int[which(pickup_min$team_name==offence)]))*100
  return(list(Off_val=point_val_off, Def_val=point_val_def))
}

# Given the current frame and eligible points find the minimum time a player will take to reach that point. separate for each time. 
#get min offense and defense times. Use speed of players and angle. Similar to arrival time in pipeline_functions.R
get_to_point <- function(x_puck,y_puck, points, offence,want_plot,s=speed_puck){#120 ft/s puck speed
  points=rbind(data.frame('angle'=c(),'r'=c(),'x_coor'=c(),'y_coor'=c()),points)
  names(points)=c('angle','r','x_coor','y_coor')
  tracks=tracking_data
  #give slight movement to avoid dividing by zero
  tracks$vel_x[which(tracks$vel_x==0)]=0.5
  tracks$vel_y[which(tracks$vel_y==0)]=0.5
  
  teams = tracks$team_name %>% unique()
  defence = teams[-which(teams==offence)]
  
  off_tracks = tracks %>% filter(team_name==offence)
  def_tracks = tracks %>% filter(team_name==defence)
  
  
  off_time_2_p = sqrt((points$x_coor-off_tracks$x_ft)^2+(points$y_coor-off_tracks$y_ft)^2)
  off_min = off_tracks[which.min(off_time_2_p),]
  off_min$min_d=min(off_time_2_p)
  off_min$puck_2_p = sqrt((points$x_coor-x_puck)^2+(points$y_coor-y_puck)^2)/s
  off_min$player_2_p = min(off_time_2_p)/sqrt(off_min$vel_x^2+off_min$vel_y^2)
  
  def_time_2_p = sqrt((points$x_coor-def_tracks$x_ft)^2+(points$y_coor-def_tracks$y_ft)^2)
  def_min = def_tracks[which.min(def_time_2_p),]#,min=min(def_time_2_p))
  def_min$min_d=min(def_time_2_p)
  def_min$puck_2_p = off_min$puck_2_p
  def_min$player_2_p = min(def_time_2_p)/sqrt(def_min$vel_x^2+def_min$vel_y^2)
  
  target_x = points$x_coor; target_y=points$y_coor
  defenders_x=x_puck; defenders_y=y_puck
  speed=10; dir=points$angle*180/pi
  blockers_xdef=def_tracks[,'x_ft']; blockers_ydef=def_tracks[,'y_ft']
  blockers_xoff=off_tracks[,'x_ft']; blockers_yoff=off_tracks[,'y_ft']
  reaction_time = 0; max_speed = 20; blocket_rime_multiplier = 5
  
  # i. Determine defenders' positions after reaction_time total seconds at their same speed and direction
  angle = case_when(
    dir <= 90 ~ 90 - dir,
    dir > 90 & dir < 180 ~ dir - 90,
    dir > 180 & dir < 270 ~ 270 - dir,
    TRUE ~ dir - 270
  )
  
  reaction_x = defenders_x + ifelse(dir < 180, cos(angle * pi/180)*(speed * reaction_time), -cos(angle * pi / 180)*(speed * reaction_time))
  reaction_y = defenders_y + ifelse(dir > 90 & dir < 270, sin(angle * pi/180)*(speed * reaction_time), -sin((angle * pi)/180)*(speed * reaction_time))
  # Set (x1,y1) = location of defender after reaction time, (x2,y2) = target location, (x3,y3) = blocker location
  i=1;k=1
  x1 = reaction_x[i]; y1 = reaction_y[i]
  x2 = target_x[k]; y2 = target_y[k]
  
  # Calculate the perpendicular projection of the blocker's position onto the line formed by the defender and the target
  b_def = -((x1-blockers_xdef)*(x2-x1)+(y1-blockers_ydef)*(y2-y1))/((x2-x1)^2+(y2-y1)^2)
  intercept_xdef = x1 + b_def*(x2 - x1)
  intercept_ydef = (y1 + b_def*(y2 - y1))
  
  intercept_def = data.frame(def_tracks[,c(3:8,11:13)],x_int = unlist(intercept_xdef),y_int = unlist(intercept_ydef))
  intercept_def= intercept_def[which((y1-intercept_def$y_int)<=(y1-y2) & 0<(y1-intercept_def$y_int)),]
  intercept_def$h_vel = sqrt(intercept_def$vel_x^2+intercept_def$vel_y^2) #hypotenuse velocity
  intercept_def$h_dist = sqrt((intercept_def$x_ft-intercept_def$x_int)^2+(intercept_def$y_ft-intercept_def$y_int)^2)
  intercept_def$v_dist = sqrt(intercept_def$vel_x^2+intercept_def$vel_y^2)
  intercept_def$hv_dist = sqrt((intercept_def$x_ft+intercept_def$vel_x-intercept_def$x_int)^2+(intercept_def$y_ft+intercept_def$vel_y-intercept_def$y_int)^2)
  intercept_def$int_angle = acos((intercept_def$h_dist^2+intercept_def$v_dist^2-intercept_def$hv_dist^2)/(2*intercept_def$h_dist*intercept_def$v_dist)) 
  #law of cosines b2=a2+c2-2accos(b), angle in radians
  intercept_def$puck_2_p = sqrt((x_puck-intercept_def$x_int)^2+(y_puck-intercept_def$y_int)^2)/s
  intercept_def$player_2_p = sqrt((intercept_def$x_ft-intercept_def$x_int)^2+(intercept_def$y_ft-intercept_def$y_int)^2)/intercept_def$h_vel
  
  b_off = -((x1-blockers_xoff)*(x2-x1)+(y1-blockers_yoff)*(y2-y1))/((x2-x1)^2+(y2-y1)^2)
  intercept_xoff = x1 + b_off*(x2 - x1)
  intercept_yoff = (y1 + b_off*(y2 - y1))
  
  intercept_off = data.frame(off_tracks[,c(3:8,11:13)],x_int = unlist(intercept_xoff),y_int = unlist(intercept_yoff))
  intercept_off= intercept_off[which((y1-intercept_off$y_int)<=(y1-y2) & 0<(y1-intercept_off$y_int)),]
  intercept_off$h_vel = sqrt(intercept_off$vel_x^2+intercept_off$vel_y^2) #hypotenuse velocity
  intercept_off$h_dist = sqrt((intercept_off$x_ft-intercept_off$x_int)^2+(intercept_off$y_ft-intercept_off$y_int)^2)
  intercept_off$v_dist = sqrt(intercept_off$vel_x^2+intercept_off$vel_y^2)
  intercept_off$hv_dist = sqrt((intercept_off$x_ft+intercept_off$vel_x-intercept_off$x_int)^2+(intercept_off$y_ft+intercept_off$vel_y-intercept_off$y_int)^2)
  intercept_off$int_angle = acos((intercept_off$h_dist^2+intercept_off$v_dist^2-intercept_off$hv_dist^2)/(2*intercept_off$h_dist*intercept_off$v_dist)) 
  #law of cosines b2=a2+c2-2accos(b), angle in radians
  intercept_off$puck_2_p = sqrt((x_puck-intercept_off$x_int)^2+(y_puck-intercept_off$y_int)^2)/s
  intercept_off$player_2_p = sqrt((intercept_off$x_ft-intercept_off$x_int)^2+(intercept_off$y_ft-intercept_off$y_int)^2)/intercept_off$h_vel
  
  value_at_p = assess_value(points,offence,def_min,off_min,intercept_def,intercept_off)
  
  if(want_plot){
    plot_pass=plot_rink(ggplot(tracks)) +
      geom_point(aes(x = x_ft, y = y_ft, fill = team_name), size = 5, shape = 21) +
      geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
      geom_point(aes(x = x_puck+2, y = y_puck-0.5), size = 2, shape = 16, colour='black') + 
      geom_point(data = points, aes(x = x_coor, y = y_coor), size = 2, shape = 4) +
      geom_point(data =intercept_def,aes(x = x_int, y = y_int),color='light blue', size = 3, shape = 8) +
      geom_point(data =intercept_off,aes(x = x_int, y = y_int),color='pink', size = 3, shape = 8) +
      scale_colour_manual(values = c("USA" = "white", "Canada" = "white")) +
      scale_fill_manual(values = c("USA" = "blue", "Canada" = "red")) +
      geom_segment(data = points,aes(x = x_puck, y = y_puck, xend = x_coor, yend = y_coor),linetype=2)+
      labs(fill = "Team") +
      guides(colour = "none")+ 
      geom_segment(aes(x = x_ft, y = y_ft, xend = x_ft+vel_x, yend = y_ft+vel_y), #/sqrt(vel_x^2+vel_y^2) to get r=1
                   arrow = arrow(length = unit(0.2, "cm")),size=1, colour='cyan') 
    return(list(value_at_p,plot_pass))
  }else{
    return(value_at_p)
  }
}
#puck speeds https://www.sidmartinbio.org/how-fast-does-the-average-hockey-puck-travel/ 

#Given the current puck location, find all points on the ice they could move the puck to. Use angles and speed of pass
create_points <- function(x_puck,y_puck,x_min = 125,x_max = 200,y_min = 0,y_max = 85, boundary_step = 8, r_step = 5){
  boundary_step = as.integer(boundary_step)
  passes = data.frame(angle=integer(),r=double(),x_coor=double(),y_coor=double())
  
  upper_outline = data.frame(
    x = c(
      rep(x_min,boundary_step),
      seq(x_min,x_max-28,length=boundary_step),
      x_max-28 + 28*sin(seq(0,pi/2,length=boundary_step)),
      rep(x_max,round(boundary_step/2)),
      x_max-28 + 28*sin(seq(pi/2,0,length=boundary_step)),
      seq(x_min,x_max-28,length=boundary_step)
    ),
    y = c(
      seq(y_min,y_max,length=boundary_step),
      rep(y_min,boundary_step), 
      y_min + 28 - 28*cos(seq(0,pi/2,length=boundary_step)),
      seq(28,y_max-28,length=round(boundary_step/2)),
      y_max - 28 + 28*cos(seq(pi/2,0,length=boundary_step)),
      rep(y_max,boundary_step)
    )
  )
  for(spot in 1:nrow(upper_outline)){
    x_spot = upper_outline[spot,1]
    y_spot = upper_outline[spot,2]
    angle = atan((x_spot-x_puck)/(y_spot-y_puck))
    
    angle = case_when(
      x_spot>x_puck & y_spot<y_puck ~ angle+pi,
      x_spot<x_puck & y_spot<y_puck ~ angle+pi,
      x_spot<x_puck & y_spot>y_puck ~ angle+2*pi,
      x_spot>x_puck & y_spot>y_puck ~ angle
    )
    
    r = sqrt((x_spot-x_puck)^2+(y_spot-y_puck)^2)
    while(r>r_step){
      passes = rbind(passes,c(angle,r,x_spot,y_spot))
      r = case_when(
        r>20 ~ r-r/r_step,
        r<=20 ~ r-2*r/r_step
      )
      x_spot = x_puck + r*sin(angle)
      y_spot = y_puck + r*cos(angle)
    }
  }
  colnames(passes) = c('angle','r','x_coor','y_coor')
  return(passes)
}

probs_to_point_old <- function(x_puck,y_puck, points, offence,want_plot=FALSE){
  points=rbind(data.frame('theta'=c(),'x'=c(),'y'=c(),'t'=c()),points)
  names(points)=c('theta','x','y','t')
  tracks=tracking_data
  #give slight movement to avoid dividing by zero
  tracks$vel_x[which(tracks$vel_x==0)]=0.5
  tracks$vel_y[which(tracks$vel_y==0)]=0.5
  
  teams = tracks$team_name %>% unique()
  defence = teams[-which(teams==offence)]
  
  off_tracks = tracks %>% filter(team_name==offence)
  def_tracks = tracks %>% filter(team_name==defence)
  
  n_step = round(sqrt((points$x-x_puck)^2+(points$y-y_puck)^2)/5)+2
  steps = seq(1,n_step,by=1) #look at every 5 foot distance along the line
  t_step = seq(0,points$t,length.out=length(steps)+1) #for now, consider equal time to target
  
  #theta_p = atan((x_puck-points$x)/(y_puck-points$y))
  x_new = (points$x+(n_step:0)*(x_puck-points$x)/n_step)[-1]
  y_new = (points$y+(n_step:0)*(y_puck-points$y)/n_step)[-1]
  
  #plot(c(x_puck, x_new,points$x),c(y_puck, y_new,points$y), col=c('black',rep('blue',length(x_new)),'red'))
  
  new_data = data.frame(x=x_new,y=y_new,t=t_step[-1])
  
  #point_val_off = 0
  #point_val_def = 0
  
  dist_to_points = matrix(nrow=n_step, ncol=nrow(tracks))
  for(player in 1:nrow(tracks)){
    dist_to_points[,player]=apply(new_data,1,dist_to_xyt,x0=tracks$x_ft[player],y0=tracks$y_ft[player],vx=tracks$vel_x[player],vy=tracks$vel_y[player])
  }
  #distance from point of interests for each player
  
  pickup_probs = abs(dnorm((dist_to_points+stick)/stick)-dnorm((dist_to_points-stick)/stick))
  new_data2 = cbind(new_data,pickup_probs)
  
  off_lines = which(tracks$team_name==offence)
  def_lines = which(tracks$team_name!=offence)
  off_probs = new_data2[,off_lines+3]
  def_probs = new_data2[,def_lines+3]
  
  off_mat = t(replicate(nrow(new_data2),tracks$team_name==offence))
  
  all_rank = t(apply(-pickup_probs,1,rank))
  rix <-  as.vector(t(replicate(ncol(pickup_probs),seq(1,nrow(pickup_probs),1))))
  
  ranked_probs <-  pickup_probs * 0
  ranked_probs[cbind(rix,as.vector(t(all_rank)))] <- as.vector(t(pickup_probs))
  
  ranked_probs[,2] = ranked_probs[,2]*(1-ranked_probs[,1])
  for (c in 3:ncol(ranked_probs)){
    ranked_probs[,c] = ranked_probs[,c]*(1-ranked_probs[,c-1])
  }
  
  ranked_off_mat <- off_mat * 0
  ranked_off_mat[cbind(rix,as.vector(t(all_rank)))] <- as.vector(t(off_mat))
  
  shot_probs <- data.frame(off = rowSums(ranked_probs * ranked_off_mat),
                           def = rowSums(ranked_probs * (1-ranked_off_mat))) %>% 
    mutate(None = 1 - off - def)
  
  # shot_probs2 <- shot_probs
  
  for (r in 2:nrow(shot_probs)){
    shot_probs[r,] = shot_probs[r,] * shot_probs$None[r-1]
  }
  
  shot_good = sum(shot_probs[,1]-shot_probs[,2]-shot_probs[nrow(shot_probs),3])
  
  if(want_plot){
    plot_pass=plot_half_rink(ggplot(tracks)) +
      geom_point(aes(x = x_ft, y = y_ft, fill = team_name), size = 5, shape = 21) +
      geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
      geom_segment(aes(x = x_ft, y = y_ft, xend = x_ft+vel_x, yend = y_ft+vel_y), #/sqrt(vel_x^2+vel_y^2) to get r=1
                   arrow = arrow(length = unit(0.2, "cm")),size=1, colour='cyan') + 
      geom_point(aes(x = x_puck+2, y = y_puck-0.5), size = 2, shape = 16, colour='black') + 
      geom_point(data = new_data2, aes(x = x, y = y), colour='dark grey',size = 1, shape = 16) +
      scale_colour_manual(values = c("USA" = "white", "Canada" = "white")) +
      scale_fill_manual(values = c("USA" = "blue", "Canada" = "red")) +
      geom_segment(data = points,aes(x = x_puck, y = y_puck, xend = x, yend = y),linetype=2)+
      geom_point(data = points, aes(x = x, y = y), size = 2, shape = 4, colour='dark grey') + 
      labs(fill = "Team") +
      guides(colour = "none") 
    return(list(shot_good,cbind(new_data,shot_probs),plot_pass))
  }else{
    return(shot_good)#list(diff_p,100*off_p/(off_p+def_p),off_probs,def_probs,off_max,def_max))
  }
}


#off_p = off_max[length(off_max)]
#for(p in (length(off_max)-1):1){
#  off_p = off_p+off_p*off_max[p]
#}

#def_max = apply(def_probs,1,max)
#def_p = def_max[length(def_max)]
#for(p in (length(def_max)-1):1){
#  def_p = def_p+def_p*def_max[p]
#}

#diff_max = off_max-def_max
#diff_max = diff_max*(length(diff_max):1)
#diff_p = mean(diff_max)

#what to return? we could return ratio as in here. Will give back present of control by the offensive team over the defensive team, always between 0 and 1.
#should we divide by number of players on either team? p_off/n_off and p_def/n_def. Also the number of points along the line depends on length of pass,
#should we standardize somehow?