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

# Given the current frame and eligible points find the minimum time a player will take to reach that point. separate for each time. 
#get min offense and defense times. Use speed of players and angle. Similar to arrival time in pipeline_functions.R
get_to_point <- function(x_puck,y_puck,tracks, points, offence){
  teams = tracks$team_name %>% unique()
  defence = teams[-which(teams==offence)]
  
  off_tracks = tracks %>% filter(team_name==offence)
  def_tracks = tracks %>% filter(team_name==defence)
  
  off_min = c()
  def_min = c()
  for(row in 1:nrow(points)){
    off_min = c(off_min, min(sqrt((points[row,'x_coor']-off_tracks$x_ft)^2+(points[row,'y_coor']-off_tracks$y_ft)^2)))
    def_min = c(def_min, min(sqrt((points[row,'x_coor']-def_tracks$x_ft)^2+(points[row,'y_coor']-def_tracks$y_ft)^2)))
  }
  
  target_x = points$x_coor; target_y=points$y_coor
  defenders_x=x_puck; defenders_y=y_puck
  speed=10; dir=points$angle*180/pi
  blockers_x=off_tracks[,'x_ft']; blockers_y=off_tracks[,'y_ft']
  reaction_time = 0; max_speed = 20; blocker_time_multiplier = 5
  
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
  x3 = blockers_x; y3 = blockers_y
  
  # Calculate the perpendicular projection of the blocker's position onto the line formed by the defender and the target
  b = -((x1-x3)*(x2-x1)+(y1-y3)*(y2-y1))/((x2-x1)^2+(y2-y1)^2)
  intercept_x = x1 + b*(x2 - x1)
  intercept_y = (y1 + b*(y2 - y1))
  
  intercept = data.frame(x_int = unlist(intercept_x),y_int = unlist(intercept_y))
  intercept= intercept[which((y1-intercept$y_int)<=(y1-y2) & 0<(y1-intercept$y_int)),]
  plot_rink(ggplot(tracks)) +
    geom_point(aes(x = x_ft, y = y_ft, fill = team_name), size = 5, shape = 21) +
    geom_text(aes(x = x_ft, y = y_ft, label = jersey_number, colour = team_name), size = 3) +
    geom_point(aes(x = x_puck, y = y_puck), size = 3, shape = 4) + 
    geom_point(data = points, aes(x = x_coor, y = y_coor), size = 2, shape = 4) +
    geom_point(data =intercept,aes(x = x_int, y = y_int),color='grey', size = 1, shape = 8) +
    scale_colour_manual(values = c("USA" = "white", "Canada" = "white")) +
    scale_fill_manual(values = c("USA" = "blue", "Canada" = "red")) +
    geom_segment(data = points,aes(x = x_puck, y = y_puck, xend = x_coor, yend = y_coor),linetype=2)+
    labs(fill = "Team") +
    guides(colour = "none") 
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





















#Use get_to_point values to make heatmap. May need to smooth for points between discrete spots. 

#Delete points that will get intercepted prior to arrival.

#Identify which points have a high probability of resulting in a successful pass or shot on goal. Or places close to the puck carrier, they could move to.

#Use these probabilities to predict the next move. With some type of model.

#Rank players who make the correct decisions.

#Other metrics
  #Calculate minimum arrival time to carrier
  #Size of convex hull for each team
  #Defender angles between shooter, defender and goal edges.



frame_rate = 1/30

