#Given the current puck location, find all points on the ice they could move the puck to. Use angles and speed of pass
create_points <- function(x_puck,y_puck,x_min = 125,x_max = 200,y_min = 0,y_max = 85,angle_step = 15,r_step = 5){
  passes = data.frame(angle=integer(),r=double(),x_coor=double(),y_coor=double())
  angle = 0
  
  while(angle<360){
    x_new = 150
    y_new = 50
    step_new = r_step
    while(x_min<x_new & x_new<x_max & y_min<y_new & y_new<y_max){
      x_new = x_puck + step_new*sin(angle)
      y_new = y_puck + step_new*cos(angle)
      passes = rbind(passes,c(angle,step_new,x_new,y_new))
      step_new=step_new+r_step
    }
    passes<-passes[1:(nrow(passes)-1),]
    angle = angle+angle_step
  }
  colnames(passes) = c('angle','r','x_coor','y_coor')
  return(passes)
}
#add x's along board for 'dump'
#add x's to sides of net for 'shot'
#add x's for 'indirect pass' off the boards angle of entry equal to exit angle


create_points2 <- function(x_puck,y_puck,x_min = 125,x_max = 200,y_min = 0,y_max = 85, boundary_step = 8, r_step = 5){
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

#Get the direction of travel for each player. Vector straight north of the starting vector. Then rotate clockwise until you get to the 
#resulting vector of the move from (x1,y1) to (x2,y2). That gives the angle of movement. Straight East would be 90, south 180, west 270, north 0 or 360.
#Possibly add to python code
get_direction <- function(frame){
  
}

#Given the current frame and eligible points find the minimum time a player will take to reach that point. separate for each time. 
#get min offense and defense times. Use speed of players and angle. Similar to arrival time in pipeline_functions.R
get_to_point <- function(frame, points, team){
  
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

speed <- function(tracks){
  save_tracks = tracks
  save_tracks$frame_id_2 = NA
  save_tracks$x_ft_2 = NA
  save_tracks$y_ft_2 = NA
  for(row in 1:nrow(save_tracks)){
    player = save_tracks[row,'track_id'] 
    all = which(save_tracks[,'track_id']==player)
    for(p in 1:(length(all)-1)){
      save_tracks[all[p],c('frame_id_2','x_ft_2','y_ft_2')] <- save_tracks[all[p+1],c('frame_id','x_ft','y_ft')]
    }
  }
  save_tracks = save_tracks %>% mutate(speed = sqrt((x_ft_2-x_ft)^2 + (y_ft_2-y_ft)^2)/(frame_rate*(frame_id_2-frame_id)))
  save_tracks = save_tracks %>% mutate(y_dir = y_ft_2-y_ft)
  save_tracks = save_tracks %>% mutate(x_dir = x_ft_2-x_ft)
  save_tracks = save_tracks %>% mutate(angle = asin((x_ft_2-x_ft)/(y_ft_2-y_ft)))
  return(save_tracks)
}


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

rink_curve_upper <- Vectorize(function(x){
  if(186<=x & x<=200){
    y=(sqrt(200-(x-186)^2)+72)-1.15 #x between 172 and 200, y between 74 and 86
  }
  if(14<x & x<186){
    y=85
  }
  if(0<=x & x<=14){
    y=(sqrt(200-(x-14)^2)+72)-1.15 #x between 0 and 14, y between 74 and 86
  }
  return(y)
})



rink_curve_lower <- Vectorize(function(x){
  if(186<=x & x<=200){
    y=-(sqrt(200-(x-186)^2)-16)-1.85 #x between 172 and 200, y between 74 and 86
  }
  if(14<x & x<186){
    y=0
  }
  if(0<=x & x<=14){
    y=-(sqrt(200-(x-14)^2)-16)-1.85 #x between 0 and 14, y between 74 and 86
  }
  return(y)
})

#curve(rink_curve_lower(x), from=0,to=200,n=100, ylim=c(0,85), xlim=c(0,200)) #190.9639 77.6002695
#curve(rink_curve_upper(x),add=TRUE)
#segments(x0=0,x1=0,y0=12.5,y1=73)
#segments(x0=200,x1=200,y0=12.5,y1=73)