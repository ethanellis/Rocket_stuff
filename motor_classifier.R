library(ggplot2)
##########################################################################SETUP###################################################################################

#define file path to motor thrust files
motor_filepath='C:/Users/clae5/OneDrive/Desktop/Rocket_stuff-main/Rocket_stuff-main/motor_files'

#input motor file names
motors=c(
  'TestArticle01.csv',
  'E12-4_motor32024-08-02 21-06-58-242.txt',
  'C6-5_motor1_2024-08-02 21-26-16-231.txt',
  'B6-4_motor1_2024-08-02 21-20-54-501.txt',
  'A8-3_motor1_2024-08-02 21-14-40-425.txt',
  'A6-4_motor1_quest_2024-08-02 21-17-52-174.txt'
)



#######################################################################FUNCTIONS##################################################################################

## define function to convert force values to newtons and time values to seconds
convert_data <- function(thrust_data){
  
  colnames(thrust_data) = c('sec','force')
  thrust_data$sec = thrust_data$sec/1000
  thrust_data$force = (thrust_data$force/1000)*9.81
  return(thrust_data)
  
}


## define function to search for the bounds of the most plausible thrust curve in full data set
find_bounds <- function(thrust_data){
  
  start=c()
  end=c()
  
  start_trigger=F
  for(row in 1:nrow(thrust_data)){
    
    if(row > 1){
      diff = thrust_data$force[row]-thrust_data$force[row-1]
      if(is.na(diff)){
        diff = 0
      }
      
      if(!start_trigger){
        if(diff > 0.05){
          start_trigger = T
          start_index = row - 5
          start = append(start, start_index)
        }
      }
      if(start_trigger){
        if((diff < 0.05 && thrust_data$force[row] < 0) || row == nrow(thrust_data)){
          end_index = row + 5
          end = append(end, end_index)
          start_trigger = F
        }
      }
    }
  }

  start[start < 0] = 0
  
  #take each combination of bounds, find the most plausible set.
  #define by - 
  #     max thrust
  #     duration (> 0.5 s)
  
  max_thrust = 0
  max_index = 0
  if(length(start > 0)){
    for(index in 1:length(start)){
      start_check = start[index]
      end_check = end[index]
      
      thrust_data_subset = thrust_data[c(start_check:end_check),]
      subset_max = max(thrust_data_subset$force)
      if(subset_max > max_thrust && (thrust_data_subset$sec[nrow(thrust_data_subset)] - thrust_data_subset$sec[1]) > 0.5){
        max_thrust = subset_max
        max_index = index
      }
      
    }
  }
  
  return_bounds = c(start[max_index], end[max_index])
  
}


## define function that, given thrust data and time bounds, computes impulse
find_impulse <- function(thrust_data, start_index, end_index){
  
  impulse = NA
  
  tryCatch(
    expr={
      rsum = c()
      for(index in (start_index+1):end_index){
        int_sum=((thrust_data$force[index]+thrust_data$force[index-1])/2)*(thrust_data$sec[index]-thrust_data$sec[index-1])
        rsum = append(rsum, int_sum)
      }
      impulse = sum(rsum)
      
      weight_comp = 0.5*abs(thrust_data$force[end_index]-thrust_data$force[start_index])*(thrust_data$sec[end_index]-thrust_data$sec[start_index])
      impulse = impulse + weight_comp
    },
    error = function(e){
      print(e)
    },
    warning = function(w){
      print(w)
    }
  )
  
  return(impulse)
  
}


## define function that finds maximum thrust
max_thrust <- function(thrust_data, start_index, end_index){
  
  max_thrust_val = NA
  
  tryCatch(
    expr={
      thrust_data_subset = thrust_data[c(start_index:end_index),]
      max_thrust_val = max(thrust_data_subset$force)
    },
    error = function(e){
      print(e)
    },
    warning = function(w){
      print(w)
    }
  )
  
  
  return(max_thrust_val)
  
}


##define function that finds burn time
burn_time <- function(thrust_data, start_index, end_index){
  
  burn_time_val = NA
  
  tryCatch(
    expr={
      burn_time_val = thrust_data$sec[end_index-5] - thrust_data$sec[start_index+5]
    },
    error=function(e){
      print(e)
    },
    warning=function(w){
      print(w)
    }
  )
  
  return(burn_time_val)
  
}





####################################################################SCRIPT########################################################################################



#primary analysis loop
all_motors = c()
for(motor in 1:length(motors)){
  
  #print out motor identity
  print("*******************************************")
  print(paste("MOTOR ANALYSIS:", motors[motor], sep=" "))
  
  #read motor thrust data
  thrust_data = read.csv(paste(motor_filepath, motors[motor], sep='/'), sep=',', head=F)
  
  #perform motor analysis and determine bounds for plotting
  thrust_data = convert_data(thrust_data)
  bounds = find_bounds(thrust_data)
  impulse = find_impulse(thrust_data, bounds[1], bounds[2])
  maxThrust = max_thrust(thrust_data, bounds[1], bounds[2])
  burnTime = burn_time(thrust_data, bounds[1], bounds[2])
  
  
  #print out motor analysis
  print(paste("TOTAL IMPULSE (N*s):", impulse, sep=" "))
  print(paste("MAX THRUST (N):", maxThrust, sep=" "))
  print(paste("BURN TIME (s):", burnTime, sep=" "))
  print("*******************************************")
  print("")
  
  #truncate data for plotting
  if(length(bounds) > 0){
    thrust_data_trunc = thrust_data[c(bounds[1]:bounds[2]),]
    thrust_data_trunc$Designation = motors[motor]
    thrust_data_trunc$sec = thrust_data_trunc$sec - thrust_data_trunc$sec[1]
    
    if(motor == 1){
      all_motors = thrust_data_trunc
    }else{
      all_motors = rbind(all_motors, thrust_data_trunc)
    }
  }
}

#plot thrust curves
if(length(all_motors) > 0){
  all_p = ggplot(data = all_motors, mapping = aes(x = sec, y = force, group = Designation, color = Designation))+
    geom_line()+
    geom_point()+
    xlab("Time (s)")+
    ylab("Thrust (N)")+
    ggtitle("Motor Thrust Curves")+
    geom_hline(yintercept = 0)
  all_p
}

