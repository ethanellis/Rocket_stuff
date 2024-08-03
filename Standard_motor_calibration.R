library(ggplot2)

################################### a6-4 motor ########################################

a6 = read.delim('C:/Users/clae5/OneDrive/Desktop/CoolTermWin64Bit/outFiles/A6-4_motor1_quest_2024-08-02 21-17-52-174.txt',
                sep=',', head=F)
colnames(a6) = c('sec','force')
a6$sec = a6$sec/1000
a6$force = (a6$force/1000)*9.81

#determine start and end indices:
start=F
start_index = 0
end_index = 0
for(row in 1:nrow(a6)){
  if(row > 1 && !start){
    diff = a6$force[row]-a6$force[row-1]
    if(diff > 0.05){
      start = T
      start_index = row - 5
    }
  }
  if(start){
    diff = a6$force[row]-a6$force[row-1]
    if(diff < 0.05 && a6$force[row] < 0.05){
      end_index = row + 5
      break
    }
  }
}

a6p = ggplot(data = a6, mapping=aes(x = sec, y = force))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 0)+
  scale_x_continuous(limits=c(a6$sec[start_index], a6$sec[end_index]))
a6p

rsum = c()
for(index in (start_index+1):end_index){
  int_sum=((a6$force[index]+a6$force[index-1])/2)*(a6$sec[index]-a6$sec[index-1])
  rsum = append(rsum, int_sum)
}
impulse = sum(rsum)
print(impulse)


a6_trunc = a6[c(start_index:end_index),]
a6_trunc$des = 'a6'
a6_trunc$sec = a6_trunc$sec-a6_trunc$sec[1]








################################### a8-3 motor ########################################

a8 = read.delim('C:/Users/clae5/OneDrive/Desktop/CoolTermWin64Bit/outFiles/A8-3_motor1_2024-08-02 21-14-40-425.txt',
                sep=',', head=F)
colnames(a8) = c('sec','force')
a8$sec = a8$sec/1000
a8$force = (a8$force/1000)*9.81

#determine start and end indices:
start=F
start_index = 0
end_index = 0
for(row in 1:nrow(a8)){
  if(row > 1 && !start){
    diff = a8$force[row]-a8$force[row-1]
    if(diff > 0.05){
      start = T
      start_index = row - 5
    }
  }
  if(start){
    diff = a8$force[row]-a8$force[row-1]
    if(diff < 0.05 && a8$force[row] < 0.05){
      end_index = row + 5
      break
    }
  }
}

a8p = ggplot(data = a8, mapping=aes(x = sec, y = force))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 0)+
  scale_x_continuous(limits=c(a8$sec[start_index], a8$sec[end_index]))
a8p

rsum = c()
for(index in (start_index+1):end_index){
  int_sum=((a8$force[index]+a8$force[index-1])/2)*(a8$sec[index]-a8$sec[index-1])
  rsum = append(rsum, int_sum)
}
impulse = sum(rsum)
print(impulse)

a8_trunc = a8[c(start_index:end_index),]
a8_trunc$des = 'a8'
a8_trunc$sec = a8_trunc$sec-a8_trunc$sec[1]






################################### b6-4 motor ########################################

b6 = read.delim('C:/Users/clae5/OneDrive/Desktop/CoolTermWin64Bit/outFiles/B6-4_motor1_2024-08-02 21-20-54-501.txt',
                sep=',', head=F)
colnames(b6) = c('sec','force')
b6$sec = b6$sec/1000
b6$force = (b6$force/1000)*9.81

#determine start and end indices:
start=F
start_index = 0
end_index = 0
for(row in 1:nrow(b6)){
  if(row > 1 && !start){
    diff = b6$force[row]-b6$force[row-1]
    if(diff > 0.05){
      start = T
      start_index = row - 5
    }
  }
  if(start){
    diff = b6$force[row]-b6$force[row-1]
    if(diff < 0.05 && b6$force[row] < 0.05){
      end_index = row + 5
      break
    }
  }
}

b6p = ggplot(data = b6, mapping=aes(x = sec, y = force))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 0)+
  scale_x_continuous(limits=c(b6$sec[start_index], b6$sec[end_index]))
b6p

rsum = c()
for(index in (start_index+1):end_index){
  int_sum=((b6$force[index]+b6$force[index-1])/2)*(b6$sec[index]-b6$sec[index-1])
  rsum = append(rsum, int_sum)
}
impulse = sum(rsum)
print(impulse)


b6_trunc = b6[c(start_index:end_index),]
b6_trunc$des = 'b6'
b6_trunc$sec = b6_trunc$sec-b6_trunc$sec[1]






################################### c6-4 motor ########################################

c6 = read.delim('C:/Users/clae5/OneDrive/Desktop/CoolTermWin64Bit/outFiles/C6-5_motor1_2024-08-02 21-26-16-231.txt',
                sep=',', head=F)
colnames(c6) = c('sec','force')
c6$sec = c6$sec/1000
c6$force = (c6$force/1000)*9.81

#determine start and end indices:                        weird curve. Possible bad engine. Roman candle
# start=F
# start_index = 0
# end_index = 0
# for(row in 1:nrow(c6)){
#   if(row > 1 && !start){
#     diff = c6$force[row]-c6$force[row-1]
#     if(diff > 0.05){
#       start = T
#       start_index = row - 5
#     }
#   }
#   if(start){
#     diff = c6$force[row]-c6$force[row-1]
#     if(diff < 0.05 && c6$force[row] < 0.05){
#       end_index = row + 5
#       break
#     }
#   }
# }

start_index=62
end_index=96


c6p = ggplot(data = c6, mapping=aes(x = sec, y = force))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 0)+
  scale_x_continuous(limits=c(c6$sec[start_index], c6$sec[end_index]))+
  scale_y_continuous(limits=c(0,0.11))
c6p

rsum = c()
for(index in (start_index+1):end_index){
  int_sum=((c6$force[index]+c6$force[index-1])/2)*(c6$sec[index]-c6$sec[index-1])
  rsum = append(rsum, int_sum)
}
impulse = sum(rsum)
print(impulse)



c6_trunc = c6[c(start_index:end_index),]
c6_trunc$des = 'c6'
c6_trunc$sec = c6_trunc$sec-c6_trunc$sec[1]








################################### e12-4 motors ########################################

e12_3 = read.delim('C:/Users/clae5/OneDrive/Desktop/CoolTermWin64Bit/outFiles/E12-4_motor32024-08-02 21-06-58-242.txt',
                sep=',', head=F)
colnames(e12_3) = c('sec','force')
e12_3$sec = e12_3$sec/1000
e12_3$force = (e12_3$force/1000)*9.81

#determine start and end indices:
start=F
start_index = 0
end_index = 0
for(row in 1:nrow(e12_3)){
  if(row > 1 && !start){
    diff = e12_3$force[row]-e12_3$force[row-1]
    if(diff > 0.05){
      start = T
      start_index = row - 5
    }
  }
  if(start){
    diff = e12_3$force[row]-e12_3$force[row-1]
    if(diff < 0.05 && e12_3$force[row] < 0.05){
      end_index = row + 5
      break
    }
  }
}

e12_3p = ggplot(data = e12_3, mapping=aes(x = sec, y = force))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 0)+
  scale_x_continuous(limits=c(e12_3$sec[start_index], e12_3$sec[end_index]))
e12_3p

rsum = c()
for(index in (start_index+1):end_index){
  int_sum=((e12_3$force[index]+e12_3$force[index-1])/2)*(e12_3$sec[index]-e12_3$sec[index-1])
  rsum = append(rsum, int_sum)
}
impulse = sum(rsum)
print(impulse)


e12_trunc = e12_3[c(start_index:end_index),]
e12_trunc$des = 'e12'
e12_trunc$sec = e12_trunc$sec-e12_trunc$sec[1]



all = rbind(a6_trunc, a8_trunc, b6_trunc, c6_trunc, e12_trunc)

all_p = ggplot(data = all, mapping = aes(x = sec, y = force, group = des, color = des))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 0)
all_p












