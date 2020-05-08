# https://research.fhcrc.org/content/dam/stripe/shou/files/2014%20Modeling%20Community%20Population%20Dynamics%20with%20the%20Open-Source%20Language%20R.pdf
#
# Function for simple LV models
# x_start = starting density of prey
# y_start = starting density of predators
# x = density of prey
# y = density of predators
# x_new = reestimation of x for each iteration
# y_new = reestimation of y for each iteration
# A = birth rate of prey
# B = rate of consumption of prey by predator
# C = birth rate of predators
# D = death rate of predators in the absence of prey
# iterations = number of iterations to estimate (i.e. time)
# time_step = time step (dt) needed for Euler's approximation
Lotka_Volterra<-function(x_start=5,y_start=2,A=1,B=0.2,C=0.04,D=0.5,iterations=10000,time_step=0.01){
  x<-x_start
  y<-y_start
  dt<-time_step
  graph_frame<-c(0,x,y)
  for( i in 1:iterations)
  {
    dx<-(A*x-B*x*y)*dt
    dy<-(C*y*x-D*y)*dt
    
    x_new<-x+dx
    y_new<-y+dy
    
    new_data<-c(i*dt,x,y)
    
    graph_frame<-rbind(graph_frame,new_data)
    
    x<-x_new
    y<-y_new
  }
  return(graph_frame)
}
t<-as.data.frame(Lotka_Volterra())

matplot(x=t[,1],y=t[,c(2,3)],pch=20)

matplot(x=t[,2], y=t[,3], pch = 20)

ggplot(t, aes(V2, V3)) + geom_point()
