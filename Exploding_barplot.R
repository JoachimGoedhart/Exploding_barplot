# Script that:
# Reads csv file (with header)
# Calculates summary statistics
# Plots the data as barplot, which explodes to reveal individual datapoints plotted according to distribution (geom_quasirandom)
# Output consists of plots, saved as .png files - can be used to create GIF (e.g. in FIJI)

# Created by: 
# Joachim Goedhart, @joachimgoedhart, 2021

#Requires the packages tidyverse, ggbeeswarm

require(ggbeeswarm)
require(tidyverse)

########## Some user-defined parameter ###############
#number of steps for the animation (more steps results = smoother animation)
nsteps=20
counter=5
p=0.4

#Limits of the Y-axis
Ymin=0.0
Ymax=4200

#Label Y-axis
y_lab <- "Area [µm²]"

# Set aspect ratio of the graph n/n = 1 = square
graph_aspect_ratio=2/1.8

#set Theme
mytheme <- (theme_light(base_size = 16)+  theme(axis.text.x = element_text(size=18, angle=0, vjust = 0, hjust = 1), axis.text.y = element_text(size=18))+theme(aspect.ratio=graph_aspect_ratio)) +     theme(panel.border = element_rect(size = 0.5, linetype = "solid", colour = "black", fill=NA))


#Clear the dataframe if it exists
if (exists("df.summary")) rm(df.summary)
if (exists("df")) rm(df)
if (exists("df_tidy")) rm(df_tidy)

#Read a text file (comma separated values)
df <- read.csv("Area_in_um-GEFs.csv", na.strings = "")

#Tidy the data, i.e. long format with each row is variable
df_tidy <- df %>% gather(key = "condition", value = "Area", na.rm = TRUE)

######### Calulcate summary statistics to fill dataframe 'df.summary' ########

df.summary <- df_tidy %>%
  group_by(condition) %>%
  summarise(n=n(),mean=mean(Area),sd=sd(Area)) %>%
  mutate(sem=sd/sqrt(n-1))


###########################################
##### Bar plot - lowering the plunger 
###########################################

for (i in 0:9) {
  
  fraction_linear = 1-i/9
  mypath <- file.path(paste("frame_0", i, ".png", sep = ""))
  set.seed(2)
  
  png(file=mypath, width = 640, height = 640, units = "px")
    myplot <- ggplot(df.summary, aes(x = condition , y = mean))  +
    geom_errorbar(aes(ymin=mean*0.9, ymax=mean+(sem*fraction_linear)), width=.3, size=1) +
    geom_bar(position = position_dodge(), stat="identity", fill="grey50", color=NA, width=0.6) +
    ylim(Ymin,Ymax) + 

    # Style the axis (font size)
    scale_x_discrete(limits=c('LARG','wt','TIAM'))+
     #Label y-axis and title
     ylab(y_lab) + ggtitle("Lowering the plunger")+
    
    #Apply theme
    mytheme
    
    print(myplot)
  dev.off()
}

########################################
######### Count Down ###################
########################################

for (i in 0:counter) {

  mypath <- file.path(paste("frame_", i+10, ".png", sep = ""))
  set.seed(2)

  png(file=mypath, width = 640, height = 640, units = "px")
  
  myplot <- ggplot(df.summary, aes(x = condition , y = mean))  +
  geom_errorbar(aes(ymin=mean, ymax=mean), width=.3, size=1)+
   geom_bar(position = position_dodge(), stat="identity", fill="grey50", color=NA, width=0.6, alpha=((counter-i)/counter)) +
    
  ylim(Ymin,Ymax) + 
    
    # Style the axis (font size)
    # Style the axis (font size)
    scale_x_discrete(limits=c('LARG','wt','TIAM'))+
    #Label y-axis and title
    ylab(y_lab) + ggtitle(paste("Count-down: ",counter-i, sep = "")) +
    
    #Apply theme
    mytheme
  
  print(myplot)
  dev.off()
}


###########################################
########### Exploding dots ################
###########################################

for (i in 0:nsteps) {
  
  #Animation effect - bounce: exponential*sine 
  #See: https://www.joshondesign.com/2013/03/01/improvedEasingEquations
  fraction_exp = 2^(-15*(0.45*i/nsteps))*sin(((0.45*i/nsteps)-p/4)*(2*3.141593/p))+1
  # print(fraction_exp)
  
  #Calculate new position for dots and store in dataframe
  df_tidy <- ddply(df_tidy, .(condition), transform, newvalue = (Area - (Area - mean(Area))*(1-fraction_exp)*1.000001))
  
  mypath <- file.path(paste("frame_", i+counter+11, ".png", sep = ""))
  set.seed(2)
  
  png(file=mypath, width = 640, height = 640, units = "px")
  
  myplot <- ggplot(df.summary, aes(x = condition , y = mean))  +
    geom_quasirandom(data = df_tidy, aes(condition, newvalue), width=(0.3*fraction_exp+0.15), size=0.5+2*fraction_exp, color="black", alpha=0.5) + ylim(Ymin,Ymax) +
    
    # Style the axis (font size)
    scale_x_discrete(limits=c('LARG','wt','TIAM'))+
    #Label y-axis and title
    ylab(y_lab) + ggtitle("Exposing the data") +

    #Apply theme
    mytheme
  
  print(mypath)
  print(myplot)
  dev.off()
}


###########################################
########### Imploding dots ################
###########################################

for (i in 0:nsteps) {
  
  #Animation effect - bounce: exponential*sine 
  #See: https://www.joshondesign.com/2013/03/01/improvedEasingEquations
  fraction_exp = 1-(2^(-15*(0.45*i/nsteps))*sin(((0.45*i/nsteps)-p/4)*(2*3.141593/p))+1)
  #print(fraction_exp)
  
  #Calculate new position for dots and store in dataframe
  df_tidy <- ddply(df_tidy, .(condition), transform, newvalue = (Area - (Area - mean(Area))*(1-fraction_exp)*1.000001))
  
  mypath <- file.path(paste("frame_", i+nsteps+counter+12, ".png", sep = ""))
  set.seed(2)
  
  png(file=mypath, width = 640, height = 640, units = "px")
  myplot <- ggplot(df_tidy, aes(x = condition, y = Area)) +
    geom_quasirandom(data = df_tidy, aes(condition, newvalue), width=(0.15*fraction_exp+0.3), size=0.5+2*fraction_exp, color="black", alpha=0.5) + ylim(Ymin,Ymax) +
    
    
    # Style the axis (font size)
    # Style the axis (font size)
    scale_x_discrete(limits=c('LARG','wt','TIAM'))+
    #Label y-axis and title
    ylab(y_lab) + ggtitle("Hiding the data") +
    
  #Apply theme
  mytheme
  
  print(myplot)
  dev.off()
}



########################################
######### From means to bars ###########
########################################


for (i in 0:counter) {
  
  mypath <- file.path(paste("frame_", i+nsteps+nsteps+counter+13, ".png", sep = ""))
  set.seed(2)
  png(file=mypath, width = 640, height = 640, units = "px")
  
#  ggplot(mapping = aes(displ, hwy))
#  geom_point(data = mpg) + 
#  geom_line(data = grid) + 
    
  myplot <- ggplot(df.summary, aes(x = condition , y = mean))  +
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.3, size=1, alpha=(i/counter))+
    geom_bar(position = position_dodge(), stat="identity", fill="grey50", color=NA, width=0.6, alpha=(i/counter)) +

#       geom_jitter(data = df_tidy, aes(x = condition, y = mean))
    
 #     geomjitter(position=position_jitter(0.5), cex=2, color="black", alpha=0.5*(counter-i)/counter)) 

 #   geom_jitter(data = df_tidy, aes(condition, newvalue), position=position_jitter(0.3), cex=0.5, color="black", ((counter-i)/counter)) +
    
    geom_quasirandom(data = df_tidy, aes(condition, newvalue), width=(0.3), size=0.5, color="black",alpha=0.5*((counter-i)/counter)) +
    
    ylim(Ymin,Ymax) +

    # Style the axis (font size)
    scale_x_discrete(limits=c('LARG','wt','TIAM'))+
    #Label y-axis and title
    ylab(y_lab) + ggtitle(paste("Hiding the data")) +
    
  #Apply theme
  mytheme
  
    print(mypath)
  print(myplot)
  dev.off()
}




