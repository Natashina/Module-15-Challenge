
MechaCar <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

model_MC <- lm(mpg ~ vehicle_length,MechaCar) #create linear model
yvals <- model_MC$coefficients['vehicle_length']*MechaCar$vehicle_length + model_MC$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(MechaCar,aes(x=vehicle_length,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model
summary(lm(mpg~vehicle_length,MechaCar)) #summarize linear model

model_MCW <- lm(mpg ~ vehicle_weight,MechaCar) #create linear model
yvals <- model_MCW$coefficients['vehicle_weight']*MechaCar$vehicle_weight + model_MCW$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(MechaCar,aes(x=vehicle_weight,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model

model_MCS <- lm(mpg ~ spoiler_angle,MechaCar) #create linear model
yvals <- model_MCS$coefficients['spoiler_angle']*MechaCar$spoiler_angle + model_MCS$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(MechaCar,aes(x=spoiler_angle,y=mpg)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model


lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar) #generate multiple linear regression model

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar)) #generate summary statistics


S_coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
summary(S_coil)
median(S_coil$PSI) # Median
mean(S_coil$PSI) # Mean
var(S_coil$PSI) # Variance
sd(S_coil$PSI) # Standard deviation

summary_S_coil <- S_coil %>% summarize(Mean_PSI=mean(S_coil$PSI),Median_PSI=median(S_coil$PSI),Variance=var(S_coil$PSI),St_dev=sd(S_coil$PSI)) #create summary table with multiple columns

shapiro.test(S_coil$PSI)

plt <- ggplot(S_coil,aes(x=(PSI))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

lot1 <- S_coil[S_coil$Manufacturing_Lot == 'Lot1',]
lot2 <- S_coil[S_coil$Manufacturing_Lot == 'Lot2',]
lot3 <- S_coil[S_coil$Manufacturing_Lot == 'Lot3',]

shapiro.test(lot1$PSI)
shapiro.test(lot2$PSI)
shapiro.test(lot3$PSI)

sample_table2 <- lot2 %>% sample_n(20) #randomly sample 20 data points
sample_table3 <- lot3 %>% sample_n(20) #randomly sample 20 data points

t.test((sample_table2$PSI),mu=mean(S_coil$PSI)) #compare sample of lot2 versus population means
t.test((sample_table3$PSI),mu=mean(S_coil$PSI)) #compare sample of lot3 versus population means

