#Script by : Vishwesh Deore
#Dataset taken from UCI (https://archive.ics.uci.edu/) : Combined Cycle Power Plant

#The following script is used to build a Linear Regression Model which can be used to predict the Energy Output[PE] of the Power Plant with the help of the given data



#Step 1 : Loading Data
#Reading Data 
PowerPlant <- read.csv("PowerPlant.csv")

#View Data
View(PowerPlant)
#Print First 6 Records
head(PowerPlant)

#Check Summary
summary(PowerPlant)
class(PowerPlant)

#Step 2 : Data Cleaning
#Checking Outliers
bx <- boxplot(PowerPlant$AT) #No Outliers Found
bx <- boxplot(PowerPlant$V)  #No Outliers Found

bx <- boxplot(PowerPlant$AP) #Outliers Exist 
#Removing Outliers :
quantile(PowerPlant$AP,seq(0,1,0.01))

# 0%       1%       2%       3%       4%       5%       6%       7%       8%       9%      10%      11%      12% 
# 992.890 1000.680 1001.937 1002.660 1003.540 1003.994 1004.550 1005.010 1005.404 1005.700 1006.037 1006.310 1006.590 
# 13%      14%      15%      16%      17%      18%      19%      20%      21%      22%      23%      24%      25% 
# 1006.840 1007.074 1007.290 1007.470 1007.640 1007.840 1008.000 1008.200 1008.410 1008.550 1008.730 1008.911 1009.100 
# 26%      27%      28%      29%      30%      31%      32%      33%      34%      35%      36%      37%      38% 
# 1009.280 1009.430 1009.620 1009.754 1009.881 1010.040 1010.190 1010.350 1010.540 1010.725 1010.870 1011.000 1011.120 
# 39%      40%      41%      42%      43%      44%      45%      46%      47%      48%      49%      50%      51% 
# 1011.270 1011.400 1011.540 1011.720 1011.870 1012.040 1012.210 1012.340 1012.510 1012.680 1012.820 1012.940 1013.070 
# 52%      53%      54%      55%      56%      57%      58%      59%      60%      61%      62%      63%      64% 
# 1013.180 1013.280 1013.402 1013.580 1013.750 1013.880 1014.029 1014.190 1014.360 1014.550 1014.760 1014.940 1015.129 
# 65%      66%      67%      68%      69%      70%      71%      72%      73%      74%      75%      76%      77% 
# 1015.280 1015.480 1015.690 1015.900 1016.060 1016.250 1016.460 1016.640 1016.840 1017.050 1017.260 1017.450 1017.620 
# 78%      79%      80%      81%      82%      83%      84%      85%      86%      87%      88%      89%      90% 
# 1017.800 1017.960 1018.186 1018.443 1018.720 1019.000 1019.313 1019.560 1019.810 1020.110 1020.340 1020.580 1020.813 
# 91%      92%      93%      94%      95%      96%      97%      98%      99%     100% 
#   1021.200 1021.670 1022.210 1022.890 1023.450 1024.340 1025.240 1026.307 1028.297 1033.300 

PowerPlant$AP <- ifelse(PowerPlant$AP<1000,1000,PowerPlant$AP) #1% data loss
PowerPlant$AP <- ifelse(PowerPlant$AP>1028,1028,PowerPlant$AP) #1% data loss

bx <- boxplot(PowerPlant$AP) #Outliers Removed

bx <- boxplot(PowerPlant$RH) #Outliers Exist
#Removing Outliers :
quantile(PowerPlant$RH,seq(0,1,0.01))

# 0%       1%       2%       3%       4%       5%       6%       7%       8%       9%      10%      11%      12% 
# 25.5600  38.0835  41.3202  43.0802  45.0600  46.5835  48.0200  49.3369  50.6200  51.7409  52.7570  53.6000  54.5000 
# 13%      14%      15%      16%      17%      18%      19%      20%      21%      22%      23%      24%      25% 
# 55.3071  55.9600  56.7705  57.6200  58.2900  58.9606  59.6373  60.3880  61.0000  61.6174  62.1941  62.8100  63.3275 
# 26%      27%      28%      29%      30%      31%      32%      33%      34%      35%      36%      37%      38% 
# 63.8000  64.4109  64.9052  65.4100  65.9910  66.4500  66.8844  67.3711  67.9200  68.3700  68.9012  69.3100  69.8300 
# 39%      40%      41%      42%      43%      44%      45%      46%      47%      48%      49%      50%      51% 
# 70.1813  70.5980  71.0700  71.5314  72.0081  72.4548  72.8500  73.2982  73.7049  74.1500  74.5500  74.9750  75.2800 
# 52%      53%      54%      55%      56%      57%      58%      59%      60%      61%      62%      63%      64% 
# 75.6484  76.0600  76.4300  76.8900  77.2800  77.7300  78.0886  78.4200  78.9100  79.2800  79.6700  80.2100  80.6788 
# 65%      66%      67%      68%      69%      70%      71%      72%      73%      74%      75%      76%      77% 
# 81.0900  81.4522  81.8300  82.2200  82.5700  82.9600  83.3200  83.6924  84.1000  84.4358  84.8300  85.1792  85.5259 
# 78%      79%      80%      81%      82%      83%      84%      85%      86%      87%      88%      89%      90% 
# 85.9426  86.3000  86.6400  87.0327  87.3400  87.7600  88.2228  88.6000  89.0600  89.5300  89.9600  90.4800  91.0000 
# 91%      92%      93%      94%      95%      96%      97%      98%      99%     100% 
#   91.5997  92.1300  92.9193  93.7000  94.5965  95.5164  96.4499  97.6332  99.2365 100.1600 

PowerPlant$RH <- ifelse(PowerPlant$RH<38,38,PowerPlant$RH) #1% Data Loss
bx <- boxplot(PowerPlant$RH) #Outliers Removed

#Step 3 : Training And Testing Data
t1=sample(1:nrow(PowerPlant),0.8*nrow(PowerPlant))
t_train1=PowerPlant[t1,]
t_test1=PowerPlant[-t1,]

#Step 4 : Creating Model
library(car)
mod1 <- lm(PE~.,data=t_train1)
summary(mod1)
vif(mod1)

#The vif value of AT is greater than 5 so another model ignoring AT can be created :

mod2 <- lm(PE~V+AP+RH,data=t_train1)
summary(mod2)
vif(mod2)

#Step 5 : Predictions
#Based on Model 1

prediction = predict(mod1,t_test1)
head(prediction)

View(prediction)
#To compare predictions with test data :
output = cbind(t_test1,prediction)
head(output)
final_output = output[-c(1:4)]
head(final_output)
#plot
plot(final_output$PE,final_output$prediction)

#Based on Model 2

prediction = predict(mod2,t_test1)

head(prediction)
View(prediction)
#To compare predictions with test data :
output = cbind(t_test1,prediction)
head(output)
final_output = output[-c(1:4)]
head(final_output)
#Plot
plot(final_output$PE,final_output$prediction)

#To check Regression Evaluation
install.packages("DMwR")
library(DMwR)
regr.eval(final_output$PE,final_output$prediction) #Run the model script first for which eval is to be printed

##############################################################################################################