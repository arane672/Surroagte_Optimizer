rm(list = ls())
set.seed(1234)
library(DiceDesign)
library(DiceOptim)
library(GPareto)


data = read.csv("C:/Users/Aditya Rane/OneDrive - Oklahoma A and M System/Research Drive/Research/MS-Thesis/Bio-printing thesis Ansys/R RESULTS/bioprint_Rdata.csv", head = TRUE)
head(data)
print(data)
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}

norm_data = as.data.frame(apply(data[1:8],2,normalize))
head(norm_data)

# design inputs 
design.grid <- as.matrix(norm_data[1:44,1:5])
print(design.grid)
# responses
response.grid <- as.matrix(norm_data[1:44,6:8])
print(response.grid) 
# test points 
#n.grid <- 24 
test_val <- seq(0, 1, length.out = 25)
test.grid <- expand.grid(var1 = test_val, var2 = test_val, var3 = test_val, var4 = test_val, var5 = test_val)
test.grid <- as.matrix(test.grid)


# ### Kernel 1 #########
#model 1
#mf11 <- km(~.,design = design.grid, response = response.grid[,1],covtype="gauss",nugget = 1e-3)
#p11 <- predict(mf11, newdata = data.frame(test.grid), type = "SK")
#RMSE11<-sqrt(mean((test.grid- p11$mean)^2))

### model 2###
#mf12 <- km(~., design = design.grid, response = response.grid[,2],covtype="gauss",nugget = 1e-3)
#p12 <- predict(mf12, newdata = data.frame(test.grid), type = "SK")
#RMSE12 <- sqrt(mean((test.grid- p12$mean)^2))

##### model 3
mf13 <- km(~., design = design.grid, response = response.grid[,3],covtype="gauss",nugget = 1e-3)
p13 <- predict(mf13, newdata = data.frame(test.grid), type = "SK")
#RMSE13<-sqrt(mean((test.grid- p13$mean)^2))

###### Results Plot ################################
library(dplyr)
tmp <- cbind(test.grid[,2], test.grid[,3], p13$sd)
tmp <- as.data.frame(tmp)  
  group_by(V1,V2) %>%
  summarise(Sd = mean(V3))
library(ggplot2)
# Plot using ggplot2
ggplot(summtmp, aes(x = V1, y = V2, fill = Sd)) +
  geom_tile() +  
  scale_fill_gradient(low = "orange", high = "red") + 
  labs(x = expression(~eta["max"]), y = expression(~eta["min"]))



#~eta["min"]
#limits = c(0.0,0.5)




####### 3d scatter ################################ #########################

tmpd <- cbind(design.grid[,2], design.grid[,4], p13$mean)
tmpd <- as.data.frame(tmp)
summtmpd <- tmpd %>%
  group_by(V1,V2) %>%
  summarise(Meand = mean(V3))
ggplot(summtmpd, aes(x = V1, y = V2, fill = Meand)) +
  geom_tile() +  # Add tiles
  scale_fill_gradient(low = "red", high = "orange", limits = c(0, 1)) +  # Choose gradient colors
  labs(x = expression(eta[~"min"]), y = expression(k))  # Add labels and title







plot_ly(data = summtmp, x = ~V1, y = ~V2, z = ~Mean, type = "scatter", mode = "markers",
        marker = list(color = ~Mean, colorscale = "Viridis", size = 10,
                      colorbar = list(title = "Mean"))) %>%
  layout(scene = list(xaxis = list(title = expression(eta[~"min"])),
                      yaxis = list(title = expression(k)),
                      zaxis = list(title = "Mean")))


######### mean estimates #######
ggplot(summtmp, aes(x = V1, y = V2, color = Mean)) +
  geom_point(size = 3) +
  scale_color_viridis_c() +  # Use the Viridis color scale
  labs(x = expression(eta[~"min"]), y = expression(k), color = "Mean") +
  theme_minimal()


##############  Kernel 2 ########
# GP model of three responses 
#model 1
mf21 <- km(~.,design = design.grid, response = response.grid[,1],covtype="exp",nugget = 1e-3)
p21 <- predict(mf21, newdata = data.frame(test.grid), type = "SK")
RMSE21<-sqrt(mean((test.grid- p21$mean)^2))

# model 2
mf22 <- km(~., design = design.grid, response = response.grid[,2],covtype="exp",nugget = 1e-3)
p22 <- predict(mf22, newdata = data.frame(test.grid), type = "SK")
RMSE22 <- sqrt(mean((test.grid- p22$mean)^2))

##### model 3
mf23 <- km(~., design = design.grid, response = response.grid[,3],covtype="exp",nugget = 1e-3)
p23 <- predict(mf23, newdata = data.frame(test.grid), type = "SK")
RMSE23<-sqrt(mean((test.grid- p23$mean)^2))

##############  kernel 3 ########
# GP model of three responses 
#model 1
mf31 <- km(~.,design = design.grid, response = response.grid[,1],covtype="matern3_2",nugget = 1e-3)
p31 <- predict(mf31, newdata = data.frame(test.grid), type = "SK")
RMSE31<-sqrt(mean((test.grid- p31$mean)^2))

# model 2
mf32 <- km(~., design = design.grid, response = response.grid[,2],covtype="matern3_2",nugget = 1e-3)
p32 <- predict(mf32, newdata = data.frame(test.grid), type = "SK")
RMSE32 <- sqrt(mean((test.grid- p32$mean)^2))

##### model 3
mf33 <- km(~., design = design.grid, response = response.grid[,3],covtype="matern3_2",nugget = 1e-3)
p33 <- predict(mf33, newdata = data.frame(test.grid), type = "SK")
RMSE33<-sqrt(mean((test.grid- p33$mean)^2))




##############  kernel 4 ########
# GP model of three responses 
#model 1
mf41 <- km(~.,design = design.grid, response = response.grid[,1],covtype="matern5_2",nugget = 1e-3)
p41 <- predict(mf41, newdata = data.frame(test.grid), type = "SK")
RMSE41<-sqrt(mean((test.grid- p41$mean)^2))

# model 2
mf42 <- km(~., design = design.grid, response = response.grid[,2],covtype="matern5_2",nugget = 1e-3)
p42 <- predict(mf42, newdata = data.frame(test.grid), type = "SK")
RMSE42 <- sqrt(mean((test.grid- p42$mean)^2))

##### model 3
mf43 <- km(~., design = design.grid, response = response.grid[,3],covtype="matern5_2",nugget = 1e-3)
p43 <- predict(mf43, newdata = data.frame(test.grid), type = "SK")
RMSE43<-sqrt(mean((test.grid- p43$mean)^2))




















##############  kernel 5 ########
# GP model of three responses 
#model 1
mf51 <- km(~.,design = design.grid, response = response.grid[,1],covtype="powexp",nugget = 1e-3)
p51 <- predict(mf51, newdata = data.frame(test.grid), type = "SK")
RMSE51<-sqrt(mean((test.grid- p51$mean)^2))

# model 2
mf52 <- km(~., design = design.grid, response = response.grid[,2],covtype="powexp",nugget = 1e-3)
p52 <- predict(mf52, newdata = data.frame(test.grid), type = "SK")
RMSE52 <- sqrt(mean((test.grid- p52$mean)^2))

##### model 3
mf53 <- km(~., design = design.grid, response = response.grid[,3],covtype="powexp",nugget = 1e-3)
p53 <- predict(mf53, newdata = data.frame(test.grid), type = "SK")
RMSE53<-sqrt(mean((test.grid- p53$mean)^2))





##### Boxplot RMSE ######
# List RMSE
RMSE_K1 <- c(RMSE11, RMSE12, RMSE13) ##### SE
print(RMSE_K1)

RMSE_K2 <- c(RMSE21, RMSE22, RMSE23) ##### EXP
print(RMSE_K2)

RMSE_K3 <- c(RMSE31, RMSE32, RMSE33) ##### mater3_2
print(RMSE_K3)

RMSE_K4 <- c(RMSE41, RMSE42, RMSE43) ##### mater4_2
print(RMSE_K4)

RMSE_K5 <- c(RMSE51, RMSE52, RMSE53) ##### pexp
print(RMSE_K5)

# Combine into a list
R <- list(K1= RMSE_K1, K2 = RMSE_K2,K3 = RMSE_K3,K4 = RMSE_K4, K5 = RMSE_K5)

# Create a boxplot
boxplot(R, ylab = "RMSE", xlab = "Kernels", col = c("orange", "lightcyan","lightgray","lightsalmon","lightyellow"), names = c("Square Exponential", "Exponential","Matern_(3/2)","Matern_(5/2)","Power Exponential"), border ="black",main="RMSE comparison of different kernels")



model <- c(mf1,mf2,mf3)
print(model)
# Pareto front 
#Front_Pareto <- t(nondominated_points(t(response.grid)))

#set_pareto <- t(nondominated_points(t(design.grid)))

dominates <- function(a, b) {
  return(all(a <= b) && any(a < b))
}

get_non_dominated_set <- function(norm_data) {
  n <- nrow(norm_data)
  non_dominated <- logical(n)
  
  for (i in 1:n) {
    non_dominated[i] <- !any(apply(norm_data, 1, dominates, b = norm_data[i, ]))
  }
  
  return(norm_data[non_dominated, ])
}
non_dominated_points <- get_non_dominated_set(response.grid)
print(non_dominated_points)



EMI_grid <- apply(test.grid, 1, crit_EMI, model = list(mf1, mf2, mf3), paretoFront = non_dominated_points ,critcontrol = list(nb_samp = 20))

lower <- rep(0, 5)
upper <- rep(1, 5)
result <- crit_optimizer(crit = "EMI", model = model, lower = lower, upper = upper,
                         optimcontrol = list(method = "genound", pop.size = 200, BFGSburnin = 2))


print(result)











View(test.grid)


















######### Pareto Front ######
library(rgl)
set.seed(5)
X <- log(response.grid+0.000000000000000000001)
Ynd <- t(nondominated_points(t(X)))
plot3d(X,size=15)
plot3d(Ynd, col="red", size=20, add=TRUE)
#plot3d(x=min(Ynd[,1]), y=min(Ynd[,2]), z=min(Ynd[,3]), col="green", size=20, add=TRUE)
X.range <- diff(apply(X,2,range))
bounds <- rbind(apply(X,2,min)-0.01*X.range,apply(X,2,max)+0.01*X.range)
plotParetoEmp(nondominatedPoints = Ynd, add=TRUE, bounds=bounds, alpha=0.3)
#title3d('main', 'sub', 'xlab', 'ylab', 'zlab')
#legend3d("topright", legend = (c('non-dominated design', 'dominated design')), pch = 16, col = c("red","black"), cex=0.9, inset=c(0.002))
#play3d(spin3d(axis = c(0, 0, 1), rpm = 1), duration = 20)

##########Pareto plot##########
library(scatterplot3d)
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
a<- log(response.grid+0.001)
pf <- scatterplot3d(a, pch = 16,grid= FALSE, box= FALSE, main="Pareto Front")
addgrids3d(a , grid = c("xy", "xz", "yz")) 
pf$points3d(a,col = "red",pch = 16)



