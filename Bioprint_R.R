set.seed(1234)
library(DiceDesign)
library(DiceOptim)
library(GPareto)


data = read.csv("D:/TH/Bio-printing thesis Ansys/R RESULTS/bioprint_Rdata.csv", head = TRUE)
head(data)
print(data)
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}

norm_data = as.data.frame(apply(data[1:8],2,normalize))
head(norm_data)

# design inputs 
design.grid <- as.matrix(norm_data[1:24,1:5])
print(design.grid)
# responses
response.grid <- as.matrix(norm_data[1:24,6:8])
print(response.grid) 
# test points 
#n.grid <- 24 
test_val <- seq(0, 1, length.out = 10)
test.grid <- expand.grid(var1 = test_val, var2 = test_val, var3 = test_val, var4 = test_val, var5 = test_val)
test.grid <- as.matrix(test.grid)

# GP model of three responses 
mf1 <- km(~., design = design.grid, response = response.grid[,1],covtype="gauss")
mf2 <- km(~., design = design.grid, response = response.grid[,2],covtype="gauss")
mf3 <- km(~., design = design.grid, response = response.grid[,3],covtype="gauss")

model <- list(mf1,mf2,mf3)
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



