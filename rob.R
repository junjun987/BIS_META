install.packages("robvis")
library(robvis)
bias <- read.csv("bias.csv",header=T)
rob_summary(bias,tool = "ROB1",weighted = FALSE,overall = TRUE)
rob_traffic_light(data = bias, 
                  tool = "ROB1",psize=8,quiet = TRUE)
