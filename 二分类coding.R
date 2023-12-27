install.packages("metafor")
library(metafor)
library(meta)
#pod
POD <- read.csv("POD.csv",header = T)
POD <- subset(POD,POD$group=="Combined")
POD <- POD[-c(8),]
POD_R <- metabin(Events.E,Total.E,Events.C,Total.C,data = POD,
                 method = "MH",sm="RR",comb.fixed=T,comb.random =F,
                 studlab =paste(POD$Author,POD$Year,sep = "-"))
POD_R                 
forest(POD_R,lab.e="BIS",lab.c="Control")                
forest(POD_R,family="sans",fontsize=9.5,lab.e="BIS",lab.c="Control",
       lwd=2,col.diamond.fixed="lightslategray",col.diamond.lines.fixed="lightslategray",
       col.diamond.random="maroon",col.diamond.lines.random="maroon",col.square="skyblue",col.study="lightslategray",
       lty.fixed=4,plotwidth="6cm",colgap.forest.left="1cm",colgap.forest.right="1cm",just.forest="right",colgap.left="0.5cm",
       colgap.right="0.5cm")
#漏斗图
funnel(POD_R, main="Funnel Plot")
meta::metabias(POD_R, k.min = 6, method.bias = "Peter")
meta::metabias(POD_R, k.min = 4, method.bias = "Egger")
#f1 <- funnel(POD_R,comb.fixed=TRUE, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
#legend(0.02, 0.01, c("0.1>p>0.05","0.05>p>0.01","<0.01"),fill=c("darkgreen", "green","lightgreen"))
#剪补漏斗
POD_trimfill<-trimfill(POD_R,comb.fixed = T,comb.random = F)
print(POD_trimfill)
summary(mytrimfill)
funnel(mytrimfill, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
legend(2, 0.01,cex=0.6,c("0.1>p>0.05","0.05>p>0.01", "<0.01"),fill=c("darkgreen","green", "lightgreen"))

#敏感性
#sensitive
forest(metainf(POD_R,pooled = "random"))
#恶心呕吐
R2 <- read.csv("R2.csv",header = T)
R2 <- subset(R2,R2$Group=="Combined")
View(R2)
R2 <- R2[-c(13),]
R2 <- R2[-c(1),]
R2_R <- meta::metabin(Events.E,Total.E,Events.C,Total.C,data = R2, 
                 method = "MH",sm="RR",comb.fixed=T,comb.random = F,
                 studlab =paste(R2$Author,R2$Year,sep = "-"))
R2_R                 
forest(R2_R,lab.e="BIS",lab.c="Control")                
forest(R2_R,family="sans",fontsize=9.5,lab.e="BIS",lab.c="Control",
       lwd=2,col.diamond.fixed="lightslategray",col.diamond.lines.fixed="lightslategray",
       col.diamond.random="maroon",col.diamond.lines.random="maroon",col.square="skyblue",col.study="lightslategray",
       lty.fixed=4,plotwidth="6cm",colgap.forest.left="1cm",colgap.forest.right="1cm",just.forest="right",colgap.left="0.5cm",
       colgap.right="0.5cm")

#funnel
funnel(R2_R, main="Funnel Plot")
meta::metabias(R2_R, k.min = 7, method.bias = "Peters")
meta::metabias(R2_R, k.min =4, method.bias = "Egger")
f1 <- funnel(R2_R,comb.fixed=TRUE, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
legend(0.02, 0.01, c("0.1>p>0.05","0.05>p>0.01","<0.01"),fill=c("darkgreen", "green","lightgreen"))
#剪补漏斗
mytrimfill<-trimfill(R2_R,comb.fixed=TRUE)
summary(mytrimfill)
funnel(mytrimfill,level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
legend(8, 0.01,cex=0.6,c("0.1>p>0.05","0.05>p>0.01", "<0.01"),fill=c("darkgreen","green", "lightgreen"))
#敏感性
#sensitive
forest(metainf(R2_R,pooled = "fixed"))

#高血压
R3 <- read.csv("R3.csv",header = T)
view(R3)
R3 <- subset(R3,R3$group=="IVA")
R3_R <- meta::metabin(Events.E,Total.E,Events.C,Total.C,data = R3, 
                      method = "MH",sm="RR",comb.fixed=T,comb.random = F,
                      studlab =paste(R3$Author,R3$Year,sep = "-"))
R3_R                 
forest(R3_R,lab.e="BIS",lab.c="Control")                
forest(R3_R,family="sans",fontsize=9.5,lab.e="BIS",lab.c="Control",
       lwd=2,col.diamond.fixed="lightslategray",col.diamond.lines.fixed="lightslategray",
       col.diamond.random="maroon",col.diamond.lines.random="maroon",col.square="skyblue",col.study="lightslategray",
       lty.fixed=4,plotwidth="6cm",colgap.forest.left="1cm",colgap.forest.right="1cm",just.forest="right",colgap.left="0.5cm",
       colgap.right="0.5cm")

#funnel
funnel(R3_R, main="Funnel Plot")
meta::metabias(R3_R, k.min = 7, method.bias = "Peters")
meta::metabias(R3_R, k.min = 3, method.bias = "Egger")
f1 <- funnel(R3_R,comb.fixed=TRUE, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
legend(0.02, 0.01, c("0.1>p>0.05","0.05>p>0.01","<0.01"),fill=c("darkgreen", "green","lightgreen"))
#剪补漏斗
mytrimfill<-trimfill(R3_R,comb.random=F, comb.fixed=T)
summary(mytrimfill)
funnel(mytrimfill, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
legend(4, 0.01,cex=0.6,c("0.1>p>0.05","0.05>p>0.01", "<0.01"),fill=c("darkgreen","green", "lightgreen"))
#异质性
R3_R2<- meta::metabin(Events.E,Total.E,Events.C,Total.C,data = R3, sm="RR",
                      byvar =Group,studlab =paste(R3$Author,R3$Year,sep = "-"))
R3_R2
forest(R3_R2,lab.e="BIS",lab.c="Control")
forest(R3_R2,family="sans",fontsize=9.5,lab.e="BIS",lab.c="Control",
       lwd=2,col.diamond.fixed="lightslategray",col.diamond.lines.fixed="lightslategray",
       col.diamond.random="maroon",col.diamond.lines.random="maroon",col.square="skyblue",col.study="lightslategray",
       lty.fixed=4,plotwidth="4cm",colgap.forest.left="1cm",colgap.forest.right="1cm",just.forest="right",colgap.left="0.5cm",
       colgap.right="0.5cm")
#敏感性
#sensitive
forest(metainf(R3_R,pooled = "random"))
#new-meta
R3_new  <- R3[-c(7,9),]
R3_new_meta  <- meta::metabin(Events.E,Total.E,Events.C,Total.C,data = R3_new ,
                      method = "MH",sm="RR",comb.fixed=T,comb.random = T,
                      studlab =paste(R3_new$Author,R3_new$Year,sep = "-"))
R3_new_meta                
forest(R3_new_meta,lab.e="BIS",lab.c="Control")                

#R4
R4 <- read.csv("R4.csv",header = T)
R4 <- subset(R4,R4$Group=="Combined")

View(R4)
R4 <- R4[-c(11),]
R4_R <- metabin(Events.E,Total.E,Events.C,Total.C,data = R4, 
                method = "MH",sm="RR",comb.fixed=T,comb.random = F,
                      studlab =paste(R4$Author,R4$Year,sep = "-"),add=0.5)
R4_R                 
forest(R4_R,lab.e="BIS",lab.c="Control")                
forest(R4_R,family="sans",fontsize=9.5,lab.e="BIS",lab.c="Control",
       lwd=2,col.diamond.fixed="lightslategray",col.diamond.lines.fixed="lightslategray",
       col.diamond.random="maroon",col.diamond.lines.random="maroon",col.square="skyblue",col.study="lightslategray",
       lty.fixed=4,plotwidth="6cm",colgap.forest.left="1cm",colgap.forest.right="1cm",just.forest="right",colgap.left="0.5cm",
       colgap.right="0.5cm")
#sensitive
forest(metainf(R4_R,pooled = "random"))

#funnel
funnel(R4_R, main="Funnel Plot")
meta::metabias(R4_R, k.min = 3, method.bias = "Egger")
meta::metabias(R4_R, k.min = 10, method.bias = "Peter")
f1 <- funnel(R4_R,comb.fixed=TRUE, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
legend(0.02, 0.01, c("0.1>p>0.05","0.05>p>0.01","<0.01"),fill=c("darkgreen", "green","lightgreen"))
#剪补漏斗
mytrimfill<-trimfill(R4_R,comb.fixed=TRUE)
summary(mytrimfill)
funnel(mytrimfill,level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
summary(mytrimfill)
legend(5, 0.01,cex=0.6, c("0.1>p>0.05","0.05>p>0.01", "<0.01"),fill=c("darkgreen","green", "lightgreen"))

#R5
R5 <- read.csv("R5.csv",header = T)
R5 <- subset(R5,R5$Group=="Combined")
R5 <- R5[-c(8),]
R5_R <- meta::metabin(Events.E,Total.E,Events.C,Total.C,data = R5,
                      method = "MH",sm="RR",comb.fixed=T,comb.random =F,
                      studlab =paste(R5$Author,R5$Year,sep = "-"))
R5_R                 
forest(R5_R,lab.e="BIS",lab.c="Control")                
forest(R5_R,family="sans",fontsize=9.5,lab.e="BIS",lab.c="Control",
       lwd=2,col.diamond.fixed="lightslategray",col.diamond.lines.fixed="lightslategray",
       col.diamond.random="maroon",col.diamond.lines.random="maroon",col.square="skyblue",col.study="lightslategray",
       lty.fixed=4,plotwidth="6cm",colgap.forest.left="1cm",colgap.forest.right="1cm",just.forest="right",colgap.left="0.5cm",
       colgap.right="0.5cm")
forest(metainf(R5_R ,pooled = "random"))
#funnel
funnel(R5_R, main="Funnel Plot")
meta::metabias(R5_R, k.min = 4, method.bias = "Egger")
meta::metabias(R5_R, k.min = 4, method.bias = "Peters")
f1 <- funnel(R5_R,comb.fixed=TRUE, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
legend(0.02, 0.01, c("0.1>p>0.05","0.05>p>0.01","<0.01"),fill=c("darkgreen", "green","lightgreen"))
#剪补漏斗
mytrimfill<-trimfill(R5_R)
summary(mytrimfill)
funnel(mytrimfill,comb.fixed=TRUE, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)

legend(5, 0.01, cex=0.6,c("0.1>p>0.05","0.05>p>0.01", "<0.01"),fill=c("darkgreen","green", "lightgreen"))

#R6
R6 <- read.csv("R6.csv",header = T)
R6 <- R6[-c(1),]
R6_R <- meta::metabin(Events.E,Total.E,Events.C,Total.C,data = R6,
                      method = "MH",sm="RR",comb.fixed=F,comb.random = T,
                      studlab =paste(R6$Author,R6$Year,sep = "-"))
R6_R                 
forest(R6_R,lab.e="BIS",lab.c="Control")                
forest(R6_R,family="sans",fontsize=9.5,lab.e="BIS",lab.c="Control",
       lwd=2,col.diamond.fixed="lightslategray",col.diamond.lines.fixed="lightslategray",
       col.diamond.random="maroon",col.diamond.lines.random="maroon",col.square="skyblue",col.study="lightslategray",
       lty.fixed=4,plotwidth="6cm",colgap.forest.left="1cm",colgap.forest.right="1cm",just.forest="right",colgap.left="0.5cm",
       colgap.right="0.5cm")
forest(metainf(R6_R ,pooled = "random"))
#funnel
funnel(R6_R, main="Funnel Plot")
meta::metabias(R6_R, k.min = 3, method.bias = "Egger")
f1 <- funnel(R6_R,comb.fixed=TRUE, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)
legend(0.02, 0.01, c("0.1>p>0.05","0.05>p>0.01","<0.01"),fill=c("darkgreen", "green","lightgreen"))
#剪补漏斗
mytrimfill<-trimfill(R6_R)
funnel(mytrimfill,comb.fixed=TRUE, level=0.95, contour=c(0.9, 0.95, 0.99), col.contour=c("darkgreen","green", "lightgreen"), lwd=2, cex=1, pch=14, studlab=TRUE,cex.studlab=0.75)

legend(6, 0.01, cex=0.6,c("0.1>p>0.05","0.05>p>0.01", "<0.01"),fill=c("darkgreen","green", "lightgreen"))

