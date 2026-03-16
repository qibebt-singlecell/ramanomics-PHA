## CODE for Drawing
## Created by zhouxuan @qibebt-SingleCell
## update 2026.01.12

##FIG2-A ------------------------------------------------------------------------------------------------------------------------------ 
##-picture2-A-------green-----------------------------------------------------------------------------
setwd('./data/PHA') #It is recommended to read data using relative paths.
data1 <- read.table('XXT2-4.txt',header = T)
 
#colors1 <- c("#8A919799", "#00CED1", "#87CEFA", "#FED43999", "#FFAEB9", "#EE7600", "#FFD39B") #  #87CEFA  #C1FFC1  #00CD66  #43CD80 深绿  #00CED1
#colors <- c("#8A919799", "#838B8B", "#C1CDCD", "#5F9EA0", "#00688B", "#009ACD","#00B2EE", "#00BFFF","#1E90FF","#104E8B")
colors <- c("#8A919799", "#8A919799", "#8A919799", "#8A919799", "#8A919799", "#8A919799") ## 
pdf("Fig2-svm-boxplot2.pdf", width = 7.5, height = 6)
boxplot(sd ~ Number, data = data1, col = colors,cex.axis = 1.5,cex.lab = 1.5) 
dev.off()

##FIG2-B 1------------------------------------------------------------------------------------------------------------------------------ 
##-----hotmap----Correlation coefficient graph----0929----------
setwd('./data/PHA')
library(ggplot2)
confusion_df <-read.table('picihotmap-ADEDF.txt',header = T)
p <- ggplot(confusion_df, aes(x = Prediction, y = Model)) +
  geom_tile(aes(fill = Accuracy), color = "white") +
  geom_text(aes(label = sprintf("%0.3f",round(Accuracy, digits = 3))), vjust = 1,size = 5, color = "black") +
  scale_fill_gradient(low = "#FFCCCC", high = '#DC1623') +  ##F4A460  #87CEFA    '#DC1623' 红   "#1E90FF"蓝   "#8A919799"灰
  #labs(title = "Confusion Matrix Heatmap with Counts") +
  theme_minimal() +
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=18,colour="black", face = "bold"))+  #angle = 45,
  theme(axis.text.y = element_text(vjust=1,size=18,colour="black",face = "bold"),
        axis.title.x =element_text(size=18,colour="black",face = "bold"), 
        axis.title.y = element_text(size=18,colour="black",face = "bold"), 
        legend.key.size = unit(20, "pt"), 
        legend.text = element_text(colour="black", size = 18, face = "bold"),
        legend.title = element_text(size=18, color = "black",face = "bold")
  ) 

print(p)
ggsave("./data/PHA/Fig2-B0306.pdf", p, width = 18, height = 17, units = "cm")

# -fig3--A-----------------------  
setwd("./data/PHA")
if (!requireNamespace("ggprism", quietly=TRUE))
  install.packages("ggprism")
if (!requireNamespace("ggsci", quietly=TRUE))
  install.packages("showtext")

library(ggplot2)
library(ggprism)
library(showtext)
library(Cairo)
library(ggpubr)
library(ggsci)
data<- read.table("zx3.txt",header = T)
data$Var1<-as.factor(data$Var1)

#data2<-within(data,{Var1<-factor(Var1,levels=c("602",  "678" , "745",  "834",  "947",  "996",  "1051", "1097", "1120", "1164", "1212", "1251", "1300" ,"1350", "1445",
#                                               "1573", "1714", "2865", "2920", "2960", "2985"))})
p=ggplot(data, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value))+ 
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-1, 1)) +
  geom_text(aes(label = value), vjust = 1,size = 6) +
  #geom_text(aes(label = sig), size = 5) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), 
        legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black',size = 12, 
                                   angle = 30, hjust = 1, vjust = 1), 
        axis.text.y = element_text(color = 'black',size = 12), 
        axis.ticks = element_line(color = 'black')) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  # theme_prism(border=TRUE,base_rect_size=0.5)+
  # coord_cartesian(clip="off") +  
  #theme(axis.ticks=element_blank())+ 
  labs(y = '', x = '', fill = 'Correlation',size = 12)+  
  geom_hline(aes(yintercept=1.5),
             colour="black",
             linewidth=0.5)+
  geom_hline(aes(yintercept=2.5),
             colour="black",
             linewidth=0.5)

p
ggsave(filename = "Fig3A-3HB-4HB-peak-cor-train3.pdf",p, width = 9, height = 3.5)

# -fig3--B------------------------  
setwd('./data/PHA')
XDATA1 <- read.table(file="A1_ZWQ_base_gyh-mean.txt",header=T)
XDATA1 <- t(XDATA1)
XDATA2 <- read.table(file="A5_ZWQ_base_gyh_mean.txt",header=T)
XDATA2 <- t(XDATA2)
cha <- XDATA2 - XDATA1
wavenumber <- as.numeric(gsub("X", "", colnames(XDATA1))) # as.numeric(colnames(XDATA1))
spc2hs <- new("hyperSpec",spc = XDATA1,wavelength = wavenumber)
setwd('./data/PHA')

pdf("Fig3B-A5-A1_plot.pdf", width=7, height=6)
plot(spc2hs)
lines(wavenumber,XDATA2,col = 'red')
legend("topright", legend = c("Control", "PHA"), col = c("black", "red"), lty = 1)

dev.off()

#####------fig3--E------------------------------------------------------------------------------------
setwd('./data/PHA')
final_data <- read.csv(file = 'D1-7_np_base_gyh-mean-fg-PHA.csv')
final_data <- final_data[,-1]


library(Hmisc) 
#install.packages('Hmisc')
res2 <- rcorr(as.matrix(final_data))
res2
library(corrplot)
corrplot(res2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(res2$r, type = "upper", order = "hclust", p.mat = res2$P, sig.level = 0.01, tl.col = "black", tl.srt = 45)
correlation <- cor(final_data$fg550, final_data$PHA)
model <- lm(final_data$PHA ~ final_data$fg550, data = final_data)
final_data$predicted <- predict(model)
cor_matrix <- matrix(correlation, ncol = 1)
#write.table(final_data,file = 'D_gyhpredict.csv',col.names = T,row.names = T,sep = ',')
setwd('./data/')
pdf("Fig3E_niheplot.pdf", width=7, height=6)
plot(final_data$fg550, final_data$PHA, 
     main = "fg550", 
     xlab = "fg550",
     ylab = "PHA",
     col.lab = "black",
     pch = 19,
     lwd = 2,cex.lab = 1.5,cex = 1.5, cex.axis = 1.5)
lines(final_data$fg550, final_data$predicted, col = "blue", lwd = 2)

legend("topleft", 
       legend = c(paste("Correlation:", round(correlation, 4)),
                  paste("Regression Equation: y =", round(coef(model)[2], 2), "x +", round(coef(model)[1], 2)),
                  paste("R^2 =", round(summary(model)$r.squared, 2))),
       #col = "blue",
       #lty = 1,
       bty = "n",cex = 1.5)
dev.off()

###3HB
setwd('./data/D-np-base-gyhmean/PHA')
final_data <- read.csv(file = 'ZX2.csv')
correlation <- cor(final_data$fg236, final_data$HB3)
model <- lm(final_data$HB3 ~ final_data$fg236, data = final_data)
final_data$predicted <- predict(model)
cor_matrix <- matrix(correlation, ncol = 1)
#write.table(final_data,file = 'D_gyhpredict.csv',col.names = T,row.names = T,sep = ',')
setwd('./data/')
pdf("Fig3F_niheplot.pdf", width=7, height=6)
plot(final_data$fg236, final_data$HB3, 
     main = "fg236", 
     xlab = "fg236",
     ylab = "3HB",
     col.lab = "black",
     pch = 19,
     lwd = 2,cex.lab = 1.5,cex = 1.5, cex.axis = 1.5)
lines(final_data$fg236, final_data$predicted, col = "blue", lwd = 2)
#lines(XDATA$true,XDATA$true, col = "red", lwd = 2)
legend("topleft", 
       legend = c(paste("Correlation:", round(correlation, 4)),
                  paste("Regression Equation: y =", round(coef(model)[2], 2), "x +", round(coef(model)[1], 2)),
                  paste("R^2 =", round(summary(model)$r.squared, 2))),
       #col = "blue",
       #lty = 1,
       bty = "n",cex = 1.5)

dev.off()


#####--------------fig4-5----------------------------------------1129-----------------------------------
setwd('./data/PHA')
final_data <- read.csv(file = 'ABD-PHA最终建模250328.csv',header = T)
final_data <- final_data[-(33:100),-(14:20)]

library(Hmisc)
res2 <- rcorr(as.matrix(final_data))
res2 
library(corrplot)
col1=colorRampPalette(colors =c('#1874CD', 'white', '#DC1623'),space="Lab") # space参数选择使用RGB或者CIE Lab颜色空间 #990000 暗红  #DC1623
#col1=colorRampPalette(colors =c('RdBu', 'white'),space="Lab")
COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)
corrplot(res2$r, type = "full", order = "hclust", tl.col = "black", tl.srt = 45)
setwd('./data/PHA')
pdf("Fig4A-plot3.pdf", width=7, height=6)
corrplot(res2$r, type = "upper", order = "hclust", p.mat = res2$P, sig.level = 0.01, tl.col = "black", tl.srt = 45, col = col1(200))  #col1(13)   COL2('RdBu', 10)
dev.off()

correlation <- cor(final_data$fg550, final_data$PHA)
model <- lm(final_data$PHA ~ final_data$fg550, data = final_data)
final_data$predicted <- predict(model)
cor_matrix <- matrix(correlation, ncol = 1)
pdf("Fig4B-plot.pdf", width=7, height=6)
plot(final_data$fg550, final_data$PHA, 
     main = "fg550", 
     xlab = "fg550",
     ylab = "PHA",
     col.lab = "black",
     pch = 19,
     lwd = 2,cex.lab = 1.5,cex = 1.5, cex.axis = 1.5)
lines(final_data$fg550, final_data$predicted, col = "blue", lwd = 2)
legend("topleft", 
       legend = c(paste("Correlation:", round(correlation, 4)),
                  paste("Regression Equation: y =", round(coef(model)[2], 2), "x +", round(coef(model)[1], 2)),
                  paste("R^2 =", round(summary(model)$r.squared, 2))),
       #col = "blue",
       #lty = 1,
       bty = "n",cex = 1.5)

dev.off()
###3HB----------------------------------------------------------------------------------------------
setwd('./data/PHA')
final_data <- read.csv(file = 'ABD-3HB最终建模2.csv')

library(Hmisc) 
res2 <- rcorr(as.matrix(final_data))
res2 
library(corrplot)
#col1=colorRampPalette(colors =c('#104E8B', 'white', '#990000'),space="Lab") # space参数选择使用RGB或者CIE Lab颜色空间 #990000 暗红  #DC1623
col1=colorRampPalette(colors =c('#1874CD', 'white', '#DC1623'),space="Lab") 
corrplot(res2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
setwd('./data/PHA')
pdf("Fig5A-plot2.pdf", width=7, height=6)
corrplot(res2$r, type = "upper", order = "hclust", p.mat = res2$P, sig.level = 0.01, tl.col = "black", tl.srt = 45, col = col1(200))
dev.off()


correlation <- cor(final_data$fg236, final_data$HB3)
model <- lm(final_data$HB3 ~ final_data$fg236, data = final_data)
final_data$predicted <- predict(model)
cor_matrix <- matrix(correlation, ncol = 1)
setwd('./data/PHA')
pdf("Fig5B-plot.pdf", width=7, height=6)
plot(final_data$fg236, final_data$HB3, 
     main = "fg236", 
     xlab = "fg236",
     ylab = "3HB",
     col.lab = "black",
     pch = 19,
     lwd = 2,cex.lab = 1.5,cex = 1.5, cex.axis = 1.5)
lines(final_data$fg236, final_data$predicted, col = "blue", lwd = 2)
legend("topleft", 
       legend = c(paste("Correlation:", round(correlation, 4)),
                  paste("Regression Equation: y =", round(coef(model)[2], 2), "x +", round(coef(model)[1], 2)),
                  paste("R^2 =", round(summary(model)$r.squared, 2))),
       #col = "blue",
       #lty = 1,
       bty = "n",cex = 1.5)

dev.off()
#####-------------------------------------------------------1129-----------------------------------


#####---------t-test----------------------------------------------1129-----------------------------------
setwd('./data/PHA')
data1 <- read.table('M-PHA.txt',header = T)
 
colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799", "#87CEFA", "#a3cb38", "#f79f1f", "#FFAEB9") #  #87CEFA  #C1FFC1  #00CD66
#boxplot(Classification_Accuracy ~ Spectral_number, data = data1, ylim = c(0.5, 1.02), col = colors, main = "Boxplot of Five Categories")
boxplot(PHA ~ class, data = data1, ylim = c(0.5, 1.02), col = colors,cex.axis = 1.5,cex.lab = 1.5)
compare_list <- list(
  c("Actual","Prediction")
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = PHA)) +
  geom_boxplot(aes(fill = class),alpha = 0.7) +
  # geom_jitter(aes(color = class))+
  scale_fill_manual(values = c("#8A919799", "#87CEFA", "#a3cb38", "#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799", "#87CEFA", "#a3cb38", "#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    label = "p.format", p.digits = 3)+  #  p.format : p   "p.signif" :（*）表示显著性水平（例如 * 代表 p < 0.05，** 代表 p < 0.01 等）
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15), 
        axis.title.y=element_text(size=15), 
        # legend.key.size = unit(40, "pt")
  )+
  scale_y_continuous(limits = c(5, 90), breaks = seq(5, 90, 25))
p
ggsave("./data/PHA/t-test-PHA-M-pvalue.pdf", p, width=12, height=8, units = "cm")
ggsave(p,filename = "./data/PHA/t-test-PHA-M-pvalue.png",width=5,height =3,dpi=600)

###3HB-------------------------------------
data1 <- read.table('M-3HB.txt',header = T)
#colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799", "#a3cb38", "#f79f1f", "#FFAEB9")#  #87CEFA  #C1FFC1  #00CD66

boxplot(M3HB ~ class, data = data1, ylim = c(0.5, 1.02), col = colors,cex.axis = 1.5,cex.lab = 1.5)

data1$class <- factor(data1$class)
compare_list <- list(
  c("Actual","Prediction")
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = M3HB)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  # geom_jitter(aes(color = class))+
  scale_fill_manual(values = c("#8A919799",  "#a3cb38", "#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799", "#a3cb38", "#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    label = "p.format", p.digits = 3)+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15), # x轴title字体大小
        axis.title.y=element_text(size=15), # y轴title字体大小
        #  legend.key.size = unit(30, "pt")
  )+
  scale_y_continuous(limits = c(15, 76), breaks = seq(15, 76, 25))
p
ggsave("./data/PHA/t-test-3HB-M-pvalue.pdf", p, width=12, height=8, units = "cm")
ggsave(p,filename = "./data/PHA/t-test-3HB-M-pvalue.png",width=5,height =3,dpi=600)

###---E----PHA
data1 <- read.table('E-PHA.txt',header = T)
#colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799", "#87CEFA","#a3cb38", "#f79f1f", "#FFAEB9")#  #87CEFA  #C1FFC1  #00CD66
boxplot(PHA ~ class, data = data1, ylim = c(0.5, 1.02), col = colors,cex.axis = 1.5,cex.lab = 1.5)
data1$class <- factor(data1$class)
compare_list <- list(
  c("Actual","Prediction")
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = PHA)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  labs(title = "PHA Pre 4th", p.digits = 3) +
  scale_fill_manual(values = c("#8A919799", "#87CEFA", "#a3cb38", "#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799","#87CEFA", "#a3cb38", "#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    label = "p.format")+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15), 
        #legend.key.size = unit(30, "pt")
  )+
  scale_y_continuous(limits = c(3, 76), breaks = seq(3, 76, 20))
p
ggsave("./data/PHA/t-test-PHA-E-pvalue.pdf", p, width=12, height=8, units = "cm")
ggsave(p,filename = "./data/PHA/t-test-PHA-E-pvalue.png",width=5,height =3,dpi=600)

###---E--3HB--4HB-----------------------------------
data1 <- read.table('E-3HB4HB.txt',header = T)
#colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799", "#a3cb38", "#f79f1f", "#FFAEB9")#  #87CEFA  #C1FFC1  #00CD66
boxplot(E3HB ~ class, data = data1, ylim = c(0.5, 1.02), col = colors,cex.axis = 1.5,cex.lab = 1.5)
data1$class <- factor(data1$class)
compare_list <- list(
  c("Actual","Prediction")
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = E3HB)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  labs(title = "3HB Pre 4th", p.digits = 3) +
  scale_fill_manual(values = c("#8A919799",  "#a3cb38", "#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799", "#a3cb38", "#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    label = "p.format")+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15), # x轴title字体大小
        axis.title.y=element_text(size=15), # y轴title字体大小
        #     legend.key.size = unit(30, "pt")
  )+
  scale_y_continuous(limits = c(18, 62), breaks = seq(18, 62, 20))
p
ggsave("./data/PHA/t-test-3HB-E-pvalue.pdf", p, width=12, height=8, units = "cm")
ggsave(p,filename = "./data/PHA/t-test-3HB-E-pvalue.png",width=5,height =3,dpi=600)




#-----E----------4HB------------
setwd('./data/PHA/')
data1 <- read.table('E-3HB4HB.txt',header = T)
#colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799",  "#f79f1f", "#FFAEB9")#  #87CEFA  #C1FFC1  #00CD66
boxplot(E4HB ~ class, data = data1, ylim = c(0.5, 1.02), col = colors,cex.axis = 1.5,cex.lab = 1.5)

data1$class <- factor(data1$class)
compare_list <- list(
  c("Actual","Prediction")
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = E4HB)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  labs(title = "4HB Pre 4th", p.digits = 3) +
  scale_fill_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    label = "p.format")+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15), 
        #    legend.key.size = unit(30, "pt")
  )+
  scale_y_continuous(limits = c(0, 32), breaks = seq(0, 32, 10))
p
ggsave("./data/PHA/t-test-4HB-E-pvalue.pdf", p, width=12, height=8, units = "cm")
ggsave(p,filename = "./data/PHA/t-test-4HB-E-pvalue.png",width=5,height =3,dpi=600)
###---DF----PHA
###---DF----PHA
data1 <- read.table('DF-PHA.txt',header = T)
#colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799", "#87CEFA","#a3cb38", "#f79f1f", "#FFAEB9")#  #87CEFA  #C1FFC1  #00CD66
boxplot(PHA ~ class, data = data1, ylim = c(0.5, 1.02), col = colors,cex.axis = 1.5,cex.lab = 1.5)
data1$class <- factor(data1$class)
compare_list <- list(
  c("Actual","Prediction") 
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = PHA)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  labs(title = "PHA Pre 5th", p.digits = 3) +
  scale_fill_manual(values = c("#8A919799", "#87CEFA", "#a3cb38", "#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799","#87CEFA", "#a3cb38", "#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    label = "p.format")+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=10, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15), 
        #  legend.key.size = unit(30, "pt")
  )+
  scale_y_continuous(limits = c(3, 70), breaks = seq(3, 70, 20))
p
ggsave("./data/PHA/t-test-PHA-DF-pvalue.pdf", p, width=12, height=8, units = "cm")
ggsave(p,filename = "./data/PHA/t-test-PHA-DF-pvalue.png",width=5,height =3,dpi=600)
###---DF--3HB--4HB-----------------------------------
data1 <- read.table('DF-3HB4HB.txt',header = T)
#colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799", "#a3cb38", "#f79f1f", "#FFAEB9")#  #87CEFA  #C1FFC1  #00CD66
boxplot(DF3HB ~ class, data = data1, ylim = c(0.5, 1.02), col = colors,cex.axis = 1.5,cex.lab = 1.5)

data1$class <- factor(data1$class)
compare_list <- list(
  c("Actual","Prediction")
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = DF3HB)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  #  geom_jitter(aes(color = class))+
  labs(title = "3HB Pre 5th") +
  scale_fill_manual(values = c("#8A919799",  "#a3cb38", "#f79f1f", "#FFAEB9"))+  ##   "#f79f1f", "#a3cb38", "#1289a7"
  scale_color_manual(values = c("#8A919799", "#a3cb38", "#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    label = "p.format", p.digits = 3)+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15), 
        axis.title.y=element_text(size=15), 
        #     legend.key.size = unit(30, "pt")
  )+
  scale_y_continuous(limits = c(15, 60), breaks = seq(15, 60, 20))#
p
ggsave("./data/PHA/t-test-3HB-DF-pvalue.pdf", p, width=12, height=8, units = "cm")
ggsave(p,filename = "./data/PHA/t-test-3HB-DF-pvalue.png",width=5,height =3,dpi=600)
#-----DF----------4HB------------
data1 <- read.table('DF-3HB4HB.txt',header = T)
#colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799",  "#f79f1f", "#FFAEB9")#  #87CEFA  #C1FFC1  #00CD66
boxplot(DF4HB ~ class, data = data1, ylim = c(0.5, 1.02), col = colors,cex.axis = 1.5,cex.lab = 1.5)
data1$class <- factor(data1$class)
compare_list <- list(
  c("Actual","Prediction")
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = DF4HB)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  #  geom_jitter(aes(color = class))+
  labs(title = "4HB Pre 5th", p.digits = 3) +
  scale_fill_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    label = "p.format")+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15), 
        axis.title.y=element_text(size=15), 
        #     legend.key.size = unit(30, "pt")
  )+
  scale_y_continuous(limits = c(2, 24), breaks = seq(2, 24, 5))
p
ggsave("./data/PHA/t-test-4HB-DF-pvalue.pdf", p, width=12, height=8, units = "cm")
ggsave(p,filename = "./data/PHA/t-test-4HB-DF-pvalue.png",width=5,height =3,dpi=600)



##----Lipid-----2025-10-02-------------------------------------------------------------------------
##--1--Correlation heatmap - Lipid
setwd('./data/Lipid/')

data<- read.table("corbatch123new.txt",header = T)
data$Var1<-as.factor(data$Var1)
#dev.off()  
#graphics.off()  
p=ggplot(data, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value))+ 
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-1, 1)) +
  geom_text(aes(label = value), vjust = 1,size = 4.5) +
  #geom_text(aes(label = sig), size = 5) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), 
        legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black',size = 12, 
                                   angle = 30, hjust = 1, vjust = 1), 
        axis.text.y = element_text(color = 'black',size = 12), 
        axis.ticks = element_line(color = 'black')) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  # theme_prism(border=TRUE,base_rect_size=0.5)+
  # coord_cartesian(clip="off") +  
  # theme(axis.ticks=element_blank())+ 
  labs(y = '', x = '', fill = 'Correlation',size = 12) +  
  geom_hline(aes(yintercept=1.5),
             colour="black",
             linewidth=0.5) +
  geom_hline(aes(yintercept=2.5),
             colour="black",
             linewidth=0.5)
p
setwd('./data/Lipid')
ggsave(filename = "CORall-np-AA2.pdf",p, width = 10, height = 2.5)

##--2--cor-heatplot--Protein--
setwd('./data/Protein')
data<- read.table("corbatch134.txt",header = T)
data$Var1<-as.factor(data$Var1)
#dev.off()  
#graphics.off()  
p=ggplot(data, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value))+ 
  scale_fill_gradientn(colors = c('#2D6DB1', 'white', '#DC1623'), limit = c(-1, 1)) +
  geom_text(aes(label = value), vjust = 1,size = 4.5) +
  #geom_text(aes(label = sig), size = 5) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black'), 
        legend.key = element_blank(), 
        axis.text.x = element_text(color = 'black',size = 12, 
                                   angle = 30, hjust = 1, vjust = 1), 
        axis.text.y = element_text(color = 'black',size = 12), 
        axis.ticks = element_line(color = 'black')) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  # theme_prism(border=TRUE,base_rect_size=0.5)+
  # coord_cartesian(clip="off") +  
  # theme(axis.ticks=element_blank())+ 
  labs(y = '', x = '', fill = 'Correlation',size = 12) +  
  geom_hline(aes(yintercept=1.5),
             colour="black",
             linewidth=0.5) +
  geom_hline(aes(yintercept=2.5),
             colour="black",
             linewidth=0.5)
p
setwd('./data/Protein')
ggsave(filename = "CORall-np-Pro-Fig7A.pdf",p, width = 10, height = 2.5)

## plot ---------------------------------------------------------------------
# FIG 7-B---lipid---
setwd('./data/Lipid')
final_data<-read.csv(file = 'regressionAA.csv',header = T)
correlation <- cor(final_data$X2839, final_data$AA)
model <- lm(final_data$AA ~ final_data$X2839, data = final_data)
final_data$predicted <- predict(model)
cor_matrix <- matrix(correlation, ncol = 1)

pdf("Fig7F-plot-AA66.pdf", width=6, height=6)
plot(final_data$X2839, final_data$AA, 
     # main = "fg550", 
     xlab = "Raman intensity at 2839",
     ylab = "Fatty acid content (%)",
     col.lab = "black",
     pch = 19,
     lwd = 2,cex.lab = 1.5,cex = 1.5, cex.axis = 1.5)
lines(final_data$X2839, final_data$predicted, col = "blue", lwd = 2)
#lines(XDATA$true,XDATA$true, col = "red", lwd = 2)
legend("topleft", 
       legend = c(paste("Correlation:", round(correlation, 4)),
                  paste("Regression Equation: y =", round(coef(model)[2], 2), "x +", round(coef(model)[1], 2)),
                  paste("R^2 =", round(summary(model)$r.squared, 2))),
       #col = "blue",
       #lty = 1,
       bty = "n",cex = 1.5)
dev.off()


# FIG 7-D---lipid---------------------------------
data1 <- read.table('TMandRM-AA.txt',header = T)  #3HB
#colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799", "#a3cb38", "#f79f1f", "#FFAEB9")#  #87CEFA  #C1FFC1  #00CD66
boxplot(AA ~ class, data = data1, ylim = c(0.5, 1.02), col = colors,cex.axis = 1.5,cex.lab = 1.5)
data1$class <- factor(data1$class)
compare_list <- list(
  c("Actual","Prediction")
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = AA)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  #  geom_jitter(aes(color = class))+
  scale_fill_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    #label = "p.signif",
    label = "p.format",
  )+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15), 
        axis.title.y=element_text(size=15), 
        #     legend.key.size = unit(30, "pt")
  )+
  scale_y_continuous(limits = c(15, 36), breaks = seq(15, 40, 5))#
p
ggsave("t-test-AA1220.pdf", p, width=6, height=6, units = "cm")
ggsave(p,filename = "t-test-A1220.png",width=5,height =3,dpi=600)

##--P--value--
p <- ggplot(data1,aes(x = class,y = AA)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  #  geom_jitter(aes(color = class))+
  scale_fill_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    #label = "p.signif",
    label = "p.format",
    symnum.args = list(cutpoints = 0, symbols = "P = ", format.func = function(p) sprintf("%.3f", p))
  )+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15), 
        axis.title.y=element_text(size=15), 
        #     legend.key.size = unit(30, "pt")
  )+
  scale_y_continuous(limits = c(15, 36), breaks = seq(15, 40, 5))
p
ggsave("t-test-AAp-pvalue.pdf", p, width=6, height=6, units = "cm")
ggsave(p,filename = "t-test-AAp-pvalue.png",width=5,height =3,dpi=600)

# ------FIG-7-F----------------------------------
setwd('.data/Protein/') 
final_data<-read.csv(file = 'regressionPRO.csv',header = T)
correlation <- cor(final_data$X1326, final_data$SCP)
model <- lm(final_data$SCP ~ final_data$X1326, data = final_data)
final_data$predicted <- predict(model)
cor_matrix <- matrix(correlation, ncol = 1)

pdf("Fig7B-plotPRO.pdf", width=5, height=5)
plot(final_data$X1326, final_data$SCP, 
     #main = "fg550", 
     xlab = "Raman intensity at 1326",
     ylab = "SCP content (%)",
     col.lab = "black",
     pch = 19,
     lwd = 2,cex.lab = 1.5,cex = 1.5, cex.axis = 1.5)
lines(final_data$X1326, final_data$predicted, col = "blue", lwd = 2)
#lines(XDATA$true,XDATA$true, col = "red", lwd = 2)
legend("topleft", 
       legend = c(paste("Correlation:", round(correlation, 4)),
                  paste("Regression Equation: y =", round(coef(model)[2], 2), "x +", round(coef(model)[1], 2)),
                  paste("R^2 =", round(summary(model)$r.squared, 2))),
       #col = "blue",
       #lty = 1,
       bty = "n",cex = 1.5)
dev.off()

# FIG 7-G---------------------------------
setwd('.data/Protein/') 
data1 <- read.table('TMandRM-PRO.txt',header = T)
#colors <- c("#8A919799", "#87CEFA", "#43CD80", "#FED43999", "#FFAEB9") 
colors <- c("#8A919799",  "#f79f1f", "#FFAEB9") #  #87CEFA  #C1FFC1  #00CD66

boxplot(SCP ~ class, data = data1, col = colors,cex.axis = 1.5,cex.lab = 1.5) #, ylim = c(0.5, 1.02)
data1$class <- factor(data1$class)
compare_list <- list(
  c("Actual","Prediction")#,
)
library(ggpubr)
p <- ggplot(data1,aes(x = class,y = SCP)) +
  geom_boxplot(aes(fill = class),alpha = 0.7)+
  #  geom_jitter(aes(color = class))+
  scale_fill_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+  ##   "#f79f1f","#a3cb38","#1289a7"
  scale_color_manual(values = c("#8A919799","#f79f1f", "#FFAEB9"))+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid = element_blank())+
  stat_compare_means(
    comparisons = compare_list,
    method = "t.test",  # wilcox
    label = "p.format")+
  theme(axis.text.x = element_text( hjust = 1,vjust=1, size=15, face = "bold"))+  #angle = 45,  , face = "bold"
  theme(axis.text.y = element_text(vjust=1,size=15, face = "bold"),    #,face = "bold"
        axis.title.x =element_text(size=15),
        axis.title.y=element_text(size=15), 
        # legend.key.size = unit(30, "pt")
  )+scale_y_continuous(limits = c(48, 64), breaks = seq(48,64,3)) 
#
p
ggsave("t-test-PRO-pvalue.pdf", p, width=7, height=7, units = "cm")
ggsave(p,filename = "t-test-PRO-pvalue.png",width=5,height =3,dpi=600)

####-lipid--
setwd('.data/Lipid/') 
XDATA1 <- read.table(file="lipid-0905-1-1_0_0_0_cell_4334_15_52_47.txt",header=F)
XDATA1 <- t(XDATA1)
wavenumber <- as.numeric(XDATA1[1,])
pdf("lipid_raman_plot.pdf", width=4.5, height=4.5)
plot(x = wavenumber,y = XDATA1[2,],
     type = 'l', 
     xlab = 'Raman shift', 
     ylab = 'Intensity', 
     col = 'black' )
dev.off()

####----Pro-
setwd('.data/Protein/') 
XDATA1 <- read.table(file="protein-12h_70mw 0.7s___cell_697_14_53_17.txt",header=F)
XDATA1 <- t(XDATA1)
wavenumber <- as.numeric(XDATA1[1,]) 
pdf("protein_raman_plot.pdf", width=4.5, height=4.5)
plot(x = wavenumber,y = XDATA1[2,],
     type = 'l', 
     xlab = 'Raman shift', 
     ylab = 'Intensity', 
     col = 'black' )
dev.off()

##---pro
library(ggplot2)
library(ggpubr)
library(dplyr)
setwd('.data/Protein/') 
df <-read.table(file = 'protein-tmrm.txt',header = T)
p <- ggplot(df, aes(x = RM, y = TM)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +
  annotate("text", x = min(df$RM), y = max(df$TM), 
           label = paste("r =", round(cor(df$RM, df$TM), 3)),
           hjust = 0, vjust = 1, color = "darkred", size = 3) +
  labs(x = "RM", y = "TM") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5))

print(p)
ggsave("protein-tmrm-r.pdf", p, width=6.5, height=6, units = "cm")

##---lipid
library(ggplot2)
library(ggpubr)
library(dplyr)
setwd('.data/Lipid/') 
df <-read.table(file = 'lipid-tmrm.txt',header = T)
p <- ggplot(df, aes(x = RM, y = TM)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "blue") +
  annotate("text", x = min(df$RM), y = max(df$TM), 
           label = paste("r =", round(cor(df$RM, df$TM), 3)),
           hjust = 0, vjust = 1, color = "darkred", size = 3) +
  labs(x = "RM", y = "TM") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) 

print(p)
ggsave("lipid-tmrm-r.pdf", p, width=6.5, height=6, units = "cm")

##---------2026.01.22----------------
############################################################
# SCI-style Raman spectrum plot
# Author: zx
# Date  : 2026-01-22
############################################################
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)

setwd('.data/PHA/') 
spectrum_data <- read.table("P34HB_Cell_0_2s_cell_29_14_54_30-base-smooth-normal.txt", header = FALSE, col.names = c("Wavenumber", "Intensity"))
cat("Wavenumber:", range(spectrum_data$Wavenumber), "\n")
cat("Intensity:", range(spectrum_data$Intensity), "\n")

peak_categories <- list(
  "Protein" = c(602, 745, 998, 1227, 1326, 1341, 1573, 2923),
  "Nucleic Acid" = c(1382),
  "Lipid" = c(1289, 1420, 1443, 2839, 2866, 2923),
  "Carbohydrate" = c(827, 855, 898, 1051, 1120),
  "PHA" = c(1722),
  "3HB" = c(1096),
  "Other" = c(2731)
)

category_labels <- c(
  "Protein" = "Protein",
  "Nucleic Acid" = "Nucleic\nAcid",
  "Lipid" = "Lipid",
  "Carbohydrate" = "Carbohydrate",
  "PHA" = "PHA",
  "3HB" = "3HB",
  "Other" = "Other"
)

peak_annotation <- data.frame()
for (category in names(peak_categories)) {
  peaks <- peak_categories[[category]]
  if (length(peaks) > 0) {
    intensities <- sapply(peaks, function(p) {
      idx <- which.min(abs(spectrum_data$Wavenumber - p))
      spectrum_data$Intensity[idx]
    })
    
    temp_df <- data.frame(
      Wavenumber = peaks,
      Intensity = intensities,
      Category = factor(category, levels = names(peak_categories)),
      Label_wavenumber = as.character(peaks),  
      Label_category = category_labels[category]  
    )
    peak_annotation <- rbind(peak_annotation, temp_df)
  }
}
max_intensity <- max(spectrum_data$Intensity)
min_intensity <- min(spectrum_data$Intensity)
intensity_range <- max_intensity - min_intensity
peak_annotation <- peak_annotation %>%
  arrange(Wavenumber) %>%
  mutate(
    norm_intensity = (Intensity - min_intensity) / intensity_range,
    prev_wavenumber = lag(Wavenumber, default = first(Wavenumber) - 100),
    wavenumber_gap = Wavenumber - prev_wavenumber
  ) %>%
  group_by(Category) %>%
  mutate(
    base_height = case_when(
      Category == "Protein" ~ 0.15,
      Category == "Lipid" ~ 0.20,
      Category == "Carbohydrate" ~ 0.25,
      Category == "Nucleic Acid" ~ 0.30,
      Category == "PHA" ~ 0.35,
      Category == "3HB" ~ 0.40,
      Category == "Other" ~ 0.45,
      TRUE ~ 0.15
    ),
    height_factor = 0.3 + 0.4 * norm_intensity,
    adjusted_factor = ifelse(wavenumber_gap < 30, 
                             height_factor * 1.2,  
                             height_factor),
    marker_height = Intensity + base_height * adjusted_factor,
    
    wavenumber_height = marker_height + 0.02 + 0.03 * norm_intensity,
    
    category_height = wavenumber_height + 0.08 + 0.05 * norm_intensity
  ) %>%
  ungroup()
close_peaks <- peak_annotation %>%
  filter(wavenumber_gap < 20) %>%
  select(Wavenumber, Intensity, Category, wavenumber_gap)
category_colors <- c(
  "Protein" = "#E41A1C",        # 红色
  "Nucleic Acid" = "#377EB8",   # 蓝色
  "Lipid" = "#4DAF4A",         # 绿色
  "Carbohydrate" = "#984EA3",  # 紫色
  "PHA" = "#FF7F00",           # 橙色
  "3HB" = "#FF7F00",           # 黄色
  "Other" = "#A65628"          # 棕色
)
x_min <- min(spectrum_data$Wavenumber, na.rm = TRUE)
x_max <- max(spectrum_data$Wavenumber, na.rm = TRUE)
p <- ggplot() +
  geom_line(data = spectrum_data, 
            aes(x = Wavenumber, y = Intensity), 
            color = "black", linewidth = 1.0) +
  geom_segment(data = peak_annotation,
               aes(x = Wavenumber, xend = Wavenumber,
                   y = min_intensity - 0.02 * intensity_range, 
                   yend = Intensity,  
                   color = Category),
               linewidth = 0.5, linetype = "solid", alpha = 0.6) +
  geom_segment(data = peak_annotation,
               aes(x = Wavenumber, xend = Wavenumber,
                   y = Intensity + 0.01,  
                   yend = marker_height - 0.02,  
                   color = Category),
               linewidth = 0.3, linetype = "dashed", alpha = 0.7) +
  geom_point(data = peak_annotation, 
             aes(x = Wavenumber, y = marker_height, color = Category), 
             size = 4.0, shape = 17, fill = "white", stroke = 1.0) +
  geom_text(data = peak_annotation,
            aes(x = Wavenumber, y = wavenumber_height,
                label = Label_wavenumber, color = Category),
            size = 6.0, angle = 90, hjust = 0, vjust = 0.5,
            family = "Arial", fontface = "bold") +
  geom_text(data = peak_annotation,
            aes(x = Wavenumber, y = category_height,
                label = Label_category, color = Category),
            size = 5.5, angle = 90, hjust = 0, vjust = 0.5,
            family = "Arial", fontface = "bold") +
  scale_color_manual(values = category_colors, 
                     breaks = names(peak_categories),
                     labels = names(peak_categories)) +
  scale_x_continuous(
    breaks = seq(round(x_min/100)*100, round(x_max/100)*100, 200),
    limits = c(x_min - 50, x_max + 50),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.1, 0.15))  
  ) +
  labs(
    x = expression(paste("Raman shift (cm"^{-1}, ")")),
    y = "Intensity (a.u.)",
    color = "Biomarker Category"
  ) +
  theme_classic(base_size = 30) +
  theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 1.0),
    panel.grid = element_blank(),
    axis.line = element_line(linewidth = 1.0),
    axis.ticks = element_line(linewidth = 1.0),
    axis.text = element_text(color = "black", size = 27),
    axis.title = element_text(color = "black", size = 30),
    legend.position = c(0.65, 0.35), 
    legend.justification = c(0.65, 0.35),  
    legend.background = element_rect(fill = "white", 
                                     colour = "black", 
                                     linewidth = 0.8),
    legend.key.size = unit(1.2, "cm"),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 27, face = "bold"),
    legend.margin = margin(15, 15, 15, 15),
    legend.box.margin = margin(15, 15, 15, 15),
    plot.margin = unit(c(1.5, 3.0, 1.5, 1.5), "cm"),  
    text = element_text(family = "Arial")
  )

print(p)

ggsave(
  filename = "Raman_Spectrum_VerticalLabels0122-1.pdf",
  plot = p,
  width = 24, 
  height = 9,
  device = "pdf"
)