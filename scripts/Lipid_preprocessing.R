## by zhouxuan @qibebt.ac.cn
## update 2026.03.12
## for Lipid SCRS preprocessing and analysis

###----2025-09-29--------------------------------------------
#####----Lipid---------------------0915----------------2025-09-29-----------------------------------------------------

##-------------
library(sos)
library(dplyr)
library(baseline)
library(hyperSpec)
library(prospectr)

#--1---Select------
folder_path <- ".data/xx" # your folder
setwd(folder_path)
file_paths <- list.files(pattern ="^.*(cell|WT).*.txt$")
dir.create(file.path(folder_path, "selectednp"), showWarnings = FALSE)

for (file_path in file_paths) {
  spectrum_data <- read.table(file_path, header = F)
  colnames(spectrum_data) <- c('wavelength','peak_intensity')
  
  peak_intensity <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 2820 & spectrum_data$wavelength <= 3057]
  peak_intensity2 <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 2500 & spectrum_data$wavelength <= 2600] 
  hight <-max(peak_intensity)-mean(peak_intensity2)
  if (any(hight > 1000 & peak_intensity2 < 15000)) { 
    write.table(spectrum_data, file.path(folder_path, "selectednp", basename(file_path)), sep="\t", row.names = FALSE, col.names = FALSE)
  }
}

#--2--FDCH---
spath <- file.path(folder_path,"selectednp",'5-1')
setwd(spath)
#20250709
file_paths <- list.files(pattern ="^.*(cell|bg).*.txt$")
dir.create(file.path(spath, "newpara"), showWarnings = FALSE)

for (file_path in file_paths) {
  spectrum_data <- read.table(file_path, header = F)
  colnames(spectrum_data) <-c('wavelength','peak_intensity')
  
  peak_intensity <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 2856 & spectrum_data$wavelength <= 3020] 
  peak_intensity2 <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 480 & spectrum_data$wavelength <= 3020]  
  hight <- (peak_intensity2/max(peak_intensity))*100
  write.table(hight, file.path(spath, "newpara", basename(file_path)), sep="\t", row.names = FALSE)
}


#--2---------
spath1 <- file.path(spath, "newpara")
setwd(spath1)
getwd()
files <-list.files(pattern ="^.*(cell|bg).*.txt$")
#files <-list.files()
final_data <- read.table(files[1], header= T, sep="\t")[1]
for (filename in files)
{
  data <- read.table(filename, header= T, sep="\t")[1]
  colnames(data) <- sub(".txt","",filename)
  final_data <- cbind(final_data, data)
}
final_data <- final_data[,-1]
final_data <- t(final_data)
setwd('.data\\selectednp\\np')        
write.table(final_data,file="5-1-np.txt",sep="\t", quote=F, row.names=F,col.names = T)

##-----------------------------------------------------------------------------------
XDATAZWQ <- read.table('5-1-np.txt',header = T)

###-4-baseline-------------------------
#wavenumber <- as.numeric(colnames(XDATAZWQ))
wavenumber <- as.numeric(gsub("V", "", colnames(XDATAZWQ)))
spc2hs <- new("hyperSpec",spc = XDATAZWQ,wavelength = wavenumber)
data_hyperSpec <- spc2hs
wave_max <- max(data_hyperSpec@wavelength)
wave_min <- min(data_hyperSpec@wavelength)
hyperspec <- data_hyperSpec
hyperspec_1 <- hyperspec - spc.fit.poly.below(hyperspec, hyperspec, poly.order = 1)
hyperspec_baseline <- hyperspec_1
data_hyperSpec_baseline <- hyperspec_baseline
spc_baseline <- as.data.frame(data_hyperSpec_baseline$spc)
spc_baseline <- spc_baseline[,!duplicated(colnames(spc_baseline))]
write.table (spc_baseline, file ="5-1-np_base.txt", row.names =F, col.names =TRUE)
plot(hyperspec_baseline)    

###-5-normalized-------------------------
normalized_df <- function(row) {  
  row_min <- min(row)  
  row_max <- max(row)  
  return((row - row_min) / (row_max - row_min))  
}
spc_baseline1 <-as.matrix(spc_baseline) 
df1<-apply(spc_baseline1,1,normalized_df)
dfgyh<-t(df1)
dfgyh<-as.data.frame(dfgyh)
#wavenumber <- as.numeric(gsub("X", "", colnames(dfgyh)))
wavenumber <- as.numeric(colnames(dfgyh))
spc2hs <- new("hyperSpec",spc = dfgyh,wavelength = wavenumber)
plot(spc2hs)
write.table(dfgyh, file ="5-1-np_base_gyh.txt", row.names =F, col.names =TRUE)
#XDATA <- read.table(file="12h-np_base_gyh.txt",header=T)

a <-colMeans(dfgyh)
as.data.frame(a)
write.table(a,file="5-1-np_base_gyh-mean.txt")

## The above code duplicates the data at each time point. By modifying the time point, you can achieve the desired result.
#  Non-code operation: Create a new folder to store the mean data

##  Merge the average spectra
setwd(".data\\selectednp\\np\\np-base-gyh-mean")
file_list <- list.files(pattern = "\\.txt$")
data_list <- list()
for (i in seq_along(file_list)) {
  current_data <- read.table(file_list[i], header = T, sep = "")
  col_name <- tools::file_path_sans_ext(file_list[i])
  data_list[[col_name]] <- current_data[, 1]
}
combined_data <- as.data.frame(data_list)
write.csv(combined_data, "combined_columns.csv", row.names = FALSE)
##No-code operation: Merge the data and integrate it into B2np_base_gyh-mean-hb.csv. Keep the wavenumber with two decimal places.

#####----2025---------Calculate the relative peak height-----------------2025--------------------------------------
library(dplyr)
XDATA <- read.csv("np-b3峰高.csv",header = TRUE)
a1 <- XDATA %>% select(X736:X764)
max_values <- apply(a1, 1, max)
fg746 <-max_values- XDATA %>% select(X764)

a1 <- XDATA %>% select(X794:X880)
max_values <- apply(a1, 1, max)
fg848 <-max_values- XDATA %>% select(X880)

a1 <- XDATA %>% select(X978:X1016)
max_values <- apply(a1, 1, max)
fg1001 <-max_values- XDATA %>% select(X978)

a1 <- XDATA %>% select(X1038:X1096)
max_values <- apply(a1, 1, max)
fg1076 <-max_values- XDATA %>% select(X1038)

a908 <- XDATA %>% select(X1103:X1133)
max_values <- apply(a908, 1, max)
fg1122 <-max_values- XDATA %>% select(X1133)

a993 <- XDATA %>% select(X1133:X1179)
max_values <- apply(a993, 1, max)
fg1149 <-max_values- XDATA %>% select(X1133)

a1116 <- XDATA %>% select(X1214:X1270)
max_values <- apply(a1116, 1, max)
fg1257 <-max_values- XDATA %>% select(X1270)

a1233 <- XDATA %>% select(X1270:X1318)
max_values <- apply(a1233, 1, max)
fg1292 <-max_values- XDATA %>% select(X1318)

a1298 <- XDATA %>% select(X1401:X1482)
max_values <- apply(a1298, 1, max)
fg1433 <-max_values- XDATA %>% select(X1482)

a1326 <- XDATA %>% select(X1482:X1538)
max_values <- apply(a1326, 1, max)
fg1505 <- max_values- XDATA %>% select(X1538)

a1436 <- XDATA %>% select(X1609:X1702)
max_values <- apply(a1436, 1, max)
fg1644 <-max_values- XDATA %>% select(X1702)

a1569 <- XDATA %>% select(X1702:X1757)
max_values <- apply(a1569, 1, max)
fg1733 <-max_values- XDATA %>% select(X1757)

a1644 <- XDATA %>% select(X2678:X2767)
max_values <- apply(a1644, 1, max)
fg2711 <-max_values- XDATA %>% select(X2767)

a1 <- XDATA %>% select(X2774:X2853)
max_values <- apply(a1, 1, max)
fg2839 <-max_values- XDATA %>% select(X2853)

# a1 <- XDATA %>% select(X2853:X2906)
# max_values <- apply(a1, 1, max)
# fg2887 <-max_values- XDATA %>% select(X2853)

# a1 <- XDATA %>% select(X2906:X3019)
# max_values <- apply(a1, 1, max)
# fg2919 <-max_values- XDATA %>% select(X2906)

aal <- cbind(fg746,fg848,fg1001,fg1076,fg1122,fg1149,fg1257,fg1292,fg1433,fg1505,fg1644,fg1733,fg2711,fg2839) # ,fg2887,fg2919
aal <- as.data.frame(aal)
write.csv(aal,file = 'np-base-gyh-mean-fgb3.csv')

####---Calculate the correlation coefficient and draw the correlation graph

plot_correlations <- function(data) {
  start_col <- 2
  end_col <- ncol(data) - 1
  last_col <- ncol(data)
  cols_to_plot <- start_col:end_col
  target_col <- last_col
  cor_results <- sapply(cols_to_plot, function(i) {
    cor(data[, i], data[, target_col], use = "complete.obs")
  })
  cor_df <- data.frame(
    Variable = names(data)[cols_to_plot],
    Correlation = cor_results,
    stringsAsFactors = FALSE
  )
  print(cor_df)
  n_plots <- length(cols_to_plot)
  n_cols <- min(4, n_plots)
  n_rows <- ceiling(n_plots / n_cols)
  par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))
  for (i in seq_along(cols_to_plot)) {
    col_idx <- cols_to_plot[i]
    plot(data[, col_idx], data[, target_col],
         xlab = names(data)[col_idx],
         ylab = names(data)[target_col],
         main = paste("Cor =", round(cor_results[i], 3)),
         pch = 19, col = rgb(0.2, 0.4, 0.6, 0.5))
    abline(lm(data[, target_col] ~ data[, col_idx]), col = "red")
  }
  par(mfrow = c(1, 1))
}

setwd('.data\\Lipid\\batch123all\\np-base-gyh-mean')
XDATA1 <- read.csv("np-base-gyh-mean-fg.csv",header = TRUE)
plot_correlations(XDATA1)
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
ggsave(filename = "CORall-base-gyh-np-yh-XDFG.pdf",p, width = 15, height = 3.5)





