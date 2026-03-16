#  2025-0828  Protein quantitative analysis
# update by zx
##-------------
library(sos)
library(dplyr)
library(baseline)
library(hyperSpec)
library(prospectr)

#--1---------
folder_path <- ".data\\20250806-HA24H"
setwd(folder_path)
file_paths <- list.files(pattern ="^.*(cell|WT).*.txt$")
dir.create(file.path(folder_path, "selected"), showWarnings = FALSE)
for (file_path in file_paths) {
  spectrum_data <- read.table(file_path, header = F)
  colnames(spectrum_data) <-c('wavelength','peak_intensity')
  peak_intensity <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 2820 & spectrum_data$wavelength <= 3057]
  peak_intensity2 <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 2500 & spectrum_data$wavelength <= 2600] 
  hight <-max(peak_intensity)-mean(peak_intensity2)
  if (any(hight > 1000 & peak_intensity2 < 12000)) {
    write.table(spectrum_data, file.path(folder_path, "selected", basename(file_path)), sep="\t", row.names = FALSE, col.names = FALSE)
  }
}

#--2-----
spath <- file.path(folder_path,"selected",'1')
setwd(spath)
file_paths <- list.files(pattern ="^.*(cell|bg).*.txt$")
dir.create(file.path(spath, "newpara"), showWarnings = FALSE)
for (file_path in file_paths) {
  spectrum_data <- read.table(file_path, header = F)
  colnames(spectrum_data) <-c('wavelength','peak_intensity')
  
  peak_intensity <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 2800 & spectrum_data$wavelength <= 3200] 
  peak_intensity2 <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 480 & spectrum_data$wavelength <= 1815]   
  hight <- (peak_intensity2/max(peak_intensity))*100
  write.table(hight, file.path(spath, "newpara", basename(file_path)), sep="\t", row.names = FALSE)
}


#--3-------
spath1 <- file.path(spath, "newpara")
setwd(spath1)
getwd()
files <-list.files(pattern ="^.*(cell|bg).*.txt$")
final_data <- read.table(files[1], header= T, sep="\t")[1]
for (filename in files)
{
  data <- read.table(filename, header= T, sep="\t")[1]
  colnames(data) <- sub(".txt","",filename)
  final_data <- cbind(final_data, data)
}
final_data <- final_data[,-1]
final_data <- t(final_data)

setwd('.data\\20250806-HA24H\\selected\\FXresults')        
write.table(final_data,file="B3_1-np.txt",sep="\t", quote=F, row.names=F,col.names = T)

##-----------------------------------------------------------------------------------
XDATAZWQ <- read.table('B3_1-np.txt',header = T)
###-4-baseline--------------------------
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
write.table (spc_baseline, file ="B3_1-np_base.txt", row.names =F, col.names =TRUE)
plot(hyperspec_baseline)    

###-5-normalized--------------------------
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
write.table(dfgyh, file ="B3_1-np_base_gyh.txt", row.names =F, col.names =TRUE)
#XDATA <- read.table(file="12h-np_base_gyh.txt",header=T)
#
a <-colMeans(dfgyh)
as.data.frame(a)
write.table(a,file="B3_1-np_base_gyh-mean.txt")

## The above code is repetitive. The data for each time point can be modified by changing the time point.
# Non-code operation: Create a new folder to store the mean data

##  Merge the average spectra
setwd(".data\\20250806-HA24H\\selected\\FXresults\\B3_HA24H1-np_base_gyh-mean")
file_list <- list.files(pattern = "\\.txt$")
data_list <- list()
for (i in seq_along(file_list)) {
  current_data <- read.table(file_list[i], header = T, sep = "")
  col_name <- tools::file_path_sans_ext(file_list[i])
  data_list[[col_name]] <- current_data[, 1]
}
combined_data <- as.data.frame(data_list)
write.csv(combined_data, "combined_columns.csv", row.names = FALSE)
## No-code operation: Integrate the merged data into B2np_base_gyh-mean-hb.csv and retain the wavenumber to two decimal places

#####----2025-0717---------Calculate the relative peak height-----------------2025-07-17-------------------------------------
library(dplyr)
XDATA <- read.csv("B3-HA24Hnp_base_gyh-mean-hb.csv",header = TRUE)

a1 <- XDATA %>% dplyr::select(X688.09:X729.47)
max_values <- apply(a1, 1, max)
fg720 <-max_values- XDATA %>% dplyr::select(X729.47)

a1 <- XDATA %>% dplyr::select(X729.47:X763.81)
max_values <- apply(a1, 1, max)
fg743 <-max_values- XDATA %>% dplyr::select(X763.81)

a1 <- XDATA %>% dplyr::select(X761.52:X791.18)
max_values <- apply(a1, 1, max)
fg777 <-max_values- XDATA %>% dplyr::select(X791.18)

a1 <- XDATA %>% dplyr::select(X829.82:X861.52)
max_values <- apply(a1, 1, max)
fg845 <-max_values- XDATA %>% dplyr::select(X861.52)

a908 <- XDATA %>% dplyr::select(X861.52:X920.1)
max_values <- apply(a908, 1, max)
fg908 <-max_values- XDATA %>% dplyr::select(X920.1)

a993 <- XDATA %>% dplyr::select(X982.76:X1011.7)
max_values <- apply(a993, 1, max)
fg993 <-max_values- XDATA %>% dplyr::select(X1011.7)

a1116 <- XDATA %>% dplyr::select(X1106.78:X1137.51)
max_values <- apply(a1116, 1, max)
fg1116 <-max_values- XDATA %>% dplyr::select(X1106.78)

a1233 <- XDATA %>% dplyr::select(X1181.24:X1268.04)
max_values <- apply(a1233, 1, max)
fg1233 <-max_values- XDATA %>% dplyr::select(X1268.04)

a1298 <- XDATA %>% dplyr::select(X1268.04:X1313.27)
max_values <- apply(a1298, 1, max)
fg1298 <-max_values- XDATA %>% dplyr::select(X1313.27)

a1326 <- XDATA %>% dplyr::select(X1313.27:X1345.44)
max_values <- apply(a1326, 1, max)
fg1326 <-max_values- XDATA %>% dplyr::select(X1345.44)

a1436 <- XDATA %>% dplyr::select(X1405.17:X1502.41)
max_values <- apply(a1436, 1, max)
fg1436 <-max_values- XDATA %>% dplyr::select(X1405.17)

a1569 <- XDATA %>% dplyr::select(X1506.61:X1581.92)
max_values <- apply(a1569, 1, max)

fg1569 <-max_values- XDATA %>% dplyr::select(X1506.61)  

a1644 <- XDATA %>% dplyr::select(X1615.18:X1714.19)
max_values <- apply(a1644, 1, max)
fg1644 <-max_values- XDATA %>% dplyr::select(X1714.19)   

aal <- cbind(fg720,fg743,fg777,fg845,fg908,fg993,fg1116,fg1233,fg1298,fg1326,fg1436,fg1569,fg1644)
aal <- as.data.frame(aal)
write.csv(aal,file = 'b3-1-5-np-base-gyh-mean-fg.csv')
getwd()


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

## Non-code operation: The first column sets the time, and the last column adds SCP.
setwd('.data\\分析结果')
XDATA1 <- read.csv("b3-np-base-gyh-mean-fg.csv",header = TRUE)

plot_correlations(XDATA1)




# Read the CSV file (replace with your file path)
setwd('.data\\分析结果')
data <- read.csv("b3-np-base-gyh-mean-fg-1.csv", header = TRUE, stringsAsFactors = FALSE)

start_col <- 2
end_col <- ncol(data) - 1
last_col <- ncol(data)
cor_results <- sapply(start_col:end_col, function(i) {
  cor(data[, i], data[, last_col], use = "complete.obs")
})
result_df <- data.frame(
  Variable = names(data)[start_col:end_col],
  Correlation = cor_results,
  stringsAsFactors = FALSE
)
#result_df <- result_df[order(-abs(result_df$Correlation)), ]
write.csv(result_df, "B3correlation_results.csv", row.names = FALSE)
print(summary(result_df$Correlation))





