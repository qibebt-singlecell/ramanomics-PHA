# by zx @ qibebt.ac.cn-----------
#-update-2024-09-06----------------------------------
#-------------------------------------------------------------------------------------------
library(sos)
library(dplyr)
library(baseline)
library(hyperSpec)
library(prospectr)

folder_path <- ".data/"
setwd(folder_path)
file_paths <- list.files(pattern ="^.*(cell|WT).*.txt$")
dir.create(file.path(folder_path, "select"), showWarnings = FALSE)
for (file_path in file_paths) {
 
  spectrum_data <- read.table(file_path, header = F)
  colnames(spectrum_data) <-c('wavelength','peak_intensity')
 
  peak_intensity <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 2800 & spectrum_data$wavelength <= 3100]
  peak_intensity2 <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 1900 & spectrum_data$wavelength <= 2100] 
  hight <-max(peak_intensity)-mean(peak_intensity2)
  if (any(hight > 1000 & peak_intensity2 < 8000)) {#& peak_intensity < 50000
    
    write.table(spectrum_data, file.path(folder_path, "select", basename(file_path)), sep="\t", row.names = FALSE)
  }
}
#--2--
spath <- file.path(folder_path,"select")
setwd(spath)
# files <-list.files(pattern ="^.*(cell|WT).*.txt$")
files <-list.files()
final_data <- read.table(files[1], header=T, sep="\t")[1]
colnames(final_data) <- 'Sample_ID'
for (filename in files)
{
  data <- read.table(filename, header=T, sep="\t")[2]
  colnames(data) <- sub(".txt","",filename)
  final_data <- cbind(final_data, data)
}
final_data <- t(final_data)
setwd('.data\\PHA\\B1data')
write.table(final_data,file="A14_YS.txt",sep="\t", quote=F, row.names=F,col.names = F)

###-1--------------
XDATA <- read.table(file="A14_YS.txt",header=T)
#XDATA <- XDATA[,-1]
colnames(XDATA) <- as.numeric(gsub("X", "", colnames(XDATA)))
z <-as.numeric(colnames(XDATA))
z <- round(z)
colnames(XDATA) <- z
XDATA_s <- XDATA
wavenumber <- as.numeric(colnames(XDATA_s))
spc2hs <- new("hyperSpec",spc = XDATA_s,wavelength = wavenumber)
plot(spc2hs)

###-2--------------
datax <- XDATA_s
#datax <- XDATA1#XDATA_s[,-1]
sg_data_ref <- savitzkyGolay(datax, p = 3, w = 11, m = 0)
Xdata <-as.data.frame(sg_data_ref)
#
wavenumber <- as.numeric(colnames(Xdata))
#wavenumber <- as.numeric(gsub("X", "", colnames(Xdata)))
spc2hs <- new("hyperSpec",spc = Xdata,wavelength = wavenumber)
plot(spc2hs)

###-3-------550-2000-------
XDATAZWQ <- Xdata %>% select(50:724) 

###-4---------------------------
wavenumber <- as.numeric(colnames(XDATAZWQ))
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
write.table (spc_baseline, file ="A14_ZWQ_base.txt", row.names =F, col.names =TRUE)

plot(hyperspec_baseline)    

###-5---------------------------
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
write.table(dfgyh, file ="A14_ZWQ_base_gyh.txt", row.names =F, col.names =TRUE)
a <-colMeans(dfgyh)
as.data.frame(a)
write.table(a,file="A14_ZWQ_base_gyh-mean.txt")
###---------------------------
