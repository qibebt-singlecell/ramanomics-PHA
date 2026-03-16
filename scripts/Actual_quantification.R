# update20250319---
# by  zhouxuan@qibebt.ac.cn

spath <- '.data\\PHA\\dl250319\\20selected'#file.path(folder_path,"selected")
setwd(spath)

file_paths <- list.files(pattern ="^.*(cell|bg).*.txt$")

dir.create(file.path(spath, "newpara"), showWarnings = FALSE)

for (file_path in file_paths) {
  spectrum_data <- read.table(file_path, header = T)
  colnames(spectrum_data) <-c('wavelength','peak_intensity')
  
  peak_intensity <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 2800 & spectrum_data$wavelength <= 3200] 
  peak_intensity2 <- spectrum_data$peak_intensity[spectrum_data$wavelength >= 515 & spectrum_data$wavelength <= 1815]   #   500-1800
  hight <- (peak_intensity2/max(peak_intensity))*100
  write.table(hight, file.path(spath, "newpara", basename(file_path)), sep="\t", row.names = FALSE)
}


#--2---------
spath1 <- file.path(spath, "newpara")
setwd(spath1)
#getwd()

files <-list.files(pattern ="^.*(cell|bg).*.txt$")
#files <-list.files()
final_data <- read.table(files[1], header= T, sep="\t")[1]

for (filename in files)
{
  data <- read.table(filename, header=T, sep="\t")[1]
  colnames(data) <- sub(".txt","",filename)
  final_data <- cbind(final_data, data)
}

final_data <- final_data[,-1]
final_data <- t(final_data)

setwd('.data\\PHA\\dl250319')   
write.table(final_data,file="C20-np.txt",sep="\t", quote=F, row.names=F,col.names = T)

##------------------------------------------------------------------------------------
XDATAZWQ <- read.table('C20-np.txt',header = T)
###-4---------------------------
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
write.table (spc_baseline, file ="C20-np_base.txt", row.names =F, col.names =TRUE)

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
write.table(dfgyh, file ="C20-np_base_gyh.txt", row.names =F, col.names =TRUE)

XDATA <- read.table(file="C20-np_base_gyh.txt",header=T)

a <-colMeans(XDATA)
as.data.frame(a)
write.table(a,file="C20-np_base_gyh-mean.txt")


##########-------------------------------------------------------------------------------------------------
##

XDATA <- read.table("C15-np_base_gyh-mean.txt",header = TRUE)
XDATA <- t(XDATA)
XDATA <- as.data.frame(XDATA)

a1 <- XDATA[,537:568]
max_values <- apply(a1, 1, max)
fg550 <-max_values-XDATA[,537]

a1 <- XDATA[,245:262]
max_values <- apply(a1, 1, max)
fg256 <-max_values-XDATA[,262]

aal <- cbind(fg256,fg550)
aal <- as.data.frame(aal)
aal$PHA <- 49.11


write.table(aal,file = 'C15_fgnp-base_gyh-pha.txt',col.names = T)
#final_data <- read.table('A18fg3-pha.txt', header=T)

#
setwd('.data\\PHA\\dl250319\\fg')
files <-list.files(pattern ="^.*(gyh).*.txt$")
final_data <- read.table(files[1], header=T)
for (filename in files)
{
  data <- read.table(filename, header=T)
  final_data <- rbind(final_data, data)
}
final_data1 <- as.data.frame(final_data)

final_data1 <- final_data1[-1,]

#a1 <- a1[1:22,]
#final_data1$PHA <-a1$PHA
#final_data <- final_data1
write.table(final_data1,file = 'C_gyhpredict.csv',col.names = T,row.names = F,sep = ',')

##########-------------------------------------------------------------------------------------------------
setwd('.data\\PHA\\\\dl250319')
XDATA <- read.table(file="C20-np_base_gyh.txt",header=T)

a1 <- XDATA[,537:568]
max_values <- apply(a1, 1, max)
fg550 <-max_values-XDATA[,537]

a1 <- XDATA[,245:262]
max_values <- apply(a1, 1, max)
fg256 <-max_values-XDATA[,262]

aal <- cbind(fg256,fg550)
aal <- as.data.frame(aal)
#aal$PHA <- 49.11
write.csv(aal,file = 'C20_gyhpredict.csv')

