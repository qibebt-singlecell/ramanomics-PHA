# Leave-one-out cross-validation
# by zhoxuuan @qibebt-singlecell

## 0. 
setwd('.data/PHA')
dat <- read.table('PHA交叉验证.txt', header = TRUE)
## 1. Leave-one-out cross-validation
classes <- 1:4
res <- vector("list", length(classes))
for(i in seq_along(classes)){
  te_cls <- classes[i]                    
  tr_cls <- setdiff(classes, te_cls)      
  
  ## 1.1 
  train_dat <- dat[dat$class %in% tr_cls, ]
  fit <- lm(PHA ~ F1722, data = train_dat)
  a <- coef(fit)[1]; b <- coef(fit)[2]
  formula_txt <- sprintf("PHA = %.4f + %.4f*F1722", a, b)
  
  ## 1.2 
  test_dat <- dat[dat$class == te_cls, ]
  pred <- predict(fit, newdata = test_dat)
  mad_val <- mean(abs(pred - test_dat$PHA))
  
  ## 1.3 
  res[[i]] <- data.frame(
    Round = i,
    LeftOut_Class = te_cls,
    Train_Classes = paste(sort(tr_cls), collapse = ","),
    Formula = formula_txt,
    MAD = mad_val
  )
}

## 2. 
final_table <- do.call(rbind, res)
print(final_table, row.names = FALSE)
library(writexl)
write_xlsx(final_table, "PHA-LOOCV_result.xlsx")


##-------------- 0. \------------------------------------------------------------------------------------------------
setwd('.data/PHA')
dat <- read.table('3HB交叉验证.txt', header = TRUE)
## 1. 
classes <- 1:4
res <- vector("list", length(classes))

for(i in seq_along(classes)){
  te_cls <- classes[i]                    
  tr_cls <- setdiff(classes, te_cls)      
  
  ## 1.1 
  train_dat <- dat[dat$class %in% tr_cls, ]
  fit <- lm(HB3 ~ F1096, data = train_dat)
  a <- coef(fit)[1]; b <- coef(fit)[2]
  formula_txt <- sprintf("HB3 = %.4f + %.4f*F1096", a, b)
  
  ## 1.2 
  test_dat <- dat[dat$class == te_cls, ]
  pred <- predict(fit, newdata = test_dat)
  mad_val <- mean(abs(pred - test_dat$HB3))
  
  ## 1.3 
  res[[i]] <- data.frame(
    Round = i,
    LeftOut_Class = te_cls,
    Train_Classes = paste(sort(tr_cls), collapse = ","),
    Formula = formula_txt,
    MAD = mad_val
  )
}

## 2. 
final_table <- do.call(rbind, res)
print(final_table, row.names = FALSE)
library(writexl)
write_xlsx(final_table, "3HB-LOOCV_result.xlsx")
