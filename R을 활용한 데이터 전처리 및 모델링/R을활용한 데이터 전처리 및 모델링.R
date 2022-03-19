## 2주차 패키지

############## CH1 모델링을 위한 데이터 전처리 ################

##문제0번
setwd("C:/Users/User/Desktop/2주차 패키지") 
getwd()

need_packages <- c("tidyverse", "ggplot2", "data.table", 'caret','randomForest', "progress", "xgboost", 'dummies') #사용할 라이브러리를 불러옵니다
options(warn = -1)
for(i in 1:length(need_packages)){
  if(require(need_packages[i], character.only = T) == 0){
    install.packages(need_packages[i])
    require(need_packages[i], character.only = T)
  }
  else{require(need_packages[i], character.only = T)}
}
rm(list = ls())

##문제1번
data <- fread('data/train.csv', data.table = F)

data %>% head()
data %>% str() ## X: 14개, Y : 1개
data %>% summary()


##문제2번
colnames(data)
colnames(data)[c(12, 13)] <- c("지하철개수", "버스개수")

##문제3번
## : data 임대료, 임대보증금 중 '-' 를 'NA'로 바꾸고,  numeric 형태로 바꿈.
data %>% str()
data$임대료[data$임대료 == '-'] <- -100
data$임대보증금[data$임대보증금 == '-'] <- -100

data %>% str()
data$임대료 <- as.numeric(data$임대료)
data$임대보증금 <- as.numeric(data$임대보증금)

data$임대료[data$임대료 == -100] <- NA
data$임대보증금[data$임대보증금 == -100] <- NA

## 열별 NA 객수 확인
dat_na = data %>% is.na() %>% colSums()
dat_na = data.frame("Col" = names(dat_na),
                      "NA_count" = dat_na,
                      row.names = NULL)

dat_na

##문제4번
graph_na <- ggplot(dat_na, aes(x=reorder(Col, NA_count), y=NA_count, fill = NA_count, colour = NA_count)) + geom_bar(stat="identity", alpha = 0.1) + scale_fill_gradient("NA개수", low = "#81D8D0", high = "#B43C8A") + scale_color_gradient(guide = "none", low = "#81D8D0", high = "#B43C8A")+ theme_light() +
  coord_flip() + labs(x = "컬럼명", y = "NA개수", title = "컬럼별 NA개수")+ geom_text(aes(label = NA_count, color = NA_count), position = position_stack(0.5), size = 5)+ theme(plot.title = element_text(size=20))
graph_na


##문제5번
data <- data %>% mutate_if(is.character, as.factor)
data <- data %>% mutate_if(is.integer, as.numeric)


##문제6번
## : NA값들을 의 면적당보증금, 면적당임대료 평균값으로 대체
data$임대료[is.na(data$임대료)] <- mean(data$임대료, na.rm = TRUE)
data$임대보증금[is.na(data$임대보증금)] <- mean(data$임대보증금, na.rm = TRUE)
data$지하철개수[is.na(data$지하철개수)] <- mean(data$지하철개수, na.rm = TRUE)
data$버스개수[is.na(data$버스개수)] <- mean(data$버스개수, na.rm = TRUE)


data %>% is.na() %>% colSums() %>% data.frame() ## 채워진것 확인

##문제7번
## : 공급유형이 '장기전세'의 임대료 0으로 채움
data$임대료[data$공급유형 == '장기전세'] <- 0

##문제8번
## : 면적당 임대료, 임대보증금 계산
data <- data %>% mutate(면적당임대료 = 임대료/전용면적)
data <- data %>% mutate(면적당보증금 = 임대보증금/전용면적)

##문제9번
## : 임대료, 임대보증금, 단지코드 열 제거
data <- data %>% select(-c(임대료, 임대보증금, 단지코드))

######################## CH2. RandomForest 및 교차검증 ############################

##Hold-out
##문제1번
set.seed(2728)
train_index <- createDataPartition(data$등록차량수, p=0.7) # 층화 추출로 데이터 분할
train_dat <- data[train_index$Resample1,]
val_dat <- data[-train_index$Resample1,]
nrow(train_dat)/nrow(data) #7:3으로 잘 나눠졌는지 확인!

data %>% str()

##문제2번

##문제3번
tune_rf1 <- expand.grid(mtry = 5:8, ntree = c(200, 300, 400))
tune_rf1$RMSE <- NA
tune_rf1

##문제4번
pb <- progress_bar$new(total = nrow(tune_rf1))
for(i in 1:nrow(tune_rf1)){ 
  print(paste0('mtry ', tune_rf1[i,'mtry']))
  print(paste0('ntree ', tune_rf1[i,'ntree']))
  fold_RMSE = NULL
  set.seed(2728)
  RF_model <- randomForest(등록차량수~., train_dat, mtry = tune_rf1[i,'mtry'], ntree = tune_rf1[i, 'ntree'])
  RF_pred <- predict(RF_model, newdata = select(val_dat, -등록차량수))
  RMSE <- RMSE(RF_pred, val_dat$등록차량수)

  tune_rf1[i,'RMSE'] <- RMSE
  pb$tick()
}

tune_rf1

##문제5번
graph_holdout <- ggplot(tune_rf1) +
  geom_tile(aes(mtry, ntree, fill = RMSE), color = 'white') + labs(x = "mtry", y = "ntree", title = "Hold-out Method 결과") +
  scale_fill_gradient("RMSE", low = "#d2f7f4", high = "#3CAEA3") + theme_light() +theme(plot.title = element_text(size=20)) 
graph_holdout

##문제6번
tune_rf1[which(tune_rf1$RMSE == min(tune_rf1$RMSE)), ]

## 5-fold CV
##문제7번
set.seed(2728)
cv <- createFolds(data$등록차량수, k = 5)

##문제8번
tune_rf2 <- expand.grid(mtry = 5:8, ntree = c(200, 300, 400))
tune_rf2$RMSE <- NA
tune_rf2


##문제9번
pb <- progress_bar$new(total = nrow(tune_rf2)*5) ##15(nrow(tune_rf))*5(fold)
for(i in 1:nrow(tune_rf2)){ 
  print(paste0('mtry ', tune_rf2[i,'mtry']))
  print(paste0('ntree ', tune_rf2[i,'ntree']))
  fold_RMSE = NULL
  for(k in 1:5){
    print(paste0(k, ' -FOLD'))
    index = cv[[k]]
    train_rf = data[-index, ]
    val_rf = data[index, ]
    val_y = val_rf$등록차량수
    
    set.seed(2728)
    RF_model <- randomForest(등록차량수~., train_rf, mtry = tune_rf2[i,'mtry'], ntree = tune_rf2[i, 'ntree'])
    rf_pred_1 <- predict(RF_model, newdata = select(val_rf, -등록차량수))
    RMSE <- RMSE(rf_pred_1, val_y)
    fold_RMSE = c(fold_RMSE, RMSE)
    
    pb$tick()
    Sys.sleep(0.01)
  }
  mean_RMSE = mean(fold_RMSE)
  tune_rf2[i,'RMSE'] <- mean_RMSE
}

tune_rf2

##문제10번
graph_CV <- ggplot(tune_rf2) +
  geom_tile(aes(mtry, ntree, fill = RMSE), color = 'white') + labs(x = "mtry", y = "ntree", title = "5-fold CV Method 결과") +
  scale_fill_gradient("RMSE", low = "#d2f7f4", high = "#3CAEA3") + theme_light() +theme(plot.title = element_text(size=20)) 
graph_CV

##문제11번
best_rf <- tune_rf2[which(tune_rf2$RMSE == min(tune_rf2$RMSE)), ]
best_rf

##문제12번
tune_rf1$method <- rep('Hold-out', nrow(tune_rf1))
tune_rf2$method <- rep('5-fold CV', nrow(tune_rf2))

tune_rf <- rbind(tune_rf1, tune_rf2)
tune_rf

for (i in 1:nrow(tune_rf)){
  tune_rf$param[i] <- paste0("param", i)
}

tune_rf <- tune_rf %>% select(method, param, RMSE)

mcolor = c('Hold-out'= "#6AA2CD", '5-fold CV'= "#B43C8A")
graph_compare <-  ggplot(tune_rf, aes(x = reorder(param, -RMSE), y = RMSE)) + geom_col( aes(fill = method, colour = method), alpha = 0.1) + theme_light() +
  coord_flip() + labs(x = "파라미터 조합", y = "RMSE", title = "Hold-out vs 5-fold CV 비교") + geom_text(aes(label = round(RMSE,2), colour = method), position = position_stack(0.5), size = 5)+ theme(plot.title = element_text(size=20)) + scale_fill_manual(values=mcolor)+ scale_colour_manual(values=mcolor)
graph_compare

##문제13번

##문제14번
RF_model <- randomForest(등록차량수~., data, mtry = best_rf[1,'mtry'], ntree = best_rf[1, 'ntree'], importance = TRUE)
### 중요도
rf_inp <- importance(RF_model) %>% data.frame()
rf_inp$Var <- rownames(rf_inp)
rf_inp %>% str()
### 그래프
graph_inp <- ggplot(rf_inp, aes(x=reorder(Var, IncNodePurity), y=IncNodePurity, fill = IncNodePurity, colour =IncNodePurity)) + geom_bar(stat="identity", alpha = 0.5) + scale_fill_gradient("IncNodePurity", low = "#81D8D0", high = "#B43C8A") + scale_color_gradient(guide = "none", low = "#81D8D0", high = "#B43C8A")+ theme_light() +
  coord_flip() + labs(x = "변수명", y = "중요도", title = "RandomForest Importance Plot")+ theme(plot.title = element_text(size=20))
graph_inp


########################## CH3 Xgboost ###########################


data %>% str()

dat <- data

##문제1번
data = dummy.data.frame(dat)

##문제2번

##문제3번
max_depth = NULL
min_child_weight = NULL
subsample = NULL
colsample_bytree = NULL

set.seed(2728)  
for (i in 1:12){
  max_depth = c(max_depth, sample(4:10, 1))
  min_child_weight = c(min_child_weight, sample(4:10, 1))
  subsample = c(subsample, runif(1, 0.5, 1))
  colsample_bytree = c(colsample_bytree, runif(1, 0.5, 1))
}

eta = rep(0.01, 12)
nrounds = rep(1000, 12)

tune_xgb = data.frame(max_depth = max_depth, 
                      min_child_weight = min_child_weight,
                      subsample = subsample,
                      colsample_bytree = colsample_bytree,
                      eta = eta,
                      nrounds = nrounds,
                      RMSE = NA
)
tune_xgb

##문제4번
set.seed(2728)
cv_xgb = createFolds(data$등록차량수, k = 5)

pb <- progress_bar$new(total = nrow(tune_xgb)*5)

for (i in 1:nrow(tune_xgb)){
  print(paste0('Tune ', i))
  params = list(objective = 'reg:linear', 
                max_depth = tune_xgb$max_depth[i],
                min_child_weight = tune_xgb$min_child_weight[i],
                subsample = tune_xgb$subsample[i],
                colsample_bytree = tune_xgb$colsample_bytree[i],
                eta = tune_xgb$eta[i]
  )
  fold_RMSE = NULL
  
  for (k in 1:5){
    print(paste0(k, ' -FOLD'))
    index <- cv_xgb[[k]]
    train_x<- data[-index,]
    val_x <- data[index,]
    train_xgb = xgb.DMatrix(as.matrix(train_x %>% select(-등록차량수)), label = train_x$등록차량수)
    vali_xgb = xgb.DMatrix(as.matrix(val_x %>% select(-등록차량수)), label = val_x$등록차량수)
    watchlist = list(train = train_xgb, test = vali_xgb)
    
    set.seed(2728)
    xgb_mod_1 = xgb.train(params = params, 
                          data = train_xgb, 
                          watchlist = watchlist, nrounds = tune_xgb$nrounds[i],
                          verbose = 0, 
                          early_stopping_rounds = 0.05*tune_xgb$nrounds[i]
    )
    xgb_pred_1 <- predict(xgb_mod_1, newdata = vali_xgb)
    RMSE <- RMSE(xgb_pred_1,val_x$등록차량수)
    fold_RMSE = c(fold_RMSE, RMSE)
    
    pb$tick()
    Sys.sleep(0.01)
  }
  mean_RMSE = mean(fold_RMSE)
  tune_xgb[i,'RMSE'] <- mean_RMSE
}

tune_xgb

##문제5번
for (i in 1:nrow(tune_xgb)){
  tune_xgb$param[i] <- paste0("param", i)
}

graph_xgb <- ggplot(tune_xgb, aes(x = reorder(param , -RMSE), y = RMSE)) +
  geom_col(aes(fill = RMSE, colour = RMSE), alpha = 0.5) + labs(x = "파라미터 조합", y = "RMSE", title = "XGboost 결과") +
  scale_fill_gradient("RMSE", low = "#81D8D0", high = "#B43C8A")+scale_colour_gradient("RMSE", low = "#81D8D0", high = "#B43C8A") + theme_light() + geom_text(aes(label = round(RMSE,2), color = RMSE), position = position_stack(0.5), size = 5) + theme(plot.title = element_text(size=20))  + coord_flip()
graph_xgb

###가장 좋게 나온 조합
best_xgb <- tune_xgb[which(tune_xgb$RMSE == min(tune_xgb$RMSE)), ]
best_xgb

############# CH4. 비교 ####################
##문제1번
test <- fread('data/test.csv', data.table = F)
test %>% head()
test %>% str()

### 지하철, 버스로 이름 바꾸기
colnames(test)[c(12, 13)] <- c("지하철개수", "버스개수")

### data 임대료, 임대보증금 중 '-' 를 'NA'로 바꾸고,  numeric 형태로 바꿈.
test$임대료[test$임대료 == '-'] <- -100
test$임대보증금[test$임대보증금 == '-'] <- -100

test %>% str()
test$임대료 <- as.numeric(test$임대료)
test$임대보증금 <- as.numeric(test$임대보증금)

test$임대료[test$임대료 == -100] <- NA
test$임대보증금[test$임대보증금 == -100] <- NA

### 범주형 변수들 factor 변환 & integer 변수들 numeric 변환
test <- test %>% mutate_if(is.character, as.factor)
test <- test %>% mutate_if(is.integer, as.numeric)

test$임대료[is.na(test$임대료)] <- mean(test$임대료, na.rm = TRUE)
test$임대보증금[is.na(test$임대보증금)] <- mean(test$임대보증금, na.rm = TRUE)
test$지하철개수[is.na(test$지하철개수)] <- mean(test$지하철개수, na.rm = TRUE)
test$버스개수[is.na(test$버스개수)] <- mean(test$버스개수, na.rm = TRUE)


test %>% is.na() %>% colSums() %>% data.frame() ## 채워진것 확인

### 공급유형이 '장기전세'의 임대료 0으로 채움
test$임대료[test$공급유형 == '장기전세'] <- 0

### 면적당 임대료, 임대보증금 계산
test <- test %>% mutate(면적당임대료 = 임대료/전용면적)
test <- test %>% mutate(면적당보증금 = 임대보증금/전용면적)

###임대료, 임대보증금 열 제거
test <- test %>% select(-c(임대료, 임대보증금, 단지코드))
test_x <- test %>% select(-등록차량수)
dat %>% str()
test %>% str()

##문제2번
set.seed(1234)
RF_model <- randomForest(등록차량수~. , dat, mtry = best_rf[1,'mtry'], ntree = best_rf[1, 'ntree'])
RF_pred <- predict(RF_model, newdata = select(test,-등록차량수))
RMSE(RF_pred, test$등록차량수)

##문제3번
test.d = dummy.data.frame(test)
train.d = dummy.data.frame(dat)
test.d %>% dim()
train.d %>% dim()

train_xgb = xgb.DMatrix(as.matrix(train.d %>% select(-등록차량수)), label = train.d$등록차량수)
test_xgb = xgb.DMatrix(as.matrix(test.d %>% select(-등록차량수)), label = test$등록차량수)
watchlist = list(train = train_xgb, test = test_xgb)

set.seed(1234)
params = list(objective = 'reg:linear', 
                       max_depth = best_xgb$max_depth[1],
                       min_child_weight = best_xgb$min_child_weight[1],
                       subsample = best_xgb$subsample[1],
                       colsample_bytree = best_xgb$colsample_bytree[1],
                       eta = best_xgb$eta[1]
)
best_xgb

xgb_mod_1 = xgb.train(params = params, 
                      data = train_xgb, 
                      watchlist = watchlist, nrounds = best_xgb$nrounds[1],
                      verbose = 0, 
                      early_stopping_rounds = 0.05*best_xgb$nrounds[1]
)
xgb_pred_1 <- predict(xgb_mod_1, newdata = test_xgb)
RMSE(test$등록차량수, xgb_pred_1)

##문제4번
model = c('RandomForest', 'XGBoost')
RMSE = c(RMSE(RF_pred, test$등록차량수),RMSE(test$등록차량수, xgb_pred_1) )

model_rmse <- data.frame(model, RMSE)
colnames(model_rmse) <- c('model', 'RMSE')

### 결과 시각화
mcolor = c('RandomForest'= "#6AA2CD", 'XGBoost'= "#B43C8A")
graph_model <- ggplot(model_rmse, aes(x = model, y = RMSE)) + geom_col(aes(fill = model, colour = model), alpha = 0.4) + labs(x = "모델", y = "RMSE", title = "모델 결과 비교") +
  scale_fill_manual(values = mcolor) +scale_colour_manual(values = mcolor) + theme_light() + geom_text(aes(label = round(RMSE,4), colour = model), position = position_stack(0.5), size = 5) + theme(plot.title = element_text(size=20))  + coord_flip()
graph_model

###############끝!!!!!!!!!##########################


