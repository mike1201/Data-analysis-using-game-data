# 비즈니스 활용 사례로 배우는 데이터분석 : R // 3장


setwd("C://Users//MinKyu//Desktop//Boaz//R스터디//data")

# CSV 파일을 읽어들이기
dau <- read.csv("section3-dau.csv", header = T, stringsAsFactors = F)
head(dau)
dpu <- read.csv("section3-dpu.csv", header = T, stringsAsFactors = F)
head(dpu)
install <- read.csv("section3-install.csv", header = T, stringsAsFactors= F)
head(install)

# DAU 데이터에 Install 데이터를 결합시키기
dau.install <- merge(dau, install, by = c("user_id", "app_name"))
head(dau.install)

# 위 데이터에 다시 DPU 데이터를 결합시키기
# all.x=T : NULL값 또한 데이터로 인정 all.x=F : NULL값 날라가

dau.install.payment <- merge(dau.install, dpu, by = c("log_date",
                                                      "app_name", "user_id"), all.x = T)
head(dau.install.payment)

# na제거 : na.omit == complete.cases
head(na.omit(dau.install.payment))
# dau.install.payment[complete.cases(dau.install.payment),]

# 비과금 유저의 과금액에 0을 넣기

dau.install.payment$payment[is.na(dau.install.payment$payment)] <- 0
head(dau.install.payment)

# 월 항목 추가 // dataframe에 변수 추가 // 문자 중 몇개만 뽑아서 쓰기
dau.install.payment$log_month <-substr(dau.install.payment$log_date, 1, 7)
dau.install.payment$install_month <- substr(dau.install.payment$install_date, 1, 7)
head(dau.install.payment)
library(plyr)
library(dplyr)

# chaining method와 ddply로 집계
# chaining method가 ddply보다 훨씬 빠름
#mau.payment <- ddply(dau.install.payment,
#                     .(log_month, user_id, install_month), # 그룹화
#                     summarize, # 집계 명령
#                     payment = sum(payment) # payment 합계
#)
mau.payment <-dau.install.payment %>% 
  group_by(log_month, user_id, install_month) %>%
  summarise(payment=sum(payment)
  )
# mau.payment1 == mau.paymentt1

head(mau.payment)


# 신규 유저인지 기존 유저인지 구분하는 항목을 추가

# 신규 유저와 기존 유저 식별후, 각 그룹별로 총 지출계산
mau.payment$user.type <- ifelse(mau.payment$install_month == mau.payment$log_month,
                                "install", "existing")
mau.payment.summary <- mau.payment %>% group_by(log_month, user.type) %>%
  summarise(total.payment=sum(payment))

head(mau.payment) 
head(mau.payment.summary)


# 그패프로 데이터를 시각화하기 （geom_bar()　->　geom_bar(stat="identity")로 수정 2014/08/22）
library("ggplot2")
library("scales")
ggplot(mau.payment.summary, aes(x = log_month, y = total.payment,
                                fill = user.type)) + geom_bar(stat="identity") + scale_y_continuous(label = comma)

# old_theme = theme_update(
# axis.title.x = theme_text(family="HiraKakuProN-W3"),
# axis.title.y = theme_text(family="HiraKakuProN-W3", angle=90),
# plot.title = theme_text(family="HiraKakuProN-W3", size=14.4))

ggplot(mau.payment[mau.payment$payment > 0 & mau.payment$user.type == "install", ], 
       aes(x = payment, fill = log_month)) + geom_histogram(position = "dodge", binwidth = 10000) + scale_x_continuous(label=comma)









# 비즈니스 활용 사례로 배우는 데이터분석 : R // 4장

setwd("C://Users//MinKyu//Desktop//Boaz//R스터디//data")
dau <- read.csv("section4-dau.csv", header = T, stringsAsFactors = F)
head(dau)
user.info <- read.csv("section4-user_info.csv", header = T, stringsAsFactors = F)
head(user.info)
dau.user.info <- merge(dau, user.info, by=c("user_id", "app_name"))
head(dau.user.info)


# 월 항목을 추가
# dau.user.info 데이터의 log_date열 데이터에서 첫 번째 문자부터 일곱 번째 문자까지 추출해서
# 그것을 dau.user.info 데이터의 log_month열에 넣는 것.
dau.user.info$log_month <- substr(dau.user.info$log_date, 1, 7) 

# dataframe에서 gender vs log_month table 만들기
table(dau.user.info[, c("log_month", "gender")])
# 카이제곱 검정
chisq.test(dau.user.info$log_month, dau.user.info$gender)


# 세그먼트 분석(연령대별로 집계）
table(dau.user.info[, c("log_month", "generation")])
chisq.test(dau.user.info$log_month, dau.user.info$generation)
head(dau.user.info)

# 세그먼트 분석（성별과 연령대를 조합해 집계）
# dcast보다는 acast쓰는 게 더 쉬운듯
library(reshape2)
A <- acast(dau.user.info, log_month ~ gender + generation, length)
# A <- dcast(dau.user.info, log_month ~ gender + generation, value.var = "user_id", length)
chisq.test(A)


# 세그먼트 분석（단말기별로 집계）
table(dau.user.info[,c("log_month","device_type")])
chisq.test(dau.user.info$log_month, dau.user.info$device_type)


# 세그먼트 분석 결과를 시각화하기

# 날짜 /  단말기별 유저수를 산출하기
library(plyr)
library(dplyr)
#dau.user.info.device.summary <- ddply(dau.user.info, .(log_date, device_type), summarize, dau = length(user_id))
dau.user.info.device.summary <- dau.user.info %>%
  group_by(log_date, device_type) %>%
  summarise(dau=length(user_id))
head(dau.user.info.device.summary)


# 날짜별 데이터 형식으로 변환하기 // as.Date
dau.user.info.device.summary$log_date <- as.Date(dau.user.info.device.summary$log_date)


# 시계열의 트렌드 그래프 그리기
library(ggplot2)
library(scales)
limits <- c(0, max(dau.user.info.device.summary$dau))
ggplot(dau.user.info.device.summary, aes(x=log_date, y=dau, col=device_type, lty=device_type, shape=device_type)) +
  geom_line(lwd=1) +
  geom_point(size=4) +
  scale_y_continuous(label=comma, limits=limits)

# aes: 좌표계 그리기, geom_line: 선 생성, geom_point: 점 생성, scale_y_continuous: 일정한 간격으로 y축 생성, col: 색깔 설정, lty: line









# 비즈니스 활용 사례로 배우는 데이터분석 : R // 5장

# 데이터 읽어 들이기
setwd("C://Users//MinKyu//Desktop//Boaz//R스터디//data")
ab.test.goal <- read.csv("section5-ab_test_goal.csv", header=T, stringsAsFactors=F) # 클릭
ab.test.imp <- read.csv("section5-ab_test_imp.csv", header=T, stringsAsFactors=F) # 표시
head(ab.test.goal)
head(ab.test.imp)

# merge : ab.test.imp에 ab.test.goal 결합시키기
# all.x=T : 대응되는 값이 없어도 NA로 발생시킴 / suffixes=c("",.1") --) 같은 변수가 있으면 .1로 바꿔줌
ab.test.imp <- merge(ab.test.imp, ab.test.goal, by="transaction_id", all.x=T, suffixes=c("",".g"))
head(ab.test.imp)

# 클릭 플레그 추가
ab.test.imp$is.goal <- ifelse(is.na(ab.test.imp$user_id.g),0,1)
head(ab.test.imp)

# 클릭률 집계하기
library(plyr)
library(dplyr)
ab.test.imp %>%
  group_by(test_case) %>%
  summarise(cvr=sum(is.goal)/length(user_id))
# ddply(ab.test.imp, .(test_case), summarize, cvr=sum(is.goal)/length(user_id))
ab.test.imp$log_10th <- substr(ab.test.imp$log_date,1,9)
head(ab.test.imp)
library(reshape2)
ab.test.imp.summary <- acast(ab.test.imp, test_case~log_10th, value.var="is.goal",length)
# ab.test.imp.summary <- dcast(ab.test.imp, test_case~log_10th, value.var='is.goal')
head(ab.test.imp.summary)

# 카이제곱 검정 실행하기
chisq.test(ab.test.imp$test_case, ab.test.imp$is.goal)

# 날짜 / 테스트 케이스별로 클릭률 산출하기
ab.test.imp.summary <-
  ab.test.imp %>%
  group_by(test_case,log_date) %>%
  summarise(cv=sum(is.goal), imp = length(user_id), cvr=sum(is.goal)/length(user_id))
# ab.test.imp.summary1 <- ddply(ab.test.imp, .(test_case, log_date), summarize, cv= sum(is.goal), imp=length(user_id), cvr=sum(is.goal)/length(user_id))
head(ab.test.imp.summary)
ab.test.imp.summary$log_10th <- substr(ab.test.imp.summary$log_date,1,9)

## 테스트 케이스별로 클릭률 산출하기
## ddply : summary는 지정하는 변수만 남기고, transform은 원래 데이터에 지정한 변수를 추가함
## %>%   : transform대신 mutate사용
ab.test.imp.summary <- ab.test.imp.summary %>%
  group_by(test_case) %>%
  mutate(cvr.arg = sum(imp))
#ab.test.imp.summary11 <- ddply(ab.test.imp.summary, .(test_case), transform, cvr.arg=sum(imp))
#ab.test.imp.summary1 == ab.test.imp.summary11
#head(ab.test.imp.summary)


## Date라는 객체 클래스가 있고, as.Date는 Date 클래스로 바꿔줌
library(ggplot2)
library(scales)
ab.test.imp.summary$log_date <- as.Date(ab.test.imp.summary$log_date)
limits <- c(0, max(ab.test.imp.summary$cvr))
ggplot(ab.test.imp.summary,aes(x=log_date,y=cvr, col=test_case,lty=test_case, shape=test_case)) +
  geom_line(lwd=1) +
  geom_point(size=4) +
  geom_line(aes(y=cvr.arg,col=test_case)) +
  scale_y_continuous(label=percent, limits=limits)




















# 비즈니스 활용 사례로 배우는 데이터분석 : R // 6장

# CSV 파일 읽어들이기
setwd("C://Users//MinKyu//Desktop//Boaz//R스터디//data")
ad.data <- read.csv("./ad_result.csv", header = T, stringsAsFactors = F)
head(ad.data)

# TV 광고의 광고비용과 신규 유저수의 산점도를 그리기
library(ggplot2)
library(scales)

ggplot(ad.data, aes(x = tvcm, y = install)) + geom_point() + 
  xlab("TV 광고비") + ylab("신규 유저수") + 
  scale_x_continuous(label = comma) +
  scale_y_continuous(label = comma)

# 잡지매체의 광고비용과 신규 유저수의 산점도를 그리기
ggplot(ad.data, aes(x = magazine, y = install)) + geom_point() + 
  xlab("잡지 광고비") + ylab("신규 유저수") + 
  scale_x_continuous(label = comma) + 
  scale_y_continuous(label = comma)

# 회귀분석 실행
fit <- lm(install ~ ., data = ad.data[, c("install", "tvcm", "magazine")])
fit

# 회귀분석 결과를 해석하기
summary(fit)












# 비즈니스 활용 사례로 배우는 데이터분석 : R // 7장

setwd("C://Users//MinKyu//Desktop//Boaz//R스터디//data")


# 게임 사용 기록 데이터
dau <- read.csv("section7-dau.csv", header = T, stringsAsFactors = F)
head(dau)

# 달 / 단말기 / id 의 고유값
mau <- unique(dau[, c("region_month", "device", "user_id")])

# mau중 fp, sp
fp.mau <- unique (dau[dau$device=="FP", c("region_month", "device", "user_id")])
sp.mau <- unique (dau[dau$device=="SP", c("region_month", "device", "user_id")])

# fp, sp를 각각 1월과 2월 데이터로 나누기
fp.mau1 <- fp.mau[fp.mau$region_month == "2013-01", ]
fp.mau2 <- fp.mau[fp.mau$region_month == "2013-02", ]
sp.mau1 <- sp.mau[sp.mau$region_month == "2013-01", ]
sp.mau2 <- sp.mau[sp.mau$region_month == "2013-02", ]


# 1월, fp 유저가 2월에 이용했으면 1 아니면 0
mau$is_access <- 1 
fp.mau1 <- merge(fp.mau1, mau[mau$region_month == "2013-02", c("user_id", "is_access")], by = "user_id", all.x = T)
fp.mau1$is_access[is.na(fp.mau1$is_access)] <- 0
head(fp.mau1)


# 1월, fp 유저가 2월, fp 이용했으면 1 아니면 0
fp.mau2$is_fp <- 1
fp.mau1 <- merge(fp.mau1, fp.mau2[, c("user_id", "is_fp")], by = "user_id", all.x = T)
fp.mau1$is_fp[is.na(fp.mau1$is_fp)] <- 0
head(fp.mau1)

# 1월, fp --) 2월, sp : 1 아니면 0
sp.mau2$is_sp <- 1
fp.mau1 <- merge(fp.mau1, sp.mau2[, c("user_id", "is_sp")], by = "user_id", all.x = T)
fp.mau1$is_sp[is.na(fp.mau1$is_sp)] <- 0
head(fp.mau1)


# 1월, fp --) 2월, 게임이용X  or 2월, sp
## 즉 1월에 피쳐폰으로 게임하다가 2월에 피쳐폰으로 계속 게임을 하지 않은 유저
fp.mau1 <- fp.mau1[fp.mau1$is_access == 0 | fp.mau1$is_sp == 1, ]
head(fp.mau1)


# 날짜별 게임 이용상황 데이터를 정리하기
## dcast(data, 행 ~ 열, value.var = 열 이름, length : 표값에 들어갈 값.
## paste는 사이에 space // paste0는 사이 공간X
## paste0("X", 1:31, "day") //paste("X", 1:31, "day")

library(reshape2)
fp.dau1 <- dau[dau$device == "FP" & dau$region_month == "2013-01", ]
fp.dau1$is_access <- 1
fp.dau1.cast <- dcast(fp.dau1, user_id ~ region_day, value.var ="is_access", length)
# 벡터이름 : names(vector)
# vector[-1] : 1항 제외
names(fp.dau1.cast)[-1] <- paste0("X", 1:31, "day")
head(fp.dau1.cast)


# 2월에 스마트폰으로 이용한 유저 데이터를 결합하기
fp.dau1.cast <- merge(fp.dau1.cast, fp.mau1[, c("user_id", "is_sp")], by = "user_id")
head(fp.dau1.cast)
# table : 변수 1개만 지정해도 이런식으로 표로 가능
table(fp.dau1.cast$is_sp) #0이 탈퇴 그리고 1이 ID 이전  


# family = binomial을 해 로지스틱 회귀분ㅅ
## step : AIC 사용해서 모델에 사용할 설명변수를 늘릴지 혹은 줄일지 자동으로 계산해서 찾아줌
fit.logit <- step(glm(is_sp ~ ., data = fp.dau1.cast[, -1],
                      family = binomial))
summary(fit.logit)


# 작성된 모델을 이용해 예측하기

# SP(스마트폰) 이전 확률
# fitted : yhat값
fp.dau1.cast$prob <- round(fitted(fit.logit), 2)

# SP(스마트폰)으로 이전할 지 예측
fp.dau1.cast$pred <- ifelse(fp.dau1.cast$prob > 0.5, 1, 0) 
#예측확률이 0.5보다 큰 것은 ID를 이전할 것이라고 예측했다는 뜻으로 1로 변경하고 0.5보다 작은 것은 탈퇴할 것이라고 예측했다는 뜻으로 0으로 변경.

head(fp.dau1.cast)

# 예측과 실제
table(fp.dau1.cast[, c("is_sp", "pred")]) 
#is_sp = 2월의 실제 ID 이전 상황, pred = 1월 데이터로 ID 이전을 예측한 것. 

# 예측결과로부터 유저군을 추측하기
fp.dau1.cast1 <- fp.dau1.cast[fp.dau1.cast$is_sp == 1 & fp.dau1.cast$pred == 1, ]
head(fp.dau1.cast1[order(fp.dau1.cast1$prob, decreasing = T), ]) #order 함수를 사용해서 prob항목이 큰 순서대로 정
#이 데이터를 통해 예측과 실제 모두 '1'인 유저는 역시 게임을 자주 이용했던 것을 알 수 있음.

fp.dau1.cast2 <- fp.dau1.cast[fp.dau1.cast$is_sp == 0 & fp.dau1.cast$pred == 1, ]
head(fp.dau1.cast2[order(fp.dau1.cast2$prob, decreasing = T), ])
#많은 유저에서 '1'이라는 데이터를 볼 수 있으며, 1월의 어느 시점까지는 열심히 게임을 이용했던 것을 알 수 있음. 이 10명의 유저가 게임에 흥미를 잃은 것은 아닌 듯 함. 

fp.dau1.cast3 <- fp.dau1.cast[fp.dau1.cast$is_sp == 0 & fp.dau1.cast$pred == 0, ]
head(fp.dau1.cast3[order(fp.dau1.cast3$prob), ])
#예측과 실제 모두 '0'이었던, 즉 게임을 그만둔 유저들의 이용 상황은 거의 게임을 이용하지 않고 있는데, 게임에 흥미를 잃으면서 서서히 발길이 줄어들었음을 알 수 있음.


