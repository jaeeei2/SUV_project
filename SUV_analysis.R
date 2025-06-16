SUV <- read.csv("C:\\Users\\Park\\Desktop\\재이\\2022-1\\R데이터\\R_dataset\\SUV_Purchase.csv",
                 fileEncoding = "UTF-8") 

str(SUV)   # 변수 구조
head(SUV)  # 앞부분 미리보기

#범주형 변수 팩터 변환
SUV$Gender <- as.factor(SUV$Gender)
SUV$Purchased <- as.factor(SUV$Purchased)

# 결측치 확인
colSums(is.na(SUV))

# 변수 분포
summary(SUV)

# 변수 제거
SUV <- SUV[, -1]

# EDA
library(ggplot2)
ggplot(SUV, aes(x=Gender, fill=as.factor(Purchased))) +
  geom_bar(position="fill") +
  labs(title="성별에 따른 구매 비율", y="비율", fill="구매여부")

ggplot(SUV, aes(x=Age, fill=as.factor(Purchased))) +
  geom_histogram(binwidth=5, position="fill", color="black") +
  labs(title="연령대에 따른 구매 비율", y="비율", fill="구매여부")

ggplot(SUV, aes(x=EstimatedSalary, y=Age, color=as.factor(Purchased))) +
  geom_point(alpha=0.6) +
  labs(title="예상 연봉과 나이에 따른 구매 여부", color="구매여부")

ggplot(SUV, aes(x = Age, y = EstimatedSalary, color = factor(Purchased))) +
  geom_point(alpha = 0.7) +
  labs(color = "구매 여부", title = "나이 vs 연봉 산점도")

# 모델링
# 필요한 경우 install.packages() 먼저
install.packages("rpart")
install.packages("rpart.plot")

# 로딩
library(rpart)
library(rpart.plot)

# 데이터 분류
set.seed(123)  # 결과 재현을 위한 설정

# 데이터 70% 학습용, 30% 테스트용으로 나누기
idx <- sample(1:nrow(SUV), size = 0.7 * nrow(SUV))
train <- SUV[idx, ]
test <- SUV[-idx, ]

# 의사결정 나무
tree_model <- rpart(Purchased ~ ., data = train, method = "class")
rpart.plot(tree_model)

# 예측
pred <- predict(tree_model, newdata = test, type = "class")

# 혼동 행렬
table(Predicted = pred, Actual = test$Purchased)

# 정확도 계산
mean(pred == test$Purchased)
confusionMatrix(pred, test$Purchased)

library(rpart.plot)
rpart.plot(tree_model)
tree_model$variable.importance

barplot(tree_model$variable.importance,
        main = "Variable Importance",
        col = "skyblue",
        xlab = "Variables",
        ylab = "Importance")

