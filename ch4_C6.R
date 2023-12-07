data_ch4_c6<-wage2
data_ch4_c6$log_wage <- log(data_ch4_c6$wage)
#install.packages("car")
library(car)
fn <- log_wage ~ educ + exper + tenure
lm_model <- lm(fn, data = data_ch4_c6)
summary(lm_model)
test_result <- linearHypothesis(lm_model, "exper - tenure = 0")
confidence_interval <- confint(lm_model, level = 0.95)

# 顯示檢定結果
print(test_result)
print(confidence_interval)

