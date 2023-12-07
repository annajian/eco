library(wooldridge)
data_ch4_c11<-htv
#(i)
model_sq<-lm(educ~motheduc+fatheduc+abil+ I(abil^2),data=data_ch4_c11)
f_test <- summary(model_sq)$fstatistic
p_value <- pf(f_test[1], f_test[2], f_test[3], lower.tail = FALSE)

print(paste("F Statistic:", f_test[1]))
print(paste("Degrees of Freedom:", f_test[2], f_test[3]))
print(paste("P-value:", p_value))
# 根據Ｐ值可以拒絕H0:educ & abil的關係是線性的
#(ii)
test_result <- linearHypothesis(model_sq, c("motheduc = fatheduc"))
print(test_result)

# (iii)
model_full<-lm(educ~motheduc+fatheduc+abil+ I(abil^2)+tuit17+tuit18,data=data_ch4_c11)
model_simplified <- lm(educ ~ motheduc + fatheduc + abil + I(abil^2), data = data_ch4_c11)
f_test <- anova(model_simplified, model_full)
print(f_test)

# (iv)
correlation <- cor(data_ch4_c11$tuit17, data_ch4_c11$tuit18)
cat("Correlation coefficient:", correlation, "\n")
#根據相關係數顯示tuit17&tuit18之間有高度相關
data_ch4_c11$avg_tuition <- rowMeans(data_ch4_c11[, c("tuit17", "tuit18")], na.rm = TRUE)
model_avg_tuition <- lm(educ ~ motheduc + fatheduc + abil + I(abil^2) + avg_tuition, data = data_ch4_c11)
f_test_avg <- anova(model_avg_tuition, model_full)
print(f_test_avg)

