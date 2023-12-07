library(wooldridge)
data<-wage1
model_exper <- lm(educ ~ exper+tenure, data = data)
re <- residuals(model_exper)
model_of_y <- lm( log(data$wage) ~ re, data = data)
model_full <- lm(log(data$wage) ~ exper+tenure+educ, data = data)
coeff_re_model_of_y <- coef(model_of_y)["re"]
coeff_educ_model_full <- coef(model_full)["educ"]
cat("Coefficient of 're' in model_of_y:", coeff_re_model_of_y, "\n")
cat("Coefficient of 'educ' in model_full:", coeff_educ_model_full, "\n")
#re和educ這兩個變數對響應變數的貢獻相當。簡而言之無論是re還是educ都在解釋log(wage)方面具有相似的影響