library(wooldridge)
data2<-(wage2)
#(i)
model_IQ <- lm(IQ ~ educ, data = data2)
coefficients(model_IQ)
#(ii)
model_logwage <- lm(log(wage) ~ educ, data = data2)
coefficients(model_logwage)
#(iii)
model_ie <- lm( log(wage) ~ IQ+educ, data = data2)
coefficients(model_ie)
######
b1<-coefficients(model_logwage)[2]
b2<-coefficients(model_ie)[3]+coefficients(model_IQ)*coefficients(model_ie)[2]
b2 <- round(as.double(b2[2]),digits=8)
b1 <- round(as.double(b1), digits=8)
if(b1==b2){
  print("yes")
}else{
  print("no")
}
