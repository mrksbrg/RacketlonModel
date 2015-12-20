library(rjags)

bugs_sv5 <- read.csv("BugsSV5_0.csv")

#define the model
model.txt <- "model {
diff <- tt_result + ba_result + sq_result + te_result

#priors
tt_result ~ dnorm(-5,0.01)
ba_result ~ dnorm(10,0.01)
sq_result ~ dnorm(15,0.01)
te_result ~ dnorm(-10,0.01)

# Do some predictions

# Not summer, not in test, not released
#log(lambda_pred_1) <- intercept + size_coeff*1.1 + 118*beta
#log(lambda_pred_1) <- intercept + size_coeff*1.1 + 81*beta
#log(lambda_pred_1) <- intercept + size_coeff*1.1 + 110*beta

}"

data <- list(sys_size=bugs_sv5$size, is_summer=bugs_sv5$summer, in_test=bugs_sv5$test, released=bugs_sv5$release, y=bugs_sv5$bugCount, x=bugs_sv5$month)
inits <- list("intercept"=10)

model.jags <- jags.model(textConnection(model.txt),
                         data=data, inits=inits, n.adapt=1000)


model_vars<- c("intercept", "beta", "size_coeff", "summer_coeff", "test_coeff", "rel_coeff")
pred_vars <- c("pred_35", "pred_40",  "pred_45", "pred_50", "pred_55", "pred_60", "pred_65", "pred_70", "pred_75", "pred_80")
samples <- coda.samples(model.jags, c(model_vars,pred_vars) , n.iter=1e6)
samples_matrix <- as.matrix(samples)
print(summary(samples_matrix))
plot(samples)
qq <- quantile(samples_matrix)

