# Atividade 3 - Resampling
# Machine Learning
# Gerson Vasconcelos

library(ISLR)
library(tidyverse)


####### 5) 
# A)

set.seed(1)
glm.fit <-  glm(default ~ income + balance, data = Default, family = binomial)

# B)

cinco <- function(split) {
  # i.
  train = sample(dim(Default)[1], dim(Default)[1]/split)
  # ii.
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  # iii.
  glm.pred = rep("No", dim(Default)[1]/2)
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # iv.
  return(mean(glm.pred != Default[-train, ]$default))
}
cinco(2)


# C)
cinco(3)
cinco(4)
cinco(5)

# Percebemos que os resultados mudam de acordo com o tamanho do nosso split

# D)

train <-  sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit <-  glm(default ~ income + balance + student, data = Default, family = binomial, 
              subset = train)
glm.pred <- rep("No", dim(Default)[1]/2)
glm.probs <- predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)


###### 6)
# A)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)

# B)
boot.fn <-  function(data, index) return(coef(glm(default ~ income + balance, 
                                                data = data, family = binomial, subset = index)))

# C)
library(boot)
boot(Default, boot.fn, 50)

# D)
# Respostas similares até o 2 ou 3 digito

##### 8)
# A)
y <-  rnorm(100)
x <-  rnorm(100)
y <-  x - 2 * x^2 + rnorm(100)

# B)
plot(x, y)

# C)
Data <- data.frame(x, y)
# i.
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta
# ii.
glm.fit = glm(y ~ poly(x, 2))
cv.glm(Data, glm.fit)$delta
# iii.
glm.fit = glm(y ~ poly(x, 3))
cv.glm(Data, glm.fit)$delta
# iv.
glm.fit = glm(y ~ poly(x, 4))
cv.glm(Data, glm.fit)$delta

# D)
set.seed(10)
# i.
glm.fit = glm(y ~ x)
cv.glm(Data, glm.fit)$delta
# ii.
glm.fit = glm(y ~ poly(x, 2))
cv.glm(Data, glm.fit)$delta
# iii.
glm.fit = glm(y ~ poly(x, 3))
cv.glm(Data, glm.fit)$delta
# iv.
glm.fit = glm(y ~ poly(x, 4))
cv.glm(Data, glm.fit)$delta

# Vai ser o mesmo porque o LOOCV estara avaliando n folds de uma unica observacao

# E)
# A forma polinomial quadrática tem o menor erro no LOOCV. Era de se esperar ja que é a verdadeira forma de Y  

# F)
summary(glm.fit)

# o p valor nos mostra a significância estatística dos termos linear e quadratico, que confirma os resultados da CV

########### 9)

# A)
library(MASS)
attach(Boston)

medv.mean = mean(medv)
medv.mean

#B)
medv.err = sd(medv)/sqrt(length(medv))
medv.err

# C)
boot.fn = function(data, index) return(mean(data[index]))

bstrap = boot(medv, boot.fn, 1000)
bstrap

# D)
t.test(medv)

c(bstrap$t0 - 1,96 * 0.4119, bstrap$t0 + 1,96 * 0.4119)

# E)
medv.med = median(medv)
medv.med

# F)
boot.fn = function(data, index) return(median(data[index]))
boot(medv, boot.fn, 1000)

# G)
medv.tenth = quantile(medv, c(0.1))
medv.tenth

# H)
boot.fn = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.fn, 1000)



























