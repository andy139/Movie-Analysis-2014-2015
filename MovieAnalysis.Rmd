---
title: "PSTAT126Project"
author: "Arthur Li-Chuan Lee & Andy Tran"
date: "September 9, 2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(leaps)


CSM1415 = read_excel("C:/Users/antr9/Desktop/project126/CSMdataset1.xlsx")



CSM2014 = read_excel("C:/Users/antr9/Desktop/project126/2014movies.xlsx")

CSM2015 = read_excel("C:/Users/antr9/Desktop/project126/2015movies.xlsx")



```

```{r}
CSM2014naomit = na.omit(CSM2014)
Ratings = CSM2014naomit$Ratings
Genre = CSM2014naomit$Genre
Budget = CSM2014naomit$Budget
Screens = CSM2014naomit$Screens
Sequel = CSM2014naomit$Sequel
Sentiment = CSM2014naomit$Sentiment
Views = CSM2014naomit$Views
Likes = CSM2014naomit$Likes
Dislikes = CSM2014naomit$Dislikes
Comments = CSM2014naomit$Comments
Aggregate.Followers = CSM2014naomit$Aggregate.Followers
Gross = CSM2014naomit$Gross
pairs(log(Gross)~Ratings+Genre+Budget+Screens+Sequel+Sentiment+Views+Likes+Dislikes+Comments+Aggregate.Followers)
```

```{r boxcox}
boxcox(Gross~Ratings+Budget+Screens+Aggregate.Followers)
CSM2014boxcox = lm(Gross^(1/3)~Ratings+Budget+Screens+Aggregate.Followers)
summary(CSM2014boxcox)
e = resid(CSM2014boxcox)
Yhat = fitted(CSM2014boxcox)
```

```{r Gross vs. Rating}
##Plotting Gross vs IMDB score in 2014
fit = lm(Gross^(1/3)~Ratings)
summary(fit)
col = sort(rnorm(163))
ggplot(CSM2014, aes(x=Ratings, y=Gross^(1/3), color=Ratings))+geom_point()+scale_color_gradientn(colours = rainbow(3))+ geom_smooth(method=lm)
##Resdiual Analysis
e = resid(fit)
Yhat = fitted(fit)
plot(Yhat, e, xlab = 'Fitted Values', ylab = 'Residual', main = 'Residual vs Fit')
abline(h = 0, lty = 2)
#slight heavy-tail problem
qqnorm(e)
qqline(e)
shapiro.test(e)
par(mfrow=c(2,2))
plot(fit)
```


```{r Gross vs. Budget}
plot(Budget, Gross, xlab ="Movie Budget", ylab ="Gross Revenue",  main = 'Movies in 2014')
mod = lm(Gross^(1/3)~Budget)
cooks.distance(mod)
e = resid(mod)
Yhat = fitted(mod)
plot(Yhat, e, xlab = 'Fitted Values', ylab = 'Residual', main = 'Residual vs Fit')
abline(h = 0, lty = 2)
```

```{r AIC}
CSM2014LoBFReduced = lm(Gross^(1/3)~1)
CSM2014LoBF = lm(log(Gross)~Ratings+Budget+Screens+Sequel+Views+Likes+Dislikes+Comments+Aggregate.Followers)
summary(step(CSM2014LoBF, scope = list(lower=CSM2014LoBFReduced, upper=CSM2014LoBF)))
```

```{r BIC}
CSM2014regsubsets = summary(regsubsets(cbind(Ratings, Genre, Budget, Screens, Sequel, Sentiment, Views, Likes, Dislikes, Comments, Aggregate.Followers), log(Gross)))
CSM2014regsubsets$which
CSM2014regsubsets$adjr2
```

```{r CP}
CSM2014regsubsets$which
CSM2014regsubsets$cp
```

```{r}
CSM2014LoBFCP = lm(Gross^(1/3)~Ratings+Genre+Screens+Dislikes)
anova(CSM2014LoBFReduced, CSM2014LoBFCP)
```

```{r}
CSM2015naomit = na.omit(CSM2015)
predict(CSM2014boxcox, CSM2015naomit)^3
```

```{r}

```

```{r}

```
