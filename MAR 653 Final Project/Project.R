#Taylor Moorman, Wes Stanis, Ron Arana
#MAR 652 Final Project
#Bank Data

library(aod)
library(ggplot2)
library(ISLR)
setwd("C:/Users/tmoorman/OneDrive - BeyondTrust Software Inc/Documents/Not Work Related/school/Spring 2019/MAR 653")


df <- read.csv("bank-additional-full.csv")

smp_siz <-floor(0.75*nrow(df))
set.seed(123)
train_ind = sample(seq_len(nrow(df)),size = smp_siz)
train =df[train_ind,]
test=df[-train_ind,]
testy <- test$y
testnoy <- test[,-21]

binarylogit <- glm(y ~ ., data = train, family = "binomial")

summary(binarylogit)

prediction <- predict(binarylogit, testnoy)

#heatmap <- table(prediction, testy)

totaltest <- cbind(test,prediction)

write.csv(totaltest, file = "testWpredictions.csv")

nova <- anova(binarylogit, test = "Chisq")

write.csv(nova, file = 'modelanova.csv')



fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))


###############################

socdf <- read.csv("bank-additional-socio.csv")

smp_siz <-floor(0.75*nrow(socdf))
set.seed(123)
strain_ind = sample(seq_len(nrow(socdf)),size = smp_siz)
strain =socdf[strain_ind,]
stest=socdf[-strain_ind,]
stesty <- stest$y
stestnoy <- stest[,-21]

sociobinarylogit <- glm(y ~ ., data = strain, family = "binomial")

summary(sociobinarylogit)

socioprediction <- predict(sociobinarylogit, stestnoy)

#heatmap <- table(prediction, testy)

sociototaltest <- cbind(stest,socioprediction)

write.csv(sociototaltest, file = "sociotestWpredictions.csv")

socionova <- anova(sociobinarylogit, test = "Chisq")

#######################################################################

cdf <- read.csv("bank-additional-client.csv")

smp_siz <-floor(0.75*nrow(cdf))
set.seed(123)
ctrain_ind = sample(seq_len(nrow(cdf)),size = smp_siz)
ctrain =cdf[ctrain_ind,]
ctest=cdf[-ctrain_ind,]
ctesty <- ctest$y
ctestnoy <- ctest[,-21]

cbinarylogit <- glm(y ~ ., data = ctrain, family = "binomial")

summary(cbinarylogit)

cprediction <- predict(cbinarylogit, ctestnoy)

#heatmap <- table(prediction, testy)

ctotaltest <- cbind(ctest,cprediction)

write.csv(ctotaltest, file = "clienttestWpredictions.csv")

cnova <- anova(cbinarylogit, test = "Chisq")

#######################################################################

mdf <- read.csv("bank-additional-campaign.csv")

smp_siz <-floor(0.75*nrow(mdf))
set.seed(123)
mtrain_ind = sample(seq_len(nrow(mdf)),size = smp_siz)
mtrain =mdf[mtrain_ind,]
mtest=mdf[-mtrain_ind,]
mtesty <- mtest$y
mtestnoy <- mtest[,-21]

mbinarylogit <- glm(y ~ ., data = mtrain, family = "binomial")

summary(mbinarylogit)

mprediction <- predict(mbinarylogit, mtestnoy)

#heatmap <- table(prediction, testy)

mtotaltest <- cbind(mtest,mprediction)

write.csv(mtotaltest, file = "campaigntestWpredictions.csv")

mnova <- anova(mbinarylogit, test = "Chisq")






