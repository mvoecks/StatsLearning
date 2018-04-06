# Michael Voecks
# APPM 4580
# Homework 4 Code

#Load necessary libraries
library(leaps)
library(klaR)
library(pROC)

# Load the dataset and set the testing data
load('spam.RData')
testdata <- dat[!train,]$spam

### Build Model A
# Determine the relative covariates using bidirectional stepwise selection
full <- glm(spam~., data=dat, subset=train)
null <- glm(spam~1, data=dat, subset=train)
#modAsubset <- step(null, scope=list(upper=full), data=dat, direction='both')

# Get the names of the relevant covariates
#names(coef(modAsubset))

# Fit Model A - Logistic Regression
modelA <- glm(spam ~ your + X000 + remove + punc_exclam + capTot + free + hp + 
	      	our + punc_dollar + internet + money + over + font + credit +
		meeting + punc_semiColon + george + edu + email + data + X1999 +
		re + you + order + will + hpl + business + project + all + X3d + 
		conference + table + original + address + X85 + make + receive + 
		parts + direct + labs + punc_pound + punc_leftParen + mail +
		pm + technology + capLong + punc_leftSquare,
       		family = binomial, data = dat, subset = train)

# Make predictions with Model A
modelA.probs <- predict(modelA, newdata=dat[!train,], type="response")
cbind(modelA.probs)

### Build Model B
# Perform bidirectional stepwise selection for lda and qda models, look at accuracy scores
#ldaBoth <- stepclass(spam~., data=dat, method="lda", direction="both", criterion="AC")
#qdaBoth <- stepclass(spam~., data=dat, method="qda", direction="both", criterion="AC")
#ldaBack <- stepclass(spam~., data=dat, method="lda", direction="backward", criterion="AC")
#qdaBack <- stepclass(spam~., data=dat, method="qda", direction="backward", criterion="AC")

modelB <- qda(spam ~ make + address + all + X3d + our + over + remove + internet +
	     	 order + receive + will + addresses + free + business + email + 
		 you + credit + your + font + X000 + money + hp + hpl + X650 + 
		 labs + data + X415 + X1999 + direct + original + project + re + 
		 edu + punc_semiColon + punc_leftParen + punc_leftSquare + 
		 punc_exclam + punc_dollar + punc_pound + capAvg + capLong + capTot, 
	 	 data=dat, subset=train)

# Make predictions with Model B
modelB.pred <- predict(modelB, dat[!train,])

### Find outliers in the model
# Model A
png('modelAbox.png')
boxplot(modelA.probs~testdata, main="Model A", xlab="Class", ylab="Probability Prediction")

# Model B
png('modelBbox.png')
boxplot(modelB.pred$posterior[,2]~testdata, main="Model B", xlab="Class", ylab="Probability Prediction")


### Create confusion matricies for both models
# Apply classification rule of 50%
modelA.predict <- rep(0, sum(!train))
modelA.predict[modelA.probs > 0.5] <- 1

modelB.predict <- rep(0, sum(!train))
modelB.predict[modelB.pred$posterior[,2] > 0.5] <- 1

# Model A
table(modelA.predict, testdata)
mean(modelA.predict[testdata==1]) #True Postive Rate / Sensitivity
1 - mean(modelA.predict[testdata==0]) #Specificity

# Model B
table(modelB.predict, testdata)
mean(modelB.predict[testdata==1]) #True Positive Rate / Sensitivity
1 - mean(modelB.predict[testdata==0]) #Specificity



### Produce ROC Curves for each model
modelA.roc <- roc(testdata~modelA.probs)
modelB.roc <- roc(testdata~modelB.pred$posterior[,2])

png('modelAroc.png')
plot(modelA.roc, main="Model A")

png('modelBroc.png')
plot(modelB.roc, main="Model B")

# Calculate Area Under Curve
auc(modelA.roc)
auc(modelB.roc)


# Generate TPR/FPR threshold plots
png('modelAth.png')
matplot(data.frame(modelA.roc$sensitivities, modelA.roc$specificities), x = modelA.roc$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR', main="Model A")

png('modelBth.png')
matplot(data.frame(modelB.roc$sensitivities, modelB.roc$specificities), x = modelB.roc$thresholds, type='l', xlab = 'threshold', ylab='TPR, TNR', main="Model B")


# Re-create confusion matricies for updated thresholds
modelA.predict <- rep(0, sum(!train))
modelA.predict[modelA.probs > 0.4] <- 1

modelB.predict <- rep(0, sum(!train))
modelB.predict[modelB.pred$posterior[,2] > 0.6] <- 1

# Model A
table(modelA.predict, testdata)
mean(modelA.predict[testdata==1]) #True Postive Rate / Sensitivity
1 - mean(modelA.predict[testdata==0]) #Specificity

# Model B
table(modelB.predict, testdata)
mean(modelB.predict[testdata==1]) #True Positive Rate / Sensitivity
1 - mean(modelB.predict[testdata==0]) #Specificity


# Calculate Brier scores for both models
mean((modelA.probs - testdata)^2) #Model A
mean((modelB.pred$posterior[,2] - testdata)^2)

