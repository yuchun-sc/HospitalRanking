# spam prediction with single predictor

library(kernlab)
data("spam")
# perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5);
table(trainIndicator)
trainSpam = spam[trainIndicator == 1, ];
testSpam = spam[trainIndicator == 0, ];

# explortatory analysis -- 1
names(trainSpam)
head(trainSpam)
table(trainSpam$type)

plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

plot(log10(trainSpam[, 1:4] + 1))

hCluster = hclust(dist(t(trainSpam[, 1:57])));
plot(hCluster)

hClusterUpdated <- hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

# statistical modeling -- 2
trainSpam$numType = as.numeric(trainSpam$type) - 1;
costFunction <- function(x, y) sum(x != (y > 0.5));
cvError = rep(NA, 57);
library(boot);
for (i in 1:57) {
    lmFormula <- reformulate(names(trainSpam)[i], response = "numType");
    glmFit <- glm(lmFormula, family = "binomial", data = trainSpam);
    cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2];
}

# which predictor has minimum cross-validated error?
predictor <- names(trainSpam)[which.min(cvError)];

# use the best model from the group
predictionModel <- glm(numType ~ predictor, family = "binomial", data = trainSpam);

# get predictions on the test set
predictionTest <- predict(predictionModel, testSpam);
predictedSpam <- rep("nonspam", dim(testSpam)[1]);

# classify as 'spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam";
# classification table
table(predictedSpam, testSpam$type)