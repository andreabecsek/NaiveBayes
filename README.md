[![Build Status](https://travis-ci.com/andreabecsek/NaiveBayes.png?branch=master)](https://travis-ci.com/andreabecsek/NaiveBayes)

# Gaussian Naive Bayes

This is an example package created for the Statistical Computing 1 module. The package fits a Gaussian Naive Bayes classifier to a given dataset and output vector. The predictions are obtained from the fitted model and the test data by first computing the posterior class probabilities and then a MAP decision rule is used to obtain the actual predictions.

The main assumption that Naive Bayes makes is that every feature is conditionally independent given the class labels. The reason why this classifier is called naive is that very often this assumption is not actually realistic.

