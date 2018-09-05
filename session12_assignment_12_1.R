#session12_assignment_12.1

#1. Use the given link Data Set. 
#Answer the below questions: 

yeast <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.data', stringsAsFactors = FALSE) 
char <- readLines('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.names') 
char<-char[(grep('^7', char) + 1):(grep('^8', char) - 1)]
char <- char[grep('\\d\\..*:', char)] 

names(yeast) <- make.names(c(sub('.*\\d\\.\\s+(.*):.*', '\\1', char), 'class'))
View(yeast)

#a. Perform ANOVA test on the discriminant analysis scores of nuclear localization signals of both nuclear 
#and non-nuclear proteins by class variables (Target).


library(ggplot2)
ggplot(yeast, aes(x =yeast$class, y = yeast$nuc)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("class variable") +
  ylab("nuc variable")


data<-lm(nuc~class, data=yeast)
data<-lm(yeast$nuc~yeast$class, data=yeast)
summary(data)

anova(data) # one way anova, single variable

library(car)
Anova(data,type = "III") # two way anaova uing car package

t.test(yeast$nuc,yeast$yeastclass) # no need of t test here





#b. Which class is significantly different from others? 

confint(data) # confidence interval test at 95%

require(ggplot2)
mod<-data.frame(Fitted = fitted(data),
                Residuals = resid(data), Treatment = yeast$class)

ggplot(mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()


# class NUC is significantly different from others

