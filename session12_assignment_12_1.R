#session12_assignment_12.1

#1. Use the given link Data Set. 
#Answer the below questions: 

yeast <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.data', stringsAsFactors = FALSE) 
l <- readLines('https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.names') 
l<-l[(grep('^7', l) + 1):(grep('^8', l) - 1)]
l <- l[grep('\\d\\..*:', l)] 

names(yeast) <- make.names(c(sub('.*\\d\\.\\s+(.*):.*', '\\1', l), 'class'))
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
t.test(yeast$nuc,yeast$yeastclass)

anova(data) # one way anova, single variable



#b. Which class is significantly different from others? 

confint(data) # confidence interval test at 95%

require(ggplot2)
mod<-data.frame(Fitted = fitted(data),
                Residuals = resid(data), Treatment = yeast$class)

ggplot(mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()


# class NUC is significantly different from others

