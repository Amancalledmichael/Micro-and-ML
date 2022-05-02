setwd("M:/Kurse/Micro and ML/Tutorial")

labsupp <- read.csv(file = 'mroz_wool.csv')
View(labsupp)

#OLS
OLS<-lm(hours ~ kidslt6 + kidsge6 + age + educ + city + exper + expersq, data = labsupp)
summary(OLS)


#1)
library(AER)
tobit<-tobit(hours ~ kidslt6 + kidsge6 + age + educ + city + exper + expersq, data = labsupp)
summary(tobit)
tobittable <- coef(summary(tobit))


#2)
library(truncreg)
trunc <- truncreg(hours ~ kidslt6 + kidsge6 + age + educ + city + exper + expersq, data = labsupp)
summary(trunc)
trunctable <- coef(summary(trunc))


#3)
# Wage is highly endogenous to hours. Hours affect wages and vice versa. "bad control" problem. 
# The regression equation can be interpreted as how family care affects labour supply of women of same
# age, education and experience living in the same 
#sdjsidjs
#break
#awdad
# lucas schreibt

#4)
income<-lm(hours ~ faminc, data = labsupp)
summary(income)
library(margins)
margins(income)

incomecond<-lm(hours ~ faminc, data = subset(labsupp, inlf==1))
summary(incomecond)
margins(incomecond)

tobitwage<-tobit(hours ~ wage, data = labsupp)
summary(tobitwage)

OLSwage<-lm(hours ~ wage, data=labsupp)
margins(OLSwage)