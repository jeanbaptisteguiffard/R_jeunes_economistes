
list_of_packages <- c('dplyr', 'stargazer', 'fixest')
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(lenght(new.packages()), install.packages(new.packages, repos="http://cran.us.r-project.org"))
invisible(lapply(list_of_packages, library, only=TRUE))
#install.packages('causaldata')
library(causaldata)
?causaldata

library(fixest)
library(stargazer)
?fixest


data("Mroz")
View(Mroz)

rm(list=ls())
gc()



fml_ols_1 <- lwg ~ inc+age
fml_ols_2 <- lwg ~ inc+age+wc+hc

ols_model_1 <- feols(fml_ols_1, data=Mroz)
ols_model_2 <- feols(fml_ols_2, data=Mroz)

summary(ols_model_1)


etable(ols_model_1,ols_model_2)

stargazer(ols_model_1,ols_model_2, type='text')

Mroz$lfp_num <- as.numeric(Mroz$lfp)


fml_binary <- lfp~inc+age+wc+hc
lpm_model <- feols(fml_binary, data=Mroz)
logit_model <- feglm(fml_binary, data=Mroz, family="logit")


etable(lpm_model,logit_model)


#install.packages("marginaleffects")
library(marginaleffects)
logit_margins <- avg_slopes(logit_model)

data('gapminder')

twfe_mo <- feols(lifeExp ~ log(gdpPercap) | country+year , data=gapminder)
etable(twfe_mo)
