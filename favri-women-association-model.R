# FAVRI Women's association
# mobile selling of safe vegetables

library(decisionSupport)
make_variables <- function(est,n=1)
{x <- random(rho=est,n=n)
for(i in colnames(x))assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("inputs_women_assoc.csv",sep="")))


