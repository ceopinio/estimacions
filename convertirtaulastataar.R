#Codi per convertir l'output d'stata en input per a l'estimació d'escons
#cal adaptar al nom dels 4 arxius que genera el codi d'stata

options(scipen = 999) 
library(readxl)
m4tot <- read_excel("m4tot.xlsx")
m4tot.t<-t(m4tot)
m4tot.t<-data.frame(m4tot.t)
names(m4tot.t) <- m4tot.t[1,]
m4tot.t <- m4tot.t[-1, ]
m4tot.t$partit<-c("PP", "ERC", "PSC", "Cs", "CUP", "Junts", "ECP", "Vox", "BAN", "NSNC")
m4tot.t<-select(m4tot.t, b, se, ll, ul, partit)
col_order <- c("partit", "b", "se",
               "ll", "ul")
m4tot.t <- m4tot.t[, col_order]
m4tot.t<-m4tot.t %>%
  rename(
    proporcio = b,
    SE = se,
    lb95 = ll,
    up95 = ul
  )

