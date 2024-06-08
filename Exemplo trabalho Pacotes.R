library(janitor)
#chisq test
tab <- tabyl(mtcars, gear, cyl)
janitor::chisq.test(tab)

#remove constante
Df <- data.frame(A =c(1,2,3,NA),
                 B=NA,
                 C = c(1,NA,1,NA))

remove_constant(Df,na.rm = T, quiet = F)


#remove empty

Df <- data.frame(A =c(1,2,3,NA),
                 B=NA,
                 C = c(1,NA,1,NA))
remove_empty(Df,which = "rows",cutoff = 0.01,quiet = F)


#Get one to One
Df <- data.frame(A=c("a","a","b","b","c"),
                 B = c(1,1,2,2,3),
                 C= c("d","d","f","h","h"),
                 D = c(12,15,16,17,18))

get_one_to_one(Df)
#single value
x <- c(1,1,2,1,1,NA)
single_value(x,missing = c(1,NA))

single_value(c(NA, 1))




Df <- data.frame(A = c(1,2,NA),
                 B = NA,
                 C= c(1,2,1))

a <- data.frame(A = rep(1:3, each = 2),
           B = c(rep(4:6, each = 2))) 
a %>% 
  dplyr::group_by(A) %>%
  dplyr::summarize(
    B = single_value(B)
  )
#Get dupes

# fisher test#
tab <- tabyl(mtcars, gear, cyl)
fisher.test(tab)
