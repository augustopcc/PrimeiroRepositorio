library(janitor)


#### Tabela de frequência e formatação #### 
#adorn_ns
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "front")

#adorn_pct_formatting
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting()

#adorn_percentages
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages("col")

#adorn_rounding
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_percentages() %>%
  adorn_rounding(digits = 2, rounding = "half up")

#adorn_title
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_title(placement = "top")

#adorn_totals
mtcars %>%
  tabyl(am, cyl) %>%
  adorn_totals()

# as_tabyl
Df <-  data.frame(A = c(1,5,6,4),
                  B = c(8,4,2,2))
as_tabyl(Df)

#tabyl
x <- c("big", "big", "small", "small", "small", NA)
tabyl(x)

#top_levels
top_levels(as.factor(mtcars$hp), 2)

#untabyl
untabyl(tabyl(mtcars$am))

#### Formatação de dataframe ####


#### Limpeza e exploração ####
#chisq test
tab <- tabyl(mtcars, gear, cyl)
janitor::chisq.test(tab)

#compare_df_cols
compare_df_cols(data.frame(A=1, B = "C"), data.frame(B=2))

#compare_df_cols_same
compare_df_cols_same(data.frame(A=1, B = "F"), data.frame(B=2))

#describe_class
class_description <- describe_class(mtcars)
print(class_description)

# fisher test #
tab <- tabyl(mtcars, gear, cyl)
fisher.test(tab)

#get_dupes
Df <- data.frame(A=c("a","a","b","b","c"),
                 B = c(1,1,2,2,3),
                 C= c("d","d","f","h","h"),
                 D = c(12,15,16,17,18))
Df %>% get_dupes(A)

#Get one to One
Df <- data.frame(A=c("a","a","b","b","c"),
                 B = c(1,1,2,2,3),
                 C= c("d","d","f","h","h"),
                 D = c(12,15,16,17,18))

get_one_to_one(Df)

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




