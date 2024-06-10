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


#### Limpeza e exploração de dados ####
# 1) chisq.test
tab <- tabyl(mtcars, gear, cyl)
chisq.test(tab)

# 2) compare_df_cols
Df1 <- data.frame(A = 1,
                  B = 3,
                  C = "a",
                  D = F)
Df2 <- data.frame(B = 3,
                  C = 3,
                  E = NA)
compare_df_cols(Df1,Df2,return = "all")
compare_df_cols(Df1,Df2,return = "match")
compare_df_cols(Df1,Df2,return = "mismatch")


# 3) compare_df_cols_same
Df1 <- data.frame(A = 1,
                  B = 3,
                  C = "a",
                  D = F)

Df2 <- data.frame(B = 3,
                  C = 3,
                  E = NA) 
compare_df_cols_same(Df1,Df2)

# 4) describe_class
describe_class(Df1)
describe_class("a")
describe_class(F)
describe_class(Inf)

# 5) fisher.test
tab <- tabyl(mtcars, gear, cyl)
fisher.test(tab)

# 6) get_dupes
Df3 <- data.frame(A = c(1,1,1,1,1,1,2,2), 
                  B = rep(c('B','C'), times = 4),
                  C = LETTERS[1:8],
                  D = rep(c(F,T), times = 4))

get_dupes(Df3,B)
get_dupes(Df3,A)
get_dupes(Df3,B,A)

# 7) get_one_to_one
foo <- data.frame(
  Lab_Test_Long=c("Cholesterol, LDL", "Cholesterol, LDL", "Glucose"),
  Lab_Test_Short=c("CLDL", "CLDL", "GLUC"),
  LOINC=c(12345, 12345, 54321),
  Person=c("Sam", "Bill", "Sam"),
  stringsAsFactors=FALSE
)
get_one_to_one(foo)

# 8) remove_constant
Df8 <- data.frame(A = c(1,1,1,1,1,1,1,1), 
                  B = rep(c('B','C'), times = 4),
                  C = LETTERS[1:8],
                  D = rep(c(F,T), times = 4),)

remove_constant(Df8)

# 9) remove_empty
Df9 <- data.frame(A = c(1,1,1,1,1,1,NA,1), 
                  B = c('B','C','B','C','B','C',NA,'C'),
                  C = c(LETTERS[1:6],NA,'i'),
                  D = NA,
                  E = rep(c(NA,T), times = 4))

remove_empty(Df9,which = "rows")
remove_empty(Df9,which = "cols")
remove_empty(Df9,which = c("rows", "cols"))

# 10) single_value 
single_value(c(NA, 1))
single_value(c(NA, "a"), missing = c(NA, "a"))



