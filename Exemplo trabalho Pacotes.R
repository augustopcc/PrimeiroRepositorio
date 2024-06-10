# Trabalho Pacote janitor
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
# 1)clean_names
Data_Frame <- data.frame(
  "Nome " = c("Alice", "João", "Maria"),
  "Idade (Anos)" = c(22.4, 19.7, 20.5),
  "Data de Matrícula" = c(44204, 44235, 44266))
Data_Frame <- clean_names(Data_Frame)
print("Após limpar os nomes das colunas:")
print(Data_Frame)

# 2)convert_to_date
convert_to_date("2009-07-06")
convert_to_date(1285)
convert_to_datetime(
  c("2009-07-06", "40000.1", "40000", NA),
  character_fun=lubridate::ymd_h, truncated=1, tz="UTC"
)

# 3)excel_numeric_to_date
Data_Frame <- data.frame(
  "Nome " = c("Alice", "João", "Maria"),
  "Idade (Anos)" = c(22.4, 19.7, 20.5) ,
  "Data de Matrícula" = c(44204, 44235, 44266))

Data_Frame <- clean_names(Data_Frame)
Data_Frame$data_de_matricula <- excel_numeric_to_date(Data_Frame$data_de_matricula)
print(Data_Frame)

# 4)find_header
df_sem_cabecalho <- data.frame(
  X1 = c(NA, "Nome", "Alice", "João", "Maria"),
  X2 = c(NA, "Idade",22.4, 19.7, 20.5),
  X3 = c(NA, "Data de Matrícula", 44204, 44235, 44266)
)
linha_de_cabeçalho <- find_header(df_sem_cabecalho)

# 5)make_clean_names
df <- data.frame(
  "Nome " = c("Alice", "João", "Maria"),
  "Idade (Anos)" = c(22.4, 19.7, 20.5) ,
  "Data de Matrícula" = c("2021-01-01", "2021-02-01", "2021-03-01")
  
)
nomes_colunas <- colnames(df)
nomes_colunas_limpos <- make_clean_names(nomes_colunas)
colnames(df) <- clean_column_names

# 6)round_half_up
df <- data.frame(
  "Nome " = c("Alice", "João", "Maria"),
  "Nota" = c(60.7, 50.8, 90.9)
  
)
print(df)
df$Nota<- round_half_up(df$Nota)


print(df)

# 7)round_to_fraction
df <- data.frame(
  "Nome " = c("Alice", "João", "Maria"),
  "Nota" = c(60.7, 50.8, 90.9)
  
)
print(df)
df$Nota<- round_to_fraction(df$Nota, denominator = 3)


print(df)

# 8)row_to_names
df <- data.frame(
X1 = c(NA, "Name", "Alice", "Bob", "Charlie"),
X2 = c(NA, "Age", 25, 30, 35),
X3 = c(NA, "Joining Date", "2021-01-01", "2021-02-01", "2021-03-01")
)
print(df)
df <- row_to_names(df, row_number = 2)

print(df)

# 9)sas_numeric_to_date
df <- data.frame(
  Nome = c("Alice", "João", "Maria"),
  Data = c(21916, 21947, 21978) # Datas no formato numérico do SAS
)
print(df)
df$Data <- sas_numeric_to_date(df$Data)

# Exibir o data frame com a coluna de datas convertida
print(df)

# 10)signif_half_up
df <- data.frame(
  Nome = c("Alice", "Bob", "Charlie"),
  Score = c(1.7345, 1.2855, 1.9365)
)

print(df)
df_clean <- df %>%
  mutate(Score = signif_half_up(Score, digits = 3))


print(df_clean)

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



