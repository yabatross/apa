#
# Preproceso
#
encode_salary <- function (str)
{
  gt <- substr(str,2,2) == ">"
  if (gt) return (1)
  else return (0)
}


csv <- read.csv ("data/adult.data", header=TRUE, sep=",", dec=",")

df <- as.data.frame (csv)
df$State.gov     <- as.numeric(df$State.gov)
df$Bachelors     <- as.numeric(df$Bachelors)
df$Never.married <- as.numeric(df$Never.married)
df$Adm.clerical  <- as.numeric(df$Adm.clerical) 
df$Not.in.family <- as.numeric(df$Not.in.family)
df$White         <- as.numeric(df$White)
df$Male          <- as.numeric(df$Male)
df$United.States <- as.numeric(df$United.States)
df$X..50K        <- sapply(as.character(df$X..50K), encode_salary)
