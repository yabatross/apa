#
# Preproces
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

# Training i test sets per cross validation

s <- nrow (df)

split1 <- s/3
split2 <- 2*s/3

chunk1 <- df[1:split1,]
chunk2 <- df[split1:split2,]
chunk3 <- df[split2:s,]

train1 <- rbind (chunk1, chunk2)
test1  <- chunk3

train2 <- rbind (chunk1, chunk3)
test2  <- chunk2

train3 <- rbind (chunk2, chunk3)
test3  <- chunk1
