library(data.table)
library(quanteda)
library(stringr)

ngrams <- readRDS("data/model/ngrams.rds")

t5 <- head(ngrams[[5]])

print(t5)

t5[, prediction := word(token, start = -1)][, stopword := prediction %in% stopwords()]
print(t5)

t5$token = word(t5$token, end = -2)
print(t5)

for (n in 1:5) {
    print(n)
    ngrams[[n]][, prediction := word(token, start = -1)][, stopword := prediction %in% stopwords()]
    ngrams[[n]]$token = word(ngrams[[n]]$token, start = -n, end = -pmin(2, length(ngrams[[n]]$token)))
    setkey(ngrams[[n]], token)
}

saveRDS(ngrams, "data/model/predictionModel.rds")
