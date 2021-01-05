library(dplyr)
library(data.table)
library(quanteda)

src <- "data/model/sample.train.csv"

print("Reading file")
con <- file(src, "r")
txt <- readLines(con)
close(con)
print(length(txt))
txt <- sapply(txt, function(t) { paste0("ENTRYSTART ", t) }, USE.NAMES = FALSE)

nlines <- 20000 #length(txt)
chunksize <- nlines / 10
chunksize

lower <- round(seq(1, nlines, by = chunksize))
upper <- pmin(lower + chunksize - 1, nlines)

print(lower)
print(upper)


dts <- lapply(1:length(lower), function(i) {
    print(paste("Tokenize", i, sep = " "))
    all_tokens <- tokens(
        txt[lower[i]:upper[i]],
        remove_punct = FALSE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_url = TRUE,
    )
    
    tokenFreq.uni <- dfm(all_tokens, tolower = FALSE) %>%
        textstat_frequency()
    
    unigram <- data.table(
        token = tokenFreq.uni$feature,
        freq = tokenFreq.uni$frequency)
    
    print(paste("Bigram", i, sep = " "))
    bitoken <- tokens_ngrams(all_tokens, n = 2, concatenator = " ")
    tokenfreq.bi <- dfm(bitoken, tolower = FALSE) %>%
        textstat_frequency()
    
    bigram <- data.table(
        token = tokenfreq.bi$feature,
        freq = tokenfreq.bi$frequency
    )
    
    c(unigram, bigram)
})

#dtsum <- dts[, .(freq = sum(freq)), by = token]

# print("Bigrams")
# bigrams <- tokens_ngrams(all_tokens, n = 2, concatenator = "_")
# tokenFreq.bi <- dfm(bigrams, tolower = FALSE) %>%
#     textstat_frequency()
# 
# bigram.dt <- data.table(
#     token = tokenFreq.bi$feature,
#     freq = tokenFreq.bi$frequency
# )
# 
# 
# print("Trigrams")
# trigrams <- tokens_ngrams(all_tokens, n = 3, concatenator = "_")
# tokenFreq.tri <- dfm(trigrams, tolower = FALSE) %>%
#     textstat_frequency()
# 
# trigram.dt <- data.table(
#     token = tokenFreq.tri$feature,
#     freq = tokenFreq.tri$frequency
# )
# 

