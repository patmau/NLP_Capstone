library(dplyr)
library(data.table)
library(quanteda)

src <- "data/model/sample.train.2.csv"

print("Reading file")
con <- file(src, "r")
txt <- readLines(con)
close(con)
print(length(txt))

nlines <- length(txt)
chunksize <- nlines %/% 25
print(chunksize)

lower <- round(seq(1, nlines, by = chunksize))
upper <- pmin(lower + chunksize - 1, nlines)

print(lower)
print(upper)

# tokenize
all_tokens <- lapply(1:length(lower), function(i) {
    print(paste("Tokenize", i, sep = " "))
    tokens_remove(
    tokens(
        txt[lower[i]:upper[i]],
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_url = TRUE
    ), pattern = stopwords("english"))
})
rm(list="txt")

print(object.size(all_tokens), units = "MB")

# unigrams    
unigrams <- rbindlist(lapply(all_tokens, function(tokens) {
    print("Unigrams")
    
    tokenFreq.uni <- dfm(tokens, tolower = FALSE) %>%
        textstat_frequency()
    
    data.table(
        token = tokenFreq.uni$feature,
        freq = tokenFreq.uni$frequency)
}))[
    , .(freq = sum(freq)), by = token
    ][
        freq > 3, 
    ]
print(object.size(unigrams), units = "MB")
setkey(unigrams, token)
#fwrite(unigrams, file = "data/model/unigram.csv")

# n-gram function (n > 1)
ngrams <- function(tokens_list, n, concatenator, cutFrequency = 0) {
    rbindlist(lapply(tokens_list, function(tokens) {
        print(paste(n, "-grams", sep = ""))
        
        tk <- tokens_ngrams(tokens, n = n, concatenator = concatenator)
        tkfreq <- dfm(tk, tolower = FALSE) %>%
            textstat_frequency()
        
        data.table(token = tkfreq$feature,
                   freq = tkfreq$frequency)
    }))[
        , .(freq = sum(freq)), by = token
    ][
        freq > cutFrequency, ]
}


bigrams <- ngrams(all_tokens, n = 2, concatenator = " ", cutFrequency = 4)
print(object.size(bigrams), units = "MB")
setkey(bigrams, token)
# fwrite(bigrams, file = "data/model/bigram.csv")
# rm(list="bigrams")

trigrams <- ngrams(all_tokens, n = 3, concatenator = " ", cutFrequency = 1)
print(object.size(trigrams), units = "MB")
setkey(trigrams, token)
# fwrite(trigrams, file = "data/model/trigram.csv")
# rm(list="trigrams")
rm
tetragrams <- ngrams(all_tokens, n = 4, concatenator = " ", cutFrequency = 1)
print(object.size(tetragrams), units = "MB")
setkey(tetragrams, token)
# fwrite(tetragrams, file = "data/model/tetragram.csv")
# rm(list="tetragrams")

pentagrams <- ngrams(all_tokens, n = 5, concatenator = " ", cutFrequency = 1)
print(object.size(pentagrams), units = "MB")
setkey(pentagrams, token)
# fwrite(pentagrams, file = "data/model/pentagram.csv")
# rm(list="pentagrams")

ngrams <- list(unigrams, bigrams, trigrams, tetragrams, pentagrams)
saveRDS(ngrams, "data/model/ngrams.rds")

