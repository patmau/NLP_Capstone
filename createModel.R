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

# tokenize
all_tokens <- lapply(1:length(lower), function(i) {
    print(paste("Tokenize", i, sep = " "))
    tokens(
        txt[lower[i]:upper[i]],
        remove_punct = FALSE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        remove_url = TRUE,
    )
})

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
        freq > 1, 
    ]
print(object.size(unigrams), units = "MB")

# bigrams
bigrams <- rbindlist(lapply(all_tokens, function(tokens) {
    print("Bigram")
    
    tk <- tokens_ngrams(tokens, n = 2, concatenator = " ")
    tkfreq <- dfm(tk, tolower = FALSE) %>%
        textstat_frequency()
    
    data.table(token = tkfreq$feature,
               freq = tkfreq$frequency)
}))[
    , .(freq = sum(freq)), by = token
    ][
        freq > 1,
    ]
print(object.size(bigrams), units = "MB")

# trigrams
trigrams <- rbindlist(lapply(all_tokens, function(tokens) {
    print("Trigram")
    
    tk <- tokens_ngrams(tokens, n = 3, concatenator = " ")
    tkfreq <- dfm(tk, tolower = FALSE) %>%
        textstat_frequency()
    
    data.table(token = tkfreq$feature,
               freq = tkfreq$frequency)
}))[
    , .(freq = sum(freq)), by = token
][
    freq > 1,
]
print(object.size(trigrams), units = "MB")


