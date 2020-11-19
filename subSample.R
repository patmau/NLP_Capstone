srcDir <- "data/final"
sampleDir <- "data/sample25"
lang <- "en_US"


sources <- c("news", "blogs", "twitter")

for (src in sources) {
    fileName <- paste("en_US", src, "txt", sep = ".")
    
    file <- paste(srcDir, lang, fileName, sep = "/")
    
    con <- file(file, "r")
    
    text <- readLines(con)
    close(con)
    
    print(length(text))
    
    # subsample 
    set.seed(999)
    take <- rbinom(length(text), 1, 0.25)
    
    sampleText <- text[take == 1]
    
    outfileName <- paste("sample25", fileName, sep = ".")
    outfile <- paste(sampleDir, outfileName, sep = "/")
    
    write(sampleText, file = outfile)
}
