vectorToString <- function(v, symb = " / "){
    ## generate a string of the elements of a vector, with separater 'symb'
    s <- as.character(v[1])
    if (length(v) > 1){for (i in 2:length(v)){s <- paste(s, symb, v[i], sep = "")}}
    return(s)
    }
