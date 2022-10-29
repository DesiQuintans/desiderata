

# Find elements in a vector that are less than the elements that come before and
# after it, and replace it with NA.
omit_dips <- function(vec) {
    # Base R lag and lead
    before <- function(vec) { c(NA, vec[seq_len(length(vec) - 1)]) }
    after <- function(vec) { c(vec[-seq_len(1)], NA) }
    
    ifelse((!is.na(before(vec) & !is.na(after(vec)))) & 
               (vec <= before(vec) & vec <= after(vec)), NA, vec)
}


# Checks if an element in a vector is isolated, i.e. if it surrounded by a value 
# that is being omitted.
isolated_element <- function(vec, omit) {
    lag_vec <- c(NA, vec[1:length(vec)-1])
    lead_vec <- c(vec[2:length(vec)], NA)
    
    lag_vec %in% omit & vec %notin% omit & lead_vec %in% omit
}

# Format CIs as 3 numbers truncated to decent length.
format_ci <- function(str) {
    ci <- 
        list(lower = nth_word(str, 1),
             estim = nth_word(str, 2),
             upper = nth_word(str, 3))
    
    ci <- lapply(ci, stringr::str_pad, width = 5, side = "left", pad = "??")
    
    sprintf("\\textsubscript{%s} %s \\textsubscript{%s}", ci$lower, ci$estim, ci$upper)
}

encode_signif <- function(vec) {
    case_when(vec <= 0.0001 ~ "****",
              vec <= 0.001  ~ "***",
              vec <= 0.01   ~ "**",
              vec <= 0.05   ~ "*",
              TRUE   ~ "ns")
}

# Is this useful?
`%has%` <- function(x, y) {
    grepl(y, x)
}

importMultiple <- function(path, ext, func, ...) {
    # Imports multiple files with a particular extension, concatenating them into a single
    # dataframe with a new 'sourceFile' column specifying the origin of each row. Inspired
    # by Gregor's Stack Overflow answer re. working with lists of dataframes.
    # http://stackoverflow.com/a/24376207
    
    listOfFiles <- file.path(path, list.files(path = path, pattern = paste0("*.", ext)))
    listOfDFs <- lapply(listOfFiles, func, ...)
    names(listOfDFs) <- gsub(paste0("\\.", ext, "$"), "", listOfFiles)
    
    return(bind_rows(listOfDFs, .id = "sourceFile"))
}

# Collapse multiple strings into one.
str_coll <- function(v, col = ",") {
    # If using space or a character as col, can use the regex
    #   \\b\\w+?\\b 
    # to grab individual words.
    paste0(v, collapse = col) %>% str_trim()
}

```{r voting-algorithm}
vote_percentages <- function(df) {
    df %>% 
        group_by(voter) %>% 
        filter(rank == min(rank)) %>% 
        ungroup() %>% 
        count(bird_breed) %>% 
        adorn_percentages(denominator = "col") %>% 
        arrange(n)
}

a_bird_has_won <- function(result) {
    any(result$n > 0.50)
}

drop_least_votes <- function(initial, result) {
    least_popular <- result[1,]$bird_breed
    
    initial %>% 
        filter(bird_breed != least_popular)
}

instant_runoff_voting <- function(df) {
    vote_result <- vote_percentages(df)
    
    if (a_bird_has_won(vote_result) == TRUE) {
        return(vote_result)
    } else {
        drop_least_votes(df, vote_result) %>% 
            instant_runoff_voting()
    }
}
```

# Return the first sentence that matches a query near the start of a sentence.
subsentence <- function(str, query, buffer = 10) {
    keywords <- ifelse(length(query) == 1,
                       query,
                       paste0("(", paste(query, collapse = "|"), ")"))
    needle   <- paste0("(\\. |; |^).{0,", buffer, "}", keywords, ".*?(\\. [a-z]|$)")
    
    str_extract(str, regex(needle, ignore_case = TRUE))
}


