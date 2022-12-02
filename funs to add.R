
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



instant_runoff_voting <- function(df) {
    vote_percentages <- function(df) {
        df %>% 
            group_by(voter) %>% 
            filter(rank == min(rank)) %>% 
            ungroup() %>% 
            count(bird_breed) %>% 
            adorn_percentages(denominator = "col") %>% 
            arrange(n)
    }
    
    someone_won <- function(result) {
        any(result$n > 0.50)
    }
    
    drop_least_votes <- function(initial, result) {
        least_popular <- result[1,]$bird_breed
        
        initial %>% 
            filter(bird_breed != least_popular)
    }
    
    vote_result <- vote_percentages(df)
    
    if (someone_won(vote_result) == TRUE) {
        return(vote_result)
    } else {
        drop_least_votes(df, vote_result) %>% 
            instant_runoff_voting()
    }
}
