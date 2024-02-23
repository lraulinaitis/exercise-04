# PRELIMINARIES ----
library(tidyverse)

scrabble_wrds <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/google-10000-english-usa-no-swears.txt"
google_words <- "https://raw.githubusercontent.com/difiore/ada-2024-datasets/main/collins-scrabble-words-2019.txt"

# STEP 1. Create load_dictionary() function & word lists ----
load_dictionary <- function(filename) {
  as.character(unlist(read_tsv(filename, col_names = TRUE)))}

# create solution_list (possible solution words) and valid_list (valid words to guess)
solution_list <- load_dictionary(scrabble_wrds)
valid_list <- load_dictionary(google_words)

# check class
str(valid_list) # looks good
str(solution_list) # looks good

rm(scrabble_wrds, google_words) # clear from environment

# STEP 2. Winnow solution_list to only include valid words ----
solution_list <- intersect(solution_list, valid_list) # 8336 words

# STEP 3. Write pick_solution() fxn ----
pick_solution <- function(solution_list, word_length = 5) {
  five_letter_words <- subset(solution_list, nchar(solution_list) == word_length) # create subset of 5-letter words
  solution <- sample(five_letter_words, 1, replace = FALSE) # choose random word from subset
  solution_char <- str_split_1(solution, "") # splits random word into vector of characters   
  return(solution_char)
}
solution <- pick_solution(solution_list)

# STEP 4. Create play_wordle() and evaluate_guess() fxns ----

play_wordle <- function(solution, valid_list, num_guesses = 6) {
  
  # evaluate guess sub-fxn 
  evaluate_guess <- function(valid_list) {
    guess <- ""
    guess_validate <- FALSE
    
    while (!guess_validate) {
      guess <- toupper(readline("Enter a 5-letter word, then press <enter>: "))
      
      if (nchar(guess) == 5 && guess %in% valid_list) {
        guess_validate <- TRUE
      } else {
        invalid_msg <- "Invalid guess. Try again."
        print(invalid_msg)
      }
    }
    return(guess)
  }
  
  # print instructions 
  print(c("You have six tries to guess a random 5-letter word.",
          "Guesses need to be valid 5-letter words.",
          "After each guess, you will be told whether each letter your guess is either",
          "[1] in the solution word and in the correct spot,",
          "[2] in the solution word but in the wrong position, or",
          "[3] not in the solution word."))
  
  # create output table
  result_output <- matrix(nrow = 0, ncol = 5, dimnames = list(c(), c("L1", "L2", "L3", "L4", "L5")))
  
  # prompt guess & read in
  guessed_letters <- list() # initialize guessed letters list
  
  for (i in 1:num_guesses) { 
    # print unguessed letters
    unguessed_letters <- setdiff(LETTERS, guessed_letters)
    print(paste("Letters left:", paste(unguessed_letters, collapse = " ")))
    
    # read in and evaluate user guess
    guess <- evaluate_guess(valid_list) # send to validation sub-fxn
    guess_char <- str_split(guess, "")[[1]] # split guess into vector of characters
    guessed_letters <- unique(c(guessed_letters, guess_char)) # add letters to list of guessed letters
    
    result <- guess_char
    for (x in seq_along(guess_char)) {
      if (guess_char[x] %in% solution) {
        result[x] <- paste0(guess_char[x], "*")
      }
      if (guess_char[x] == solution[x]) {
        result[x] <- paste0(guess_char[x], "^")
      }
    }
    
    # add to output table
    result_output <- rbind(result_output, result)
    
    # print outputs
    print(paste("Guess", i, "of", num_guesses))
    print(result_output)
    print(c("Result indicators:",
            "^ : in solution and in correct position",
            "* : in solution but in wrong position", 
            "- : not in the solution word"))
    
    if (all(guess_char == solution)) {
      print("Congratulations! You won!")
      return(invisible())
    }
  }
  print("Try again!")
  
  restart <- toupper(readline("New Game? (Y/N): "))
  if (restart[1] == "Y") {
    play_wordle()
  } else {
    print("Thanks for playing!")
  }
}

# Run game ----
play_wordle(solution, valid_list)