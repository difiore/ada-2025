rm(list = ls())

library(tidyverse)
library(sjmisc)
library(huxtable)

load_dictionary <- function(filename){
  require(tidyverse)
  dictionary <- read_csv(filename, col_names = TRUE)
  # names(dictionary) <- c("words")
  dictionary <- dictionary[["words"]]
  dictionary <- toupper(dictionary)
  return(dictionary)
}

pick_solution <- function(dictionary, word_length = 5) {
  require(tidyverse)
  possible_solutions <- dictionary[nchar(dictionary) == word_length]
  solution <- sample(possible_solutions, 1)
  # print(solution)
  solution_vector <- str_split(solution, "")[[1]]
  # print(solution_vector)
  return(solution_vector)
}

play_wordle <- function(solution, valid_list, num_guesses = 6, output_type = "text") {
  require(tidyverse)
  require(sjmisc)
  require(huxtable)
  word_length <- length(solution)
  print(paste0("You have ", num_guesses, " chances to guess a word of length ", word_length))
  letters_left <- LETTERS # a built-in set of capital letters
  guess_history <- data.frame(matrix(nrow = num_guesses, ncol = word_length))
  result_history <- data.frame(matrix(nrow = num_guesses, ncol = word_length))
  if (output_type == "graphic"){
    guess_history <- as_huxtable(guess_history)
    result_history <- as_huxtable(result_history)
  }
  for (i in 1:num_guesses) {
    # display "keyboard"
    print(paste0(c("Letters left: ", letters_left), collapse = " "))
    # read in guess and confirm length and validity
    guess <- readline(paste0("Enter guess ", i, ": ")) %>% toupper()
    while (nchar(guess) != word_length) {
      guess <- readline(paste0("Guess must have ", word_length, " characters: ")) %>% toupper()
    }
    while (guess %nin% valid_list){
      guess <- readline(paste0("Hmm, that word is not in my dictionary of valid words: ")) %>% toupper()
    }
    # print(guess) # check output
    guess <- str_split(guess, "")[[1]]
    # print(guess) # check output

    # evaluate guess
    result <- evaluate_guess(guess, solution, output_type)

    # update keyboard
    letters_left <- setdiff(letters_left, guess)

    # print results
    guess_history[i,] <- guess
    result_history[i,] <- result

    if (output_type == "text") {
      if (all(result == "*")) {
        guess_history <- guess_history %>% na.omit()
        result_history <- result_history %>% na.omit()
        print(paste0("You won in ", i, " guesses!"))
        guess_history <- guess_history %>% unite(everything(), sep="", col="guess", remove=TRUE)
        result_history <- result_history %>% unite(everything(), sep="", col="result", remove=TRUE)
        history <- data.frame(
          guess = guess_history,
          result = result_history)
        print(history)
        return(invisible(history))
      } else {
        history <- data.frame(
          guess = paste0(guess, collapse = ""),
          result = paste0(result, collapse = ""))
        print(history)
      }
    }

    if (output_type == "graphic") {
      if (all(background_color(result) == "#6BA964")) {
        history <- result_history %>% na.omit()
        print(paste0("You won in ", i, " guesses!"))
        print(history, colnames = FALSE)
        return(invisible(history))
      } else {
        print(result, colnames = FALSE)
      }
    }
  }
  print(paste0("Sorry, you lost! Solution was ", paste0(solution, collapse = "")))

  if (output_type == "text"){
    guess_history <- guess_history %>% unite(everything(), sep="", col="guess", remove=TRUE)
    result_history <- result_history %>% unite(everything(), sep="", col="result", remove=TRUE)
    history <- data.frame(
      guess = guess_history,
      result = result_history)
    print(history)
    return(invisible(history))
  }

  if (output_type == "graphic"){
    history <- result_history
    print(history, colnames = FALSE)
    return(invisible(history))
  }

  return()
}

evaluate_guess <- function(guess, solution, output_type) {
  word_length <- length(solution)
  text_result <- rep("-", word_length)
  for (i in 1:word_length){
     text_result[i] <-
       case_when (
         guess[i] %in% solution & guess[i] == solution[i] ~ "*",
         guess[i] %in% solution & guess[i] != solution[i] ~ "+",
         guess[i] %nin% solution ~ "-"
       )
  }
  # format for graphic output
  graphic_result <- t(data.frame(guess)) %>%
     as_huxtable() %>%
     theme_basic()%>%
     set_all_padding(10) %>%
     set_text_color("#FFFFFF") %>%
     set_align("center") %>%
     set_bold(TRUE) %>%
     set_all_borders(brdr(4, "solid", "white")) %>%
     set_font("arial") %>%
     set_font_size(18)
  for (i in 1:word_length){
     graphic_result <- set_background_color(
       graphic_result, 1, i, case_when(
         text_result[i] == "*" ~ "#6BA964",
         text_result[i] == "+" ~ "#C9B458",
         text_result[i] == "-" ~ "#787C7E"
       )
     )
  }
  if (output_type == "text") {
      return(text_result)
    } else {
      return(graphic_result)
    }
}

f_solution_list <- "data/google-10000-english-usa-no-swears.txt"
f_valid_list <- "data/collins-scrabble-words-2019.txt"

valid_list <- load_dictionary(f_valid_list)
str(valid_list)
solution_list <- load_dictionary(f_solution_list)
solution_list <- intersect(solution_list,valid_list) # makes sure possible solution words are in valid_list
str(solution_list)
solution <- pick_solution(solution_list, word_length = 5)
str(solution)
game <- play_wordle(solution, valid_list, num_guesses = 6, output = "graphic")

