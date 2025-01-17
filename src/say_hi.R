# this function takes one argument, x, appends the value of that argument
# to a greeting, and then prints the whole greeting
say_hi <- function(x) {
  hi <- paste0("Greetings, ", x, "!")
  # the paste command allows string concatenation
  return(hi)
}
