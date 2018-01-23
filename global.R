library(shiny)
library(digest)
  
# set the number of failed attempts allowed before user is locked out

num_fails_to_lockout <- 5
