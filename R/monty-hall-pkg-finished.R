#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'  Create a function to select an initial door
#' 
#' @description
#'  `select_door()` generates a new function that allows a contestant to pick 
#'  one of three doors in the game
#' 
#' @details
#'  According to the rules, to get the game started, a contestant 
#'  may choose any of the three doors in the game with no limitations
#'  placed on which one is chosen. the chosen door is then kept a secret, 
#'  so the contestant does not know if they have chosen a losing goat door  
#'  or a winning car door.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a number between 1 and 3 that corresponds 
#' with a door number
#' 
#' @examples
#'   select_door()
#'   
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'  Create a function that opens a goat door
#'  
#' @description
#'  `open_goat_door()` generates a new function that opens one of the two 
#'  remaining doors that is a losing goat door
#'
#' @details
#'  After the contestant selects their chosen door, the host 
#'  will open one of the two remaining doors. However, the 
#'  door that is opened MUST be one of the losing goat doors.
#'  
#' @param  ... no arguments are used by the function.
#' 
#' @return 
#' The function returns a number between 1 and 3 that corresponds with a goat door
#' 
#' @examples
#'   open_goat_door()
#' 
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'  Create a change door function.
#' 
#' @description
#'  `change_door()` generates a new function that allows the contestant to either 
#'  keep their initial selected door, or change to the remaining unopened door.
#' 
#' @details
#'  After one losing door is revealed by the host, the contestant is then given 
#'  the option to either keep their initial door, or change their selection to the
#'  final remaining door.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'  The function returns a number between 1 and 3 that corresponds with a door
#' 
#' @examples
#'  change_door()
#' 
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title 
#'  Determine if the contestant has won or not. 
#'  
#' @description
#'  `determine_winner()` function reveals if the contestant's choice has
#'  resulted in winning or losing the game.
#' 
#' @details
#'  The contestant will win if they have chosen a door with a
#'  car behind it and lose if they have chosen a door with a goat 
#'  behind it.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'  This function will return 'WIN' if the contestant has chosen a door with a
#'  car behind it and LOSE' if the contestant has chosen a door with a goat 
#'  behind it.
#' 
#' @examples
#'   determine_winner()
#' 
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'  Complete game play through
#' 
#' @description
#'  `play_game()` function will do a complete put together play through of the 
#'  Monty Hall game.
#' 
#' @details
#'  The contestant will make a choice of door, the host will open a non-winning door,
#'  the contestant the has the choice to switch to the remaining door or keep their original 
#'  one. It will then be revealed if they win based on whether or not they have chosen to 
#'  stay or switch their original choice of door. 
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'  The function will return a data frame based on the contestant's chosen strategy
#' 
#' 
#' @examples
#'   play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Simulation of 100 randomized Monty Hall games 
#'  
#' @description
#'  `play_n_games()` function will run a series of 100 random Monty Hall games 
#'  
#' @details
#'  To determine what the best player strategy is, a series of 100 random games 
#'  will be played to see if staying with the original door or 
#'  switching to the remaining door, is the more effective strategy.
#' 
#' @param ... no arguments are used by the function.
#' 
#' @return 
#'  The function will return a table of game results based off of a simulation 
#'  of 100 different games with randomized player strategies of staying or switching. 
#' 
#' @examples
#' play_n_games()
#' 
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
