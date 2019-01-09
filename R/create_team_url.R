#'Create a URL for team information
#'@team The team you want information from.
#'@type The type of information you want.
#'@export
nfl_create_team_url <- function(team, type = "draft"){

    first_part <- "https://www.pro-football-reference.com"

    if(type == "draft"){
        second_part <- "draft.htm"
    }

    paste0(first_part, team, second_part)
}
