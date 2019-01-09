#'get the links for each nfl team
#'@param file The location to store a data frame with the link for each team.
#'@importFrom xml2 read_html
#'@import rvest
#'@importFrom dplyr bind_cols
#'@importFrom tibble tibble
#'@export

nfl_get_team_info <- function(file){

    # URL with team links
    url <- "https://www.pro-football-reference.com/teams/"

    # Read the URL
    html <- xml2::read_html(url)





    # Extract all the nodes with nfl team links
    nodes <- html %>%
        rvest::html_nodes("#div_teams_active") %>%
        rvest::html_nodes("table")  %>%
        xml2::xml_find_all(".//tbody/tr/th[@class='left ']")  %>%
        rvest::html_nodes("a")

    # Function that extracts the link and the name of the team.
    get_data <- function(ns){
        link <- ns %>%
            rvest::html_attrs() %>%
            tibble::tibble(link = .)

        name <- ns %>%
            rvest::html_text() %>%
            tibble::tibble(name = .)

        dplyr::bind_cols(name,link)
    }

    # Convert list of nodes to a data frame.
     df <- nodes %>%
         purrr::map_df(get_data)

    # Save the file to disk.
     readr::write_rds(df, file)

}
