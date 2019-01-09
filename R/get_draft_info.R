#'@import dplyr
#'@importFrom xml2 read_html
#'@export
nfl_get_draft_info <- function(url){

    html <- xml2::read_html(url)

    get_row_info <- function(ns){

        player_link <- ns %>%
            html_nodes("a") %>%
            html_attr("href") %>%
            .[stringr::str_detect(.,"players")] %>%
            tibble(link = .)

        children <- ns %>%
            xml_children()

        col_names <- children %>%
            xml_attr("data-stat")

        value <- children %>%
            html_text()

        df <- dplyr::tibble(col_names,value) %>%
            tidyr::spread(col_names, value) %>%
            dplyr::select_at(col_names)

        if(nrow(player_link) == 0){
            return(df)
        } else {
            bind_cols(df, player_link)
        }

    }

    df <- html %>%
        rvest::html_nodes("table#draft") %>%
        xml_find_all(".//tbody") %>%
        xml_children() %>%
        map(get_row_info) %>%
        bind_rows() %>%
        filter(year_id != "Year")


}
