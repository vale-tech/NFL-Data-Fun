library("tidyverse")
library("jsonlite")

get_sleeper_players <- function() {
    uri <- "https://api.sleeper.app/v1/players/nfl"
    sleeper_json <- fromJSON(uri)

    players_unlisted <- lapply(sleeper_json, function(x) {
        x[sapply(x, is.null)] <- NA
        unlist(x)
    })

    sleeper_players <- do.call(bind_rows, players_unlisted)

    return(sleeper_players)
}

sleeper_players <- get_sleeper_players()