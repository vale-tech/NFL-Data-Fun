# standard
library("tidyverse")
library("here")
library("nflfastR") # v3.0.0

# Air Yards data from nflfastR
# This script uses 2019 in order to compare with AirYards.com data

pbp_raw <- data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

# Position data is fairly important to air yards so going to decode the rosters
pbp <- pbp_raw %>%
        nflfastR::decode_player_ids() %>%
        mutate(receiver_id = str_trim(receiver_id, side = "both")) # cleanup join fields, have encountered leading spaces

# Get the rosters
roster <- fast_scraper_roster(c(2019))
roster <- roster %>% mutate(gsis_id = str_trim(gsis_id, side = "both")) # cleanup join fields, have encountered leading spaces

# Initial pass data
# My filter is correct I think....
# Faking position and headshot_url for unknowns
# Fixing headshot_url to use https to avoid headaches....
# Generating a target feature to be more specific than counting rows
# Yards after catch NA cleanup
pbp_reg_season_pass_attempts <- pbp %>%
                                filter(
                                    season_type == 'REG' &
                                    season >= 2019 &
                                    !is.na(posteam) & !is.na(defteam),
                                    pass_attempt == 1,
                                    sack == 0,
                                    !is.na(air_yards)
                               ) %>%
                               mutate(
                                   receiver = if_else(is.na(receiver), "UNK", receiver),
                                   receiver_id = if_else(is.na(receiver_id), "-1", receiver_id)
                               ) %>%
                               left_join(roster, by = c("season" = "season", "receiver_id" = "gsis_id")) %>%
                               mutate(
                                   position = if_else(is.na(position), "UNK", position),
                                   full_name = if_else(is.na(full_name), "UNK", full_name),
                                   headshot_url = if_else(is.na(headshot_url), "http://static.nfl.com/#static/content/public/static/img/fantasy/transparent/200x200/BOY619498.png", as.character(headshot_url)),
                                   headshot_url = str_replace(headshot_url, "http://", "https://"),
                                   target = if_else((complete_pass == 1 | incomplete_pass == 1 | interception == 1), 1, 0),
                                   yards_after_catch = replace_na(yards_after_catch, 0)
                               ) %>%
                               select(
                                   posteam,
                                   receiver_id,
                                   receiver,
                                   target,
                                   air_yards,
                                   touchdown,
                                   yards_after_catch,
                                   yards_gained,
                                   complete_pass,
                                   incomplete_pass,
                                   interception,
                                   week,
                                   season,
                                   headshot_url,
                                   full_name,
                                   position,
                                   desc,
                                   game_id
                               )

# Calculate team level stats per game
# There is probably a better way to do this, but I have not discovered yet
pbp_reg_season_pass_attempts_team_rollup <- pbp_reg_season_pass_attempts %>%
                                            group_by(game_id, posteam) %>%
                                            summarize(
                                                team_air_yards = sum(air_yards),
                                                team_attempts = sum(target)
                                            ) %>%
                                            ungroup

# Now we generate some metrics for the week
air_yards_by_week <- pbp_reg_season_pass_attempts %>%
                     group_by(game_id, season, week, posteam, receiver_id, receiver, full_name, position, headshot_url) %>%
                     summarize(
                         air_yards = sum(air_yards),
                         touchdown = sum(touchdown),
                         targets = sum(target),
                         reception = sum(complete_pass),
                         receiving_yards = sum(yards_gained),
                         yards_after_catch = sum(yards_after_catch),
                         incomplete_passes = sum(incomplete_pass),
                         interceptions = sum(interception)
                     ) %>%
                     ungroup %>%
                     inner_join(pbp_reg_season_pass_attempts_team_rollup, by = c("game_id", "posteam")) %>%
                     group_by(game_id, season, week, posteam, receiver_id, receiver, full_name, position, headshot_url) %>%
                     mutate(
                        wopr = round(0.7 * (air_yards / team_air_yards) + 1.5 * (targets / team_attempts), 2),
                        racr = receiving_yards / air_yards,
                        aDot = air_yards / targets,
                        target_share = targets / team_attempts,
                        ms_air = air_yards / team_air_yards
                    ) %>%
                    ungroup

# Note: validated against AirYards.com using 2019 Mike. Values line up fairly close, but there seems to be some subtle differences in Air Yards, but I am not sure exactly why if AirYards.com is also using nflfastR.
