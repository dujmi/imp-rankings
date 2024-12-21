source("build_leagues.R")
source("load_league_odds.R")

if (!file.exists("data/input/leagues.rds")) {
    leagues <- build_leagues()
}
else {
    leagues <- readRDS("data/input/leagues.rds")
}

seasons <- c(23, 24) # start years

for (i in 1:nrow(leagues)) {
    league <- leagues[i, ]

    league_odds <- load_league_odds(seasons, league$fdck_id, league$name)

    league_odds <- league_odds |>
        tidytable::mutate(
            datetime = lubridate::dmy_hms(paste(date, time)),
            hprob = 1 / avg_ch,
            dprob = 1 / avg_cd,
            aprob = 1 / avg_ca,
            total_prob = hprob + dprob + aprob,
            h_nv = hprob / total_prob,
            d_nv = dprob / total_prob,
            a_nv = aprob / total_prob
        ) |>
        tidytable::select(-date, -time, -hprob, -dprob, -aprob, -total_prob) |>
        tidytable::arrange(desc(datetime))

    teams_in_league <- length(unique(c(odds[season == seasons[2]]$home_team, odds[season == seasons[2]]$away_team)))
    games_in_season <- nrow(odds[odds$season == seasons[2], ]) / (teams_in_league / 2)
    half_games <- ifelse(
        games_in_season < (teams_in_league - 1) / 2,
        games_in_season,
        (teams_in_league -  1) / 2
    )
    alpha <- 0.75 ** (1 / half_games)

    home_games_odds <- odds |>
        tidytable::select(datetime, competition, season, home_team, h_nv, d_nv) |>
        tidytable::mutate(
            xp = 3 * h_nv + 1 * d_nv,
        ) |>
        tidytable::rename(
            team = home_team,
        ) |>
        tidytable::select(-h_nv, -d_nv)

    away_games_odds <- odds |>
        tidytable::select(datetime, competition, season, away_team, a_nv, d_nv) |>
        tidytable::mutate(
            xp = 3 * a_nv + 1 * d_nv,
        ) |>
        tidytable::rename(
            team = away_team,
        ) |>
        tidytable::select(-a_nv, -d_nv)

    all_teams_games_odds <- rbind(home_games_odds, away_games_odds)

    relegated_teams <- all_teams_games_odds |>
        tidytable::filter(!(team %in% unique(all_teams_games_odds |>
            tidytable::filter(season == seasons[2]) |>
            tidytable::pull(team)))) |>
        tidytable::pull(team) |>
        unique()

    promoted_teams <- all_teams_games_odds |>
        tidytable::filter(!(team %in% unique(all_teams_games_odds |>
            tidytable::filter(season == seasons[1]) |>
            tidytable::pull(team)))) |>
        tidytable::pull(team) |>
        unique()

    all_teams_games_odds <- all_teams_games_odds |>
        tidytable::arrange(desc(datetime)) |>
        tidytable::group_by(competition, team) |>
        tidytable::arrange(team, desc(datetime)) |>
        tidytable::mutate(
            rank = row_number()
        ) |>
        tidytable::filter(rank <= (teams_in_league - 1) * 2) |>
        tidytable::ungroup() |>
        tidytable::mutate(
            xp_w = case_when(
                !(team %in% relegated_teams) ~ xp * alpha ** (rank - 1),
                (team %in% relegated_teams) & (rank <= 38 - games_in_season) ~ xp * alpha ** (rank - 1 + games_in_season),
                TRUE ~ 0
            )
        )

    season_rankings <- all_teams_games_odds |>
        tidytable::group_by(competition, team) |>
        tidytable::summarise(
            xp_w = sum(xp_w)
        )

    mean_relegated_teams_xp <- season_rankings |>
        tidytable::filter(team %in% relegated_teams) |>
        tidytable::summarise(mean_xp = mean(xp_w, na.rm = TRUE)) |>
        tidytable::pull(mean_xp)

    season_rankings <- season_rankings |>
        tidytable::mutate(
            xp_w = ifelse(
                (team %in% promoted_teams),
                xp_w + mean_relegated_teams_xp, xp_w
            )
        ) |>
        tidytable::filter(
            !(team %in% relegated_teams)
        ) |>
        arrange(desc(xp_w))
}

