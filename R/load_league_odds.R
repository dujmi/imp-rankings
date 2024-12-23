load_league_odds <- function(seasons, fdck_id, name) {
    odds <- NULL
    for (season in seasons) {
        if (season == seasons[2]) {
            path <- sprintf(
                "https://www.football-data.co.uk/mmz4281/%d%d/%s.csv",
                season, season + 1, fdck_id
            )
        } else {
            path <- sprintf(
                "data/input/%s.csv",
                fdck_id
            )
        }
        season_odds <- tidytable::tidytable(readr::read_csv(path)) |>
            tidytable::select(Date, Time, HomeTeam, AwayTeam, AvgCH, AvgCD, AvgCA) |>
            tidytable::mutate(
                competition = name,
                season = season
            ) |>
            janitor::clean_names()

        if (is.null(odds)) {
            odds <- season_odds
        } else {
            odds <- tidytable::bind_rows(list(odds, season_odds))
        }
    }

    return (odds)
}
