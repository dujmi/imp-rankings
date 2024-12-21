load_league_odds <- function(seasons, fdck_id, name) {
    odds <- tidytable::tidytable()
    for (season in seasons) {
        season_odds <- tidytable::tidytable(readr::read_csv(
            sprintf(
                "https://www.football-data.co.uk/mmz4281/%d%d/%s.csv",
                season, season + 1, fdck_id
            )
        )) |>
            tidytable::select(Date, Time, HomeTeam, AwayTeam, AvgCH, AvgCD, AvgCA) |>
            tidytable::mutate(
                competition = name,
                season = season
            ) |>
            janitor::clean_names()

        odds <- tidytable::rbind(odds, season_odds)
    }
}
