build_leagues <- function() {
    leagues <- tidytable(
        name = c("Premier League", "La Liga", "Serie A", "Ligue 1", "Bundesliga"),
        fb_url = c(
            "https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures",
            "https://fbref.com/en/comps/12/schedule/La-Liga-Scores-and-Fixtures",
            "https://fbref.com/en/comps/11/schedule/Serie-A-Scores-and-Fixtures",
            "https://fbref.com/en/comps/13/schedule/Ligue-1-Scores-and-Fixtures",
            "https://fbref.com/en/comps/20/schedule/Bundesliga-Scores-and-Fixtures"
        ),
        fdck_id = c("E0", "SP1", "I1", "F1", "D1"),
        format = c(2, 2, 2, 2, 2)
    )

    saveRDS(leagues, file = "data/input/leagues.rds")
    return leagues
}
