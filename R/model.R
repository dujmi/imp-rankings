if (!file.exists("data/input/leagues.rds")) {
    leagues <- build_leagues()
}
else {
    leagues <- readRDS("data/input/leagues.rds")
}

seasons <- c(23, 24) # start year

for (i in 1:nrow(leagues)) {
    league = leagues[i, ]

    for (season in seasons) {
        
    }
}

