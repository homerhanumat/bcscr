railtrail <- mosaicData::RailTrail

n <- nrow(railtrail)
season <- character(n)
for ( i in 1:n ) {
  if ( railtrail$spring[i] ) {
    season[i] <- "spring"
  } else if ( railtrail$summer[i] ) {
    season[i] <- "summer"
  } else if ( railtrail$fall[i] ) {
    season[i] <- "fall"
  }
}
railtrail$season <- factor(season,
                           levels = c("spring", "summer", "fall"))
railtrail$spring <- railtrail$summer <- railtrail$fall <- NULL
save(railtrail, file = "data/railtrail.rda")
