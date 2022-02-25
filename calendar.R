mvv_calendar <- read.csv(file.path("mvv", "calendar.txt"))
mvg_calendar <- read.csv(file.path("mvg", "calendar.txt"))
mvv_calendar$source <- "mvv"
mvg_calendar$source <- "mvg"
calendar_bothNames <- intersect(names(mvv_calendar), names(mvg_calendar))
calendar <- rbind.data.frame(mvv_calendar[, calendar_bothNames],
                                  mvg_calendar[, calendar_bothNames])
rm("mvv_calendar", "mvg_calendar")
