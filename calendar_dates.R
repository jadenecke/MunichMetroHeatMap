mvv_calendarDates <- read.csv(file.path("mvv", "calendar_dates.txt"))
mvg_calendarDates <- read.csv(file.path("mvg", "calendar_dates.txt"))
mvv_calendarDates$source <- "mvv"
mvg_calendarDates$source <- "mvg"
calendarDates_bothNames <- intersect(names(mvv_calendarDates), names(mvg_calendarDates))
calendarDates <- rbind.data.frame(mvv_calendarDates[, calendarDates_bothNames],
                           mvg_calendarDates[, calendarDates_bothNames])
rm("mvv_calendarDates", "mvg_calendarDates")
