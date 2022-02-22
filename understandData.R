files <- list.files("mvv", "*.txt", full.names = TRUE)
dl <- lapply(files, read.csv, header = TRUE)
names(dl) <- basename(files)
View(dl)



