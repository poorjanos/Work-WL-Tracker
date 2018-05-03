# Procedure for near-time tracking of workloads
# Writes log on local storage and outputs plots to visually track wl
# Script can be run 5 mins apart at minium (at best: 30 mins)

# Redirect stdout to logfile
scriptLog <- file("scriptLog", open = "wt")
sink(scriptLog, type = "message")

# Load required libs
library(config)
library(here)
library(ggplot2)
library(scales)
library(dplyr)
library(lubridate)
library(stringr)

# Quit if sysdate == weekend ------------------------------------------------------------
stopifnot(!(strftime(Sys.Date(), '%u') == 7 | hour(Sys.time()) >= 18))

# Create default dirs
dir.create(here::here("Reports"), showWarnings = FALSE)

##########################################################################################
# Extract Data ###########################################################################
##########################################################################################

# Set JAVA_HOME, set max. memory, and load rJava library
Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jre1.8.0_171")
options(java.parameters = "-Xmx2g")
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Load RJDBC library
library(RJDBC)

# Create connection driver and open connection
jdbcDriver <-
  JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = "C:\\Users\\PoorJ\\Desktop\\ojdbc7.jar")

# Get Kontakt credentials
kontakt <-
  config::get("kontakt",
              file = "C:\\Users\\PoorJ\\Projects\\config.yml")

# Open connection
jdbcConnection <-
  dbConnect(
    jdbcDriver,
    url = kontakt$server,
    user = kontakt$uid,
    password = kontakt$pwd
  )

# Get SQL scripts
readQuery <-
  function(file)
    paste(readLines(file, warn = FALSE), collapse = "\n")

WL_tracker <-
  readQuery(here::here("SQL", "WL_tracker.sql"))

# Run queries
snapshot_v3 <- dbGetQuery(jdbcConnection, WL_tracker)

# Close connection
dbDisconnect(jdbcConnection)


#########################################################################################
# Set Up Folder Struct to Save Results ##################################################
#########################################################################################
dirlist <-
  list.dirs(here::here("Reports"),
            full.names = FALSE,
            recursive = FALSE)

hits_dir <-
  unlist(sapply(dirlist, function(x)
    grepl(floor_date(Sys.Date(
      
    ), "day"), x)))

if (sum(hits_dir) == 0) {
  dir.create(here::here("Reports", floor_date(Sys.Date(), "day")))
}

# Init or append daily log on local storage
flist <-
  list.files(here::here("Reports", floor_date(Sys.Date(), "day")), ".csv")
ma <- floor_date(Sys.Date(), "day")

if (length(flist) == 0) {
  history <- snapshot_v3
} else {
  history <-
    read.csv(here::here(
      "Reports",
      floor_date(Sys.Date(), "day"),
      paste0("Snapshot_", ma, ".csv")
    ),
    stringsAsFactors = FALSE)
  
  snapshot_v3 <-
    snapshot_v3[!snapshot_v3$IDOPONT %in% unique(history$IDOPONT), ] #duplikált idõpontok kizárása
  history <- rbind(history, snapshot_v3)
}

write.csv(history,
          here::here(
            "Reports",
            floor_date(Sys.Date(), "day"),
            paste0("Snapshot_", ma, ".csv")
          ),
          row.names = FALSE)


#########################################################################################
# Transform Data ########################################################################
#########################################################################################
history$IDOPONT <- ymd_hms(history$IDOPONT)
history <-
  history[day(history$IDOPONT) == day(Sys.Date()), ] # exclude cases leaking from prev day
history$IDOPONT <- format(history$IDOPONT, "%H:%M")

history$PRIORITAS <- as.factor(history$PRIORITAS)
history$PRIORITAS <-
  factor(
    history$PRIORITAS,
    levels = c(
      "-1",
      "-2",
      "-3",
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9+",
      "KRITIKUS",
      "NAP: 9+",
      "NAP: 8",
      "NAP: 7",
      "NAP: 6",
      "NAP: 5",
      "NAP: 4",
      "NAP: 3",
      "NAP: 2",
      "NAP: 1",
      "NAP: 0"
    )
  ) #reorder factor

history[is.na(history$TIPUS) == TRUE, "TIPUS"] <-
  "Egyéb" #TIPUS = NA recode
history[is.na(history$TIPUS) == TRUE, "LEOSZTAS_OK"] <-
  "Egyéb" #LEOSZTAS_OK = NA recode
history[is.na(history$TEV) == TRUE, "TEV"] <-
  "Egyéb" #TEV = NA recode





history <-
  history %>% mutate(
    TEV = case_when(
      str_detect(.$TEV, "Függõ") ~ "Függõ díjkezelés",
      str_detect(.$TEV, "Banki") ~ "Banki betöltés",
      str_detect(.$TEV, "Igényfelmérõ") ~ "Igényfelmérõ",
      TRUE ~ TEV
    )
  ) %>% mutate(TIPUS = case_when(
    .$LEAN_TIP == 'AL' ~ paste('AL_', TIPUS),
    .$LEAN_TIP == 'IL' ~ paste('IL_', TEV)
  )) %>%
  group_by(TEV, LEAN_TIP, SAPI, IDOPONT, LEOSZTAS_OK, TIPUS, PRIORITAS) %>%
  summarise(DARAB = sum(DARAB)) %>%
  ungroup()

history$SAPI <-
  as.factor(history$SAPI) # recode to factor


#########################################################################################
# Plot WL ###############################################################################
#########################################################################################
for (i in levels(history[["SAPI"]])) {
  # Looping over sapis
  dirlist_sapi <-
    list.dirs(here::here("Reports", floor_date(Sys.Date(), "day")),
              full.names = FALSE,
              recursive = FALSE)
  
  hits_dir_sapi <-
    unlist(sapply(dirlist_sapi, function(x)
      grepl(i, x)))
  
  if (sum(hits_dir_sapi) == 0) {
    dir.create(here::here("Reports", floor_date(Sys.Date(), "day"), i),
               floor_date(Sys.Date(), "day"),
               i)
  }
  
  #Plot TIPUS
  history_tipus <- history[history[["SAPI"]] == i,]
  history_tipus <-
    group_by(history_tipus, SAPI, IDOPONT, TIPUS, PRIORITAS) %>%
    summarize(DARAB = sum(DARAB))
  history_tipus <- history_tipus[history_tipus[["DARAB"]] > 1, ]
  
  if (!nrow(history_tipus) == 0) {
    p1 <- ggplot(history_tipus, aes(x = IDOPONT, y = DARAB)) +
      geom_bar(stat = "identity") +
      facet_grid(PRIORITAS ~ TIPUS) +
      theme(strip.text.y = element_text(size = 8, angle = 0)) +
      theme(strip.text.x = element_text(size = 8, angle = 90)) +
      theme(axis.text.x = element_text(size = 8, angle = 90)) +
      theme(axis.text.y = element_text(size = 6)) +
      ggtitle(sprintf("%s gyûjtõ tartalma prioritások szerint", i))
  }
  
  #Plot: LEOSZTAS_OK
  history_lok <- history[history[["SAPI"]] == i,]
  history_lok <-
    group_by(history_lok, SAPI, IDOPONT, LEOSZTAS_OK, PRIORITAS) %>%
    summarize(DARAB = sum(DARAB))
  history_lok <- history_lok[history_lok[["DARAB"]] > 1, ]
  
  if (!nrow(history_lok) == 0) {
    p2 <- ggplot(history_lok, aes(x = IDOPONT, y = DARAB)) +
      geom_bar(stat = "identity") +
      facet_grid(PRIORITAS ~ LEOSZTAS_OK,  labeller = label_wrap_gen(width =
                                                                       0.1)) +
      theme(strip.text.y = element_text(size = 10, angle = 0)) +
      theme(axis.text.x = element_text(size = 8, angle = 90)) +
      theme(axis.text.y = element_text(size = 6)) +
      ggtitle(sprintf("%s gyûjtõ tartalma prioritások szerint", i))
  }
  
  i <- chartr("áéóõöûüíÁÉÓÕÖÛÜÍ", "aeooouuiAEOOOUUI", i)
  
  ggsave(
    here::here(
      "Reports",
      floor_date(Sys.Date(), "day"),
      i,
      paste0("TIPUS_", i, ".png")
    ),
    p1,
    width = 14,
    height = 7,
    dpi = 500
  )
  
  ggsave(
    "C:/Users/PoorJ/Desktop/Mischung/R/AFC_publish/Lean_snapshot.png",
    p1,
    width = 14,
    height = 7,
    dpi = 500
  )
  
  ggsave(
    here::here(
      "Reports",
      floor_date(Sys.Date(), "day"),
      i,
      paste0("LEOSZTAS_OK_", i, ".png")
    ),
    p2,
    width = 12,
    height = 7,
    dpi = 300
  )
}

# Redirect stdout back to console
sink(type = "message")
close(scriptLog)
