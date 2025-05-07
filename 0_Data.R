#  OFFICE TENURE IN MEDIEVAL HUNGARY ------------------
#    R script by Davor Salihovic, University of Antwerp                                                 
#                      davor.salihovic@uantwerpen.be 

rm(list = ls())

library(dplyr); library(tidyr); library(lubridate); library(data.table)
library(survival); library(eha); library(survminer); library(bshazard)
library(ggplot2); library(brms); library(pastecs); library(car)

#### 0. DATA ----
engel <- read.csv("data/engel.csv")

castles <- as.data.frame(readxl::read_xlsx("data/castles.xlsx")) %>% 
  gather(year, castles, c("1387", "1397", "1407", "1417", "1427", "1437", "1458", "1490")) %>%
  mutate(year = as.numeric(year))

d <- as.data.frame(readxl::read_xlsx("data/officers.xlsx")) %>%
  mutate(
    status = ifelse(dead == 0, 1, 0),
    start = as.Date(start_date),
    stop = as.Date(stop_date)
  )

d <- d[!grepl("-01-01", d$start_date),]

d <- d[, c("id", "p_id", "name", "start", "stop", "office", "status", "dead")]
d <- d[!is.na(d$stop), ]
d <- d[!is.na(d$start), ]

king_dates <- c(
  "louis_II" = "1516-03-13",
  "vladislaus" = "1490-05-17",
  "matthias" = "1458-01-24",
  "posthumous" = "1453-01-29",
  "hunyadi" = "1446-06-06",
  "ladislaus" = "1440-07-17",
  "albert" = "1437-12-18",
  "sigismund" = "1387-03-31",
  "inter2" = "1382-09-11",
  "louis" = "1342-07-17",
  "charles" = "1308-11-28",
  "inter1" = "1301-01-01"
)

d <- d %>%
  mutate(king_appointed = case_when(
    start >= as.Date("1516-03-13") & start <= as.Date("1526-08-29") ~ "louis_II",
    start >= as.Date("1490-07-15") & start <= as.Date("1516-03-12") ~ "vladislaus",
    start >= as.Date("1458-01-24") & start <= as.Date("1490-04-06") ~ "matthias",
    start >= as.Date("1453-01-29") & start <= as.Date("1457-11-23") ~ "posthumous",
    start >= as.Date("1446-06-06") & start <= as.Date("1453-01-03") ~ "hunyadi",
    start >= as.Date("1440-07-17") & start <= as.Date("1444-11-10") ~ "ladislaus",
    start >= as.Date("1437-12-18") & start <= as.Date("1439-10-27") ~ "albert",
    start >= as.Date("1387-03-31") & start <= as.Date("1437-12-09") ~ "sigismund",
    start >= as.Date("1382-09-11") & start <= as.Date("1387-03-30") ~ "inter2",
    start >= as.Date("1342-07-17") & start <= as.Date("1382-09-10") ~ "louis",
    start >= as.Date("1308-11-28") & start <= as.Date("1342-07-16") ~ "charles",
    start >= as.Date("1301-01-01") & start <= as.Date("1308-11-27") ~ "inter1",
    TRUE ~ NA_character_
  ))

d <- d %>%
  mutate(king_dismissed = case_when(
    stop >= as.Date("1516-03-13") & stop <= as.Date("1526-08-29") ~ "louis_II",
    stop >= as.Date("1490-07-15") & stop <= as.Date("1516-03-12") ~ "vladislaus",
    stop >= as.Date("1458-01-24") & stop <= as.Date("1490-04-06") ~ "matthias",
    stop >= as.Date("1453-01-29") & stop <= as.Date("1457-11-23") ~ "posthumous",
    stop >= as.Date("1446-06-06") & stop <= as.Date("1453-01-03") ~ "hunyadi",
    stop >= as.Date("1440-07-17") & stop <= as.Date("1444-11-10") ~ "ladislaus",
    stop >= as.Date("1437-12-18") & stop <= as.Date("1439-10-27") ~ "albert",
    stop >= as.Date("1387-03-31") & stop <= as.Date("1437-12-09") ~ "sigismund",
    stop >= as.Date("1382-09-11") & stop <= as.Date("1387-03-30") ~ "inter2",
    stop >= as.Date("1342-07-17") & stop <= as.Date("1382-09-10") ~ "louis",
    stop >= as.Date("1308-11-28") & stop <= as.Date("1342-07-16") ~ "charles",
    stop >= as.Date("1301-01-01") & stop <= as.Date("1308-11-27") ~ "inter1",
    TRUE ~ NA_character_
  ))

d <- d %>%
  mutate(start_since_king = as.numeric(start - as.Date(king_dates[king_appointed])),
         stop_since_king = as.numeric(stop - as.Date(king_dates[king_appointed])),
         time_in_office = as.numeric(stop_since_king - start_since_king))


d1382 <- engel %>% group_by(Y1382) %>% summarise(n = n()) %>% mutate(year = 1382)
d1439 <- engel %>% group_by(Y1439) %>% summarise(n = n()) %>% mutate(year = 1439)
d1498 <- engel %>% group_by(Y1498) %>% summarise(n = n()) %>% mutate(year = 1498)

names(d1382) <- c("owner", "n", "year")
names(d1439) <- c("owner", "n", "year")
names(d1498) <- c("owner", "n", "year")

engel <- rbind(d1382, d1439, d1498)

d$year <- substr(d$start, 1, 4)

d <- d %>% mutate(
  baronial_estates = case_when(
    year <= 1382 ~ NA,
    year >= 1383 & year <= 1439 ~ 1151,
    year >= 1440 & year <= 1526 ~ 1186
  )
) %>%
  mutate(
    royal_estates = case_when(
      year <= 1382 ~ 870,
      year >= 1383 & year <= 1439 ~ 300,
      year >= 1440 & year <= 1526 ~ 168
    )
  )

d <- d %>% mutate(
  old_regime = as.factor(ifelse(year <= 1439, 1, 0)),
  royal_factor = as.factor(royal_estates)
)

setDT(d)
for (i in castles$family) {
  d[grepl(i, d$name, ignore.case = T), family := i]
}

castles <- castles %>%
  mutate(year_interval = case_when(
    year >= 1387 & year <= 1396 ~ 1387,
    year >= 1397 & year <= 1406 ~ 1397,
    year >= 1407 & year <= 1416 ~ 1407,
    year >= 1417 & year <= 1426 ~ 1417,
    year >= 1427 & year <= 1436 ~ 1427,
    year >= 1437 & year <= 1457 ~ 1437,
    year >= 1458 & year <= 1489 ~ 1458,
    year >= 1490 ~ 1490,
    TRUE ~ NA_real_
  )) %>%
  select(family, year_interval, castles)

d <- d %>%
  mutate(year_interval = case_when(
    year >= 1387 & year <= 1396 ~ 1387,
    year >= 1397 & year <= 1406 ~ 1397,
    year >= 1407 & year <= 1416 ~ 1407,
    year >= 1417 & year <= 1426 ~ 1417,
    year >= 1427 & year <= 1436 ~ 1427,
    year >= 1437 & year <= 1457 ~ 1437,
    year >= 1458 & year <= 1489 ~ 1458,
    year >= 1490 ~ 1490,
    TRUE ~ NA_real_
  ))

castles <- castles %>%
  distinct(family, year_interval, .keep_all = TRUE)

d <- d %>% 
  left_join(castles, by = c("family", "year_interval"))

d <- as.data.frame(d)

ddays <- d %>%
  rowwise() %>%
  mutate(date = list(seq.Date(start, stop, by = "day"))) %>%
  unnest(cols = c(date)) %>%
  mutate(
    status = case_when(
      date == stop & status == 1 ~ 1,
      TRUE ~ 0
    ),
    day_number = as.numeric(date - start + 1), # Add day number from start date
    id = id
  )

ddays <- ddays %>%
  mutate(
    king = case_when(
      date >= as.Date("1516-03-13") & date <= as.Date("1526-08-29") ~ "louis_II",
      date >= as.Date("1490-07-15") & date <= as.Date("1516-03-12") ~ "vladislaus",
      date >= as.Date("1458-01-24") & date <= as.Date("1490-04-06") ~ "matthias",
      date >= as.Date("1453-01-29") & date <= as.Date("1457-11-23") ~ "posthumous",
      date >= as.Date("1446-06-06") & date <= as.Date("1453-01-03") ~ "hunyadi",
      date >= as.Date("1440-07-17") & date <= as.Date("1444-11-10") ~ "ladislaus",
      date >= as.Date("1437-12-18") & date <= as.Date("1439-10-27") ~ "albert",
      date >= as.Date("1387-03-31") & date <= as.Date("1437-12-09") ~ "sigismund",
      date >= as.Date("1382-09-11") & date <= as.Date("1387-03-30") ~ "inter2",
      date >= as.Date("1342-07-17") & date <= as.Date("1382-09-10") ~ "louis",
      date >= as.Date("1308-11-28") & date <= as.Date("1342-07-16") ~ "charles",
      date >= as.Date("1301-01-01") & date <= as.Date("1308-11-27") ~ "inter1",
      TRUE ~ NA_character_
    )
  )

ddays <- ddays %>% mutate(
  king_date = as.Date(king_dates[king])
)

ddays <- ddays %>% 
  rowwise() %>% 
  mutate(
    change0 = ifelse(king_appointed == king, 0, 1)
  )

ddays <- ddays %>%
  arrange(id, date) %>% 
  group_by(id) %>%      
  mutate(
    time_diff = as.numeric(difftime(date, king_date, units = "days")),
    year_diff = time_diff >= 365,
    king_appointed = ifelse(year_diff & king != king_appointed,
                            king,
                            king_appointed)
  ) %>%
  ungroup() %>%
  select(-time_diff, -year_diff) 

ddays <- ddays %>%
  rowwise() %>%
  mutate(
    change = ifelse(king_appointed == king, 0, 1),
  )

ddays <- ddays %>%
  rowwise() %>%
  mutate(
    king_day_number = as.numeric((date - king_date) + 1)
  )

ddays <- ddays[ddays$old_regime == 0,]
ddays$rid <- paste(ddays$id, ddays$king)

 # Collapse this large data set into a more manageable framework
setDT(ddays)
setorder(ddays, rid, king_day_number)

ddays[, grp := rleid(rid, change0)]

collapsed <- ddays[, .(
  time0 = min(king_day_number),
  time1 = max(king_day_number),
  change = first(change0),
  status = max(status), 
  castles = mean(castles),
  p_id = first(p_id), 
  name = first(name),
  office = first(office)
), by = .(rid, grp)]

collapsed[, grp := NULL]

collapsed <- as.data.frame(collapsed)
collapsed <- collapsed[!is.na(collapsed$time0),]

 # Prepare data for analysis
d1 <- d[, c("id", "start_since_king", "stop_since_king",  "start", "stop", "status", "p_id", "name", "office", "king_dismissed", "castles")]
names(d1) <- c("id", "time0", "time1", "start", "stop", "status", "p_id", "name", "office", "king", "castles")
d1$year <- substr(d1$stop, 1, 4)
d1$angevine_regime <- ifelse(d1$year <= 1382, 1, 0)
d1$old_regime <- ifelse(d1$year <= 1439, 1, 0)
d1$tenure <- as.numeric(as.Date(d1$stop) - as.Date(d1$start))

d1 <- d1 %>%
  mutate(
    regime = case_when(
      year <= 1382 ~ "angevine",
      year > 1382 & year <= 1439 ~ "luxembourg",
      year > 1439 ~ "post-luxembourg"
    )
  )

d1$regime <- as.factor(d1$regime)

contrast1 <- c(-2, 1, 1)
contrast2 <- c(0, -1, 1)

contrasts(d1$regime) <- cbind(contrast1, contrast2)

d2 <- collapsed
d3 <- d1[d1$king == "matthias",]
old_aristocrats <- c("Michael Orszag Guti", "Nicholas Banfi Alsolendvai", "Nicholas Ujlaki",
                     "Stephen Batori", "Emeric Paloci", "John Rozgonyi", "George Turoci", 
                     "Lawrence Ujlaki", "Ladislaus Paloci", "Stephen Perenyi", "Matthias Maroti",
                     "Bartholomew Dragfi Belteki", "William Vitovec")
d3$aristocrat <- ifelse(d3$name %in% old_aristocrats, 1, 0)
d3$castles <- ifelse(is.na(d3$castles), 0, d3$castles)
d3$change <- d2$change[match(d3$p_id, d2$p_id)]
d3$change <- ifelse(is.na(d3$change), 0, d3$change)

d2 <- d2[complete.cases(d2),]
d3 <- d3[complete.cases(d3),]

#### Save data files:
rm(list = setdiff(ls(), c("d1", "d2", "d3")))
save.image("data/data.RData")
