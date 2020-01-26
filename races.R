#####################
## CREATE races.md ##
#####################

#############
## STARTUP ##
#############

## set working directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

## suppress warnings
options(warn = -1)

## load required packages
library(readxl)
library(lubridate)
library(dplyr)
library(stringr)
library(measurements)
library(tidyr)
library(knitr)

###############
## READ DATA ##
###############
dat <- read_xlsx("races.xlsx", sheet = "final", trim_ws = TRUE,
                 col_types = c("date", "text", "text", "text", "text", 
                               "text", "text", "text", "text", "text", 
                               "text", "text", "text"))
##################
## FUTURE TABLE ##
##################

## create future table by filtering to only future dates
dat_future <- dat %>% filter(Date > today()) %>% 
  
  ## drop unnedded fields
  select(c(Date, Race, `Race URL`, Distance, Location)) %>% 
  
  ## format links appropriately for markdown
  mutate(Race = paste0("[", `Race`, "](", `Race URL`, ")")) %>% 
  
  ## remove URL field
  select(-`Race URL`)

################
## PAST TABLE ##
################

## create two past tables
## - dat_past includes original time formatting
## - dat_past2 converts time formatting to hms objects
dat_past <- dat %>% filter(Date <= today())
dat_past2 <- dat_past %>% mutate_at(vars(matches("Results"), 
                                         -matches("URL")), hms, 
                                    quiet = TRUE)


## format past table, replacing NAs
dat_past  <- dat_past %>% replace_na(list(`Steve Bib Number` = "",
                                          `Steve Results` = "",
                                          `Elizabeth Bib Number` = "",
                                          `Elizabeth Results` = "")) %>%
  ## format links appropriately for markdown
  mutate(Race = ifelse(is.na(`Race URL`),
                       `Race`, 
                       paste0("[", `Race`, "](", 
                              `Race URL`, ")")), 
         `Steve Results` = 
           paste0(ifelse(is.na(`Steve Results URL`), 
                         `Steve Results`, 
                         paste0("[", 
                                `Steve Results`, 
                                "](", 
                                `Steve Results URL`, ")")),
                  ifelse(!is.na(`Steve Note`), 
                         paste0("<br />", `Steve Note`), "")
           ),
         `Elizabeth Results` =
           paste0(ifelse(is.na(`Elizabeth Results URL`), 
                         `Elizabeth Results`, 
                         paste0("[", 
                                `Elizabeth Results`, 
                                "](", 
                                `Elizabeth Results URL`, ")")),
                  ifelse(!is.na(`Elizabeth Note`), 
                         paste0("<br />", `Elizabeth Note`), "")
                  
           )) %>%
  ## drop unnedded fields  
  select(-c(`Race URL`, `Steve Results URL`, `Steve Note`, `Elizabeth Results URL`, `Elizabeth Note`))

###################
## SUMMARY TABLE ##
###################

## count races for both Steve and Elizabeth; not done in main summary 
## table because of desire to count "untimed" races
race_counts <- dat_past %>% group_by(Distance) %>% 
  summarize(`Races Completed (Steve)` = sum(!is.na(`Steve Results`)), 
            `Races Completed (Elizabeth)` = 
              sum(!is.na(`Elizabeth Results`)))

## pattern to extract from distance
pat <- "\\d+\\.*\\d*"

## build summary table
race_summary <- dat_past2 %>% mutate_at(vars(matches("Results"), 
                                            -matches("URL")), 
                                        as.duration) %>%
  ## convert all distances to numbers in the same units
    mutate(num_Distance = ifelse(str_detect(Distance, "3mi run") == TRUE,
                               60,
                               ifelse(str_detect(Distance, "swim") == 
                                        TRUE, 50,
                                      as.numeric(str_extract(Distance, 
                                                             pat)))),
                      si_Distance = ifelse(str_detect(Distance, "mi") == 
                                             TRUE, 
                               conv_unit(num_Distance, "mi", "km"), 
                               num_Distance)) %>%
  ## summarize by distance
  group_by(Distance) %>% 
  summarize(si_Distance = first(si_Distance),
              `Steve PR` = min(`Steve Results`, na.rm = TRUE), 
              `Steve Avg.` = mean(`Steve Results`, na.rm = TRUE),
              `Elizabeth PR` = min(`Elizabeth Results`, na.rm = TRUE), 
              `Elizabeth Avg.` = mean(`Elizabeth Results`, 
                                      na.rm = TRUE)) %>%

  ## replace Inf and NaN with 0
  mutate_all(~replace(., is.infinite(.), 0)) %>%
  mutate_all(~replace(., is.nan(.), 0)) %>%
  
  ## convert times to period objects
  mutate_at(c("Steve PR", "Steve Avg.", "Elizabeth PR", "Elizabeth Avg."),
            seconds_to_period) %>%
  
  ## format periods as times
  mutate_at(.vars = c("Steve PR", "Steve Avg.", "Elizabeth PR", 
                             "Elizabeth Avg."), 
             .funs = list(~ sprintf("%02d:%02d:%02d", as.integer(hour(.)), as.integer(minute(.)), 
                                 as.integer(second(.)))))
  
## combine summary and count tables
race_summary <- left_join(race_summary, race_counts) %>%
  
  ## sort by distance, drop si_Distance column, replace zeroes with blanks
  arrange(si_Distance) %>% select(Distance, `Races Completed (Steve)`,
                                  `Steve PR`, `Steve Avg.`, 
                                  `Races Completed (Elizabeth)`, 
                                  `Elizabeth PR`, `Elizabeth Avg.`) %>% 
  mutate_all(funs(recode(as.character(.), "00:00:00"="")))

###################
## EXPORT TO .MD ##
###################

## filename to export to
filename <- "races.md"

## write heading and summary table
cat(c("### Summary", ""), file = filename, sep = "\n")
kable(race_summary, "markdown") %>% cat(., file = filename, sep = "\n", append = TRUE)

## write heading and future table
cat(c("", "### Future", ""), file = filename, sep = "\n", append = TRUE)
kable(dat_future, "markdown") %>% cat(., file = filename, sep = "\n", append = TRUE)

## write heading and past table
cat(c("", "### Past", ""), file = filename, sep = "\n", append = TRUE)
kable(dat_past, "markdown") %>% cat(., file = filename, sep = "\n", append = TRUE)

###################
## RUN IFRAMEIFY ##
###################

source("iframeify.R")
iframeify("races")

## turn warnings back on
options(warn = 0)