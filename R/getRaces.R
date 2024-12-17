# Timing from skiresults must be done politely
# Timing must produce the output in a raw JSON style format, commonly transferable to csv, df etc.
# Timing must package an event with all information incl:
#     - Event details
#     - Race(s) details
#     - Racer details (with link to profile and SSE number / code if present)
# Timing may reproduce race details in a FIS standard exchange format.

library(polite)

library(rvest)
library(xml2)
library(dplyr)
library(tidyr)
library(magrittr)

skiresults_bow <- polite::bow(
  url = "https://www.skiresults.co.uk/",
  user_agent = "Justin Townsend, justinjtownsend@gmail.com", #identify ourselves
  force = TRUE
)

# All race tags on the page
# //*[contains(@*,"race-")]|//*[contains(@id,"race-")]

xpath_select <- "//*[contains(@*, 'race-')]|//*[contains(@id, 'race-')]"

get_races <- function(evt_num, bow = skiresults_bow, xpath_select) {
  
  # 1. Agree modification of session path with host.
  session <- polite::nod(
    bow = skiresults_bow,
    path = paste0("events/", evt_num)
  )
  
  # 2. Scrape the whole page from the altered URL
  scraped_page <- polite::scrape(session)
  
  node_result <- rvest::html_elements(
    scraped_page,
    xpath = xpath_select
  )
  
  # 3. Get race types, race numbers
  races_slct = "//ul[contains(@*, 'tabs')]"
  
  race_types <- rvest::html_elements(node_result,
                             xpath = races_slct
  ) %>% rvest::html_children() %>% rvest::html_text2()
  
  race_nums <- rvest::html_elements(node_result,
                                    xpath = races_slct
  ) %>% rvest::html_elements("a") %>% rvest::html_attr("href")
  
  race_dtls <- base::cbind(race_types, race_nums)
  
  # 4. Get races
  get_race <- function(race_num, html_resp) {
    
    race_num <- base::gsub('[#]', '', race_num)
    
    race_slct <- base::paste0(
      "//*[contains(@id, '",
      race_num,
      "')]")
    
    race_tbl <-
      rvest::html_elements(html_resp, xpath = race_slct) %>%
      rvest::html_table()
    
    return(race_tbl)
    
  }
  
  race_n <- race_dtls[1,2]
  r_chk <- get_race(race_n, node_result)
  
  
  # 4.1. Check for points column and handle
  
  # 5. Extract points to an additional data frame
  
  # 5.1. Is the header nested? If so, then extend into further columns (pivot_wider?)
  
  # Is the body nested? If so, then extend into further columns (pivot_wider?)
  
  # 6 Render result as a list of data frames
  # list name = event_num
  # data frames name = race_num
  # extra dataframes:
  # - racers = registered racers (regardless of race status)?
  # - individual points race_num + points name?
  
  return(node_result)

}

race <- get_races(1319, bow = skiresults_bow, xpath_select)

