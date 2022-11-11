# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Help Information ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

backColorStyle <- tags$style(".well {background-color:white;}")

help_time_input <- c(
    "Time range input is in a 24-hour format.", 
    "You can additionally select the time range in 15-min intervals.",
    "",
    "Note:",
    "Date selection of 2022-01-01 to 2022-01-03 and time selection of 07:00 to 07:30 displays aggregated speed for these half-hour intervals corresponding to three days."
)

help_bearing <- c(
    "Select one of East or West and one of North or South directions at a time (as in the default).",
    "Or, select 'None' if you want to view just one directional corridors on the map."
)

help_conf_score <- c(
    "Confidence level corresponds to the quality of segment-level data.",
    "",
    "High: real-time data",
    "Medium: combination of expected and real-time data",
    "Low: primarily historical data",
    "",
    "Data of low confidence level are more common during nighttime."
)

help_ffs <- HTML(
    "Free-flow speed corresponds to the 85th percentile of speeds during off-peak hours.",
    "Off-peak hours are defined as 9 am to 4 pm & 7 pm to 10 am on weekdays and 6 am to 10 pm on weekends.",
    "Off-peak hours from March and September of 2021 are used to compute segment-level free-flow speeds."
)

help_fft <- HTML(
    "Free-flow travel time corresponds to the 85th percentile of travel times during off-peak hours.",
    "Off-peak hours are defined as 9 am to 4 pm & 7 pm to 10 am on weekdays and 6 am to 10 pm on weekends.",
    "Off-peak hours from March and September of 2021 are used to compute segment-level free-flow travel times."
)

help_psl <- HTML("Data source: City of Phoenix Department of Transportation")

help_tti <- HTML(
    "Travel time index is the ratio of average travel time during peak period to free-flow travel time."
)

help_si <- HTML(
    "Speeding index is the ratio of average observed speed during a time interval to the posted speed limit."
)

help_pti <- HTML(
    "Planning time index is the ratio of 95th percentile of travel time during a period to the free-flow travel time.",
    "A PTI value of 1.5 means that for a 30 minute trip under the considered time interval, 45 minutes should be planned."
)