# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages ----------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(RODBC)
library(data.table)
library(sf)
library(shiny)
library(shinydashboard)
library(shinyTime)
library(shinyhelper)
library(leaflet)
library(plotly)

source("ignore/keys.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Segment Data Import ------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

query_seg <- "SELECT t1.SegmentID, t1.FRC, t2.Name, t1.Miles, t1.Bearing, t1.StartLong, t2.Longitude, t1.EndLong, t1.StartLat, t2.Latitude, t1.EndLat
FROM [ADOT_INRIX].[dbo].[InrixSegments_Geometry] AS t1
LEFT JOIN [ADOT_INRIX].[dbo].[InrixSegments] AS t2
ON t1.SegmentID = t2.ID
WHERE t1.District = 'Phoenix' AND t1.Bearing IN ('E','W','N','S')"

cols_fct <- c("FRC", "Bearing")
cols_num <- c("Miles", "StartLong", "Longitude", "EndLong", "StartLat", "Latitude", "EndLat")
seg <- as.data.table(sqlQuery(getSQLConnection("STL5"), query_seg, as.is = TRUE))
seg[, (cols_fct) := lapply(.SD, as.factor), .SDcols = cols_fct]
seg[, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num]
seg <- seg[!is.na(Longitude) & !is.na(Latitude), ]
seg <- seg[, CorrMiles := round(sum(Miles), 2), by = .(Name, Bearing)]

query_seg_PSL <- "SELECT * FROM [Pramesh].[dbo].[PHX_seg_PSL_join]"
query_seg_FF <- "SELECT * FROM [Pramesh].[dbo].[PHX_FFSTT_ArCoLo]"
query_PSL <- "SELECT * FROM [Pramesh].[dbo].[PHX_PSL]"

seg_PSL <- as.data.table(sqlQuery(getSQLConnection("STL4"), query_seg_PSL))[, .(SegmentID, SpeedLimit)]
seg_PSL[, SegmentID := as.character(SegmentID)]
seg_PSL_geo <- seg[seg_PSL, on = .(SegmentID)][!is.na(Bearing) & !is.na(SpeedLimit), ]

seg_FF <- as.data.table(sqlQuery(getSQLConnection("STL4"), query_seg_FF))[, .(SegmentID, speed_85, tt_85)]
seg_FF[, SegmentID := as.character(SegmentID)]
seg_FF_geo <- seg[seg_FF, on = .(SegmentID)][!is.na(Bearing) & tt_85 != 0, ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User-defined Variables -------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

v_frc5 <- c("Freeways", "Highways", "Arterials", "Collectors", "Local Streets")
v_frc3 <- c("Highways", "Arterials", "Collectors")
v_bearing <- c("East", "West", "North", "South")
v_days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
disc_PM <- c("0.00 - 0.25", "0.25 - 0.50", "0.50 - 0.75", "0.75 - 1.00", "1.00 - 1.25", "1.25 - 1.50", "1.50 - 1.75", "1.75 - 2.00", "> 2.00")

# Returns segment geometry
getSegmentGeometry <- function(x) {
    x$geometry <- sprintf(
        "LINESTRING(%s %s, %s %s, %s %s)", 
        x$StartLong, x$StartLat, 
        x$Longitude, x$Latitude, 
        x$EndLong, x$EndLat
    )
    x[, c("StartLong", "EndLong", "StartLat", "EndLat") := NULL]
    st_as_sf(x, crs = "+proj=longlat +datum=WGS84", wkt = "geometry")
}

# Returns rounded values for a given accuracy
getRound <- function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
}

# Returns timestamp entry for SQL query
getDT4Query <- function(input_dateStart, input_dateEnd, input_timeStart, input_timeEnd){
    seqDate <- seq(as.Date(input_dateStart), as.Date(input_dateEnd), by = 1)
    qStartTime <- format(input_timeStart, format = "%H:%M")
    qEndTime <- format(input_timeEnd, format = "%H:%M")
    carry_q <- ""
    for(i in 1:length(seqDate)){
        # s = start, e = end, q = query, DT = datetime
        sDT <- as.POSIXct(paste0(seqDate[i], qStartTime), format = "%Y-%m-%d %H:%M") + 7*3600
        eDT <- as.POSIXct(paste0(seqDate[i], qEndTime), format = "%Y-%m-%d %H:%M") + 7*3600
        qsDT <- paste0(carry_q, "timestamp BETWEEN ", "'", sDT, "'")
        qeDT <- paste0("'", eDT, "'")
        qDT <- paste0(qsDT, " AND ", qeDT)
        carry_q <- paste0(qDT, " OR ")
    }
    return(qDT)
}

# Returns DT with confidence score for plots
getScore4Plot <- function(x) {
    x[, score := fcase(score == "30", "High",
                       score == "20", "Medium",
                       score == "10", "Low",
                       !score %in% c("10", "20", "30"), "Unknown")]
    x[, score := factor(score, levels = c("High", "Medium", "Low", "Unknown"))]
}

# Returns DT with FRC for plots
getFRC4Plot <- function(x) {
    x[, FRC := fcase(FRC == "0", "Freeway",
                     FRC == "1", "Highway",
                     FRC == "2", "Arterial",
                     FRC == "3", "Collector",
                     FRC == "4", "Local Street",
                     !FRC %in% c("0", "1", "2", "3", "4"), "Unknown")]
    x[, FRC := factor(FRC, levels = c("Freeway", "Highway", "Arterial", "Collector", "Local Street", "Unknown"))]
}

# Returns DT with bearing for plots
getDirc4Plot <- function(x) {
    x[, Bearing := fcase(Bearing == "E", "East",
                         Bearing == "W", "West",
                         Bearing == "N", "North",
                         Bearing == "S", "South",
                         !Bearing %in% c("E", "W", "N", "S"), "Unknown")]
}

getSegPM4Plot <- function(x) {
    x[, d_SPM := as.factor(fcase(SPM >= 0.00 & SPM < 0.25, "0.00 - 0.25",
                                 SPM >= 0.25 & SPM < 0.50, "0.25 - 0.50",
                                 SPM >= 0.50 & SPM < 0.75, "0.50 - 0.75",
                                 SPM >= 0.75 & SPM < 1.00, "0.75 - 1.00",
                                 SPM >= 1.00 & SPM < 1.25, "1.00 - 1.25",
                                 SPM >= 1.25 & SPM < 1.50, "1.25 - 1.50",
                                 SPM >= 1.50 & SPM < 1.75, "1.50 - 1.75",
                                 SPM >= 1.75 & SPM < 2.00, "1.75 - 2.00",
                                 SPM >= 2, "> 2.00"))]
}

getCorPM4Plot <- function(x) {
    x[, d_CPM := as.factor(fcase(CPM >= 0.00 & CPM < 0.25, "0.00 - 0.25",
                                 CPM >= 0.25 & CPM < 0.50, "0.25 - 0.50",
                                 CPM >= 0.50 & CPM < 0.75, "0.50 - 0.75",
                                 CPM >= 0.75 & CPM < 1.00, "0.75 - 1.00",
                                 CPM >= 1.00 & CPM < 1.25, "1.00 - 1.25",
                                 CPM >= 1.25 & CPM < 1.50, "1.25 - 1.50",
                                 CPM >= 1.50 & CPM < 1.75, "1.50 - 1.75",
                                 CPM >= 1.75 & CPM < 2.00, "1.75 - 2.00",
                                 CPM >= 2, "> 2.00"))]
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shiny: UI ---------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("help.R")

header <- dashboardHeader(
    title = "Corridor Assessment Tool v1.1", 
    titleWidth = "400px"
)

sidebar <- dashboardSidebar(
    width = "250px",
    sidebarMenu(
        menuItem(
            "Speed Analysis",
            tabName = "Speed_Analysis",
            icon = icon("tachometer-alt"),
            startExpanded = TRUE
        ),
        menuItem(
            "Performance Measures",
            tabName = "Performance Measures",
            icon = icon("chart-line"),
            startExpanded = TRUE,
            collapsible = FALSE,
            menuSubItem(
                "Travel Time Index",
                tabName = "Travel_Time_Index",
                icon = icon("clock")
            ),
            menuSubItem(
                "Planning Time Index",
                tabName = "Planning_Time_Index",
                icon = icon("clock")
            ),
            menuSubItem(
                "Speeding Index",
                tabName = "Speeding_Index",
                icon = icon("gavel")
            )
        )
    )
)

map_height <- 700

## UI: Speed Analysis ----

tabItem_SA <- tabItem(
    tabName = "Speed_Analysis",
    fluidPage(
        column(
            4, 
            box(
                fluidRow(
                    column(
                        6,
                        dateInput(
                            "dateStart_OS", 
                            "Start Date",
                            value = as.POSIXct((Sys.Date() - 2), format = "%Y-%m-%d", tz = "UTC"),
                            min = as.POSIXct("2020-01-01", format = "%Y-%m-%d", tz = "UTC"),
                            max = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC")
                        ),
                        dateInput(
                            "dateEnd_OS", 
                            "End Date",
                            value = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC"),
                            min = as.POSIXct("2020-01-01", format = "%Y-%m-%d", tz = "UTC"),
                            max = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC")
                        )
                    ),
                    column(
                        6,
                        timeInput(
                            "timeStart_OS", 
                            "Start Time", 
                            value = as.POSIXct("16:00", format = "%H:%M", tz = "UTC"), 
                            seconds = FALSE, 
                            minute.steps = 15
                        ) |> 
                            helper(
                                icon = "question-circle",
                                type = "inline",
                                title = "Help",
                                content = help_time_input,
                                size = "m"
                            ),
                        timeInput(
                            "timeEnd_OS", 
                            "End Time", 
                            value = as.POSIXct("17:00", format = "%H:%M", tz = "UTC"), 
                            seconds = FALSE, 
                            minute.steps = 15
                        )
                    )
                ),
                fluidRow(
                    column(
                        6,
                        radioButtons(
                            "dirc1_OS",
                            "Select Direction E-W",
                            choiceNames = c("East", "West", "None"),
                            choiceValues = c("E", "W", NA),
                            selected = c("E"), 
                            inline = TRUE
                        )
                    ),
                    column(
                        6,
                        radioButtons(
                            "dirc2_OS",
                            "Select Direction N-S",
                            choiceNames = c("North", "South", "None"),
                            choiceValues = c("N", "S", NA),
                            selected = c("N"), 
                            inline = TRUE
                        ) |> 
                            helper(
                                icon = "question-circle",
                                type = "inline",
                                title = "Help",
                                content = help_bearing,
                                size = "m"
                            )
                    )
                ),
                checkboxGroupInput(
                    "frc_OS", 
                    "Select Road Type", 
                    choiceNames = v_frc5, 
                    choiceValues = c(0, 1, 2, 3, 4), 
                    selected = c(1, 2, 3), 
                    inline = TRUE
                ),
                checkboxGroupInput(
                    "score_OS", 
                    "Select Confidence Level",
                    choiceNames = c("High", "Medium", "Low"), 
                    choiceValues = c(30, 20, 10),
                    selected = c(30), 
                    inline = TRUE
                ) |> 
                    helper(
                        icon = "question-circle",
                        type = "inline",
                        title = "Help",
                        content = help_conf_score,
                        size = "m"
                    ),
                radioButtons(
                    "pm_OS", 
                    "Select Observed Speed Type",
                    choiceNames = c("Average speed", "Median speed", "85th percentile speed"),
                    choiceValues = c("sMean", "sMed", "s85"), 
                    selected = "sMean", 
                    inline = TRUE
                ),
                actionButton(
                    "update_SA", 
                    "Update Map", 
                    class = "btn-success"
                ),
                title = "Data Inputs",
                status = "success",
                width = 12,
                solidHeader = TRUE,
                collapsible = FALSE
            ),
            box(
                plotlyOutput(
                    "speedDist_OS",
                    height = 400
                ),
                title = "Observed Speed Boxplot",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "ffs85_dist_SA",
                    height = 400
                ),
                title = "Free-Flow Speed Boxplot",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "classLength_OS",
                    height = 400
                ),
                title = "Road Length",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "pieScore_OS",
                    height = 300
                ),
                title = "Data Confidence",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            )
        ),
        column(
            8,
            box(
                leafletOutput(
                    "speed_OS",
                    height = map_height
                ),
                title = "Observed Speed",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                leafletOutput(
                    "ffs85_SA",
                    height = map_height
                ) |> 
                    helper(
                        icon = "question-circle",
                        colour = "red",
                        type = "inline",
                        title = "Help",
                        content = help_ffs,
                        size = "m"
                    ),
                title = "Free-Flow Speed",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                leafletOutput(
                    "psl_SA",
                    height = map_height
                ) |> 
                    helper(
                        icon = "question-circle",
                        colour = "red",
                        type = "inline",
                        title = "Help",
                        content = help_psl,
                        size = "m"
                    ),
                title = "Posted Speed Limit",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            )
        )
    )
)

## UI: Performance Measure: TTI ----

tabItem_TTI <- tabItem(
    tabName = "Travel_Time_Index",
    fluidPage(
        column(
            4, 
            box(
                fluidRow(
                    column(
                        6,
                        dateInput(
                            "dateStart_TTI", 
                            "Start Date",
                            value = as.POSIXct((Sys.Date() - 2), format = "%Y-%m-%d", tz = "UTC"),
                            min = as.POSIXct("2020-01-01", format = "%Y-%m-%d", tz = "UTC"),
                            max = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC")
                        ),
                        dateInput(
                            "dateEnd_TTI", 
                            "End Date",
                            value = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC"),
                            min = as.POSIXct("2020-01-01", format = "%Y-%m-%d", tz = "UTC"),
                            max = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC")
                        )
                    ),
                    column(
                        6,
                        timeInput(
                            "timeStart_TTI", 
                            "Start Time", 
                            value = as.POSIXct("16:00", format = "%H:%M", tz = "UTC"), 
                            seconds = FALSE, 
                            minute.steps = 15
                        ) |> 
                            helper(
                                icon = "question-circle",
                                type = "inline",
                                title = "Help",
                                content = help_time_input,
                                size = "m"
                            ),
                        timeInput(
                            "timeEnd_TTI", 
                            "End Time", 
                            value = as.POSIXct("17:00", format = "%H:%M", tz = "UTC"), 
                            seconds = FALSE, 
                            minute.steps = 15
                        )
                    )
                ),
                fluidRow(
                    column(
                        6,
                        radioButtons(
                            "dirc1_TTI",
                            "Select Direction E-W",
                            choiceNames = c("East", "West", "None"),
                            choiceValues = c("E", "W", NA),
                            selected = c("E"), 
                            inline = TRUE
                        )
                    ),
                    column(
                        6,
                        radioButtons(
                            "dirc2_TTI",
                            "Select Direction N-S",
                            choiceNames = c("North", "South", "None"),
                            choiceValues = c("N", "S", NA),
                            selected = c("N"), 
                            inline = TRUE
                        ) |> 
                            helper(
                                icon = "question-circle",
                                type = "inline",
                                title = "Help",
                                content = help_bearing,
                                size = "m"
                            )
                    )
                ),
                checkboxGroupInput(
                    "frc_TTI", 
                    "Select Road Type", 
                    choiceNames = v_frc3, 
                    choiceValues = c(1, 2, 3), 
                    selected = c(1, 2, 3), 
                    inline = TRUE
                ),
                checkboxGroupInput(
                    "score_TTI", 
                    "Select Confidence Level",
                    choiceNames = c("High", "Medium", "Low"), 
                    choiceValues = c(30, 20, 10),
                    selected = c(30), 
                    inline = TRUE
                ) |> 
                    helper(
                        icon = "question-circle",
                        type = "inline",
                        title = "Help",
                        content = help_conf_score,
                        size = "m"
                    ),
                actionButton(
                    "update_TTI", 
                    "Update Map", 
                    class = "btn-success"
                ),
                title = "Data Inputs",
                status = "success",
                width = 12,
                solidHeader = TRUE,
                collapsible = FALSE
            ),
            box(
                tabBox(
                    side = "left",
                    selected = "Segment TTI Boxplot",
                    width = 12,
                    tabPanel(
                        "Segment TTI Boxplot",
                        plotlyOutput(
                            "segmentBP_TTI",
                            height = 400
                        )
                    ),
                    tabPanel(
                        "Corridor TTI Boxplot",
                        plotlyOutput(
                            "corridorBP_TTI",
                            height = 400
                        )
                    )
                ),
                title = "Travel Time Index Boxplot",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "classLength_TTI",
                    height = 400
                ),
                title = "Road Length",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "pieScore_TTI",
                    height = 300
                ),
                title = "Data Confidence",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            )
        ),
        column(
            8,
            box(
                leafletOutput(
                    "segment_TTI" ,
                    height = map_height
                ) |> 
                    helper(
                        icon = "question-circle",
                        colour = "red",
                        type = "inline",
                        title = "Help",
                        content = help_tti,
                        size = "m"
                    ),
                title = "Segment Travel Time Index",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                leafletOutput(
                    "corridor_TTI",
                    height = map_height
                ),
                title = "Corridor Travel Time Index",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "top_corridor_TTI",
                    height = 400
                ),
                title = "Travel Time Index Corridor Ranking",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            )
        )
    )
)

## UI: Performance Measure: PTI ----

tabItem_PTI <- tabItem(
    tabName = "Planning_Time_Index",
    fluidPage(
        column(
            4, 
            box(
                fluidRow(
                    column(
                        6,
                        dateInput(
                            "dateStart_PTI", 
                            "Start Date",
                            value = as.POSIXct((Sys.Date() - 2), format = "%Y-%m-%d", tz = "UTC"),
                            min = as.POSIXct("2020-01-01", format = "%Y-%m-%d", tz = "UTC"),
                            max = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC")
                        ),
                        dateInput(
                            "dateEnd_PTI", 
                            "End Date",
                            value = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC"),
                            min = as.POSIXct("2020-01-01", format = "%Y-%m-%d", tz = "UTC"),
                            max = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC")
                        )
                    ),
                    column(
                        6,
                        timeInput(
                            "timeStart_PTI", 
                            "Start Time", 
                            value = as.POSIXct("16:00", format = "%H:%M", tz = "UTC"), 
                            seconds = FALSE, 
                            minute.steps = 15
                        ) |> 
                            helper(
                                icon = "question-circle",
                                type = "inline",
                                title = "Help",
                                content = help_time_input,
                                size = "m"
                            ),
                        timeInput(
                            "timeEnd_PTI", 
                            "End Time", 
                            value = as.POSIXct("17:00", format = "%H:%M", tz = "UTC"), 
                            seconds = FALSE, 
                            minute.steps = 15
                        )
                    )
                ),
                fluidRow(
                    column(
                        6,
                        radioButtons(
                            "dirc1_PTI",
                            "Select Direction E-W",
                            choiceNames = c("East", "West", "None"),
                            choiceValues = c("E", "W", NA),
                            selected = c("E"), 
                            inline = TRUE
                        )
                    ),
                    column(
                        6,
                        radioButtons(
                            "dirc2_PTI",
                            "Select Direction N-S",
                            choiceNames = c("North", "South", "None"),
                            choiceValues = c("N", "S", NA),
                            selected = c("N"), 
                            inline = TRUE
                        ) |> 
                            helper(
                                icon = "question-circle",
                                type = "inline",
                                title = "Help",
                                content = help_bearing,
                                size = "m"
                            )
                    )
                ),
                checkboxGroupInput(
                    "frc_PTI", 
                    "Select Road Type", 
                    choiceNames = v_frc3, 
                    choiceValues = c(1, 2, 3), 
                    selected = c(1, 2, 3), 
                    inline = TRUE
                ),
                checkboxGroupInput(
                    "score_PTI", 
                    "Select Confidence Level",
                    choiceNames = c("High", "Medium", "Low"), 
                    choiceValues = c(30, 20, 10),
                    selected = c(30), 
                    inline = TRUE
                ) |> 
                    helper(
                        icon = "question-circle",
                        type = "inline",
                        title = "Help",
                        content = help_conf_score,
                        size = "m"
                    ),
                actionButton(
                    "update_PTI", 
                    "Update Map", 
                    class = "btn-success"
                ),
                title = "Data Inputs",
                status = "success",
                width = 12,
                solidHeader = TRUE,
                collapsible = FALSE
            ),
            box(
                tabBox(
                    side = "left",
                    selected = "Segment PTI Boxplot",
                    width = 12,
                    tabPanel(
                        "Segment PTI Boxplot",
                        plotlyOutput(
                            "segmentBP_PTI",
                            height = 400
                        )
                    ),
                    tabPanel(
                        "Corridor PTI Boxplot",
                        plotlyOutput(
                            "corridorBP_PTI",
                            height = 400
                        )
                    )
                ),
                title = "Planning Time Index Boxplot",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "classLength_PTI",
                    height = 400
                ),
                title = "Road Length",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "pieScore_PTI",
                    height = 300
                ),
                title = "Data Confidence",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            )
        ),
        column(
            8,
            box(
                leafletOutput(
                    "segment_PTI",
                    height = map_height
                ) |> 
                    helper(
                        icon = "question-circle",
                        colour = "red",
                        type = "inline",
                        title = "Help",
                        content = help_pti,
                        size = "m"
                    ),
                title = "Segment Planning Time Index",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                leafletOutput(
                    "corridor_PTI",
                    height = map_height
                ),
                title = "Corridor Planning Time Index",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "top_corridor_PTI",
                    height = 400
                ),
                title = "Planning Time Index Corridor Ranking",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            )
        )
    )
)

## UI: Performance Measure: SI ----

tabItem_SI <- tabItem(
    tabName = "Speeding_Index",
    fluidPage(
        column(
            4, 
            box(
                fluidRow(
                    column(
                        6,
                        dateInput(
                            "dateStart_SI", 
                            "Start Date",
                            value = as.POSIXct((Sys.Date() - 2), format = "%Y-%m-%d", tz = "UTC"),
                            min = as.POSIXct("2020-01-01", format = "%Y-%m-%d", tz = "UTC"),
                            max = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC")
                        ),
                        dateInput(
                            "dateEnd_SI", 
                            "End Date",
                            value = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC"),
                            min = as.POSIXct("2020-01-01", format = "%Y-%m-%d", tz = "UTC"),
                            max = as.POSIXct((Sys.Date() - 1), format = "%Y-%m-%d", tz = "UTC")
                        )
                    ),
                    column(
                        6,
                        timeInput(
                            "timeStart_SI", 
                            "Start Time", 
                            value = as.POSIXct("16:00", format = "%H:%M", tz = "UTC"), 
                            seconds = FALSE, 
                            minute.steps = 15
                        ) |> 
                            helper(
                                icon = "question-circle",
                                type = "inline",
                                title = "Help",
                                content = help_time_input,
                                size = "m"
                            ),
                        timeInput(
                            "timeEnd_SI", 
                            "End Time", 
                            value = as.POSIXct("17:00", format = "%H:%M", tz = "UTC"), 
                            seconds = FALSE, 
                            minute.steps = 15
                        )
                    )
                ),
                fluidRow(
                    column(
                        6,
                        radioButtons(
                            "dirc1_SI",
                            "Select Direction E-W",
                            choiceNames = c("East", "West", "None"),
                            choiceValues = c("E", "W", NA),
                            selected = c("E"), 
                            inline = TRUE
                        )
                    ),
                    column(
                        6,
                        radioButtons(
                            "dirc2_SI",
                            "Select Direction N-S",
                            choiceNames = c("North", "South", "None"),
                            choiceValues = c("N", "S", NA),
                            selected = c("N"), 
                            inline = TRUE
                        ) |> 
                            helper(
                                icon = "question-circle",
                                type = "inline",
                                title = "Help",
                                content = help_bearing,
                                size = "m"
                            )
                    )
                ),
                checkboxGroupInput(
                    "frc_SI", 
                    "Select Road Type", 
                    choiceNames = v_frc3, 
                    choiceValues = c(1, 2, 3), 
                    selected = c(1, 2, 3), 
                    inline = TRUE
                ),
                checkboxGroupInput(
                    "score_SI", 
                    "Select Confidence Level",
                    choiceNames = c("High", "Medium", "Low"), 
                    choiceValues = c(30, 20, 10),
                    selected = c(30), 
                    inline = TRUE
                ) |> 
                    helper(
                        icon = "question-circle",
                        type = "inline",
                        title = "Help",
                        content = help_conf_score,
                        size = "m"
                    ),
                actionButton(
                    "update_SI", 
                    "Update Map", 
                    class = "btn-success"
                ),
                title = "Data Inputs",
                status = "success",
                width = 12,
                solidHeader = TRUE,
                collapsible = FALSE
            ),
            box(
                tabBox(
                    side = "left",
                    selected = "Segment SI Boxplot",
                    width = 12,
                    tabPanel(
                        "Segment SI Boxplot",
                        plotlyOutput(
                            "segmentBP_SI",
                            height = 400
                        )
                    ),
                    tabPanel(
                        "Corridor SI Boxplot",
                        plotlyOutput(
                            "corridorBP_SI",
                            height = 400
                        )
                    )
                ),
                title = "Speeding Index Boxplot",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "classLength_SI",
                    height = 400
                ),
                title = "Road Length",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "pieScore_SI",
                    height = 300
                ),
                title = "Data Confidence",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            )
        ),
        column(
            8,
            box(
                leafletOutput(
                    "segment_SI",
                    height = map_height
                ) |> 
                    helper(
                        icon = "question-circle",
                        colour = "red",
                        type = "inline",
                        title = "Help",
                        content = help_si,
                        size = "m"
                    ),
                title = "Segment Speeding Index",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                leafletOutput(
                    "corridor_SI",
                    height = map_height
                ),
                title = "Corridor Speeding Index",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            ),
            box(
                plotlyOutput(
                    "top_corridor_SI",
                    height = 400
                ),
                title = "Speeding Index Corridor Ranking",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE
            )
        )
    )
)

body <- dashboardBody(
    tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),
    tags$head(tags$style(HTML("div.col-sm-12 {padding:5px; margin-top:-5px; margin-right:-30px;};"))),
    fluidRow(
        tabItems(
            tabItem_SA,
            tabItem_TTI,
            tabItem_PTI,
            tabItem_SI
        )
    )
)

ui <- dashboardPage(header, sidebar, body)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Shiny: Server -----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output, session) {
    
    observe({
        showModal(
            modalDialog(
                title = "Use Policy",
                "This Corridor Assessment Tool is under construction. Neither the University of Arizona nor the City of Phoenix Department of Transportation are liable for any of the contents contained herein. Use at your own risk.",
                easyClose = TRUE,
                fade = TRUE
            )
        )
    })
    
    observe_helpers(withMathJax = TRUE)
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Server: Speed Analysis ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    sqlData_SA <- eventReactive(input$update_SA, {
        
        dt_query <- getDT4Query(input$dateStart_OS, input$dateEnd_OS, input$timeStart_OS, input$timeEnd_OS)
        dirc_OS <- c(input$dirc1_OS, input$dirc2_OS)
        query <- paste0(
            "SELECT [timestamp],[SegmentID],[speed],[score] FROM [ADOT_INRIX].[dbo].[Inrix_Realtime] WHERE SegmentID IN (",
            "SELECT [SegmentID] FROM [ADOT_INRIX].[dbo].[InrixSegments_Geometry] WHERE District = 'Phoenix' AND FRC IN (",
            paste(sQuote(input$frc_OS, "'"), collapse = ","),
            ") AND Bearing IN (",
            paste(sQuote(dirc_OS, "'"), collapse = ","),
            ")) AND (",
            dt_query,
            ")"
        )
        
        sqlData <- as.data.table(sqlQuery(getSQLConnection("STL5"), query))
        sqlData[, SegmentID := as.character(SegmentID)]
        sqlData[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S") - 7 * 3600]
        
        score_pie <- sqlData[, score := as.factor(score)][, .N, by = score]
        score_pie <- getScore4Plot(score_pie)[order(score)]
        
        sqlData <- sqlData[SegmentID %in% seg$SegmentID & score %in% input$score_OS, ]
        
        if(input$pm_OS == "s85") {
            sqlData <- sqlData[, .(OS = round(quantile(speed, c(0.85)), 1)), by = SegmentID]
        } else if(input$pm_OS == "sMed") {
            sqlData <- sqlData[, .(OS = round(median(speed), 1)), by = SegmentID]
        } else {
            sqlData <- sqlData[, .(OS = round(mean(speed), 1)), by = SegmentID]
        }
        
        return(list(sqlData = sqlData, score_pie = score_pie, seg_sql = sqlData$SegmentID))
    })
    
    output$speed_OS <- renderLeaflet({
        
        mapData <- seg[sqlData_SA()$sqlData, on = .(SegmentID)]
        mapData <- getSegmentGeometry(mapData)
        
        Bins <- c(seq(getRound(min(mapData$OS), 10, floor), getRound(max(mapData$OS), 10, ceiling), 10))
        binpal <- colorBin(palette = "RdYlGn", domain = mapData$OS, bins = Bins)
        bincolor <- binpal(mapData$OS)
        
        leaflet() |> 
            addTiles(group = "OSM") |> 
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark CartoDB") |> 
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |> 
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |> 
            fitBounds(-112.196511, 33.681232, -111.944520, 33.377272) |>
            addPolylines(
                data = mapData,
                color = bincolor,
                opacity = 3,
                weight = 5,
                smoothFactor = 2,
                layerId = ~SegmentID
            ) |> 
            addCircleMarkers(
                data = mapData,
                lng = ~Longitude,
                lat = ~Latitude,
                popup = paste0(
                    as.character(mapData$OS), " mph bound ",
                    as.character(mapData$Bearing), " on ",
                    as.character(mapData$Name)
                ),
                group = "Markers",
                clusterOptions = markerClusterOptions()
            ) |> 
            addLegend(
                "topright",
                pal = binpal,
                values = mapData$OS,
                title = "Speed (mph)",
                opacity = 3
            ) |> 
            addLayersControl(
                baseGroups = c("Dark CartoDB", "CartoDB", "Toner", "OSM"),
                overlayGroups = c("Markers"),
                options = layersControlOptions(collapsed = TRUE, position = "bottomright")
            )
    })
    
    output$ffs85_SA <- renderLeaflet({
        mapData <- seg[seg_FF, on = .(SegmentID)][SegmentID %in% sqlData_SA()$seg_sql, ]
        mapData <- getSegmentGeometry(mapData)
        
        Bins <- c(seq(getRound(min(mapData$speed_85), 10, floor), getRound(max(mapData$speed_85), 10, ceiling), 10))
        binpal <- colorBin(palette = "RdYlGn", domain = mapData$speed_85, bins = Bins)
        bincolor <- binpal(mapData$speed_85)
        
        leaflet() |> 
            addTiles(group = "OSM") |> 
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark CartoDB") |> 
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |> 
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |> 
            fitBounds(-112.196511, 33.681232, -111.944520, 33.377272) |>
            addPolylines(
                data = mapData,
                color = bincolor,
                opacity = 3,
                weight = 5,
                smoothFactor = 2,
                layerId = ~SegmentID
            ) |> 
            addCircleMarkers(
                data = mapData,
                lng = ~Longitude,
                lat = ~Latitude,
                popup = paste0(
                    as.character(mapData$speed_85), " mph bound ",
                    as.character(mapData$Bearing), " on ",
                    as.character(mapData$Name)
                ),
                group = "Markers",
                clusterOptions = markerClusterOptions()
            ) |> 
            addLegend(
                "topright",
                pal = binpal,
                values = mapData$speed_85,
                title = "Speed (mph)",
                opacity = 3
            ) |> 
            addLayersControl(
                baseGroups = c("Dark CartoDB", "CartoDB", "Toner", "OSM"),
                overlayGroups = c("Markers"),
                options = layersControlOptions(collapsed = TRUE, position = "bottomright")
            )
    })
    
    output$psl_SA <- renderLeaflet({
        PSL <- as.data.table(sqlQuery(getSQLConnection("STL4"), query_PSL))
        
        lst <- lapply(PSL$geometry, function(x) {
            v <- eval(parse(text = x))
            m <- matrix(v, ncol = 2)
            st_linestring(m)
        })
        
        PSL$geometry <- lst
        PSL$geometry <- st_as_sfc(PSL$geometry)
        PSL <- st_as_sf(PSL, crs = "+proj=longlat +datum=WGS84")
        
        binpal <- colorFactor(palette = "RdYlGn", domain = PSL$SpeedLimit)
        bincolor <- binpal(PSL$SpeedLimit)
        
        leaflet() |> 
            addTiles(group = "OSM") |> 
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark CartoDB") |> 
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |> 
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |> 
            fitBounds(-112.196511, 33.681232, -111.944520, 33.377272) |>
            addPolylines(
                data = PSL,
                color = bincolor,
                opacity = 3,
                weight = 5,
                smoothFactor = 2,
                popup = paste0(
                    as.character(PSL$SpeedLimit), " mph on ",
                    as.character(PSL$LOCATION)
                ),
            ) |> 
            addLegend(
                "topright",
                pal = binpal,
                values = PSL$SpeedLimit,
                title = "Speed (mph)",
                opacity = 3
            ) |> 
            addLayersControl(
                baseGroups = c("Dark CartoDB", "CartoDB", "Toner", "OSM"),
                options = layersControlOptions(collapsed = TRUE, position = "bottomright")
            )
    })
    
    output$speedDist_OS <- renderPlotly({
        speedDist <- seg[sqlData_SA()$sqlData, on = .(SegmentID)]
        speedDist <- getFRC4Plot(speedDist)
        speedDist <- getDirc4Plot(speedDist)
        
        plot_ly(speedDist, type = "box", x = ~FRC, y = ~OS, color = ~Bearing, boxmean = TRUE) |> 
            layout(title = "Observed Speed by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Observed speed (mph)"),
                   boxmode = "group")
    })
    
    output$ffs85_dist_SA <- renderPlotly({
        speedDist85 <- seg[seg_FF, on = .(SegmentID)][SegmentID %in% sqlData_SA()$seg_sql, ]
        speedDist85 <- getFRC4Plot(speedDist85)
        speedDist85 <- getDirc4Plot(speedDist85)
        
        plot_ly(speedDist85, type = "box", x = ~FRC, y = ~speed_85, color = ~Bearing, boxmean = TRUE) |> 
            layout(title = "Free-Flow Speed by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Free-flow speed (mph)"),
                   boxmode = "group")
    })
    
    output$classLength_OS <- renderPlotly({
        classLength <- seg[sqlData_SA()$sqlData, on = .(SegmentID)][, .(Length = round(sum(Miles), 2)), by = c("FRC", "Bearing")]
        classLength <- getFRC4Plot(classLength)
        classLength <- getDirc4Plot(classLength)
        
        plot_ly(classLength, type = "bar", x = ~FRC, y = ~Length, color = ~Bearing) |> 
            layout(title = "Road Length by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Length (miles)"),
                   barmode = "group")
    })
    
    output$pieScore_OS <- renderPlotly({
        plot_ly(sqlData_SA()$score_pie, type = "pie", values = ~N, labels = ~score, rotation = 270) |> 
            layout(title = "Data Confidence Pie Chart",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Server: Performance Measures: TTI ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    sqlData_TTI <- eventReactive(input$update_TTI, {
        
        dt_query <- getDT4Query(input$dateStart_TTI, input$dateEnd_TTI, input$timeStart_TTI, input$timeEnd_TTI)
        dirc_TTI <- c(input$dirc1_TTI, input$dirc2_TTI)
        query <<- paste0(
            "SELECT [timestamp],[SegmentID],[travelTimeMinutes],[score] FROM [ADOT_INRIX].[dbo].[Inrix_Realtime] WHERE SegmentID IN (",
            "SELECT [SegmentID] FROM [ADOT_INRIX].[dbo].[InrixSegments_Geometry] WHERE District = 'Phoenix' AND FRC IN (",
            paste(sQuote(input$frc_TTI, "'"), collapse = ","),
            ") AND Bearing IN (",
            paste(sQuote(dirc_TTI, "'"), collapse = ","),
            ")) AND (",
            dt_query,
            ")"
        )
        
        sqlData <- as.data.table(sqlQuery(getSQLConnection("STL5"), query))
        sqlData[, SegmentID := as.character(SegmentID)]
        sqlData[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S") - 7 * 3600]
        
        score_pie <- sqlData[, score := as.factor(score)][, .N, by = score]
        score_pie <- getScore4Plot(score_pie)[order(score)]
        
        sqlData <- sqlData[SegmentID %in% seg$SegmentID & score %in% input$score_OS, ]
        sqlData <- sqlData[, .(tt_mean = mean(travelTimeMinutes)), by = .(SegmentID)]
        
        sqlData <- seg_FF_geo[sqlData, on = .(SegmentID), nomatch = NULL]
        sqlData[, SPM := round(tt_mean / tt_85, 2)]
        sqlData[, CPM := sum(round(SPM * Miles / CorrMiles, 2), na.rm = TRUE), by = .(Name, Bearing)]
        sqlData <- getSegPM4Plot(sqlData)
        sqlData[, d_SPM := factor(d_SPM, levels = disc_PM)]
        sqlData <- getFRC4Plot(sqlData)
        sqlData <- getDirc4Plot(sqlData)
        
        return(list(sqlData = sqlData, score_pie = score_pie, seg_sql = sqlData$SegmentID))
    })
    
    output$segment_TTI <- renderLeaflet({
        
        mapData <- sqlData_TTI()$sqlData
        mapData <- getSegmentGeometry(mapData)
        
        binpal <- colorFactor(palette = "RdYlGn", domain = mapData$d_SPM)
        bincolor <- binpal(mapData$d_SPM)
        
        leaflet() |>
            addTiles(group = "OSM") |>
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark CartoDB") |>
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |>
            fitBounds(-112.196511, 33.681232, -111.944520, 33.377272) |>
            addPolylines(
                data = mapData,
                color = bincolor,
                opacity = 3,
                weight = 5,
                smoothFactor = 2,
                layerId = ~SegmentID
            ) |>
            addCircleMarkers(
                data = mapData,
                lng = ~Longitude,
                lat = ~Latitude,
                popup = paste0(
                    as.character(mapData$SPM), " bound ",
                    as.character(mapData$Bearing), " on ",
                    as.character(mapData$Name)
                ),
                group = "Markers",
                clusterOptions = markerClusterOptions()
            ) |>
            addLegend(
                "topright",
                pal = binpal,
                values = mapData$d_SPM,
                title = "Travel Time Index",
                opacity = 3
            ) |>
            addLayersControl(
                baseGroups = c("Dark CartoDB", "CartoDB", "Toner", "OSM"),
                overlayGroups = c("Markers"),
                options = layersControlOptions(collapsed = TRUE, position = "bottomright")
            )
    })
    
    output$corridor_TTI <- renderLeaflet({
        
        mapData <- sqlData_TTI()$sqlData[CorrMiles >= 2 & nchar(Name) != 0, ]
        mapData <- getCorPM4Plot(mapData)
        mapData[, d_CPM := factor(d_CPM, levels = disc_PM)]
        mapData <- getSegmentGeometry(mapData)
        
        binpal <- colorFactor(palette = "RdYlGn", domain = mapData$d_CPM)
        bincolor <- binpal(mapData$d_CPM)
        
        leaflet() |>
            addTiles(group = "OSM") |>
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark CartoDB") |>
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |>
            fitBounds(-112.196511, 33.681232, -111.944520, 33.377272) |>
            addPolylines(
                data = mapData,
                color = bincolor,
                opacity = 3,
                weight = 5,
                smoothFactor = 2,
                layerId = ~SegmentID
            ) |>
            addCircleMarkers(
                data = mapData,
                lng = ~Longitude,
                lat = ~Latitude,
                popup = paste0(
                    as.character(mapData$CPM), " bound ",
                    as.character(mapData$Bearing), " on ",
                    as.character(mapData$Name)
                ),
                group = "Markers",
                clusterOptions = markerClusterOptions()
            ) |>
            addLegend(
                "topright",
                pal = binpal,
                values = mapData$d_CPM,
                title = "Travel Time Index",
                opacity = 3
            ) |>
            addLayersControl(
                baseGroups = c("Dark CartoDB", "CartoDB", "Toner", "OSM"),
                overlayGroups = c("Markers"),
                options = layersControlOptions(collapsed = TRUE, position = "bottomright")
            )
    })
    
    output$top_corridor_TTI <- renderPlotly({
        plotData <- sqlData_TTI()$sqlData[CorrMiles >= 2 & nchar(Name) != 0, .(Name, Bearing, CPM)]
        plotData <- unique(plotData)[order(CPM)] |> head(10)
        plot_ly(plotData, type = "bar", x = ~CPM, y = ~Name, orientation = "h") |> 
            layout(title = "Top 10 Corridors with Lowest TTI",
                   xaxis = list(title = "Travel Time Index"),
                   yaxis = list(title = "", categoryorder = "total descending"))
    })
    
    output$segmentBP_TTI <- renderPlotly({
        plot_ly(sqlData_TTI()$sqlData, type = "box", x = ~FRC, y = ~SPM, color = ~Bearing, boxmean = TRUE) |> 
            layout(title = "Segment TTI by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Travel Time Index"),
                   boxmode = "group")
    })
    
    output$corridorBP_TTI <- renderPlotly({
        plot_ly(sqlData_TTI()$sqlData, type = "box", x = ~FRC, y = ~CPM, color = ~Bearing, boxmean = TRUE) |>
            layout(title = "Corridor TTI by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Travel Time Index"),
                   boxmode = "group")
    })
    
    output$classLength_TTI <- renderPlotly({
        classLength <- sqlData_TTI()$sqlData[, .(Length = round(sum(Miles), 2)), by = c("FRC", "Bearing")]
        
        plot_ly(classLength, type = "bar", x = ~FRC, y = ~Length, color = ~Bearing) |>
            layout(title = "Road Length by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Length (miles)"),
                   barmode = "group")
    })
    
    output$pieScore_TTI <- renderPlotly({
        plot_ly(sqlData_TTI()$score_pie, type = "pie", values = ~N, labels = ~score, rotation = 270) |> 
            layout(title = "Data Confidence Pie Chart",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Server: Performance Measures: PTI ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    sqlData_PTI <- eventReactive(input$update_PTI, {
        
        dt_query <- getDT4Query(input$dateStart_PTI, input$dateEnd_PTI, input$timeStart_PTI, input$timeEnd_PTI)
        dirc_PTI <- c(input$dirc1_PTI, input$dirc2_PTI)
        query <<- paste0(
            "SELECT [timestamp],[SegmentID],[travelTimeMinutes],[score] FROM [ADOT_INRIX].[dbo].[Inrix_Realtime] WHERE SegmentID IN (",
            "SELECT [SegmentID] FROM [ADOT_INRIX].[dbo].[InrixSegments_Geometry] WHERE District = 'Phoenix' AND FRC IN (",
            paste(sQuote(input$frc_PTI, "'"), collapse = ","),
            ") AND Bearing IN (",
            paste(sQuote(dirc_PTI, "'"), collapse = ","),
            ")) AND (",
            dt_query,
            ")"
        )
        
        sqlData <- as.data.table(sqlQuery(getSQLConnection("STL5"), query))
        sqlData[, SegmentID := as.character(SegmentID)]
        sqlData[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S") - 7 * 3600]
        
        score_pie <- sqlData[, score := as.factor(score)][, .N, by = score]
        score_pie <- getScore4Plot(score_pie)[order(score)]
        
        sqlData <- sqlData[SegmentID %in% seg$SegmentID & score %in% input$score_OS, ]
        sqlData <- sqlData[, .(tt_p95 = quantile(travelTimeMinutes, probs = 0.95, na.rm = TRUE)), by = .(SegmentID)]
        
        sqlData <- seg_FF_geo[sqlData, on = .(SegmentID), nomatch = NULL]
        sqlData[, SPM := round(tt_p95 / tt_85, 2)]
        sqlData[, CPM := sum(round(SPM * Miles / CorrMiles, 2), na.rm = TRUE), by = .(Name, Bearing)]
        sqlData <- getSegPM4Plot(sqlData)
        sqlData[, d_SPM := factor(d_SPM, levels = disc_PM)]
        sqlData <- getFRC4Plot(sqlData)
        sqlData <- getDirc4Plot(sqlData)
        
        return(list(sqlData = sqlData, score_pie = score_pie, seg_sql = sqlData$SegmentID))
    })
    
    output$segment_PTI <- renderLeaflet({
        
        mapData <- sqlData_PTI()$sqlData
        mapData <- getSegmentGeometry(mapData)
        
        binpal <- colorFactor(palette = "RdYlGn", domain = mapData$d_SPM)
        bincolor <- binpal(mapData$d_SPM)
        
        leaflet() |>
            addTiles(group = "OSM") |>
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark CartoDB") |>
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |>
            fitBounds(-112.196511, 33.681232, -111.944520, 33.377272) |>
            addPolylines(
                data = mapData,
                color = bincolor,
                opacity = 3,
                weight = 5,
                smoothFactor = 2,
                layerId = ~SegmentID
            ) |>
            addCircleMarkers(
                data = mapData,
                lng = ~Longitude,
                lat = ~Latitude,
                popup = paste0(
                    as.character(mapData$SPM), " bound ",
                    as.character(mapData$Bearing), " on ",
                    as.character(mapData$Name)
                ),
                group = "Markers",
                clusterOptions = markerClusterOptions()
            ) |>
            addLegend(
                "topright",
                pal = binpal,
                values = mapData$d_SPM,
                title = "Planning Time Index",
                opacity = 3
            ) |>
            addLayersControl(
                baseGroups = c("Dark CartoDB", "CartoDB", "Toner", "OSM"),
                overlayGroups = c("Markers"),
                options = layersControlOptions(collapsed = TRUE, position = "bottomright")
            )
    })
    
    output$corridor_PTI <- renderLeaflet({
        
        mapData <- sqlData_PTI()$sqlData[CorrMiles >= 2 & nchar(Name) != 0, ]
        mapData <- getCorPM4Plot(mapData)
        mapData[, d_CPM := factor(d_CPM, levels = disc_PM)]
        mapData <- getSegmentGeometry(mapData)
        
        binpal <- colorFactor(palette = "RdYlGn", domain = mapData$d_CPM)
        bincolor <- binpal(mapData$d_CPM)
        
        leaflet() |>
            addTiles(group = "OSM") |>
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark CartoDB") |>
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |>
            fitBounds(-112.196511, 33.681232, -111.944520, 33.377272) |>
            addPolylines(
                data = mapData,
                color = bincolor,
                opacity = 3,
                weight = 5,
                smoothFactor = 2,
                layerId = ~SegmentID
            ) |>
            addCircleMarkers(
                data = mapData,
                lng = ~Longitude,
                lat = ~Latitude,
                popup = paste0(
                    as.character(mapData$CPM), " bound ",
                    as.character(mapData$Bearing), " on ",
                    as.character(mapData$Name)
                ),
                group = "Markers",
                clusterOptions = markerClusterOptions()
            ) |>
            addLegend(
                "topright",
                pal = binpal,
                values = mapData$d_CPM,
                title = "Planning Time Index",
                opacity = 3
            ) |>
            addLayersControl(
                baseGroups = c("Dark CartoDB", "CartoDB", "Toner", "OSM"),
                overlayGroups = c("Markers"),
                options = layersControlOptions(collapsed = TRUE, position = "bottomright")
            )
    })
    
    output$top_corridor_PTI <- renderPlotly({
        plotData <- sqlData_PTI()$sqlData[CorrMiles >= 2 & nchar(Name) != 0, .(Name, Bearing, CPM)]
        plotData <- unique(plotData)[order(CPM)] |> head(10)
        plot_ly(plotData, type = "bar", x = ~CPM, y = ~Name, orientation = "h") |> 
            layout(title = "Top 10 Corridors with Lowest PTI",
                   xaxis = list(title = "Planning Time Index"),
                   yaxis = list(title = "", categoryorder = "total descending"))
    })
    
    output$segmentBP_PTI <- renderPlotly({
        plot_ly(sqlData_PTI()$sqlData, type = "box", x = ~FRC, y = ~SPM, color = ~Bearing, boxmean = TRUE) |> 
            layout(title = "Segment PTI by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Planning Time Index"),
                   boxmode = "group")
    })
    
    output$corridorBP_PTI <- renderPlotly({
        plot_ly(sqlData_PTI()$sqlData, type = "box", x = ~FRC, y = ~CPM, color = ~Bearing, boxmean = TRUE) |>
            layout(title = "Corridor PTI by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Planning Time Index"),
                   boxmode = "group")
    })
    
    output$classLength_PTI <- renderPlotly({
        classLength <- sqlData_PTI()$sqlData[, .(Length = round(sum(Miles), 2)), by = c("FRC", "Bearing")]
        
        plot_ly(classLength, type = "bar", x = ~FRC, y = ~Length, color = ~Bearing) |>
            layout(title = "Road Length by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Length (miles)"),
                   barmode = "group")
    })
    
    output$pieScore_PTI <- renderPlotly({
        plot_ly(sqlData_PTI()$score_pie, type = "pie", values = ~N, labels = ~score, rotation = 270) |> 
            layout(title = "Data Confidence Pie Chart",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Server: Performance Measures: SI ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    sqlData_SI <- eventReactive(input$update_SI, {
        
        dt_query <- getDT4Query(input$dateStart_SI, input$dateEnd_SI, input$timeStart_SI, input$timeEnd_SI)
        dirc_SI <- c(input$dirc1_SI, input$dirc2_SI)
        query <<- paste0(
            "SELECT [timestamp],[SegmentID],[speed],[score] FROM [ADOT_INRIX].[dbo].[Inrix_Realtime] WHERE SegmentID IN (",
            "SELECT [SegmentID] FROM [ADOT_INRIX].[dbo].[InrixSegments_Geometry] WHERE District = 'Phoenix' AND FRC IN (",
            paste(sQuote(input$frc_SI, "'"), collapse = ","),
            ") AND Bearing IN (",
            paste(sQuote(dirc_SI, "'"), collapse = ","),
            ")) AND (",
            dt_query,
            ")"
        )
        
        sqlData <- as.data.table(sqlQuery(getSQLConnection("STL5"), query))
        sqlData[, SegmentID := as.character(SegmentID)]
        sqlData[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S") - 7 * 3600]
        
        score_pie <- sqlData[, score := as.factor(score)][, .N, by = score]
        score_pie <- getScore4Plot(score_pie)[order(score)]
        
        sqlData <- sqlData[SegmentID %in% seg$SegmentID & score %in% input$score_OS, ]
        sqlData <- sqlData[, .(speed_mean = mean(speed, na.rm = TRUE)), by = .(SegmentID)]
        
        sqlData <- seg_PSL_geo[sqlData, on = .(SegmentID), nomatch = NULL]
        sqlData[, SPM := round(speed_mean / SpeedLimit, 2)]
        sqlData[, CPM := sum(round(SPM * Miles / CorrMiles, 2), na.rm = TRUE), by = .(Name, Bearing)]
        sqlData <- getSegPM4Plot(sqlData)
        sqlData[, d_SPM := factor(d_SPM, levels = disc_PM)]
        sqlData <- getFRC4Plot(sqlData)
        sqlData <- getDirc4Plot(sqlData)
        
        return(list(sqlData = sqlData, score_pie = score_pie, seg_sql = sqlData$SegmentID))
    })
    
    output$segment_SI <- renderLeaflet({
        
        mapData <- sqlData_SI()$sqlData
        mapData <- getSegmentGeometry(mapData)
        
        binpal <- colorFactor(palette = "RdYlGn", domain = mapData$d_SPM)
        bincolor <- binpal(mapData$d_SPM)
        
        leaflet() |>
            addTiles(group = "OSM") |>
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark CartoDB") |>
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |>
            fitBounds(-112.196511, 33.681232, -111.944520, 33.377272) |>
            addPolylines(
                data = mapData,
                color = bincolor,
                opacity = 3,
                weight = 5,
                smoothFactor = 2,
                layerId = ~SegmentID
            ) |>
            addCircleMarkers(
                data = mapData,
                lng = ~Longitude,
                lat = ~Latitude,
                popup = paste0(
                    as.character(mapData$SPM), " bound ",
                    as.character(mapData$Bearing), " on ",
                    as.character(mapData$Name)
                ),
                group = "Markers",
                clusterOptions = markerClusterOptions()
            ) |>
            addLegend(
                "topright",
                pal = binpal,
                values = mapData$d_SPM,
                title = "Speeding Index",
                opacity = 3
            ) |>
            addLayersControl(
                baseGroups = c("Dark CartoDB", "CartoDB", "Toner", "OSM"),
                overlayGroups = c("Markers"),
                options = layersControlOptions(collapsed = TRUE, position = "bottomright")
            )
    })
    
    output$corridor_SI <- renderLeaflet({
        
        mapData <- sqlData_SI()$sqlData[CorrMiles >= 2 & nchar(Name) != 0, ]
        mapData <- getCorPM4Plot(mapData)
        mapData[, d_CPM := factor(d_CPM, levels = disc_PM)]
        mapData <- getSegmentGeometry(mapData)
        
        binpal <- colorFactor(palette = "RdYlGn", domain = mapData$d_CPM)
        bincolor <- binpal(mapData$d_CPM)
        
        leaflet() |>
            addTiles(group = "OSM") |>
            addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark CartoDB") |>
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") |>
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |>
            fitBounds(-112.196511, 33.681232, -111.944520, 33.377272) |>
            addPolylines(
                data = mapData,
                color = bincolor,
                opacity = 3,
                weight = 5,
                smoothFactor = 2,
                layerId = ~SegmentID
            ) |>
            addCircleMarkers(
                data = mapData,
                lng = ~Longitude,
                lat = ~Latitude,
                popup = paste0(
                    as.character(mapData$CPM), " bound ",
                    as.character(mapData$Bearing), " on ",
                    as.character(mapData$Name)
                ),
                group = "Markers",
                clusterOptions = markerClusterOptions()
            ) |>
            addLegend(
                "topright",
                pal = binpal,
                values = mapData$d_CPM,
                title = "Speeding Index",
                opacity = 3
            ) |>
            addLayersControl(
                baseGroups = c("Dark CartoDB", "CartoDB", "Toner", "OSM"),
                overlayGroups = c("Markers"),
                options = layersControlOptions(collapsed = TRUE, position = "bottomright")
            )
    })
    
    output$top_corridor_SI <- renderPlotly({
        plotData <- sqlData_SI()$sqlData[CorrMiles >= 2 & nchar(Name) != 0, .(Name, Bearing, CPM)]
        plotData <- unique(plotData)[order(-CPM)] |> head(10)
        plot_ly(plotData, type = "bar", x = ~CPM, y = ~Name, orientation = "h") |> 
            layout(title = "Top 10 Corridors with Highest SI",
                   xaxis = list(title = "Speeding Index"),
                   yaxis = list(title = "", categoryorder = "total ascending"))
    })
    
    output$segmentBP_SI <- renderPlotly({
        plot_ly(sqlData_SI()$sqlData, type = "box", x = ~FRC, y = ~SPM, color = ~Bearing, boxmean = TRUE) |> 
            layout(title = "Segment SI by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Speeding Index"),
                   boxmode = "group")
    })
    
    output$corridorBP_SI <- renderPlotly({
        plot_ly(sqlData_SI()$sqlData, type = "box", x = ~FRC, y = ~CPM, color = ~Bearing, boxmean = TRUE) |>
            layout(title = "Corridor SI by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Speeding Index"),
                   boxmode = "group")
    })
    
    output$classLength_SI <- renderPlotly({
        classLength <- sqlData_SI()$sqlData[, .(Length = round(sum(Miles), 2)), by = c("FRC", "Bearing")]
        
        plot_ly(classLength, type = "bar", x = ~FRC, y = ~Length, color = ~Bearing) |>
            layout(title = "Road Length by Class & Direction",
                   xaxis = list(title = "Functional class"),
                   yaxis = list(title = "Length (miles)"),
                   barmode = "group")
    })
    
    output$pieScore_SI <- renderPlotly({
        plot_ly(sqlData_SI()$score_pie, type = "pie", values = ~N, labels = ~score, rotation = 270) |> 
            layout(title = "Data Confidence Pie Chart",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
}

shinyApp(ui, server)