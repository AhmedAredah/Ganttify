#' @importFrom plotly plot_ly add_trace layout
#' @importFrom htmlwidgets onRender
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats setNames
NULL

# ============================================
# PLOTTING FUNCTION - STANDALONE
# ============================================

#' Create Interactive Gantt Chart with WBS Structure
#'
#' Creates a Primavera-style interactive Gantt chart with Work Breakdown Structure (WBS)
#' hierarchy and activities. The chart features color-coded WBS items, indented labels,
#' scrollable view for large projects, and dynamic date formatting.
#'
#' @param wbs_structure A data frame with 3 columns: ID (character), Name (character),
#'   and Parent (character). Parent should be "None" or "" for root level items.
#' @param activities A data frame with 5 required columns and 2 optional columns:
#'   \itemize{
#'     \item WBS_ID (character): Associated WBS item identifier
#'     \item Activity_ID (character): Unique activity identifier
#'     \item Activity_Name (character): Activity name
#'     \item Start_Date (character): Planned start date in MM/DD/YYYY format
#'     \item End_Date (character): Planned end date in MM/DD/YYYY format
#'     \item Start_Date_Actual (character, optional): Actual start date in MM/DD/YYYY format
#'     \item End_Date_Actual (character, optional): Actual end date in MM/DD/YYYY format.
#'       If Start_Date_Actual is provided but End_Date_Actual is missing, the actual bar
#'       will show from Start_Date_Actual to today (if today > Start_Date_Actual).
#'   }
#'   When actual dates are provided, activities display as stacked bars: planned on top
#'   (solid color) and actual on bottom (diagonal stripe pattern).
#' @param show_wbs_labels Logical. Show WBS item names on the y-axis. Default TRUE.
#' @param show_wbs_names_on_bars Logical. Show WBS names at the end of WBS bars. Default TRUE.
#' @param show_activity_names_on_bars Logical. Show activity names at the end of activity bars.
#'   Default FALSE.
#' @param show_today_line Logical. Display a vertical line marking today's date. Default TRUE.
#' @param dim_past_activities Logical. Reduce opacity of activities that end before today.
#'   Default FALSE.
#' @param dim_opacity Numeric. Opacity level for dimmed activities (0-1). Only used when
#'   dim_past_activities is TRUE. Default 0.3.
#' @param wbs_colors Named list. Custom colors for each WBS ID (e.g., list("W1" = "#FF6B6B")).
#'   If NULL, uses default color palette.
#' @param wbs_opacity Numeric. Opacity of WBS bars (0-1). Default 0.3.
#' @param wbs_bar_height Numeric. Height of WBS bars. Default 0.3.
#' @param activity_bar_height Numeric. Height of activity bars. Default 0.8.
#' @param chart_title Character. Title displayed at the top of the chart.
#'   Default "Project Gantt Chart with WBS".
#' @param buffer_days Numeric. Number of days to add before and after the project timeline
#'   for margin. Default 30.
#' @param indent_size Numeric. Number of spaces per indentation level in y-axis labels.
#'   Default 4.
#' @param x_range Character vector. Date range for x-axis zoom (e.g., c("2024-01-01", "2024-12-31")).
#'   If NULL, shows full project range.
#' @param max_visible_rows Numeric. Maximum number of rows visible at once (enables scrolling).
#'   Default 20.
#' @param y_scroll_position Numeric. Initial scroll position (bottom of visible range).
#'   If NULL, starts at the bottom.
#'
#' @return A plotly object containing the interactive Gantt chart. Can be displayed directly
#'   or saved using htmlwidgets::saveWidget().
#'
#' @examples
#' \donttest{
#' # Load test data
#' data(test_project)
#'
#' # Basic Gantt chart
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   wbs_colors = test_project$colors
#' )
#' chart
#'
#' # With dimmed past activities
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   wbs_colors = test_project$colors,
#'   dim_past_activities = TRUE,
#'   dim_opacity = 0.4
#' )
#' chart
#'
#' # With names on bars
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   wbs_colors = test_project$colors,
#'   show_wbs_names_on_bars = TRUE,
#'   show_activity_names_on_bars = TRUE
#' )
#' chart
#' }
#'
#' @export
Ganttify <- function(
    wbs_structure,
    activities,
    show_wbs_labels = TRUE,
    show_wbs_names_on_bars = TRUE,
    show_activity_names_on_bars = FALSE,
    show_today_line = TRUE,
    dim_past_activities = FALSE,  # PARAMETER: Dim activities that end before today
    dim_opacity = 0.3,  # PARAMETER: Opacity level for dimmed activities (default 0.3)
    wbs_colors = NULL,
    wbs_opacity = 0.3,
    wbs_bar_height = 0.3,
    activity_bar_height = 0.8,
    chart_title = "Project Gantt Chart with WBS",
    buffer_days = 30,
    indent_size = 4,
    x_range = NULL,
    max_visible_rows = 20,  # Maximum rows visible at once
    y_scroll_position = NULL  # Current scroll position (bottom of visible range)
) {
  
  # ============================================
  # 1. DATA VALIDATION AND PREPARATION
  # ============================================

  colnames(wbs_structure) <- c("ID", "Name", "Parent")

  # Check for actual date columns
  has_actual_dates <- ncol(activities) >= 7

  if (has_actual_dates) {
    colnames(activities) <- c("WBS_ID", "Activity_ID", "Activity_Name", "Start_Date", "End_Date",
                              "Start_Date_Actual", "End_Date_Actual")
  } else {
    colnames(activities) <- c("WBS_ID", "Activity_ID", "Activity_Name", "Start_Date", "End_Date")
  }

  # Parse planned dates
  activities$Start_Date <- as.Date(activities$Start_Date, format = "%m/%d/%Y")
  activities$End_Date <- as.Date(activities$End_Date, format = "%m/%d/%Y")

  if (any(is.na(activities$Start_Date)) || any(is.na(activities$End_Date))) {
    stop("Date parsing error. Please ensure planned dates are in MM/DD/YYYY format")
  }

  # Parse actual dates if present
  if (has_actual_dates) {
    activities$Start_Date_Actual <- as.Date(activities$Start_Date_Actual, format = "%m/%d/%Y")
    activities$End_Date_Actual <- as.Date(activities$End_Date_Actual, format = "%m/%d/%Y")

    # Handle missing End_Date_Actual: use today if after Start_Date_Actual
    today_date <- Sys.Date()
    for (i in 1:nrow(activities)) {
      if (!is.na(activities$Start_Date_Actual[i]) && is.na(activities$End_Date_Actual[i])) {
        if (today_date > activities$Start_Date_Actual[i]) {
          activities$End_Date_Actual[i] <- today_date
        } else {
          activities$End_Date_Actual[i] <- activities$Start_Date_Actual[i]
        }
      }
    }
  }
  
  # ============================================
  # 2. BUILD WBS HIERARCHY
  # ============================================
  
  wbs_structure$Level <- 0
  wbs_structure$Start_Date <- as.Date(NA)
  wbs_structure$End_Date <- as.Date(NA)
  
  calculate_level <- function(id, wbs_df) {
    parent <- wbs_df$Parent[wbs_df$ID == id]
    if (is.na(parent) || parent == "None" || parent == "") {
      return(0)
    } else {
      return(1 + calculate_level(parent, wbs_df))
    }
  }
  
  for (i in 1:nrow(wbs_structure)) {
    wbs_structure$Level[i] <- calculate_level(wbs_structure$ID[i], wbs_structure)
  }
  
  # ============================================
  # 3. CALCULATE WBS DATES FROM ACTIVITIES
  # ============================================
  
  get_all_children <- function(wbs_id, wbs_df) {
    children <- wbs_df$ID[wbs_df$Parent == wbs_id]
    if (length(children) == 0) {
      return(wbs_id)
    }
    all_descendants <- c(wbs_id)
    for (child in children) {
      all_descendants <- c(all_descendants, get_all_children(child, wbs_df))
    }
    return(all_descendants)
  }
  
  get_direct_children <- function(wbs_id, wbs_df) {
    return(wbs_df$ID[wbs_df$Parent == wbs_id])
  }
  
  for (i in nrow(wbs_structure):1) {
    wbs_id <- wbs_structure$ID[i]
    descendants <- get_all_children(wbs_id, wbs_structure)
    related_activities <- activities[activities$WBS_ID %in% descendants, ]
    
    if (nrow(related_activities) > 0) {
      # Collect all dates (planned and actual) for min/max calculation
      all_start_dates <- related_activities$Start_Date
      all_end_dates <- related_activities$End_Date

      # Include actual dates if they exist
      if ("Start_Date_Actual" %in% colnames(related_activities)) {
        all_start_dates <- c(all_start_dates, related_activities$Start_Date_Actual)
        all_end_dates <- c(all_end_dates, related_activities$End_Date_Actual)
      }

      # Calculate WBS span as earliest start to latest end (across both planned and actual)
      wbs_structure$Start_Date[i] <- min(all_start_dates, na.rm = TRUE)
      wbs_structure$End_Date[i] <- max(all_end_dates, na.rm = TRUE)
    }
  }
  
  # ============================================
  # 4. FILTER BY X-AXIS RANGE (ZOOM)
  # ============================================
  
  if (!is.null(x_range)) {
    visible_start <- as.Date(x_range[1])
    visible_end <- as.Date(x_range[2])
    
    activities <- activities %>%
      filter(.data$End_Date >= visible_start & .data$Start_Date <= visible_end)
    
    visible_wbs_ids <- unique(activities$WBS_ID)
    
    get_all_parents <- function(wbs_id, wbs_df) {
      parents <- c(wbs_id)
      current_id <- wbs_id
      while (TRUE) {
        parent <- wbs_df$Parent[wbs_df$ID == current_id]
        if (is.na(parent) || parent == "None" || parent == "") {
          break
        }
        parents <- c(parents, parent)
        current_id <- parent
      }
      return(parents)
    }
    
    all_visible_wbs <- unique(unlist(lapply(visible_wbs_ids, function(x) get_all_parents(x, wbs_structure))))
    
    wbs_structure <- wbs_structure %>%
      filter(.data$ID %in% all_visible_wbs)
  }
  
  # ============================================
  # 5. CREATE DISPLAY ORDER
  # ============================================
  
  traverse_tree <- function(wbs_id, wbs_df, activities_df) {
    result <- list()
    
    if (!(wbs_id %in% wbs_df$ID)) {
      return(result)
    }
    
    # Add WBS item
    result <- c(result, list(list(
      type = "WBS",
      id = wbs_id,
      name = wbs_df$Name[wbs_df$ID == wbs_id],
      level = wbs_df$Level[wbs_df$ID == wbs_id],
      start = wbs_df$Start_Date[wbs_df$ID == wbs_id],
      end = wbs_df$End_Date[wbs_df$ID == wbs_id]
    )))
    
    # Add activities for this WBS
    wbs_activities <- activities_df[activities_df$WBS_ID == wbs_id, ]
    if (nrow(wbs_activities) > 0) {
      for (j in 1:nrow(wbs_activities)) {
        activity_item <- list(
          type = "Activity",
          id = wbs_activities$Activity_ID[j],
          name = wbs_activities$Activity_Name[j],
          wbs_id = wbs_id,
          level = wbs_df$Level[wbs_df$ID == wbs_id] + 1,
          start = wbs_activities$Start_Date[j],
          end = wbs_activities$End_Date[j]
        )

        # Add actual dates if they exist
        if ("Start_Date_Actual" %in% colnames(wbs_activities)) {
          activity_item$start_actual <- wbs_activities$Start_Date_Actual[j]
          activity_item$end_actual <- wbs_activities$End_Date_Actual[j]
        }

        result <- c(result, list(activity_item))
      }
    }
    
    # Recursively add children WBS items
    children <- get_direct_children(wbs_id, wbs_df)
    for (child in children) {
      result <- c(result, traverse_tree(child, wbs_df, activities_df))
    }
    
    return(result)
  }
  
  roots <- wbs_structure$ID[is.na(wbs_structure$Parent) | 
                              wbs_structure$Parent == "None" | 
                              wbs_structure$Parent == ""]
  
  display_order <- c()
  for (root in roots) {
    display_order <- c(display_order, traverse_tree(root, wbs_structure, activities))
  }
  
  # ============================================
  # 6. PREPARE PLOT DATA WITH INDENTATION
  # ============================================
  
  plot_data <- data.frame(
    y_position = numeric(),
    y_label = character(),
    y_label_html = character(),  # HTML version with bold for WBS
    start = as.Date(character()),
    end = as.Date(character()),
    start_actual = as.Date(character()),
    end_actual = as.Date(character()),
    type = character(),
    level = numeric(),
    id = character(),
    wbs_id = character(),
    stringsAsFactors = FALSE
  )
  
  if (length(display_order) > 0) {
    y_pos <- length(display_order)
    for (item in display_order) {
      # Create indentation using non-breaking spaces
      indent <- paste(rep("\u00A0", item$level * indent_size), collapse = "")
      
      if (item$type == "WBS") {
        # WBS labels - just the name (will be bolded in HTML)
        label <- paste0(indent, item$name)
        label_html <- paste0(indent, "<b>", item$name, "</b>")
      } else {
        # Activity labels - with bullet symbol
        label <- paste0(indent, "\u2022 ", item$name)
        label_html <- paste0(indent, "\u2022 ", item$name)
      }
      
      plot_data <- rbind(plot_data, data.frame(
        y_position = y_pos,
        y_label = label,
        y_label_html = label_html,
        start = item$start,
        end = item$end,
        start_actual = if (!is.null(item$start_actual)) item$start_actual else as.Date(NA),
        end_actual = if (!is.null(item$end_actual)) item$end_actual else as.Date(NA),
        type = item$type,
        level = item$level,
        id = item$id,
        wbs_id = ifelse(item$type == "Activity", item$wbs_id, item$id),
        stringsAsFactors = FALSE
      ))
      y_pos <- y_pos - 1
    }
  }
  
  # ============================================
  # 7. DETERMINE Y-AXIS RANGE FOR SCROLLING
  # ============================================
  
  total_rows <- nrow(plot_data)
  
  if (is.null(y_scroll_position)) {
    y_range_min <- max(1, total_rows - max_visible_rows + 1) - 0.5
    y_range_max <- total_rows + 0.5
  } else {
    y_range_min <- y_scroll_position - 0.5
    y_range_max <- y_scroll_position + max_visible_rows - 0.5
  }
  
  # ============================================
  # 8. CALCULATE X-AXIS RANGE
  # ============================================
  
  if (nrow(plot_data) > 0 && any(!is.na(plot_data$start)) && any(!is.na(plot_data$end))) {
    overall_min <- min(plot_data$start, na.rm = TRUE)
    overall_max <- max(plot_data$end, na.rm = TRUE)
    plot_min_date <- overall_min - buffer_days
    plot_max_date <- overall_max + buffer_days
  } else {
    plot_min_date <- Sys.Date()
    plot_max_date <- Sys.Date() + 365
  }
  
  # ============================================
  # 9. ASSIGN COLORS TO WBS ITEMS
  # ============================================
  
  unique_wbs <- unique(plot_data$wbs_id)
  
  if (is.null(wbs_colors)) {
    default_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                         "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    wbs_colors <- setNames(
      rep(default_palette, length.out = length(unique_wbs)),
      unique_wbs
    )
  } else {
    wbs_colors <- as.list(wbs_colors)
  }
  
  # ============================================
  # 10. CREATE PLOTLY FIGURE WITH BARS
  # ============================================
  
  fig <- plot_ly()
  
  # Define today's date (10/22/2025 as specified by user)
  today_date <- as.Date("2025-10-22")
  
  # Storage for text annotations
  text_annotations <- list()
  
  if (nrow(plot_data) > 0) {
    wbs_data <- plot_data[plot_data$type == "WBS", ]
    activity_data <- plot_data[plot_data$type == "Activity", ]
    
    # Add WBS bars (thinner lines)
    if (nrow(wbs_data) > 0) {
      for (i in 1:nrow(wbs_data)) {
        wbs_id <- wbs_data$wbs_id[i]
        bar_color <- if (!is.null(wbs_colors) && wbs_id %in% names(wbs_colors)) {
          wbs_colors[[wbs_id]]
        } else {
          "#95A5A6"
        }
        
        # Add the bar line (without text)
        fig <- fig %>% add_trace(
          type = "scatter",
          mode = "lines",
          x = c(wbs_data$start[i], wbs_data$end[i]),
          y = c(wbs_data$y_position[i], wbs_data$y_position[i]),
          line = list(color = bar_color, width = 5),
          opacity = wbs_opacity,
          name = "WBS",
          showlegend = FALSE,
          hoverinfo = "text",
          hovertext = paste0(
            "<b>", gsub("\u00A0", "", wbs_data$y_label[i]), "</b><br>",
            "Type: WBS<br>",
            "Start: ", format(wbs_data$start[i], "%Y-%m-%d"), "<br>",
            "End: ", format(wbs_data$end[i], "%Y-%m-%d"), "<br>",
            "Duration: ", as.numeric(wbs_data$end[i] - wbs_data$start[i]) + 1, " days"
          )
        )
        
        # Add text annotation at the END of the bar if requested
        if (show_wbs_names_on_bars) {
          text_annotations <- c(text_annotations, list(list(
            x = wbs_data$end[i],
            y = wbs_data$y_position[i],
            text = gsub("\u00A0", "", wbs_data$y_label[i]),
            xanchor = "left",
            xshift = 5,
            showarrow = FALSE,
            font = list(size = 9, color = "black")
          )))
        }
      }
    }
    
    # Add activity bars (constant thickness lines or stacked planned/actual)
    if (nrow(activity_data) > 0) {
      for (i in 1:nrow(activity_data)) {
        wbs_id <- activity_data$wbs_id[i]
        bar_color <- if (!is.null(wbs_colors) && wbs_id %in% names(wbs_colors)) {
          wbs_colors[[wbs_id]]
        } else {
          "#3498DB"
        }

        # Determine if activity should be dimmed
        activity_opacity <- 1.0  # Default full opacity
        if (dim_past_activities && activity_data$end[i] < today_date) {
          activity_opacity <- dim_opacity  # Dim activities that end before today
        }

        # Check if actual dates exist for this activity
        has_actuals <- !is.na(activity_data$start_actual[i]) && !is.na(activity_data$end_actual[i])

        if (has_actuals) {
          # STACKED BARS: Planned (top) and Actual (bottom)

          # Calculate planned duration and variance
          planned_duration <- as.numeric(activity_data$end[i] - activity_data$start[i]) + 1
          actual_duration <- as.numeric(activity_data$end_actual[i] - activity_data$start_actual[i]) + 1
          variance_days <- actual_duration - planned_duration

          # Planned bar (upper half)
          fig <- fig %>% add_trace(
            type = "scatter",
            mode = "lines",
            x = c(activity_data$start[i], activity_data$end[i]),
            y = c(activity_data$y_position[i] + 0.2, activity_data$y_position[i] + 0.2),
            line = list(color = bar_color, width = 10),
            opacity = activity_opacity,
            name = "Planned",
            showlegend = FALSE,
            hoverinfo = "text",
            hovertext = paste0(
              "<b>", gsub("\u00A0", "", activity_data$y_label[i]), "</b><br>",
              "Type: Activity<br><br>",
              "<b>Planned:</b><br>",
              "Start: ", format(activity_data$start[i], "%Y-%m-%d"), "<br>",
              "End: ", format(activity_data$end[i], "%Y-%m-%d"), "<br>",
              "Duration: ", planned_duration, " days<br><br>",
              "<b>Actual:</b><br>",
              "Start: ", format(activity_data$start_actual[i], "%Y-%m-%d"), "<br>",
              "End: ", format(activity_data$end_actual[i], "%Y-%m-%d"), "<br>",
              "Duration: ", actual_duration, " days<br>",
              "Variance: ", ifelse(variance_days > 0, paste0("+", variance_days), variance_days), " days"
            )
          )

          # Actual bar (lower half) with diagonal stripe effect
          # Base actual bar
          fig <- fig %>% add_trace(
            type = "scatter",
            mode = "lines",
            x = c(activity_data$start_actual[i], activity_data$end_actual[i]),
            y = c(activity_data$y_position[i] - 0.2, activity_data$y_position[i] - 0.2),
            line = list(color = bar_color, width = 10),
            opacity = activity_opacity * 0.4,  # Lighter background
            name = "Actual",
            showlegend = FALSE,
            hoverinfo = "skip"
          )

          # Create diagonal stripe pattern using multiple thin lines
          num_stripes <- 8
          bar_duration <- as.numeric(activity_data$end_actual[i] - activity_data$start_actual[i])
          if (bar_duration > 0) {
            stripe_interval <- bar_duration / num_stripes
            for (s in 1:num_stripes) {
              stripe_x <- activity_data$start_actual[i] + (s - 1) * stripe_interval
              fig <- fig %>% add_trace(
                type = "scatter",
                mode = "lines",
                x = c(stripe_x, stripe_x),
                y = c(activity_data$y_position[i] - 0.35, activity_data$y_position[i] - 0.05),
                line = list(color = bar_color, width = 2),
                opacity = activity_opacity * 0.8,
                name = "Actual Stripe",
                showlegend = FALSE,
                hoverinfo = "skip"
              )
            }
          }

        } else {
          # SINGLE BAR: Only planned dates (original behavior)
          fig <- fig %>% add_trace(
            type = "scatter",
            mode = "lines",
            x = c(activity_data$start[i], activity_data$end[i]),
            y = c(activity_data$y_position[i], activity_data$y_position[i]),
            line = list(color = bar_color, width = 20),
            opacity = activity_opacity,
            name = "Activity",
            showlegend = FALSE,
            hoverinfo = "text",
            hovertext = paste0(
              "<b>", gsub("\u00A0", "", activity_data$y_label[i]), "</b><br>",
              "Type: Activity<br>",
              "Start: ", format(activity_data$start[i], "%Y-%m-%d"), "<br>",
              "End: ", format(activity_data$end[i], "%Y-%m-%d"), "<br>",
              "Duration: ", as.numeric(activity_data$end[i] - activity_data$start[i]) + 1, " days"
            )
          )
        }

        # Add text annotation at the END of the bar if requested
        if (show_activity_names_on_bars) {
          text_annotations <- c(text_annotations, list(list(
            x = if (has_actuals) max(activity_data$end[i], activity_data$end_actual[i]) else activity_data$end[i],
            y = activity_data$y_position[i],
            text = gsub("\u00A0", "", activity_data$y_label[i]),
            xanchor = "left",
            xshift = 5,
            showarrow = FALSE,
            font = list(size = 9, color = "black")
          )))
        }
      }
    }
  }
  
  # ============================================
  # 10A. ADD TODAY'S DATE LINE (OPTIONAL)
  # ============================================
  
  if (show_today_line) {
    # Only show the line if today falls within the plot range
    if (today_date >= plot_min_date && today_date <= plot_max_date) {
      fig <- fig %>% add_trace(
        type = "scatter",
        mode = "lines",
        x = c(today_date, today_date),
        y = c(y_range_min, y_range_max),
        line = list(
          color = "red",
          width = 2,
          dash = "dash"
        ),
        name = "Today",
        showlegend = TRUE,
        hoverinfo = "text",
        hovertext = paste0("<b>Today</b><br>", format(today_date, "%Y-%m-%d"))
      )
    }
  }
  
  # ============================================
  # 11. CONFIGURE LAYOUT WITH Y-AXIS SCROLLING
  # ============================================
  
  fig <- fig %>% layout(
    title = list(
      text = paste0(chart_title),
      font = list(size = 16)
    ),
    xaxis = list(
      title = "Timeline",
      type = "date",
      tickformat = "%Y-%m-%d",
      showgrid = FALSE,  # Disabled vertical grid
      range = c(plot_min_date, plot_max_date),
      fixedrange = FALSE
    ),
    yaxis = list(
      title = "",
      tickmode = "array",
      tickvals = if(nrow(plot_data) > 0) plot_data$y_position else c(),
      ticktext = if(nrow(plot_data) > 0) plot_data$y_label_html else c(),
      showgrid = TRUE,
      autorange = FALSE,
      range = c(y_range_min, y_range_max),
      tickfont = list(family = "Courier New, monospace", size = 11),
      side = "left",
      tickangle = 0,
      fixedrange = FALSE
    ),
    shapes = if (show_today_line) {
      list(
        list(
          type = "line",
          x0 = today_date,
          x1 = today_date,
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = "red", width = 2, dash = "dash")
        )
      )
    } else {
      list()
    },
    annotations = text_annotations,
    hovermode = "closest",
    plot_bgcolor = "white",  # Changed to white for better contrast with alternating backgrounds
    paper_bgcolor = "white",
    margin = list(l = 300, r = 50, t = 80, b = 80),
    dragmode = "pan"
  )
  
  # ============================================
  # 12. ADD LEFT-ALIGN CSS AND SCROLL SUPPORT
  # ============================================
  
  fig <- fig %>% onRender("
    function(el) {
      // Function to left-align y-axis tick labels
      function alignYAxisLabels() {
        var yAxisLabels = el.querySelectorAll('.yaxislayer-above text');
        yAxisLabels.forEach(function(label) {
          label.setAttribute('text-anchor', 'start');
          label.setAttribute('x', '0');
        });
      }
      
      // Function to update x-axis date format based on visible range
      function updateDateFormat() {
        if (!el.layout || !el.layout.xaxis || !el.layout.xaxis.range) {
          return;
        }
        
        var xRange = el.layout.xaxis.range;
        var startDate = new Date(xRange[0]);
        var endDate = new Date(xRange[1]);
        var daysDiff = (endDate - startDate) / (1000 * 60 * 60 * 24);
        
        var newFormat;
        var tickMode = 'auto';
        var tickVals = null;
        var tickText = null;
        var backgroundShapes = [];
        
        // Define alternating colors
        var color1 = 'rgba(240, 240, 240, 0.6)';  // Light gray
        var color2 = 'rgba(255, 255, 255, 0)';    // Transparent/white
        
        if (daysDiff > 730) {
          // More than 2 years: show year only, centered in each year
          newFormat = '%Y';
          tickMode = 'array';
          tickVals = [];
          tickText = [];
          
          var year = startDate.getFullYear();
          var endYear = endDate.getFullYear();
          
          var colorIndex = 0;
          for (var y = year; y <= endYear + 1; y++) {
            // Create background for each year
            var yearStart = new Date(y, 0, 1);
            var yearEnd = new Date(y + 1, 0, 1);
            
            backgroundShapes.push({
              type: 'rect',
              xref: 'x',
              yref: 'paper',
              x0: yearStart.toISOString().split('T')[0],
              x1: yearEnd.toISOString().split('T')[0],
              y0: 0,
              y1: 1,
              fillcolor: colorIndex % 2 === 0 ? color1 : color2,
              line: { width: 0 },
              layer: 'below'
            });
            
            // Position label at July 1st (middle of year)
            var midYear = new Date(y, 6, 1);
            if (midYear >= startDate && midYear <= endDate) {
              tickVals.push(midYear.toISOString().split('T')[0]);
              tickText.push(y.toString());
            }
            colorIndex++;
          }
        } else if (daysDiff > 365) {
          // More than 1 year: show month and year, centered in each month
          newFormat = '%b %Y';
          tickMode = 'array';
          tickVals = [];
          tickText = [];
          
          var current = new Date(startDate.getFullYear(), startDate.getMonth(), 1);
          var colorIndex = 0;
          
          while (current <= endDate) {
            var nextMonth = new Date(current.getFullYear(), current.getMonth() + 1, 1);
            
            backgroundShapes.push({
              type: 'rect',
              xref: 'x',
              yref: 'paper',
              x0: current.toISOString().split('T')[0],
              x1: nextMonth.toISOString().split('T')[0],
              y0: 0,
              y1: 1,
              fillcolor: colorIndex % 2 === 0 ? color1 : color2,
              line: { width: 0 },
              layer: 'below'
            });
            
            var midMonth = new Date(current.getFullYear(), current.getMonth(), 15);
            if (midMonth >= startDate && midMonth <= endDate) {
              tickVals.push(midMonth.toISOString().split('T')[0]);
              var monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
              tickText.push(monthNames[current.getMonth()] + ' ' + current.getFullYear());
            }
            
            current = nextMonth;
            colorIndex++;
          }
        } else if (daysDiff > 180) {
          // More than 6 months: show abbreviated month and year, centered
          newFormat = '%b %Y';
          tickMode = 'array';
          tickVals = [];
          tickText = [];
          
          var current = new Date(startDate.getFullYear(), startDate.getMonth(), 1);
          var colorIndex = 0;
          
          while (current <= endDate) {
            var nextMonth = new Date(current.getFullYear(), current.getMonth() + 1, 1);
            
            backgroundShapes.push({
              type: 'rect',
              xref: 'x',
              yref: 'paper',
              x0: current.toISOString().split('T')[0],
              x1: nextMonth.toISOString().split('T')[0],
              y0: 0,
              y1: 1,
              fillcolor: colorIndex % 2 === 0 ? color1 : color2,
              line: { width: 0 },
              layer: 'below'
            });
            
            var midMonth = new Date(current.getFullYear(), current.getMonth(), 15);
            if (midMonth >= startDate && midMonth <= endDate) {
              tickVals.push(midMonth.toISOString().split('T')[0]);
              var monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
              tickText.push(monthNames[current.getMonth()] + ' ' + current.getFullYear());
            }
            
            current = nextMonth;
            colorIndex++;
          }
        } else if (daysDiff > 60) {
          // More than 2 months: show month and day, centered in middle of month
          newFormat = '%b %d';
          tickMode = 'array';
          tickVals = [];
          tickText = [];
          
          var current = new Date(startDate.getFullYear(), startDate.getMonth(), 1);
          var colorIndex = 0;
          
          while (current <= endDate) {
            var nextMonth = new Date(current.getFullYear(), current.getMonth() + 1, 1);
            
            backgroundShapes.push({
              type: 'rect',
              xref: 'x',
              yref: 'paper',
              x0: current.toISOString().split('T')[0],
              x1: nextMonth.toISOString().split('T')[0],
              y0: 0,
              y1: 1,
              fillcolor: colorIndex % 2 === 0 ? color1 : color2,
              line: { width: 0 },
              layer: 'below'
            });
            
            var midMonth = new Date(current.getFullYear(), current.getMonth(), 15);
            if (midMonth >= startDate && midMonth <= endDate) {
              tickVals.push(midMonth.toISOString().split('T')[0]);
              var monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
              tickText.push(monthNames[current.getMonth()] + ' ' + current.getDate());
            }
            
            current = nextMonth;
            colorIndex++;
          }
        } else {
          // Less than 2 months: show full date with week-based backgrounds
          newFormat = '%Y-%m-%d';
          tickMode = 'auto';
          
          // Create weekly alternating backgrounds
          var currentWeekStart = new Date(startDate);
          currentWeekStart.setDate(currentWeekStart.getDate() - currentWeekStart.getDay()); // Start of week (Sunday)
          
          var colorIndex = 0;
          while (currentWeekStart <= endDate) {
            var weekEnd = new Date(currentWeekStart);
            weekEnd.setDate(weekEnd.getDate() + 7);
            
            backgroundShapes.push({
              type: 'rect',
              xref: 'x',
              yref: 'paper',
              x0: currentWeekStart.toISOString().split('T')[0],
              x1: weekEnd.toISOString().split('T')[0],
              y0: 0,
              y1: 1,
              fillcolor: colorIndex % 2 === 0 ? color1 : color2,
              line: { width: 0 },
              layer: 'below'
            });
            
            currentWeekStart = weekEnd;
            colorIndex++;
          }
        }
        
        // Preserve the today line shape if it exists
        var existingShapes = el.layout.shapes || [];
        var todayLineShape = existingShapes.find(function(shape) {
          return shape.line && shape.line.dash === 'dash' && shape.line.color === 'red';
        });
        
        if (todayLineShape) {
          backgroundShapes.push(todayLineShape);
        }
        
        // Build the update object
        var updateObj = {
          'xaxis.tickformat': newFormat, 
          'xaxis.tickmode': tickMode,
          'shapes': backgroundShapes
        };
        
        if (tickMode === 'array' && tickVals && tickVals.length > 0) {
          updateObj['xaxis.tickvals'] = tickVals;
          updateObj['xaxis.ticktext'] = tickText;
        }
        
        // Only update if something changed
        var needsUpdate = el.layout.xaxis.tickformat !== newFormat || 
                         el.layout.xaxis.tickmode !== tickMode;
        
        if (needsUpdate || true) {  // Always update to refresh backgrounds
          Plotly.relayout(el, updateObj);
        }
      }
      
      // Apply alignment on initial render
      setTimeout(alignYAxisLabels, 100);
      
      // Apply initial date format
      setTimeout(updateDateFormat, 150);
      
      // Re-apply alignment after every plot update (pan, zoom, etc.)
      el.on('plotly_afterplot', alignYAxisLabels);
      
      // Store the y-axis range to prevent zoom (but allow pan)
      var currentYRange = null;
      
      // Capture the initial y-axis range
      if (el.layout && el.layout.yaxis && el.layout.yaxis.range) {
        currentYRange = el.layout.yaxis.range.slice(); // Copy the range
      }
      
      // Intercept relayout events to prevent y-axis zoom
      el.on('plotly_relayout', function(eventData) {
        // Update date format when x-axis range changes
        if (eventData['xaxis.range[0]'] !== undefined || eventData['xaxis.range[1]'] !== undefined || eventData['xaxis.range'] !== undefined) {
          setTimeout(updateDateFormat, 50);
        }
        
        // Check if y-axis range is being changed
        if (eventData['yaxis.range[0]'] !== undefined || eventData['yaxis.range[1]'] !== undefined) {
          var newYRange = [
            eventData['yaxis.range[0]'] !== undefined ? eventData['yaxis.range[0]'] : el.layout.yaxis.range[0],
            eventData['yaxis.range[1]'] !== undefined ? eventData['yaxis.range[1]'] : el.layout.yaxis.range[1]
          ];
          
          // Calculate the range size
          var currentSize = currentYRange ? (currentYRange[1] - currentYRange[0]) : null;
          var newSize = newYRange[1] - newYRange[0];
          
          // If range size changed (zoom), restore the original size but allow shift (pan)
          if (currentSize && Math.abs(newSize - currentSize) > 0.01) {
            // This is a zoom operation - restore the range size
            var center = (newYRange[0] + newYRange[1]) / 2;
            var halfSize = currentSize / 2;
            Plotly.relayout(el, {
              'yaxis.range': [center - halfSize, center + halfSize]
            });
          } else {
            // This is a pan operation - update our stored range
            currentYRange = newYRange.slice();
          }
        }
        
        // Update stored range when x-axis changes (to handle any resets)
        if (eventData['yaxis.range'] !== undefined) {
          currentYRange = eventData['yaxis.range'].slice();
        }
      });
    }
  ")
  
  return(fig)
}
