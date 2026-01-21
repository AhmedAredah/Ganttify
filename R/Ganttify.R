#' @importFrom plotly plot_ly add_trace layout
#' @importFrom htmlwidgets onRender
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom utils tail
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
#' @param activities A data frame with 5 required columns, 2 optional columns, and any number of additional columns:
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
#'     \item Additional columns (optional): Any extra columns (e.g., Status, Agency, Priority) are preserved
#'       and can be used for attribute-based coloring via color_config with mode="attribute".
#'   }
#'   When actual dates are provided, activities display as stacked bars: planned on top
#'   (solid color) and actual on bottom (diagonal stripe pattern).
#' @param chart_title Character. Title displayed at the top of the chart.
#'   Default "Project Gantt Chart with WBS".
#' @param x_range Character vector. Date range for x-axis zoom (e.g., c("2024-01-01", "2024-12-31")).
#'   If NULL, shows full project range.
#' @param milestone_lines Data frame or NULL. Optional milestone markers to display on the chart.
#'   Supports both single-date vertical lines and date-range shaded areas.
#'   If provided, must be a data frame with the following columns:
#'   \itemize{
#'     \item date (required): Either a single date (for vertical line) or a vector of 2 dates
#'       (for shaded area). Use a list column to mix both types. Dates can be character
#'       in MM/DD/YYYY format or Date objects.
#'     \item label (required): Text label to display on the milestone
#'     \item color (optional): Color for line or area (e.g., "red", "#FF0000"). Defaults to color palette.
#'     \item dash (optional): Line style for single-date milestones - "solid", "dash", "dot", or "dashdot". Default "dash".
#'     \item width (optional): Line width in pixels for single-date milestones. Default 2.
#'     \item fill_opacity (optional): Opacity for shaded areas (0-1). Default 0.15. Ignored for lines.
#'     \item label_position (optional): Label position - "top", "middle", or "bottom". Default "top".
#'   }
#'   Example with mixed types:
#'   \preformatted{
#'   milestones <- data.frame(
#'     label = c("Kickoff", "Review Period", "Deadline"),
#'     color = c("green", "blue", "red")
#'   )
#'   milestones$date <- list(
#'     "01/15/2025",
#'     c("03/01/2025", "03/31/2025"),
#'     "12/31/2025"
#'   )}
#'   Default NULL (no milestone markers).
#' @param color_config List or NULL. Configuration for chart colors. Structure depends on mode:
#'   \itemize{
#'     \item mode="wbs" (default if NULL): Activities inherit colors from parent WBS
#'       \preformatted{list(mode = "wbs", wbs = list("W1" = "#FF6B6B", "W2" = "#4ECDC4"))}
#'     \item mode="uniform": All activities same color, WBS same color
#'       \preformatted{list(mode = "uniform", uniform = list(wbs = "#34495E", activity = "#2ECC71"))}
#'     \item mode="attribute": Color activities by attribute column (e.g., Status)
#'       \preformatted{list(mode = "attribute",
#'            attribute = list(column = "Status",
#'                           mapping = list("completed" = "green", "in-progress" = "orange"),
#'                           wbs = "#34495E"))}
#'   }
#'   If NULL, uses mode="wbs" with default color palette. Default NULL.
#' @param display_config List or NULL. Controls visibility of chart elements. Structure:
#'   \itemize{
#'     \item wbs: List with show (logical), show_labels (logical), show_names_on_bars (logical)
#'     \item activity: List with show (logical), show_names_on_bars (logical)
#'   }
#'   Example: \preformatted{list(
#'     wbs = list(show = TRUE, show_labels = TRUE, show_names_on_bars = TRUE),
#'     activity = list(show = TRUE, show_names_on_bars = FALSE)
#'   )}
#'   If NULL, uses defaults shown above. Default NULL.
#' @param label_config List or NULL. Template strings for labels. Structure:
#'   \itemize{
#'     \item activity: List with yaxis (template for y-axis labels) and bar (template for bar labels)
#'     \item wbs: List with yaxis and bar templates
#'   }
#'   Available placeholders for activity: \code{name}, \code{id}, \code{start}, \code{end}, \code{start_actual}, \code{end_actual}, \code{duration}, \code{wbs_id} (use with curly braces)
#'   Available placeholders for wbs: \code{name}, \code{id}, \code{start}, \code{end}, \code{duration} (use with curly braces)
#'   Example: \preformatted{list(
#'     activity = list(yaxis = "{name} ({start} - {end})", bar = "{name}"),
#'     wbs = list(yaxis = "{name}", bar = "{name}")
#'   )}
#'   If NULL, uses default template for all labels. Default NULL.
#' @param bar_config List or NULL. Styling configuration for bars. Structure:
#'   \itemize{
#'     \item wbs: List with opacity (0-1) and height (numeric)
#'     \item activity: List with opacity (0-1), height (numeric), dim_opacity (0-1), and dim_past_activities (logical)
#'   }
#'   The dim_past_activities field controls whether activities that end before today are dimmed.
#'   When TRUE, completed activities use the dim_opacity value instead of the regular opacity.
#'   Note: Short-duration activities are automatically kept visible at any zoom level through
#'   dynamic bar width adjustment. The original dates are preserved in hover tooltips.
#'   Example: \preformatted{list(
#'     wbs = list(opacity = 0.3, height = 0.3),
#'     activity = list(opacity = 1.0, height = 0.8, dim_opacity = 0.3, dim_past_activities = FALSE)
#'   )}
#'   If NULL, uses defaults shown above. Default NULL.
#' @param layout_config List or NULL. Chart layout settings. Structure:
#'   \itemize{
#'     \item buffer_days: Numeric, days to add before/after timeline for margin
#'     \item indent_size: Numeric, spaces per indentation level
#'     \item max_visible_rows: Numeric, maximum visible rows (enables scrolling)
#'     \item y_scroll_position: Numeric or NULL, initial scroll position
#'     \item yaxis_label_width: Numeric, width of y-axis label area in pixels (default 300)
#'     \item yaxis_label_max_chars: Numeric or NULL, maximum characters for labels before truncating with "..." (NULL = no truncation)
#'     \item hover_popup_max_chars: Numeric, maximum characters per line in hover popups before wrapping to next line (default 50)
#'     \item show_yaxis_labels: Logical, whether to show y-axis labels (default TRUE).
#'       When FALSE, activity labels are hidden. If display_config$wbs$show_labels is TRUE,
#'       WBS labels will still be shown; otherwise all y-axis labels are hidden.
#'   }
#'   Example: \preformatted{list(
#'     buffer_days = 30,
#'     indent_size = 2,
#'     max_visible_rows = 20,
#'     y_scroll_position = NULL,
#'     yaxis_label_width = 300,
#'     yaxis_label_max_chars = NULL,
#'     hover_popup_max_chars = 50,
#'     show_yaxis_labels = TRUE
#'   )}
#'   If NULL, uses defaults shown above. Default NULL.
#' @param tooltip_config List or NULL. Configuration for custom tooltip fields. Structure:
#'   \itemize{
#'     \item wbs: Character vector of column names from wbs_structure to display in WBS tooltips
#'     \item activity: Character vector of column names from activities to display in activity tooltips
#'   }
#'   Fields that don't exist in the data or have NA/empty values will be automatically hidden.
#'   Example: \preformatted{list(
#'     wbs = c("Owner", "Budget"),
#'     activity = c("Status", "Agency", "Priority", "Notes")
#'   )}
#'   If NULL, only default fields (Type, Start, End, Duration) are shown. Default NULL.
#'
#' @return A plotly object containing the interactive Gantt chart. Can be displayed directly
#'   or saved using htmlwidgets::saveWidget().
#'
#' @examples
#' \donttest{
#' # Load test data
#' data(test_project)
#'
#' # Basic Gantt chart with WBS colors
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   color_config = list(mode = "wbs", wbs = test_project$colors)
#' )
#' chart
#'
#' # Uniform color mode
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   color_config = list(
#'     mode = "uniform",
#'     uniform = list(wbs = "#34495E", activity = "#2ECC71")
#'   )
#' )
#' chart
#'
#' # Attribute-based coloring (requires extra column in activities)
#' # Add a Status column to activities dataframe
#' activities_with_status <- test_project$activities
#' activities_with_status$Status <- sample(c("completed", "in-progress", "pending"),
#'                                         nrow(activities_with_status), replace = TRUE)
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = activities_with_status,
#'   color_config = list(
#'     mode = "attribute",
#'     attribute = list(
#'       column = "Status",
#'       mapping = list("completed" = "green", "in-progress" = "orange", "pending" = "gray"),
#'       wbs = "#34495E"
#'     )
#'   )
#' )
#' chart
#'
#' # WBS-only view using display_config
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   display_config = list(activity = list(show = FALSE))
#' )
#' chart
#'
#' # Custom labels showing date ranges
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   label_config = list(
#'     activity = list(yaxis = "{name} ({start} - {end})")
#'   )
#' )
#' chart
#'
#' # Customize bar heights and enable dimming for past activities
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   bar_config = list(
#'     wbs = list(opacity = 0.5, height = 0.4),
#'     activity = list(height = 0.9, dim_past_activities = TRUE, dim_opacity = 0.4)
#'   )
#' )
#' chart
#'
#' # Add "today" line as a milestone
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   milestone_lines = data.frame(
#'     date = Sys.Date(),
#'     label = "Today",
#'     color = "red"
#'   )
#' )
#' chart
#'
#' # Narrow label area with truncation
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   layout_config = list(
#'     yaxis_label_width = 200,
#'     yaxis_label_max_chars = 25
#'   )
#' )
#' chart
#'
#' # Custom tooltip fields (add extra columns to show in hover popups)
#' # First add custom columns to your data
#' activities_extended <- test_project$activities
#' activities_extended$Status <- sample(c("On Track", "Delayed", "Complete"),
#'                                       nrow(activities_extended), replace = TRUE)
#' activities_extended$Agency <- "TTI"
#'
#' wbs_extended <- test_project$wbs_structure
#' wbs_extended$Owner <- "Project Manager"
#'
#' chart <- Ganttify(
#'   wbs_structure = wbs_extended,
#'   activities = activities_extended,
#'   tooltip_config = list(
#'     wbs = c("Owner"),
#'     activity = c("Status", "Agency")
#'   )
#' )
#' chart
#' }
#'
#' @export
Ganttify <- function(
    wbs_structure,
    activities,
    chart_title = "Project Gantt Chart with WBS",
    x_range = NULL,
    milestone_lines = NULL,
    color_config = NULL,
    display_config = NULL,
    label_config = NULL,
    bar_config = NULL,
    layout_config = NULL,
    tooltip_config = NULL
) {
  
  # ============================================
  # 1. DATA VALIDATION AND PREPARATION
  # ============================================

  # Helper function to format labels using templates
  format_label <- function(template, data_list) {
    result <- template
    for (key in names(data_list)) {
      value <- data_list[[key]]
      # Handle NA values
      if (is.na(value)) {
        value <- ""
      } else if (inherits(value, "Date")) {
        value <- format(value, "%m/%d/%Y")
      } else {
        value <- as.character(value)
      }
      result <- gsub(paste0("\\{", key, "\\}"), value, result)
    }
    return(result)
  }

  # Helper function to truncate labels if they exceed max characters
  truncate_label <- function(label, max_chars, preserve_html = FALSE) {
    if (is.null(max_chars)) return(label)

    # Extract indent (leading spaces/nbsp)
    indent_match <- regexpr("^(\\s|\u00A0)+", label)
    if (indent_match > 0) {
      indent <- substring(label, 1, attr(indent_match, "match.length"))
      label_content <- substring(label, attr(indent_match, "match.length") + 1)
    } else {
      indent <- ""
      label_content <- label
    }

    # Remove HTML tags for character counting if present
    if (preserve_html) {
      # Extract text between HTML tags
      label_text <- gsub("<b>", "", label_content)
      label_text <- gsub("</b>", "", label_text)
    } else {
      label_text <- label_content
    }

    # Check if truncation needed
    if (nchar(label_text) > max_chars) {
      truncated_text <- substring(label_text, 1, max_chars - 3)
      if (preserve_html && grepl("<b>", label_content)) {
        # Preserve HTML tags
        return(paste0(indent, "<b>", truncated_text, "...</b>"))
      } else {
        return(paste0(indent, truncated_text, "..."))
      }
    }

    return(label)
  }

  # Helper function to generate intermediate points for hover coverage
  # Adapts point density based on activity duration
  generate_hover_points <- function(start_date, end_date) {
    duration <- as.numeric(end_date - start_date)

    if (duration == 0) {
      # Same day: just 2 points (start and end)
      return(c(start_date, start_date))
    } else if (duration <= 7) {
      # 1 week or less: daily points for smooth hover
      return(seq(start_date, end_date, by = 1))
    } else if (duration <= 90) {
      # 1-3 months: every 3 days
      points <- seq(start_date, end_date, by = 3)
      # Always include end date
      if (tail(points, 1) != end_date) points <- c(points, end_date)
      return(points)
    } else if (duration <= 365) {
      # 3-12 months: weekly points
      points <- seq(start_date, end_date, by = 7)
      if (tail(points, 1) != end_date) points <- c(points, end_date)
      return(points)
    } else {
      # > 1 year: bi-weekly points (14 days)
      points <- seq(start_date, end_date, by = 14)
      if (tail(points, 1) != end_date) points <- c(points, end_date)
      return(points)
    }
  }

  # Helper function to wrap text for hover popups
  wrap_text_for_hover <- function(text, max_chars) {
    # If no limit or text is short enough, return as-is
    if (is.null(max_chars) || nchar(text) <= max_chars) {
      return(text)
    }

    # Split text into words
    words <- strsplit(text, "\\s+")[[1]]
    lines <- character()
    current_line <- ""

    for (word in words) {
      # Test if adding this word would exceed the limit
      test_line <- if (current_line == "") word else paste(current_line, word)

      if (nchar(test_line) <= max_chars) {
        # Word fits on current line
        current_line <- test_line
      } else {
        # Word doesn't fit, start new line
        if (current_line != "") lines <- c(lines, current_line)
        current_line <- word

        # If single word is longer than max, truncate it
        if (nchar(word) > max_chars) {
          current_line <- substr(word, 1, max_chars)
        }
      }
    }

    # Add the last line
    if (current_line != "") lines <- c(lines, current_line)

    # Join lines with HTML line break
    return(paste(lines, collapse = "<br>"))
  }

  # Validate WBS structure columns
  required_wbs_cols <- c("ID", "Name", "Parent")
  if (!all(required_wbs_cols %in% colnames(wbs_structure))) {
    missing_cols <- setdiff(required_wbs_cols, colnames(wbs_structure))
    stop(paste0("WBS structure must have columns: ", paste(required_wbs_cols, collapse = ", "),
                ". Missing: ", paste(missing_cols, collapse = ", ")))
  }

  # Validate activities columns
  required_activity_cols <- c("WBS_ID", "Activity_ID", "Activity_Name", "Start_Date", "End_Date")
  if (!all(required_activity_cols %in% colnames(activities))) {
    missing_cols <- setdiff(required_activity_cols, colnames(activities))
    stop(paste0("Activities dataframe must have columns: ", paste(required_activity_cols, collapse = ", "),
                ". Missing: ", paste(missing_cols, collapse = ", ")))
  }

  # Check for actual date columns
  has_actual_dates <- all(c("Start_Date_Actual", "End_Date_Actual") %in% colnames(activities))

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
  # 1B. PROCESS MILESTONE LINES
  # ============================================

  milestone_data <- NULL
  if (!is.null(milestone_lines)) {
    # Validate that milestone_lines is a data frame
    if (!is.data.frame(milestone_lines)) {
      stop("milestone_lines must be a data frame")
    }

    # Check required columns
    if (!"date" %in% names(milestone_lines) || !"label" %in% names(milestone_lines)) {
      stop("milestone_lines must have 'date' and 'label' columns")
    }

    # Process each milestone row
    n_milestones <- nrow(milestone_lines)
    milestone_data <- data.frame(
      label = milestone_lines$label,
      milestone_type = character(n_milestones),
      date = as.Date(rep(NA, n_milestones)),
      start_date = as.Date(rep(NA, n_milestones)),
      end_date = as.Date(rep(NA, n_milestones)),
      stringsAsFactors = FALSE
    )

    # Handle date column - can be list, vector, or single values
    date_col <- milestone_lines$date

    for (i in 1:n_milestones) {
      # Get the date value for this row
      if (is.list(date_col)) {
        date_val <- date_col[[i]]
      } else {
        date_val <- date_col[i]
      }

      # Determine type based on length
      if (length(date_val) == 1) {
        # Single date - vertical line
        milestone_data$milestone_type[i] <- "line"
        if (is.character(date_val)) {
          milestone_data$date[i] <- as.Date(date_val, format = "%m/%d/%Y")
        } else {
          milestone_data$date[i] <- as.Date(date_val)
        }
      } else if (length(date_val) == 2) {
        # Two dates - shaded area
        milestone_data$milestone_type[i] <- "area"
        if (is.character(date_val)) {
          milestone_data$start_date[i] <- as.Date(date_val[1], format = "%m/%d/%Y")
          milestone_data$end_date[i] <- as.Date(date_val[2], format = "%m/%d/%Y")
        } else {
          milestone_data$start_date[i] <- as.Date(date_val[1])
          milestone_data$end_date[i] <- as.Date(date_val[2])
        }
      } else {
        stop(paste0("Invalid date format for milestone '", milestone_lines$label[i],
                    "'. Date must be a single value or a vector of 2 values."))
      }
    }

    # Check for invalid dates
    line_rows <- milestone_data$milestone_type == "line"
    area_rows <- milestone_data$milestone_type == "area"

    if (any(line_rows) && any(is.na(milestone_data$date[line_rows]))) {
      stop("Invalid dates in milestone_lines. Please use MM/DD/YYYY format or Date objects")
    }
    if (any(area_rows) && (any(is.na(milestone_data$start_date[area_rows])) ||
                           any(is.na(milestone_data$end_date[area_rows])))) {
      stop("Invalid date range in milestone_lines. Please use MM/DD/YYYY format or Date objects")
    }

    # Add default values for optional columns
    if (!"color" %in% names(milestone_lines)) {
      default_colors <- c("#8B4513", "#2E8B57", "#4682B4", "#9932CC", "#FF6347",
                         "#FFD700", "#00CED1", "#FF1493", "#32CD32", "#FF8C00")
      milestone_data$color <- rep(default_colors, length.out = n_milestones)
    } else {
      milestone_data$color <- milestone_lines$color
    }

    if (!"dash" %in% names(milestone_lines)) {
      milestone_data$dash <- "dash"
    } else {
      milestone_data$dash <- milestone_lines$dash
    }

    if (!"width" %in% names(milestone_lines)) {
      milestone_data$width <- 2
    } else {
      milestone_data$width <- milestone_lines$width
    }

    if (!"fill_opacity" %in% names(milestone_lines)) {
      # Default: 0.15 for areas, 1.0 for lines
      milestone_data$fill_opacity <- ifelse(milestone_data$milestone_type == "area", 0.15, 1.0)
    } else {
      milestone_data$fill_opacity <- milestone_lines$fill_opacity
    }

    if (!"label_position" %in% names(milestone_lines)) {
      milestone_data$label_position <- "top"
    } else {
      milestone_data$label_position <- milestone_lines$label_position
    }

    # Validate label_position values
    valid_positions <- c("top", "middle", "bottom")
    invalid_positions <- !milestone_data$label_position %in% valid_positions
    if (any(invalid_positions)) {
      milestone_data$label_position[invalid_positions] <- "top"
      warning("Invalid label_position values found. Using 'top' as default. Valid values: 'top', 'middle', 'bottom'")
    }
  }

  # ============================================
  # 1C. PARSE AND VALIDATE COLOR CONFIG
  # ============================================

  # Set default if NULL
  if (is.null(color_config)) {
    color_config <- list(mode = "wbs")
  }

  # Validate mode
  valid_modes <- c("wbs", "uniform", "attribute")
  if (!"mode" %in% names(color_config)) {
    stop("color_config must have a 'mode' field")
  }
  if (!color_config$mode %in% valid_modes) {
    stop("color_config$mode must be one of: 'wbs', 'uniform', or 'attribute'")
  }

  # Extract and validate based on mode
  activity_color_mode <- color_config$mode

  if (activity_color_mode == "wbs") {
    # WBS mode
    if (is.null(color_config$wbs)) {
      # Use default palette
      unique_wbs <- unique(wbs_structure$ID)
      default_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                          "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
      wbs_colors <- setNames(
        rep(default_palette, length.out = length(unique_wbs)),
        unique_wbs
      )
    } else {
      wbs_colors <- as.list(color_config$wbs)
    }
    uniform_activity_color <- NULL
    uniform_wbs_color <- NULL
    activity_color_column <- NULL
    activity_color_mapping <- NULL

  } else if (activity_color_mode == "uniform") {
    # Uniform mode
    if (is.null(color_config$uniform)) {
      stop("color_config$uniform must be provided when mode='uniform'")
    }
    if (is.null(color_config$uniform$wbs)) {
      uniform_wbs_color <- "#34495E"  # default dark gray
    } else {
      uniform_wbs_color <- color_config$uniform$wbs
    }
    if (is.null(color_config$uniform$activity)) {
      uniform_activity_color <- "#2ECC71"  # default green
    } else {
      uniform_activity_color <- color_config$uniform$activity
    }
    wbs_colors <- NULL
    activity_color_column <- NULL
    activity_color_mapping <- NULL

  } else if (activity_color_mode == "attribute") {
    # Attribute mode
    if (is.null(color_config$attribute)) {
      stop("color_config$attribute must be provided when mode='attribute'")
    }
    if (is.null(color_config$attribute$column)) {
      stop("color_config$attribute$column must be specified")
    }

    activity_color_column <- color_config$attribute$column

    # Check if column exists
    if (!activity_color_column %in% colnames(activities)) {
      stop(paste0("Column '", activity_color_column, "' not found in activities dataframe"))
    }

    # Prepare color mapping
    if (is.null(color_config$attribute$mapping)) {
      # Use default palette
      unique_values <- unique(activities[[activity_color_column]])
      default_palette <- c("#27AE60", "#F39C12", "#E74C3C", "#3498DB", "#9B59B6",
                          "#1ABC9C", "#E67E22", "#95A5A6", "#34495E", "#16A085")
      activity_color_mapping <- setNames(
        rep(default_palette, length.out = length(unique_values)),
        unique_values
      )
      warning(paste0("No mapping provided in color_config$attribute$mapping. Using default colors for '",
                    activity_color_column, "'"))
    } else {
      activity_color_mapping <- as.list(color_config$attribute$mapping)
    }

    # WBS color for attribute mode
    if (is.null(color_config$attribute$wbs)) {
      uniform_wbs_color <- "#34495E"  # default dark gray
    } else {
      uniform_wbs_color <- color_config$attribute$wbs
    }

    wbs_colors <- NULL
    uniform_activity_color <- NULL
  }

  # ============================================
  # 1D. PARSE AND VALIDATE DISPLAY CONFIG
  # ============================================

  # Set defaults if NULL
  if (is.null(display_config)) {
    display_config <- list(
      wbs = list(show = TRUE, show_labels = TRUE, show_names_on_bars = TRUE),
      activity = list(show = TRUE, show_names_on_bars = FALSE)
    )
  }

  # Extract WBS display settings
  if (is.null(display_config$wbs)) {
    display_config$wbs <- list(show = TRUE, show_labels = TRUE, show_names_on_bars = TRUE)
  }
  show_wbs <- display_config$wbs$show %||% TRUE
  show_wbs_labels <- display_config$wbs$show_labels %||% TRUE
  show_wbs_names_on_bars <- display_config$wbs$show_names_on_bars %||% TRUE

  # Extract activity display settings
  if (is.null(display_config$activity)) {
    display_config$activity <- list(show = TRUE, show_names_on_bars = FALSE)
  }
  show_activities <- display_config$activity$show %||% TRUE
  show_activity_names_on_bars <- display_config$activity$show_names_on_bars %||% FALSE

  # ============================================
  # 1E. PARSE AND VALIDATE LABEL CONFIG
  # ============================================

  # Set defaults if NULL
  if (is.null(label_config)) {
    label_config <- list(
      activity = list(yaxis = "{name}", bar = "{name}"),
      wbs = list(yaxis = "{name}", bar = "{name}")
    )
  }

  # Extract activity label templates
  if (is.null(label_config$activity)) {
    label_config$activity <- list(yaxis = "{name}", bar = "{name}")
  }
  activity_label_template <- label_config$activity$yaxis %||% "{name}"
  activity_bar_label_template <- label_config$activity$bar %||% "{name}"

  # Extract WBS label templates
  if (is.null(label_config$wbs)) {
    label_config$wbs <- list(yaxis = "{name}", bar = "{name}")
  }
  wbs_label_template <- label_config$wbs$yaxis %||% "{name}"
  wbs_bar_label_template <- label_config$wbs$bar %||% "{name}"

  # ============================================
  # 1F. PARSE AND VALIDATE BAR CONFIG
  # ============================================

  # Set defaults if NULL
  if (is.null(bar_config)) {
    bar_config <- list(
      wbs = list(opacity = 0.3, height = 0.3),
      activity = list(opacity = 1.0, height = 0.8, dim_opacity = 0.3, dim_past_activities = FALSE)
    )
  }

  # Extract WBS bar settings
  if (is.null(bar_config$wbs)) {
    bar_config$wbs <- list(opacity = 0.3, height = 0.3)
  }
  wbs_opacity <- bar_config$wbs$opacity %||% 0.3
  wbs_bar_height <- bar_config$wbs$height %||% 0.3

  # Extract activity bar settings
  if (is.null(bar_config$activity)) {
    bar_config$activity <- list(opacity = 1.0, height = 0.8, dim_opacity = 0.3, dim_past_activities = FALSE)
  }
  activity_opacity <- bar_config$activity$opacity %||% 1.0
  activity_bar_height <- bar_config$activity$height %||% 0.8
  dim_opacity <- bar_config$activity$dim_opacity %||% 0.3
  dim_past_activities <- bar_config$activity$dim_past_activities %||% FALSE

  # ============================================
  # 1G. PARSE AND VALIDATE LAYOUT CONFIG
  # ============================================

  # Set defaults if NULL
  if (is.null(layout_config)) {
    layout_config <- list(
      buffer_days = 30,
      indent_size = 2,
      max_visible_rows = 20,
      y_scroll_position = NULL,
      yaxis_label_width = 300,
      yaxis_label_max_chars = NULL,
      hover_popup_max_chars = 50,
      show_yaxis_labels = TRUE
    )
  }

  buffer_days <- layout_config$buffer_days %||% 30
  indent_size <- layout_config$indent_size %||% 2
  max_visible_rows <- layout_config$max_visible_rows %||% 20
  y_scroll_position <- layout_config$y_scroll_position  # Can be NULL
  yaxis_label_width <- layout_config$yaxis_label_width %||% 300
  yaxis_label_max_chars <- layout_config$yaxis_label_max_chars  # Can be NULL
  hover_popup_max_chars <- layout_config$hover_popup_max_chars %||% 50
  show_yaxis_labels <- layout_config$show_yaxis_labels %||% TRUE

  # ============================================
  # 1H. PARSE AND VALIDATE TOOLTIP CONFIG
  # ============================================

  # Set defaults if NULL
  if (is.null(tooltip_config)) {
    tooltip_config <- list(
      wbs = character(0),
      activity = character(0)
    )
  }

  # Extract custom tooltip fields for WBS
  tooltip_wbs_fields <- if (!is.null(tooltip_config$wbs)) {
    as.character(tooltip_config$wbs)
  } else {
    character(0)
  }

  # Extract custom tooltip fields for activities
  tooltip_activity_fields <- if (!is.null(tooltip_config$activity)) {
    as.character(tooltip_config$activity)
  } else {
    character(0)
  }

  # Helper function to build custom tooltip entries
  # Returns HTML string with field: value pairs, skipping missing/empty values
 build_custom_tooltip <- function(data_row, fields, data_source, max_chars) {
    if (length(fields) == 0) return("")

    tooltip_parts <- character(0)

    for (field in fields) {
      # Check if field exists in data source
      if (!field %in% colnames(data_source)) {
        next
      }

      # Get the value
      value <- data_row[[field]]

      # Skip if value is NA, NULL, or empty string
      if (is.null(value) || length(value) == 0) next
      if (is.na(value)) next
      if (is.character(value) && trimws(value) == "") next

      # Format the value
      if (inherits(value, "Date")) {
        value <- format(value, "%Y-%m-%d")
      } else {
        value <- as.character(value)
      }

      # Wrap text if needed
      wrapped_value <- wrap_text_for_hover(value, max_chars)

      tooltip_parts <- c(tooltip_parts, paste0(field, ": ", wrapped_value))
    }

    if (length(tooltip_parts) == 0) return("")

    # Join with line breaks and add a leading line break
    return(paste0("<br>", paste(tooltip_parts, collapse = "<br>")))
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
    
    # Add activities for this WBS (only if show_activities is TRUE)
    if (show_activities) {
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

          # Add color attribute if using attribute mode
          if (activity_color_mode == "attribute" && !is.null(activity_color_column)) {
            activity_item$color_attribute <- wbs_activities[[activity_color_column]][j]
          }

          result <- c(result, list(activity_item))
        }
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
    y_label_full = character(),  # Untruncated version for hover popups
    start = as.Date(character()),
    end = as.Date(character()),
    start_actual = as.Date(character()),
    end_actual = as.Date(character()),
    type = character(),
    level = numeric(),
    id = character(),
    wbs_id = character(),
    color_attribute = character(),  # For attribute-based coloring
    stringsAsFactors = FALSE
  )
  
  if (length(display_order) > 0) {
    y_pos <- length(display_order)
    for (item in display_order) {
      # Create indentation using non-breaking spaces
      indent <- paste(rep("\u00A0", item$level * indent_size), collapse = "")

      if (item$type == "WBS") {
        # WBS labels - format using template
        duration <- if (!is.na(item$start) && !is.na(item$end)) {
          as.numeric(item$end - item$start) + 1
        } else {
          NA
        }

        label_text <- format_label(wbs_label_template, list(
          name = item$name,
          id = item$id,
          start = item$start,
          end = item$end,
          duration = duration
        ))

        label <- truncate_label(paste0(indent, label_text), yaxis_label_max_chars)
        label_html <- truncate_label(paste0(indent, "<b>", label_text, "</b>"), yaxis_label_max_chars, preserve_html = TRUE)
        label_full <- paste0(indent, label_text)  # Store untruncated for hover
      } else {
        # Activity labels - format using template with bullet symbol
        duration <- if (!is.na(item$start) && !is.na(item$end)) {
          as.numeric(item$end - item$start) + 1
        } else {
          NA
        }

        label_text <- format_label(activity_label_template, list(
          name = item$name,
          id = item$id,
          start = item$start,
          end = item$end,
          start_actual = if (!is.null(item$start_actual)) item$start_actual else NA,
          end_actual = if (!is.null(item$end_actual)) item$end_actual else NA,
          duration = duration,
          wbs_id = item$wbs_id
        ))

        label <- truncate_label(paste0(indent, "\u2022 ", label_text), yaxis_label_max_chars)
        label_html <- truncate_label(paste0(indent, "\u2022 ", label_text), yaxis_label_max_chars)
        label_full <- paste0(indent, "\u2022 ", label_text)  # Store untruncated for hover
      }

      plot_data <- rbind(plot_data, data.frame(
        y_position = y_pos,
        y_label = label,
        y_label_html = label_html,
        y_label_full = label_full,
        start = item$start,
        end = item$end,
        start_actual = if (!is.null(item$start_actual)) item$start_actual else as.Date(NA),
        end_actual = if (!is.null(item$end_actual)) item$end_actual else as.Date(NA),
        type = item$type,
        level = item$level,
        id = item$id,
        wbs_id = ifelse(item$type == "Activity", item$wbs_id, item$id),
        color_attribute = if (!is.null(item$color_attribute)) as.character(item$color_attribute) else "",
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

        # Determine WBS bar color based on activity color mode
        if (activity_color_mode == "wbs") {
          # Use WBS-specific colors (original behavior)
          bar_color <- if (!is.null(wbs_colors) && wbs_id %in% names(wbs_colors)) {
            wbs_colors[[wbs_id]]
          } else {
            "#95A5A6"
          }
        } else {
          # Use uniform WBS color for uniform or attribute modes
          bar_color <- uniform_wbs_color
        }
        
        # Add the bar line (without text)
        # Generate intermediate points for full hover coverage
        hover_x <- generate_hover_points(wbs_data$start[i], wbs_data$end[i])
        hover_y <- rep(wbs_data$y_position[i], length(hover_x))

        # Create hover content
        # Get WBS row for custom tooltip fields
        wbs_row <- wbs_structure[wbs_structure$ID == wbs_id, ]
        custom_tooltip_wbs <- build_custom_tooltip(wbs_row, tooltip_wbs_fields, wbs_structure, hover_popup_max_chars)

        hover_content <- paste0(
          "<b>", wrap_text_for_hover(gsub("\u00A0", "", wbs_data$y_label_full[i]), hover_popup_max_chars), "</b><br>",
          "Type: WBS<br>",
          "Start: ", format(wbs_data$start[i], "%Y-%m-%d"), "<br>",
          "End: ", format(wbs_data$end[i], "%Y-%m-%d"), "<br>",
          "Duration: ", as.numeric(wbs_data$end[i] - wbs_data$start[i]) + 1, " days",
          custom_tooltip_wbs,
          "<extra></extra>"
        )

        fig <- fig %>% add_trace(
          type = "scatter",
          mode = "lines",
          x = hover_x,
          y = hover_y,
          line = list(color = bar_color, width = 5),
          opacity = wbs_opacity,
          name = "WBS",
          showlegend = FALSE,
          hovertemplate = hover_content,
          customdata = list(list(
            type = "wbs",
            original_start = as.character(wbs_data$start[i]),
            original_end = as.character(wbs_data$end[i])
          ))
        )
        
        # Add text annotation at the END of the bar if requested
        if (show_wbs_names_on_bars) {
          # Format bar label using template
          duration <- as.numeric(wbs_data$end[i] - wbs_data$start[i]) + 1
          bar_label_text <- format_label(wbs_bar_label_template, list(
            name = wbs_structure$Name[wbs_structure$ID == wbs_id],
            id = wbs_id,
            start = wbs_data$start[i],
            end = wbs_data$end[i],
            duration = duration
          ))

          text_annotations <- c(text_annotations, list(list(
            x = wbs_data$end[i],
            y = wbs_data$y_position[i],
            text = bar_label_text,
            xanchor = "left",
            xshift = 5,
            showarrow = FALSE,
            font = list(size = 9, color = "black")
          )))
        }
      }
    }
    
    # Add activity bars (constant thickness lines or stacked planned/actual)
    if (show_activities && nrow(activity_data) > 0) {
      for (i in 1:nrow(activity_data)) {
        wbs_id <- activity_data$wbs_id[i]

        # Determine activity bar color based on mode
        if (activity_color_mode == "wbs") {
          # Inherit color from WBS (original behavior)
          bar_color <- if (!is.null(wbs_colors) && wbs_id %in% names(wbs_colors)) {
            wbs_colors[[wbs_id]]
          } else {
            "#3498DB"
          }
        } else if (activity_color_mode == "uniform") {
          # Use uniform activity color
          bar_color <- uniform_activity_color
        } else if (activity_color_mode == "attribute") {
          # Color by attribute value
          attr_value <- activity_data$color_attribute[i]
          if (attr_value != "" && attr_value %in% names(activity_color_mapping)) {
            bar_color <- activity_color_mapping[[attr_value]]
          } else {
            # Default color if attribute value not found in mapping
            bar_color <- "#95A5A6"  # Gray
          }
        } else {
          bar_color <- "#3498DB"  # Fallback
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

          # Generate intermediate points for full hover coverage using original dates
          # JavaScript will adjust dynamically on zoom
          hover_x_planned <- generate_hover_points(activity_data$start[i], activity_data$end[i])
          hover_y_planned <- rep(activity_data$y_position[i] + 0.2, length(hover_x_planned))
          hover_x_actual <- generate_hover_points(activity_data$start_actual[i], activity_data$end_actual[i])
          hover_y_actual <- rep(activity_data$y_position[i] - 0.2, length(hover_x_actual))

          # Create hover content for planned bar
          # Get activity row for custom tooltip fields
          activity_row <- activities[activities$Activity_ID == activity_data$id[i], ]
          custom_tooltip_activity <- build_custom_tooltip(activity_row, tooltip_activity_fields, activities, hover_popup_max_chars)

          hover_content_planned <- paste0(
            "<b>", wrap_text_for_hover(gsub("\u00A0", "", activity_data$y_label_full[i]), hover_popup_max_chars), "</b><br>",
            "Type: Activity<br><br>",
            "<b>Planned:</b><br>",
            "Start: ", format(activity_data$start[i], "%Y-%m-%d"), "<br>",
            "End: ", format(activity_data$end[i], "%Y-%m-%d"), "<br>",
            "Duration: ", planned_duration, " days<br><br>",
            "<b>Actual:</b><br>",
            "Start: ", format(activity_data$start_actual[i], "%Y-%m-%d"), "<br>",
            "End: ", format(activity_data$end_actual[i], "%Y-%m-%d"), "<br>",
            "Duration: ", actual_duration, " days<br>",
            "Variance: ", ifelse(variance_days > 0, paste0("+", variance_days), variance_days), " days",
            custom_tooltip_activity,
            "<extra></extra>"
          )

          # Planned bar (upper half) - JavaScript will adjust dates dynamically on zoom
          fig <- fig %>% add_trace(
            type = "scatter",
            mode = "lines",
            x = hover_x_planned,
            y = hover_y_planned,
            line = list(color = bar_color, width = 10),
            opacity = activity_opacity,
            name = "Planned",
            showlegend = FALSE,
            hovertemplate = hover_content_planned,
            customdata = list(list(
              type = "activity_planned",
              original_start = as.character(activity_data$start[i]),
              original_end = as.character(activity_data$end[i])
            ))
          )

          # Actual bar (lower half) with diagonal stripe effect
          fig <- fig %>% add_trace(
            type = "scatter",
            mode = "lines",
            x = hover_x_actual,
            y = hover_y_actual,
            line = list(color = bar_color, width = 10),
            opacity = activity_opacity * 0.4,  # Lighter background
            name = "Actual",
            showlegend = FALSE,
            hoverinfo = "skip",
            customdata = list(list(
              type = "activity_actual",
              original_start = as.character(activity_data$start_actual[i]),
              original_end = as.character(activity_data$end_actual[i])
            ))
          )

          # Create diagonal stripe pattern using multiple thin lines
          # Note: Stripes don't need customdata - they're decorative and won't be resized
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
          # SINGLE BAR: Only planned dates
          # Generate intermediate points for full hover coverage
          hover_x <- generate_hover_points(activity_data$start[i], activity_data$end[i])
          hover_y <- rep(activity_data$y_position[i], length(hover_x))

          # Create hover content
          # Get activity row for custom tooltip fields
          activity_row <- activities[activities$Activity_ID == activity_data$id[i], ]
          custom_tooltip_activity <- build_custom_tooltip(activity_row, tooltip_activity_fields, activities, hover_popup_max_chars)

          hover_content <- paste0(
            "<b>", wrap_text_for_hover(gsub("\u00A0", "", activity_data$y_label_full[i]), hover_popup_max_chars), "</b><br>",
            "Type: Activity<br>",
            "Start: ", format(activity_data$start[i], "%Y-%m-%d"), "<br>",
            "End: ", format(activity_data$end[i], "%Y-%m-%d"), "<br>",
            "Duration: ", as.numeric(activity_data$end[i] - activity_data$start[i]) + 1, " days",
            custom_tooltip_activity,
            "<extra></extra>"
          )

          # Add the bar with original dates - JavaScript will adjust dynamically on zoom
          fig <- fig %>% add_trace(
            type = "scatter",
            mode = "lines",
            x = hover_x,
            y = hover_y,
            line = list(color = bar_color, width = 20),
            opacity = activity_opacity,
            name = "Activity",
            showlegend = FALSE,
            hovertemplate = hover_content,
            customdata = list(list(
              type = "activity",
              original_start = as.character(activity_data$start[i]),
              original_end = as.character(activity_data$end[i])
            ))
          )
        }

        # Add text annotation at the END of the bar if requested
        if (show_activity_names_on_bars) {
          # Get activity details from the original activities dataframe
          activity_row <- activities[activities$Activity_ID == activity_data$id[i], ]

          duration <- as.numeric(activity_data$end[i] - activity_data$start[i]) + 1
          bar_label_text <- format_label(activity_bar_label_template, list(
            name = activity_row$Activity_Name,
            id = activity_row$Activity_ID,
            start = activity_data$start[i],
            end = activity_data$end[i],
            start_actual = activity_data$start_actual[i],
            end_actual = activity_data$end_actual[i],
            duration = duration,
            wbs_id = activity_data$wbs_id[i]
          ))

          text_annotations <- c(text_annotations, list(list(
            x = if (has_actuals) max(activity_data$end[i], activity_data$end_actual[i], na.rm = TRUE) else activity_data$end[i],
            y = activity_data$y_position[i],
            text = bar_label_text,
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
  # 10. ADD MILESTONE LINES AND AREAS (OPTIONAL)
  # ============================================

  # Storage for milestone shapes (used for areas)
  milestone_shapes <- list()

  if (!is.null(milestone_data)) {
    for (i in 1:nrow(milestone_data)) {

      if (milestone_data$milestone_type[i] == "line") {
        # SINGLE DATE - VERTICAL LINE
        milestone_date <- milestone_data$date[i]

        # Only show the line if it falls within the plot range
        if (milestone_date >= plot_min_date && milestone_date <= plot_max_date) {
          # Add the vertical line
          fig <- fig %>% add_trace(
            type = "scatter",
            mode = "lines",
            x = c(milestone_date, milestone_date),
            y = c(y_range_min, y_range_max),
            line = list(
              color = milestone_data$color[i],
              width = milestone_data$width[i],
              dash = milestone_data$dash[i]
            ),
            name = milestone_data$label[i],
            showlegend = FALSE,
            hoverinfo = "text",
            hovertext = paste0(
              "<b>", wrap_text_for_hover(milestone_data$label[i], hover_popup_max_chars), "</b><br>",
              "Date: ", format(milestone_date, "%Y-%m-%d")
            )
          )

          # Determine y position for label based on label_position
          label_y_position <- switch(
            milestone_data$label_position[i],
            "top" = y_range_max,
            "middle" = (y_range_min + y_range_max) / 2,
            "bottom" = y_range_min,
            y_range_max  # default to top
          )

          # Determine vertical alignment based on position
          label_yanchor <- switch(
            milestone_data$label_position[i],
            "top" = "bottom",
            "middle" = "middle",
            "bottom" = "top",
            "bottom"  # default
          )

          # Add text annotation for the milestone label
          text_annotations <- c(text_annotations, list(list(
            x = milestone_date,
            y = label_y_position,
            text = milestone_data$label[i],
            xanchor = "center",
            yanchor = label_yanchor,
            yshift = if (milestone_data$label_position[i] == "top") 5 else if (milestone_data$label_position[i] == "bottom") -5 else 0,
            showarrow = FALSE,
            font = list(
              size = 10,
              color = milestone_data$color[i],
              family = "Arial, sans-serif"
            ),
            bgcolor = "rgba(255, 255, 255, 0.8)",
            bordercolor = milestone_data$color[i],
            borderwidth = 1,
            borderpad = 3
          )))
        }

      } else if (milestone_data$milestone_type[i] == "area") {
        # DATE RANGE - SHADED AREA (or line if too narrow)
        start_date <- milestone_data$start_date[i]
        end_date <- milestone_data$end_date[i]

        # Check if area overlaps with plot range
        if (end_date >= plot_min_date && start_date <= plot_max_date) {

          # Check if the date range is too narrow - if so, draw a line instead
          # Uses same 0.3% threshold as activity bars
          total_range <- as.numeric(plot_max_date - plot_min_date)
          milestone_duration <- as.numeric(end_date - start_date)
          min_area_threshold <- total_range * 0.003  # 0.3% of range, same as bars

          if (milestone_duration < min_area_threshold) {
            # Too narrow - draw as a vertical line at the midpoint
            mid_date <- start_date + (end_date - start_date) / 2

            fig <- fig %>% add_trace(
              type = "scatter",
              mode = "lines",
              x = c(mid_date, mid_date),
              y = c(y_range_min, y_range_max),
              line = list(
                color = milestone_data$color[i],
                width = milestone_data$width[i],
                dash = milestone_data$dash[i]
              ),
              name = milestone_data$label[i],
              showlegend = FALSE,
              hoverinfo = "text",
              hovertext = paste0(
                "<b>", wrap_text_for_hover(milestone_data$label[i], hover_popup_max_chars), "</b><br>",
                "Start: ", format(start_date, "%Y-%m-%d"), "<br>",
                "End: ", format(end_date, "%Y-%m-%d"), "<br>",
                "Duration: ", milestone_duration + 1, " days"
              )
            )

            # Determine y position for label
            label_y_position <- switch(
              milestone_data$label_position[i],
              "top" = y_range_max,
              "middle" = (y_range_min + y_range_max) / 2,
              "bottom" = y_range_min,
              y_range_max
            )

            label_yanchor <- switch(
              milestone_data$label_position[i],
              "top" = "bottom",
              "middle" = "middle",
              "bottom" = "top",
              "bottom"
            )

            text_annotations <- c(text_annotations, list(list(
              x = mid_date,
              y = label_y_position,
              text = milestone_data$label[i],
              xanchor = "center",
              yanchor = label_yanchor,
              yshift = if (milestone_data$label_position[i] == "top") 5 else if (milestone_data$label_position[i] == "bottom") -5 else 0,
              showarrow = FALSE,
              font = list(
                size = 10,
                color = milestone_data$color[i],
                family = "Arial, sans-serif"
              ),
              bgcolor = "rgba(255, 255, 255, 0.8)",
              bordercolor = milestone_data$color[i],
              borderwidth = 1,
              borderpad = 3
            )))

          } else {
            # Wide enough - draw as shaded area
            area_color <- milestone_data$color[i]
            fill_opacity <- milestone_data$fill_opacity[i]

            # Add shape for the shaded area
            # Use separate opacity parameter instead of rgba (more reliable in Plotly)
            milestone_shapes <- c(milestone_shapes, list(list(
              type = "rect",
              xref = "x",
              yref = "paper",
              x0 = as.character(start_date),
              x1 = as.character(end_date),
              y0 = 0,
              y1 = 1,
              fillcolor = area_color,
              opacity = fill_opacity,
              line = list(
                color = area_color,
                width = 1
              ),
              layer = "below"
            )))

            # Add invisible trace for hover functionality on area
            mid_date <- start_date + (end_date - start_date) / 2
            fig <- fig %>% add_trace(
              type = "scatter",
              mode = "lines",
              x = c(start_date, end_date),
              y = c((y_range_min + y_range_max) / 2, (y_range_min + y_range_max) / 2),
              line = list(color = "transparent", width = 0),
              opacity = 0,
              name = milestone_data$label[i],
              showlegend = FALSE,
              hoverinfo = "text",
              hovertext = paste0(
                "<b>", wrap_text_for_hover(milestone_data$label[i], hover_popup_max_chars), "</b><br>",
                "Start: ", format(start_date, "%Y-%m-%d"), "<br>",
                "End: ", format(end_date, "%Y-%m-%d"), "<br>",
                "Duration: ", as.numeric(end_date - start_date) + 1, " days"
              )
            )

            # Determine y position for label based on label_position
            label_y_position <- switch(
              milestone_data$label_position[i],
              "top" = y_range_max,
              "middle" = (y_range_min + y_range_max) / 2,
              "bottom" = y_range_min,
              y_range_max  # default to top
            )

            # Determine vertical alignment based on position
            label_yanchor <- switch(
              milestone_data$label_position[i],
              "top" = "bottom",
              "middle" = "middle",
              "bottom" = "top",
              "bottom"  # default
            )

            # Add text annotation for the milestone label (centered on area)
            text_annotations <- c(text_annotations, list(list(
              x = mid_date,
              y = label_y_position,
              text = milestone_data$label[i],
              xanchor = "center",
              yanchor = label_yanchor,
              yshift = if (milestone_data$label_position[i] == "top") 5 else if (milestone_data$label_position[i] == "bottom") -5 else 0,
              showarrow = FALSE,
              font = list(
                size = 10,
                color = milestone_data$color[i],
                family = "Arial, sans-serif"
              ),
              bgcolor = "rgba(255, 255, 255, 0.8)",
              bordercolor = milestone_data$color[i],
              borderwidth = 1,
              borderpad = 3
            )))
          }
        }
      }
    }
  }

  # ============================================
  # 11. CONFIGURE LAYOUT WITH Y-AXIS SCROLLING
  # ============================================

  # Determine which y-axis labels to show based on configuration
  if (nrow(plot_data) > 0) {
    if (show_yaxis_labels) {
      # Show all labels (default behavior)
      yaxis_ticktext <- plot_data$y_label_html
      effective_label_width <- yaxis_label_width
    } else if (show_wbs_labels) {
      # Only show WBS labels, hide activity labels
      yaxis_ticktext <- ifelse(plot_data$type == "WBS", plot_data$y_label_html, "")
      effective_label_width <- yaxis_label_width
    } else {
      # Hide all labels - use minimal margin
      yaxis_ticktext <- rep("", nrow(plot_data))
      effective_label_width <- 50
    }
  } else {
    yaxis_ticktext <- c()
    effective_label_width <- 50
  }

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
      ticktext = yaxis_ticktext,
      showgrid = TRUE,
      autorange = FALSE,
      range = c(y_range_min, y_range_max),
      tickfont = list(family = "Courier New, monospace", size = 11),
      side = "left",
      tickangle = 0,
      fixedrange = FALSE
    ),
    annotations = text_annotations,
    shapes = if (length(milestone_shapes) > 0) milestone_shapes else NULL,
    hovermode = "closest",
    plot_bgcolor = "white",  # Changed to white for better contrast with alternating backgrounds
    paper_bgcolor = "white",
    margin = list(l = effective_label_width, r = 50, t = 80, b = 80),
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
        
        // Preserve milestone shapes (areas with borders, not alternating backgrounds)
        var existingShapes = el.layout.shapes || [];
        existingShapes.forEach(function(shape) {
          // Preserve shapes that have a border (milestone areas) or are today lines
          var isMilestoneArea = shape.type === 'rect' && shape.line && shape.line.width > 0;
          var isTodayLine = shape.line && shape.line.dash === 'dash' && shape.line.color === 'red';
          if (isMilestoneArea || isTodayLine) {
            backgroundShapes.push(shape);
          }
        });
        
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

      // ============================================
      // DYNAMIC BAR WIDTH FUNCTIONS
      // ============================================

      // Calculate minimum bar width and return adjusted dates
      function ensureMinBarWidth(originalStart, originalEnd, rangeMin, rangeMax, minWidthPercent) {
        var totalRange = rangeMax.getTime() - rangeMin.getTime();
        var duration = originalEnd.getTime() - originalStart.getTime();
        var minDuration = totalRange * (minWidthPercent / 100);

        if (duration >= minDuration) {
          return { start: originalStart, end: originalEnd };
        }

        var midDate = new Date((originalStart.getTime() + originalEnd.getTime()) / 2);
        var halfMin = minDuration / 2;
        return {
          start: new Date(midDate.getTime() - halfMin),
          end: new Date(midDate.getTime() + halfMin)
        };
      }

      // Generate hover points for a date range (mirrors R function)
      function generateHoverPoints(startDate, endDate) {
        var points = [];
        var duration = (endDate.getTime() - startDate.getTime()) / (1000 * 60 * 60 * 24); // days
        var step;

        if (duration <= 0) {
          return [startDate.toISOString().split('T')[0], startDate.toISOString().split('T')[0]];
        } else if (duration <= 7) {
          step = 1;
        } else if (duration <= 90) {
          step = 3;
        } else if (duration <= 365) {
          step = 7;
        } else {
          step = 14;
        }

        var current = new Date(startDate);
        while (current <= endDate) {
          points.push(current.toISOString().split('T')[0]);
          current.setDate(current.getDate() + step);
        }
        if (points.length === 0 || points[points.length - 1] !== endDate.toISOString().split('T')[0]) {
          points.push(endDate.toISOString().split('T')[0]);
        }
        return points;
      }

      // Update all bar traces based on current x-axis range
      function updateBarWidths(el) {
        if (!el.layout || !el.layout.xaxis || !el.layout.xaxis.range) {
          return;
        }

        var xRange = el.layout.xaxis.range;
        var rangeMin = new Date(xRange[0]);
        var rangeMax = new Date(xRange[1]);
        var minWidthPercent = 0.3;

        var indicesToUpdate = [];
        var xUpdates = [];
        var yUpdates = [];

        el.data.forEach(function(trace, idx) {
          if (!trace.customdata || !trace.customdata[0]) return;
          var meta = trace.customdata[0];
          if (!meta.type || !meta.original_start || !meta.original_end) return;

          var yVal = trace.y[0];
          var origStart = new Date(meta.original_start);
          var origEnd = new Date(meta.original_end);
          var adjusted = ensureMinBarWidth(origStart, origEnd, rangeMin, rangeMax, minWidthPercent);
          var newX = generateHoverPoints(adjusted.start, adjusted.end);

          var newY = [];
          for (var i = 0; i < newX.length; i++) {
            newY.push(yVal);
          }

          indicesToUpdate.push(idx);
          xUpdates.push(newX);
          yUpdates.push(newY);
        });

        if (indicesToUpdate.length > 0) {
          Plotly.restyle(el, { x: xUpdates, y: yUpdates }, indicesToUpdate);
        }
      }

      // Debounce function to avoid excessive updates
      var updateBarWidthsDebounced = (function() {
        var timeout;
        return function() {
          clearTimeout(timeout);
          timeout = setTimeout(function() {
            updateBarWidths(el);
          }, 100);
        };
      })();

      // Apply alignment on initial render
      setTimeout(alignYAxisLabels, 100);

      // Apply initial date format
      setTimeout(updateDateFormat, 150);

      // Apply initial bar width adjustment (after date format is applied and relayout completes)
      setTimeout(function() { updateBarWidths(el); }, 500);
      
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
        // Update date format and bar widths when x-axis range changes
        if (eventData['xaxis.range[0]'] !== undefined || eventData['xaxis.range[1]'] !== undefined || eventData['xaxis.range'] !== undefined) {
          setTimeout(updateDateFormat, 50);
          // Use debounced version to avoid excessive updates during rapid zoom/pan
          updateBarWidthsDebounced();
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
