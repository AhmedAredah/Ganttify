#' Test Project Data for Ganttify
#'
#' A sample project dataset for testing and demonstrating the Ganttify package.
#' Contains a complete Work Breakdown Structure (WBS), activities, and custom colors
#' for an example software development project.
#'
#' @format A list with 3 components:
#' \describe{
#'   \item{wbs_structure}{A data frame with 8 rows and 3 columns:
#'     \itemize{
#'       \item ID: WBS item identifier (W1-W8)
#'       \item Name: WBS item name (e.g., "Project Summary", "Design Phase")
#'       \item Parent: Parent WBS ID or "None" for root items
#'     }
#'   }
#'   \item{activities}{A data frame with 15 rows and 7 columns:
#'     \itemize{
#'       \item WBS_ID: Associated WBS item identifier
#'       \item Activity_ID: Activity identifier (A1-A15)
#'       \item Activity_Name: Activity name (e.g., "Design UI", "Code Frontend")
#'       \item Start_Date: Planned start date (YYYY-MM-DD or MM/DD/YYYY character or Date class)
#'       \item End_Date: Planned end date (YYYY-MM-DD or MM/DD/YYYY character or Date class)
#'       \item Start_Date_Actual: Actual start date (YYYY-MM-DD or MM/DD/YYYY character or Date class, some NA for not started)
#'       \item End_Date_Actual: Actual end date (YYYY-MM-DD or MM/DD/YYYY character or Date class, some NA for in-progress)
#'     }
#'     Includes examples of on-time, delayed, ahead-of-schedule, and in-progress activities.
#'   }
#'   \item{colors}{A named list of 8 colors:
#'     \itemize{
#'       \item Each WBS item (W1-W8) is assigned a custom hex color
#'     }
#'   }
#' }
#'
#' @examples
#' # Load the test data
#' data(test_project)
#'
#' # View structure
#' str(test_project)
#'
#' # Create a Gantt chart
#' \donttest{
#' chart <- Ganttify(
#'   wbs_structure = test_project$wbs_structure,
#'   activities = test_project$activities,
#'   color_config = list(mode = "wbs", wbs = test_project$colors)
#' )
#' chart
#' }
#'
#' @source Example software development project
"test_project"
