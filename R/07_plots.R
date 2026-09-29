##################################
#                                #
#   Plotting data                #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Plot one or more days of diabetes data for a single subject
#'
#' Draws a two-panel plot per day. The upper panel shows the CGM trace over a
#' shaded target range, with the SMBG measurements as diamonds; the lower panel
#' shows the basal rate as a filled step area, the boluses as labelled circles
#' and the carbohydrates as labelled boxes, headed by the insulin delivered over
#' the whole day. Both panels share a 0-24 h axis of the local (wall-clock) time,
#' and a legend is drawn underneath them.
#'
#' The daily totals are taken from \code{total_basal} and \code{total_bolus}
#' where the export provides them, and are summed over the day otherwise. They
#' therefore cover insulin that the drawn data may not, such as the basal
#' delivered in automated mode by an Omnipod, and they are unaffected by
#' \code{min_bolus}. A total that is not recorded at all is shown as \code{"-"}.
#' Where \code{min_bolus} leaves boluses out of the labelled circles, a note next
#' to the totals says so.
#'
#' @param data A list with all data found for one individual (the output of
#'   \code{\link{parse_data}}) or for multiple individuals (the output of
#'   \code{\link{merge_all}}). Requires cleaned and standardized data. The data
#'   types \code{cgm}, \code{basal}, \code{bolus}, \code{carbs} and \code{SMBG}
#'   are drawn where they are present; a type that is absent or that holds no
#'   data for a day is skipped.
#' @param id Character or numeric participant identifier, naming the individual
#'   to plot. Can be omitted if \code{data} holds the data of a single
#'   individual.
#' @param days The day(s) to plot, as a \code{Date} or as a \code{"YYYY-MM-DD"}
#'   string. Defaults to every day with CGM data, i.e. one plot per day.
#' @param target Numeric vector of length two with the glucose target range,
#'   drawn as a shaded band.
#' @param ylim_cgm Numeric vector of length two with the limits of the glucose
#'   axis.
#' @param min_bolus Numeric, the smallest bolus that is drawn in full, in units.
#'   Small correction boluses crowd the panel without adding much, and
#'   \code{min_bolus = 1} draws everything below 1 U as a bare tick at its time,
#'   without a circle and without its value. Compared against the total of a
#'   bolus, so that a small immediate part followed by a large extended one is
#'   drawn in full. Defaults to \code{0}, i.e. every bolus is drawn in full.
#' @param units Character string, the units of the glucose data, either
#'   \code{"mg/dl"} or \code{"mmol/l"}.
#' @param main Character string with the plot title. Defaults to the id and the
#'   day. Note that the same title is used for every day if several are plotted.
#'
#' @return No return value, called for side effects: one plot is drawn per day.
#' @examples
#' path <- system.file("extdata", "glooko_example.zip", package = "aidR")
#' data <- parse_data(id = "1", paths = path)
#' plot_day(data, days = "2026-06-22")
#'
#' # Custom target range, with boluses below 1 U drawn as ticks only
#' plot_day(data, days = "2026-06-22", target = c(70, 140), min_bolus = 1)
#' @export
plot_day <- function(data, id = NULL, days = NULL, target = NULL, ylim_cgm = NULL,
                     min_bolus = 0, units = "mg/dl", main = NULL) {
  if (!inherits(data, "list") ||
    !("cgm" %in% names(data)) ||
    !("value" %in% names(data$cgm))) {
    stop("Require a list with attribute 'cgm'.
         Please make sure parse_data() was run with clean = TRUE.")
  }

  if (!is.numeric(min_bolus) || length(min_bolus) != 1 || is.na(min_bolus) || min_bolus < 0) {
    stop("Require a single non-negative number for 'min_bolus'.")
  }

  if (!units %in% c("mg/dl", "mmol/l")) {
    stop("Require 'units' to be either 'mg/dl' or 'mmol/l'.")
  }

  # Set default target and limits according to units
  if (is.null(target)) {
    target <- if (units == "mg/dl") c(70, 180) else c(3.9, 10)
  }

  if (is.null(ylim_cgm)) {
    ylim_cgm <- if (units == "mg/dl") c(40, 400) else c(2.2, 22)
  }

  # One plot shows one individual at a time -> pick exactly one
  all_ids <- unique(as.character(data$cgm$id))
  if (is.null(id)) {
    if (length(all_ids) > 1) {
      stop(
        "Data of more than one individual found (", paste(all_ids, collapse = ", "),
        "), but plotting function shows a single individual.
           Please provide the individual to plot via 'id'."
      )
    }
    id <- all_ids
  } else {
    id <- as.character(id)
    if (length(id) != 1) {
      stop("Require a single 'id', got ", length(id), ".")
    }
    if (!(id %in% all_ids)) {
      stop("No data found for id ", id, "!")
    }
  }

  # Decide on the day(s) to plot
  all_days <- unique(date(data$cgm$timestamp[as.character(data$cgm$id) == id]))
  days <- if (is.null(days)) sort(all_days) else as.Date(days)

  # Indexing (rather than iterating) keeps the Date class of the day
  for (i in seq_along(days)) {
    if (!(days[i] %in% all_days)) {
      stop("Id ", id, ": No data found for day ", format(days[i]), "!")
    }
    .plot_single_day.aidR(data, id, days[i], target, ylim_cgm, min_bolus, units, main)
  }

  invisible(NULL)
}

#------------------------
# Helper functions
#------------------------

#' Select the data of one individual and one day, for plotting
#'
#' @param df A standardized aidR data frame, or \code{NULL} if the data type is
#'   absent.
#' @param id Character string with the participant identifier.
#' @param day The day to select, as a \code{Date}.
#'
#' @return The rows of \code{df} of that individual and that day, with an
#'   additional column \code{hour_of_day} holding the local (wall-clock) time as
#'   a fraction of hours. \code{NULL} if there is no such row.
#'
#' @keywords internal
.prep_plot_data.aidR <- function(df, id, day) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  df <- df[!is.na(df$id) & as.character(df$id) == id, , drop = FALSE]
  df <- df[date(df$timestamp) == day, , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }

  # Hour of the day, taken from the local (wall-clock) time
  df$hour_of_day <- as.numeric(lubridate::hms(format(df$timestamp, "%H:%M:%S"))) / 3600

  return(df[order(df$hour_of_day), , drop = FALSE])
}

#' Draw a single day of diabetes data for one individual
#'
#' @param data A list with the standardized aidR data frames, named by aidR type
#'   (\code{cgm}, \code{basal}, ...).
#' @param id Character string with the participant identifier.
#' @param day The day to draw, as a \code{Date}.
#' @param target Numeric vector of length two with the glucose target range.
#' @param ylim_cgm Numeric vector of length two with the limits of the glucose axis.
#' @param min_bolus Numeric, the smallest bolus that is drawn in full, in units.
#'   Smaller ones are drawn as bare ticks.
#' @param units Character string, the units of the glucose data, either
#'   \code{"mg/dl"} or \code{"mmol/l"}.
#' @param main Character string with the plot title, \code{NULL} for the default.
#'
#' @return No return value, called for side effects: one plot is drawn.
#'
#' @keywords internal
.plot_single_day.aidR <- function(data, id, day, target, ylim_cgm, min_bolus, units, main) {
  cgm <- .prep_plot_data.aidR(data$cgm, id, day)
  basal <- .prep_plot_data.aidR(data$basal, id, day)
  bolus <- .prep_plot_data.aidR(data$bolus, id, day)
  carbs <- .prep_plot_data.aidR(data$carbs, id, day)
  SMBG <- .prep_plot_data.aidR(data$SMBG, id, day)

  # The totals cover the whole day and are therefore taken before 'min_bolus'
  # leaves any bolus out
  totals <- .daily_insulin_totals.aidR(data, id, day, basal, bolus)

  # Set aside the boluses that are too small to be worth a labelled circle: they
  # are drawn as bare ticks instead, so that they are still visible without
  # crowding the panel. A bolus whose total is not recorded is drawn in full,
  # since there is nothing to judge it by
  bolus_small <- NULL
  if (!is.null(bolus) && min_bolus > 0) {
    is_small <- !is.na(bolus$total) & bolus$total < min_bolus
    bolus_small <- bolus[is_small, , drop = FALSE]
    bolus <- bolus[!is_small, , drop = FALSE]
    if (nrow(bolus_small) == 0) {
      bolus_small <- NULL
    }
    if (nrow(bolus) == 0) {
      bolus <- NULL
    }
  }

  if (is.null(main)) {
    main <- paste0(id, "   -   ", format(day, "%a %d %b %Y"))
  }

  # Layout: glucose panel on top, therapy panel below
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  layout(matrix(c(1, 2, 3), nrow = 3), heights = c(2, 1, 0.3))

  # A layout of three or more rows makes R shrink the base character size to
  # 0.66 by itself, which would make every label of the plot smaller. Every panel
  # therefore sets it back, so that the text keeps the size it is asked for
  par(mar = c(0.5, 4.5, 3, 2), xaxs = "i", yaxs = "i", cex = 1)

  xlim <- c(0, 24)
  xticks <- seq(0, 24, by = 2)

  # Plot settings
  CARB_BOX_HALF_WIDTH <- 0.35 # Half the width of a carbohydrate box, in hours
  BOLUS_CIRCLE_CEX <- 3.2 # Character expansion of a bolus circle
  LEADER_LINE_TOL <- 0.2 # How far a marker has to be moved, as a fraction of the distance markers are spread apart by, before a leader line to its true time is worth drawing

  # Colours, shared between the panels and the legend
  COL_CGM <- "black"
  COL_SMBG <- "#D32F2F"
  COL_TARGET <- "#E4F5E4"
  COL_TARGET_LINE <- "#4CAF50"
  COL_BASAL <- "#F8BBD0"
  COL_BASAL_BORDER <- "#F48FB1"
  COL_BOLUS <- "#7E8CE0"
  COL_BOLUS_BORDER <- "#3F51B5"
  COL_CARBS <- "#EF6C00"

  #----- Panel 1: CGM and SMBG -----
  plot(NA, xlim = xlim, ylim = ylim_cgm, xlab = "", ylab = "", axes = FALSE, main = main)

  # Target range band and light vertical gridlines
  rect(xlim[1], target[1], xlim[2], target[2], col = COL_TARGET, border = NA)
  abline(h = target, col = COL_TARGET_LINE, lwd = 1)
  abline(v = xticks, col = "grey90")

  if (!is.null(cgm)) {
    lines(cgm$hour_of_day, cgm$value, col = COL_CGM, lwd = 2)
  }
  if (!is.null(SMBG)) {
    points(SMBG$hour_of_day, SMBG$value, pch = 23, bg = COL_SMBG, col = "black", cex = 1)
  }

  axis(2, las = 1, cex.axis = 0.8)
  mtext(paste0("Glucose (", units, ")"), side = 2, line = 3, cex = 0.9)
  box()

  #----- Panel 2: basal, bolus and carbohydrates -----
  par(mar = c(3, 4.5, 0.5, 2), cex = 1)

  # The basal rates set the scale of the panel, with room above for the markers
  max_rate <- if (is.null(basal)) 1 else max(basal$rate, na.rm = TRUE)
  if (!is.finite(max_rate) || max_rate <= 0) {
    max_rate <- 1
  }
  ylim_panel <- max_rate * 3

  plot(NA, xlim = xlim, ylim = c(0, ylim_panel), xlab = "", ylab = "", axes = FALSE)
  abline(v = xticks, col = "grey90")

  # Basal as filled steps. The duration is in hours for all formats, matching the
  # rate in U/h, and is cut off at the next segment or at midnight
  if (!is.null(basal)) {
    start <- basal$hour_of_day
    next_start <- c(start[-1], 24)
    end <- ifelse(is.na(basal$duration), next_start, pmin(start + basal$duration, next_start))

    # A segment without a positive duration runs until the next one
    end[end <= start] <- next_start[end <= start]

    for (i in seq_len(nrow(basal))) {
      rect(start[i], 0, end[i], basal$rate[i], col = COL_BASAL, border = COL_BASAL_BORDER)
    }
  }

  # Carbohydrates as labelled boxes, above the basal steps. Boxes that would
  # overlap are moved apart and a leader line points at the true time
  if (!is.null(carbs)) {
    y_carbs <- ylim_panel * 0.42
    min_sep <- 2 * CARB_BOX_HALF_WIDTH + 0.05
    x_carbs <- .spread_markers.aidR(
      carbs$hour_of_day, min_sep,
      xlim + c(1, -1) * CARB_BOX_HALF_WIDTH
    )

    .draw_leader_lines.aidR(
      carbs$hour_of_day, x_carbs, y_carbs - ylim_panel * 0.05,
      y_carbs - ylim_panel * 0.13, ylim_panel * 0.03, COL_CARBS,
      LEADER_LINE_TOL * min_sep
    )
    for (i in seq_len(nrow(carbs))) {
      rect(x_carbs[i] - CARB_BOX_HALF_WIDTH, y_carbs - ylim_panel * 0.05,
        x_carbs[i] + CARB_BOX_HALF_WIDTH, y_carbs + ylim_panel * 0.05,
        col = COL_CARBS, border = NA
      )
      text(x_carbs[i], y_carbs, as.character(carbs$value[i]),
        col = "white", cex = 0.75, font = 2
      )
    }
  }

  # Boluses in the upper part of the panel: the part delivered immediately as a
  # labelled circle, the extended part as a line spanning its duration
  y_bolus <- ylim_panel * 0.66

  # The boluses left out by 'min_bolus' are only marked by a tick at their true
  # time, drawn first so that a full bolus next to them stays on top
  if (!is.null(bolus_small)) {
    segments(bolus_small$hour_of_day, y_bolus - ylim_panel * 0.05,
      bolus_small$hour_of_day, y_bolus + ylim_panel * 0.05,
      col = COL_BOLUS_BORDER, lwd = 2
    )
  }

  if (!is.null(bolus)) {
    is_extended <- !is.na(bolus$duration_extended) & bolus$duration_extended > 0

    # The circle shows the immediate part: the whole bolus for a normal one, the
    # initial part of a dual wave, and nothing at all for a square wave
    initial <- if (is.null(bolus$normal)) rep(NA_real_, nrow(bolus)) else as.numeric(bolus$normal)
    no_split <- !is_extended & is.na(initial)
    initial[no_split] <- as.numeric(bolus$total)[no_split]
    has_initial <- !is.na(initial) & initial > 0

    # A circle is as wide and as high as its character size, which scales with
    # the device rather than with the data
    circle_radius_x <- 0.55 * BOLUS_CIRCLE_CEX * par("cxy")[1]
    circle_radius_y <- 0.55 * BOLUS_CIRCLE_CEX * par("cxy")[2]

    # The extended part spans a real duration and is therefore drawn at the true
    # time, in its own lane just above the circles so that the two never collide
    if (any(is_extended)) {
      y_extended <- y_bolus + circle_radius_y / 2 + ylim_panel * 0.04
      start <- bolus$hour_of_day[is_extended]
      end <- pmin(start + bolus$duration_extended[is_extended], xlim[2])

      segments(start, y_extended, end, y_extended, col = COL_BOLUS_BORDER, lwd = 3)
      segments(c(start, end), y_extended - ylim_panel * 0.025,
        c(start, end), y_extended + ylim_panel * 0.025,
        col = COL_BOLUS_BORDER, lwd = 3
      )
      text((start + end) / 2, y_extended + ylim_panel * 0.07,
        formatC(round(as.numeric(bolus$extended)[is_extended], digits = 2), format = "fg"),
        col = COL_BOLUS_BORDER, cex = 0.7, font = 2
      )
    }

    if (any(has_initial)) {
      min_sep <- 2.1 * circle_radius_x
      x_bolus <- .spread_markers.aidR(
        bolus$hour_of_day[has_initial], min_sep,
        xlim + c(1, -1) * circle_radius_x
      )

      .draw_leader_lines.aidR(
        bolus$hour_of_day[has_initial], x_bolus,
        y_bolus,
        y_bolus - ylim_panel * 0.08,
        ylim_panel * 0.03, COL_BOLUS_BORDER, LEADER_LINE_TOL * min_sep
      )
      points(x_bolus, rep(y_bolus, length(x_bolus)),
        pch = 21, bg = COL_BOLUS, col = COL_BOLUS_BORDER, cex = BOLUS_CIRCLE_CEX
      )
      text(x_bolus, rep(y_bolus, length(x_bolus)),
        formatC(round(initial[has_initial], digits = 2), format = "fg"),
        col = "white", cex = 0.7, font = 2
      )
    }
  }

  # The insulin delivered over the whole day, in the header of the panel
  text(xlim[1] + 0.2, ylim_panel * 0.93,
    adj = c(0, 0.5), cex = 0.8, font = 2,
    labels = paste0(
      "TDD ", .format_units.aidR(totals["total"]),
      "          Total daily basal ", .format_units.aidR(totals["basal"]),
      "          Total daily bolus ", .format_units.aidR(totals["bolus"])
    )
  )

  # Boluses drawn as a bare tick are easily mistaken for missing data
  if (!is.null(bolus_small)) {
    text(xlim[1] + 0.2, ylim_panel * 0.85,
      adj = c(0, 0.5), cex = 0.7, font = 3, col = "grey30",
      labels = paste0(
        "Only boluses of at least ", formatC(min_bolus, format = "fg"),
        " U are shown in full, smaller ones are marked by a tick"
      )
    )
  }

  axis(1, at = xticks, labels = sprintf("%02d", xticks), cex.axis = 0.8)
  axis(2, at = pretty(c(0, max_rate)), las = 1, cex.axis = 0.8)
  mtext("Basal (U/h)", side = 2, line = 3, cex = 0.9)
  mtext("Time (h)", side = 1, line = 2, cex = 0.9)
  box()

  #----- Legend, below both panels -----
  par(mar = c(0, 4.5, 0, 2), cex = 1)
  plot.new()
  legend("center",
    horiz = TRUE, bty = "n", cex = 0.75, x.intersp = 0.8,
    legend = c(
      "CGM", "SMBG", "Target range", "Basal", "Bolus",
      "Extended bolus", "Carbs (g)"
    ),
    lty = c(1, NA, NA, NA, NA, 1, NA),
    lwd = c(2, NA, NA, NA, NA, 3, NA),
    pch = c(NA, 23, 22, 22, 21, NA, 22),
    col = c(
      COL_CGM, "black", COL_TARGET_LINE, COL_BASAL_BORDER,
      COL_BOLUS_BORDER, COL_BOLUS_BORDER, COL_CARBS
    ),
    pt.bg = c(NA, COL_SMBG, COL_TARGET, COL_BASAL, COL_BOLUS, NA, COL_CARBS),
    pt.cex = c(NA, 1.1, 1.6, 1.6, 1.3, NA, 1.6)
  )

  invisible(NULL)
}

#' The insulin delivered on one day, for the header of the therapy panel
#'
#' Uses the daily totals of \code{data} where they are available, since a
#' platform that reports them itself covers insulin that the individual data
#' points may not (e.g. the basal delivered in automated mode by an Omnipod).
#' Where they are absent, the day is summed over the data that is drawn.
#'
#' @param data A list with the standardized aidR data frames, named by aidR type
#'   (\code{cgm}, \code{basal}, ...).
#' @param id Character string with the participant identifier.
#' @param day The day to total, as a \code{Date}.
#' @param basal The basal data of that individual and that day, or \code{NULL}.
#' @param bolus The bolus data of that individual and that day, or \code{NULL}.
#'
#' @return A named numeric vector with entries \code{basal}, \code{bolus} and
#'   \code{total}, in units. An entry is \code{NA} where the insulin is not
#'   recorded, and \code{total} is \code{NA} as soon as either part is.
#'
#' @keywords internal
.daily_insulin_totals.aidR <- function(data, id, day, basal, bolus) {
  reported <- function(type) {
    df <- data[[paste0("total_", type)]]
    if (is.null(df) || nrow(df) == 0) {
      return(NA_real_)
    }

    df <- df[as.character(df$id) == id & as.Date(df$date) == day, , drop = FALSE]
    if (nrow(df) == 0) {
      return(NA_real_)
    }

    # A day reported more than once (overlapping exports) is taken only once
    return(as.numeric(df[[paste0("total_", type)]])[1])
  }

  total_basal <- reported("basal")
  if (is.na(total_basal) && !is.null(basal)) {
    total_basal <- .sum_or_na.aidR(as.numeric(basal$rate) * as.numeric(basal$duration))
  }

  total_bolus <- reported("bolus")
  if (is.na(total_bolus) && !is.null(bolus)) {
    total_bolus <- .sum_or_na.aidR(as.numeric(bolus$total))
  }

  return(c(
    basal = total_basal, bolus = total_bolus,
    total = sum(total_basal, total_bolus)
  ))
}

#' Write an amount of insulin for the plot
#'
#' @param x Numeric, an amount of insulin in units, or \code{NA} if it is not
#'   recorded.
#'
#' @return A character string, \code{"-"} where \code{x} is \code{NA}.
#'
#' @keywords internal
.format_units.aidR <- function(x) {
  if (is.na(x)) {
    return("-")
  }

  return(paste0(formatC(x, format = "f", digits = 1), " U"))
}

#' Move markers apart until they no longer overlap
#'
#' Markers that are closer together than \code{min_sep} are unreadable when they
#' are drawn at their own time. They are pushed apart, keeping their order and
#' staying within \code{xlim}, so that a leader line can point back at the time
#' they actually occurred (see \code{\link{.draw_leader_lines.aidR}}).
#'
#' @param x Numeric vector with the positions of the markers, in hours.
#' @param min_sep Numeric, the smallest distance two markers may have, in hours.
#' @param xlim Numeric vector of length two with the limits of the axis.
#'
#' @return A numeric vector, the positions at which to draw the markers, in the
#'   order of \code{x}.
#'
#' @keywords internal
.spread_markers.aidR <- function(x, min_sep, xlim) {
  if (length(x) <= 1) {
    return(x)
  }

  # More markers than the axis can hold at this distance: spread them evenly
  # instead, which is as readable as the axis allows and still fits on it
  min_sep <- min(min_sep, diff(xlim) / (length(x) - 1))

  order_x <- order(x)
  true <- x[order_x]
  pos <- true

  # Push every marker that is too close to its predecessor to the right, then
  # centre each group that formed on the times it stands for, so that the markers
  # end up on both sides of them rather than all after them. Centring can make
  # two groups touch again, so both steps are repeated until nothing moves.
  for (pass in 1:length(pos)) {
    for (i in seq_along(pos)[-1]) {
      if (pos[i] - pos[i - 1] < min_sep) {
        pos[i] <- pos[i - 1] + min_sep
      }
    }

    group <- cumsum(c(TRUE, diff(pos) > min_sep + 1e-9))
    centred <- pos
    for (g in unique(group)) {
      in_group <- group == g
      centred[in_group] <- pos[in_group] + (mean(true[in_group]) - mean(pos[in_group]))
    }

    if (isTRUE(all.equal(centred, pos))) {
      break
    }
    pos <- centred
  }

  # Pull the markers back in where this pushed them off either edge
  if (pos[length(pos)] > xlim[2]) {
    pos[length(pos)] <- xlim[2]
    for (i in rev(seq_along(pos))[-1]) {
      if (pos[i + 1] - pos[i] < min_sep) {
        pos[i] <- pos[i + 1] - min_sep
      }
    }
  }
  if (pos[1] < xlim[1]) {
    pos[1] <- xlim[1]
    for (i in seq_along(pos)[-1]) {
      if (pos[i] - pos[i - 1] < min_sep) {
        pos[i] <- pos[i - 1] + min_sep
      }
    }
  }

  spread <- numeric(length(x))
  spread[order_x] <- pos

  return(spread)
}

#' Draw a leader line from a moved marker to the time it occurred
#'
#' Only markers that were actually moved by \code{\link{.spread_markers.aidR}}
#' get a line, ending in a small tick at the true time.
#'
#' @param x_true Numeric vector with the times the markers occurred, in hours.
#' @param x_drawn Numeric vector with the positions the markers are drawn at.
#' @param y_marker Numeric, the edge of the marker the line starts at.
#' @param y_anchor Numeric, the height the line points down to.
#' @param tick Numeric, the height of the tick at the true time.
#' @param col Character string with the colour of the line.
#' @param tol Numeric, how far a marker has to have moved to get a line. Markers
#'   nudged by less than this are drawn close enough to their time that a line
#'   would only add clutter.
#'
#' @return No return value, called for side effects: the lines are drawn.
#'
#' @keywords internal
.draw_leader_lines.aidR <- function(x_true, x_drawn, y_marker, y_anchor, tick, col, tol) {
  moved <- abs(x_drawn - x_true) > tol
  if (!any(moved)) {
    return(invisible(NULL))
  }

  # An elbow: straight down out of the marker, across, and down to the true time
  segments(x_drawn[moved], y_marker, x_drawn[moved], y_anchor + tick, col = col, lwd = 1)
  segments(x_drawn[moved], y_anchor + tick, x_true[moved], y_anchor + tick, col = col, lwd = 1)
  segments(x_true[moved], y_anchor + tick, x_true[moved], y_anchor, col = col, lwd = 1)

  invisible(NULL)
}
