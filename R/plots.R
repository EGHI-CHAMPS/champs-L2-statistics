#' Plot the syndrome combination tabulations from a computed CHAMPS object
#'
#' @param x A data object obtained from \code{\link{calc_syndrome_combinations}}
#' @param x_name One of the names objects from
#'   \code{\link{calc_syndrome_combinations}}
#'  - "table", "age_breakdown", or "tac_age_breakdown". Defaults to table.
#' @param legend_location Can be one of "none", "bottom", "right", "left".
#'   Defaults to none.
#' @param plot_type Either Pie Chart "pie" or Bar Chart "bar".
#' @param include_text Whether to include the names "name", percentages
#'   "percent", numbers "number", names with numbers "full_number",
#'   or names with percentages "full_percent". Defaults to NULL which is none.
#' @param full_separator the text to put between the name and full element.
#'   Defaults to a line break.
#' @details x_name sets the color scale for each of the three items. The output
#'   from this function is a ggplot object so you can alter colors if needed.
#'
#' Note that the labels may be cut off and that you would use a larger image
#'   size to include long names.
#' @importFrom ggplot2 ggplot aes geom_text theme_bw theme element_text
#'   labs ylim
#' @importFrom forcats fct_reorder fct_inorder
#' @importFrom ggforce geom_arc_bar stat_arc_bar GeomArcBar stat_pie
#' @import ggfittext
#' @importFrom stats lag
#' @export
#' @examples
#' mock_syndrome <- calc_syndrome_combinations(mock,
#' condition = "Streptococcus pneumoniae",
#' syndrome_names = c(
#'   "Lower respiratory infections",
#'   "Meningitis/Encephalitis",
#'   "Neonatal sepsis",
#'   "Congenital infection"),
#' syndrome_values = c(
#'   "Pneumonia",
#'   "Meningitis",
#'   "Sepsis",
#'   "Sepsis"),
#' specimen_types = c(
#'   "Cerebrospinal fluid sample",
#'   "Tissue specimen from lung",
#'   "Whole blood")
#' )
#'
#' plot_syndrome_combinations(mock_syndrome, legend_location = "none")
#' plot_syndrome_combinations(mock_syndrome, legend_location = "none",
#'   plot_type = "bar")
#' plot_syndrome_combinations(mock_syndrome, legend_location = "none",
#'   plot_type = "bar", include_text = "full_percent")
#' plot_syndrome_combinations(mock_syndrome, legend_location = "none",
#'   include_text = "full_percent")
#' plot_syndrome_combinations(mock_syndrome, "age_breakdown",
#'   legend_location = "none", include_text = "full_percent")
#' plot_syndrome_combinations(mock_syndrome, "tac_age_breakdown",
#'   legend_location = "none", include_text = "full_number")
#' plot_syndrome_combinations(mock_syndrome, "age_breakdown",
#'   legend_location = "none", include_text = "full_number",
#'   full_separator = ",")
#' plot_syndrome_combinations(mock_syndrome, "age_breakdown",
#'   plot_type = "pie", legend_location = "none",
#'   include_text = "full_number", full_separator = ",")
#' plot_syndrome_combinations(mock_syndrome, "tac_age_breakdown",
#'   plot_type = "bar", legend_location = "none",
#'   include_text = "full_number", full_separator = ",")
plot_syndrome_combinations <- function(x,
  x_name = "table",
  legend_location = "bottom",
  plot_type = "pie",
  include_text = NULL,
  full_separator = "\n") {

  check_champs_object(x)

  if (!any(x_name %in% c("table", "age_breakdown",
    "tac_age_breakdown")))
    stop("Expecting one of 'pie' or 'bar'")

  # check for correct x_names
  if (x_name == "table") {
    piecolors <- c("#4c98a4", "#938595", "#405b9f", "#5494c8",
      "#1e73cb", "#73849e")
    cname <- "syndrome"
  } else if (x_name == "age_breakdown") {
    piecolors <- c("#ffb82b", "#9b9b9b", "#ee6f34", "#3966b9",
      "#4d90cc", "#579bd2")
    cname <- "age_group"
  } else {
    piecolors <- c("#f1eef6", "#d4b9da", "#c994c7", "#df65b0",
      "#dd1c77", "#980043")
    cname <- "age_group"
  }

  # build the labels and pie chart elements for use in the charts below

  # handle sorting for non age group variables
  if (x_name == "table") {
    # non age group sorted by count
    d1 <- dplyr::ungroup(x[[x_name]]) %>%
      dplyr::mutate(x_plot = .data[[cname]],
        xuse = forcats::fct_reorder(.data$x_plot, .data$n, .desc = TRUE))
  } else {
    # age groups stay in table order
    d1 <- dplyr::ungroup(x[[x_name]]) %>%
      dplyr::mutate(x_plot = .data[[cname]],
        xuse = .data$x_plot)
  }
  d1 <- d1 %>%
    mutate(percent_noround = 100 * .data$n / sum(.data$n),
      percent = round(.data$percent_noround, 0),
      percent_label = dplyr::case_when(.data$percent <= 1 ~ "",
        percent <= 2 ~ paste0(.data$percent),
        TRUE ~ paste0(percent, "%")),
      full_percent = paste0(.data$x_plot, full_separator, .data$percent, "%"),
      full_number = paste0(.data$x_plot, full_separator, .data$n),
      end = 2 * pi * cumsum(.data$percent_noround / 100),
      start = dplyr::lag(.data$end, default = 0),
      middle = 0.5 * (.data$start + .data$end),
      hjust = ifelse(.data$middle > pi, 1, 0),
      vjust = ifelse(.data$middle < pi / 2 | .data$middle > 3 * pi / 2, 0, 1)
    )

  if (nrow(d1) > 6)
    stop("Including more than 6 groups is not supported")

  # labels are based on include_text argument
  # ifelse handles the NULL check issue
  if (is.null(include_text)) {
    include_text <- "none"
  } else {
    if (!any(include_text %in% c("full_number", "full_percent", "percent",
      "number", "name"))) {
      stop("Pick available options for the 'include_text' argument.")
    }
  }

  # plot_type can only be "pie" or "bar"
  if (!any(plot_type %in% c("pie", "bar")))
    stop("Expecting one of 'pie' or 'bar'")

  # Exterior labels leveraging ggforce package
  # https://ggforce.data-imaginist.com/reference/geom_arc_bar.html
  if (plot_type == "pie") {
    p_out <- d1 %>%
    ggplot2::ggplot() +
    ggforce::geom_arc_bar(ggplot2::aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
      start = .data$start, end = .data$end, fill = .data$x_plot),
      color = "grey") +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_manual(values = piecolors) +
    ggplot2::scale_x_continuous(limits = c(-1.75, 1.75), name = "",
      breaks = NULL, labels = NULL) +
    ggplot2::scale_y_continuous(limits = c(-1.75, 1.75), name = "",
      breaks = NULL, labels = NULL) +
    ggplot2::labs(fill = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = legend_location)

    # label type for pie charts
    # c("full_number", "full_percent", "percent", "number", "name")
    if (include_text == "full_percent") {
      p_out <- p_out +
        geom_text(aes(x = 1.05 * sin(.data$middle),
          y = 1.05 * cos(.data$middle),
          label = .data$full_percent, hjust = .data$hjust, vjust = .data$vjust))
    } else if (include_text == "full_number") {
      p_out <- p_out +
        geom_text(aes(x = 1.05 * sin(.data$middle),
          y = 1.05 * cos(.data$middle),
          label = .data$full_number, hjust = .data$hjust, vjust = .data$vjust))
    } else if (include_text == "percent") {
      p_out <- p_out +
        geom_text(aes(x = 1.05 * sin(.data$middle),
          y = 1.05 * cos(.data$middle),
          label = .data$percent_label, hjust = .data$hjust,
          vjust = .data$vjust))
    } else if (include_text == "number") {
      p_out <- p_out +
        geom_text(aes(x = 1.05 * sin(.data$middle),
          y = 1.05 * cos(.data$middle),
          label = .data$n, hjust = .data$hjust, vjust = .data$vjust))
    } else if (include_text == "name") {
      p_out <- p_out +
        geom_text(aes(x = 1.05 * sin(.data$middle),
          y = 1.05 * cos(.data$middle),
          label = .data$x_plot, hjust = .data$hjust, vjust = .data$vjust))
    } else {
      p_out <- p_out +
        geom_text(aes(x = 1.05 * sin(.data$middle),
          y = 1.05 * cos(.data$middle),
          label = "", hjust = .data$hjust, vjust = .data$vjust))
    }

  # bar chart section that uses ggfittext
  # https://github.com/wilkox/ggfittext
  } else {
    # label type for bar charts
    # c("full_number", "full_percent", "percent", "number", "name")
    if (include_text == "percent") {
      p_out <- d1  %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$xuse,
          label = .data$percent_label, y = .data$percent, fill = .data$xuse))
    } else if (include_text == "number") {
      p_out <- d1  %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$xuse, label = .data$n,
          y = .data$percent, fill = .data$xuse))
    } else if (include_text == "full_number") {
      p_out <- d1  %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$xuse, label = .data$full_number,
          y = .data$percent, fill = .data$xuse))
    } else if (include_text == "full_percent") {
      p_out <- d1  %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$xuse, label = .data$full_percent,
          y = .data$percent, fill = .data$xuse))
    } else if (include_text == "name") {
      p_out <- d1  %>%
        ggplot2::ggplot(ggplot2::aes(x = .data$xuse, label = .data$xuse,
          y = .data$percent, fill = .data$xuse))
    } else {
      p_out <- d1  %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data$xuse, label = "",
          y = .data$percent, fill = .data$xuse))
    }

    p_out <- p_out +
      ggplot2::geom_col() +
      ggplot2::scale_fill_manual(values = piecolors) +
      ggplot2::labs(fill = "", y = "Percentage") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid.major.x =  ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.position = legend_location)

    if (any(include_text %in% c("full_number", "full_percent", "number",
      "name", "percent"))) {
      p_out <- p_out +
        ggfittext::geom_bar_text(reflow = TRUE, contrast = TRUE)
    }
  }

  if (any(legend_location %in% c("bottom", "top"))) {
    p_out <- p_out +
      ggplot2::guides(fill = ggplot2::guide_legend("", nrow = 2, byrow = TRUE))
  }

  p_out
}



#' Plot the etiologies of a computed CHAMPS table
#'
#' @param x A data object obtained from \code{\link{calc_top_etiol_by_age}}.
#' @param etiologies The etiologies to include in the graphic with their
#'   shortened names as a named vector. Defaults to NULL. If NULL than the top
#'   variable is used and the names are shortened automatically.
#' @param top The number of etiologies to include in the graphic
#' @param bar_width defines the size of the bar the chart. Defaults to 0.5.
#' @importFrom ggplot2 ggplot aes geom_text theme_bw theme element_text
#'   labs ylim
#' @export
#' @examples
#' mock_top_etiol_by_age <- calc_top_etiol_by_age(mock,
#'   age_groups = c(
#'     "Death in the first 24 hours",
#'     "Early Neonate (24-72 hours)",
#'     "Early Neonate (72+hrs to 6 days)",
#'     "Late Neonate (7 to 27 days)"))
#'
#' plot_top_etiol_by_age(mock_top_etiol_by_age)
plot_top_etiol_by_age <- function(x,
  etiologies = NULL,
  top = 6,
  bar_width = 0.5
) {
  check_champs_object(x)

  d1 <- dplyr::ungroup(x$etiol_counts)

  # total deaths
  d1_deaths <- sum(d1$n)

  if (is.null(etiologies)) {
    common_etiol <- d1 %>%
      dplyr::group_by(.data$etiol) %>%
      dplyr::summarise(count = sum(.data$n)) %>%
      dplyr::arrange(dplyr::desc(.data$count)) %>%
      dplyr::filter(!is.na(.data$etiol)) %>%
      dplyr::slice(1:top) %>%
      dplyr::pull(.data$etiol)

    names_table <- stringr::str_split_fixed(common_etiol, pattern = " ", n = 2)

    # find the cases that don't need to be shortened.
    one_word_name <- names_table[, 2] == ""
    spp_name <- stringr::str_detect(names_table[, 2], "spp")

    # spp names shouldn't be shortened.
    names_table[, 2][spp_name] <- paste(
      names_table[, 1][spp_name],
      names_table[, 2][spp_name])

    # one word names shouldn't be shortened
    # put word in second column then make first word NA
    names_table[, 2][one_word_name] <- names_table[, 1][one_word_name]
    names_table[, 1] <- paste0(stringr::str_trunc(names_table[, 1], 1,
      ellipsis = ""), ".")

    names_table[, 1][one_word_name] <- ""
    names_table[, 1][spp_name] <-  ""

    short_names <- paste(names_table[, 1], names_table[, 2]) %>%
      stringr::str_trim()

    # build etiologies object to be like non-NULL value potentially
    # entered by user
    etiologies <- short_names
    names(etiologies) <- common_etiol
  } else {
    common_etiol <- names(etiologies)
    short_names <- etiologies
  }

  group_colors <- c("#405b9f", "#5494c8", "#1e73cb", "#73849e")

  p1 <- d1 %>%
    dplyr::filter(.data$etiol %in% common_etiol) %>%
    dplyr::mutate(eshort = etiologies[.data$etiol]  %>%
      forcats::fct_reorder(.data$n, .fun = sum, .desc = TRUE)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$eshort, y = .data$n,
      fill = .data$age_group_subcat)) +
    ggplot2::geom_col(width = bar_width) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = group_colors) +
    ggplot2::scale_y_continuous(breaks = seq(0, d1_deaths, by = 20)) +
    ggplot2::labs(y = "", x = "", fill = "",
      title = paste0("Common Etiologies of CHAMPS\n",
        "Neonatal Infectious Deaths: All Sites (N=", d1_deaths, ")"))

  p1
}

#' Plot the margins (row and column totals) of a computed CHAMPS table
#'
#' @param x A computed CHAMPS object that includes a numerator and
#'   denominator of counts.
#' @param include_text Whether to include the names "name",
#'   percentages "percent", fraction "fraction", names with fraction
#'   "full_fraction", or names with percentages "full_percent".
#'   Defaults to NULL which is none.
#' @param include_x_label Whether or not to include x-axis labels.
#'   Defaults to TRUE.
#' @param plot_title is the title to include above the two charts.
#' @param reorder_left whether to sort the left chart's non-total x values
#'   by percent TRUE or not FALSE. Defaults to TRUE
#' @param reorder_right whether to sort the right chart's non-total x values
#'   by percent TRUE or not FALSE. Defaults to FALSE
#' @param full_separator the text to put between the name and full element.
#'   Defaults to a line break.
#' @importFrom ggplot2 ggplot aes geom_text theme_minimal theme
#'   element_text labs ylim
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @importFrom utils tail
#' @export
#' @examples
#' specimen_types = c(
#'   "Cerebrospinal fluid sample",
#'   "Tissue specimen from lung",
#'   "Whole blood",
#'   "Rectal swab",
#'   "Plasma or spun blood specimen")
#'
#' mock_dcd <- calc_cc_allcases_by_site_age(mock,
#'   condition =  "Streptococcus pneumoniae")
#' mock_tac <- calc_detected_allcases_by_site_age(mock,
#'   condition =  "Streptococcus pneumoniae",
#'   pathogen = "Group B Streptococcus")
#' mock_both <- calc_cc_detected_by_site_age(mock,
#'   condition = "Streptococcus agalactiae",
#'   pathogen = "Group B Streptococcus",
#'   specimen_types = specimen_types)
#' plot_margins_site_age(mock_tac, include_x_label = FALSE,
#'   include_text = "full_fraction",
#'   plot_title = "TAC Marginal Distributions for Streptococcus pneumoniae")
#'
#' plot_margins_site_age(mock_dcd, include_x_label = TRUE,
#'   include_text = "fraction",
#'   plot_title = "DeCoDe Marginal Distribution for Streptococcus pneumoniae")
#'
#' plot_margins_site_age(mock_both, include_x_label = FALSE,
#'   include_text = "full_fraction",
#'   plot_title = "DeCoDe S. pneumoniae where TAC contains S. pneumoniae")
plot_margins_site_age <- function(x,
  include_text = NULL,
  include_x_label = TRUE,
  plot_title = "",
  reorder_left = TRUE,
  reorder_right = FALSE,
  full_separator = "\n"
) {

  numerator_name <- "Site"
  denominator_name <- "Age Group"

  check_champs_object(x)

  if (is.null(x$numerator) || is.null(x$denominator))
    stop("Currently only tables can be output that have both",
      " a numerator and denominator.")

  get_data <- function(nx, dx, nms, which) {
    tibble::tibble(
      pct = as.vector(unname(100 * nx / dx)),
      var = forcats::fct_relevel(nms, nms),
      txt = paste(nx, dx, sep = "/"),
      txt_long = paste(.data$var, .data$txt, sep = full_separator),
      pct_long = paste(.data$var, round(.data$pct, 1), sep = full_separator),
      which = which,
      color = dplyr::case_when(.data$var == "Total" ~ "darkgrey",
        TRUE ~ "black")
    )
  }

  nn <- x$numerator
  dd <- x$denominator

  nms <- colnames(dd)
  nms[length(nms)] <- "Total"
  nx <- utils::tail(nn, 1)
  dx <- utils::tail(dd, 1)

  d1 <- get_data(nx, dx, nms, numerator_name)

  nms <- rownames(dd)
  nms[length(nms)] <- "Total"
  nx <- nn[, ncol(nn)]
  dx <- dd[, ncol(dd)]

  d2 <- get_data(nx, dx, nms, denominator_name)

  ylims <- range(c(d1$pct, d2$pct, 0))
  ylims[2] <- ylims[2] + 0.3

  # labels are based on include_text argument
  # ifelse handles the NULL check issue
  if (is.null(include_text)) {
    include_text <- "none"
  } else {
    if (!any(include_text %in% c("full_fraction", "full_percent",
      "percent", "fraction", "name"))) {
      stop("Pick available options for the 'include_text' argument.")
    }
  }

  if (include_text == "name") {
    d1 <- dplyr::mutate(d1, label_var = .data$var)
    d2 <- dplyr::mutate(d2, label_var = .data$var)
  } else if (include_text == "percent") {
    d1 <- dplyr::mutate(d1, label_var = paste(.data$pct, "%"))
    d2 <- dplyr::mutate(d2, label_var = paste(.data$pct, "%"))
  } else if (include_text == "fraction") {
    d1 <- dplyr::mutate(d1, label_var = .data$txt)
    d2 <- dplyr::mutate(d2, label_var = .data$txt)
  } else if (include_text == "full_percent") {
    d1 <- dplyr::mutate(d1, label_var = .data$pct_long)
    d2 <- dplyr::mutate(d2, label_var = .data$pct_long)
  } else if (include_text == "full_fraction") {
    d1 <- dplyr::mutate(d1, label_var = .data$txt_long)
    d2 <- dplyr::mutate(d2, label_var = .data$txt_long)
  } else {
    d1 <- dplyr::mutate(d1, label_var = "")
    d2 <- dplyr::mutate(d2, label_var = "")
  }

  if (reorder_left) d1 <- dplyr::mutate(d1,
    var = forcats::fct_reorder(.data$var, .data$pct) %>%
      forcats::fct_relevel("Total", after = Inf))

  if (reorder_right) d2 <- dplyr::mutate(d2,
    var = forcats::fct_reorder(.data$var, .data$pct) %>%
      forcats::fct_relevel("Total", after = Inf))

  p1 <- ggplot2::ggplot(d1, ggplot2::aes(.data$var, .data$pct,
    label = .data$label_var,
    fill = .data$color)) +
    ggplot2::geom_col() +
    ggfittext::geom_bar_text(reflow = TRUE, contrast = TRUE) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::labs(y = "Percent", x = NULL, title = numerator_name) +
    ggplot2::ylim(ylims)

  p2 <- ggplot2::ggplot(d2, ggplot2::aes(.data$var, .data$pct,
      label = .data$label_var, fill = .data$color)) +
    ggplot2::geom_col() +
    ggfittext::geom_bar_text(reflow = TRUE, contrast = TRUE) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()) +
    ggplot2::labs(y = "Percent", x = NULL, title = denominator_name) +
    ggplot2::ylim(ylims)
  if (!include_x_label) {
    p1 <- p1 +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank())
    p2 <- p2 +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank())
  }

  plot_out <- cowplot::plot_grid(p1, p2, align = "h", axis = "b")

  # now add the title
  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      plot_title,
      fontface = "bold",
      x = 0,
      hjust = 0
    ) +
    ggplot2::theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = ggplot2::margin(0, 0, 0, 7)
    )

 cowplot::plot_grid(
    title, plot_out,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
  )
}

#' Plot the percents of a computed CHAMPS table
#'
#' @param x A computed CHAMPS object that includes a numerator and
#'   denominator of counts.
#' @param include_text Whether to include the percentages "percent", or
#' fraction "fraction". Defaults to NULL which is none.
#' @param plot_title is the title for the plot.
#' @param plot_subtitle is the subtitle for the plot.
#' @export
#' @examples
#' mock_calc <- calc_cc_allcases_by_site_age(mock,
#'   condition = 'Streptococcus agalactiae')
#' heatmap_site_age(mock_calc, include_text = "percent",
#'   plot_title = "TAC Results", plot_subtitle = "Streptococcus pneumoniae")
#' heatmap_site_age(mock_calc, include_text = "fraction",
#'   plot_title = "TAC Results", plot_subtitle = "Streptococcus pneumoniae")
heatmap_site_age <- function(x,
  plot_title = "",
  plot_subtitle = "",
  include_text = "percent"
) {
  check_champs_object(x)

  if (is.null(x$numerator) || is.null(x$denominator))
    stop("Currently only tables can be output that have both",
      " a numerator and denominator.")

  d_tidy <-  tibble::as_tibble(x$numerator, n = "numerator") %>%
    dplyr::rename_all(~stringr::str_remove(., pattern = "^.*\\$")) %>%
    dplyr::left_join(tibble::as_tibble(x$denominator, n = "denominator") %>%
      dplyr::rename_all(~stringr::str_remove(., pattern = "^.*\\$")),
        by = c("age_group", "site")) %>%
    dplyr::mutate(proportion = .data$numerator / .data$denominator) %>%
    dplyr::mutate_if(is.character, forcats::fct_inorder) %>%
    dplyr::mutate(print_percent =
      stringr::str_c(round(100 * .data$proportion, 1), "%"),
      print_fraction = stringr::str_c(.data$numerator, "/", .data$denominator))
  # labels are based on include_text argument
  # ifelse handles the NULL check issue
  if (is.null(include_text)) {
    include_text <- "none"
  } else {
    if (!any(include_text %in% c("percent", "fraction"))) {
      stop("Pick available options for the 'include_text' argument.")
    }
  }

  if (include_text == "percent") {
    d_tidy <- dplyr::mutate(d_tidy, label_var = .data$print_percent)
  } else if (include_text == "fraction") {
    d_tidy <- dplyr::mutate(d_tidy, label_var = .data$print_fraction)
  } else {
    d_tidy <- dplyr::mutate(d_tidy, label_var = "")
  }

  d_tidy %>%
    dplyr::mutate(age_group = forcats::fct_rev(.data$age_group)) %>%
    ggplot2::ggplot(aes(x = .data$site, y = .data$age_group,
      fill = .data$proportion)) +
    ggplot2::geom_tile(color = "white", show.legend = FALSE) +
    ggplot2::geom_text(aes(label = .data$label_var), color = "white") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = "", y = "",
         title = plot_title,
         subtitle = plot_subtitle) +
    ggplot2::theme_minimal()
}
