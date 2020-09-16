get_table <- function(
  x, digits = 1,
  margin_pct = TRUE,
  margin_denom = FALSE,
  inside_pct = FALSE,
  inside_denom = FALSE
) {
  if (is.null(x$numerator) || is.null(x$denominator))
    stop("Currently only tables can be output that have both",
      " a numerator and denominator.")

  nn <- x$numerator
  dd <- x$denominator

  nms <- colnames(nn)
  ncol <- length(nms)
  col_tots <- dd[nrow(dd), ]
  nms <- paste0(nms, " (n=", col_tots, ")")
  nms[ncol] <- "Total"

  margin_pct <- TRUE
  digits <- 1
  tbl_pars <- lapply(seq_along(nms), function(ii) {
    res <- unname(nn[, ii])
    if (nms[ii] == "Total") {
      if (margin_denom) {
        res <- paste0(res, "/", dd[, ii])
      }
      if (margin_pct) {
        pct <- round(100 * nn[, ii] / dd[, ii], digits)
        res <- paste0(res, " (", pct, "%)")
      }
    } else {
      n <- length(res)
      ss <- seq_len(n - 1)
      if (inside_denom) {
        res[ss] <- paste0(res[ss],
          ifelse(dd[ss, ii] == 0, "", "/"),
          ifelse(dd[ss, ii] == 0, "", dd[ss, ii]))
      }
      if (margin_denom) {
        res[n] <- paste0(res[n],
          ifelse(dd[n, ii] == 0, "", "/"),
          ifelse(dd[n, ii] == 0, "", dd[n, ii]))
      }
      if (margin_pct) {
        pct <- round(100 * nn[n, ii] / dd[n, ii], digits)
        res[n] <- paste0(res[n], " (", pct, "%)")
      }
      if (inside_pct) {
        pct <- round(100 * nn[ss, ii] / dd[ss, ii], digits)
        res[ss] <- paste0(res[ss], " (", pct, "%)")
      }
    }
    res
  })

  names(tbl_pars) <- nms

  rnms <- rownames(nn)
  nrow <- length(rnms)
  rnms[nrow] <- "Total"
  row_tots <- dd[, ncol(dd)]
  rnms <- paste0(rnms, " (n=", row_tots, ")")

  tbl_pars <- c(list(`Case Type` = rnms), tbl_pars)

  do.call(tibble::tibble, tbl_pars)
}

#' Create an html table of a computed CHAMPS object
#'
#' @param x A computed CHAMPS object that contains a numerator and
#'   denominator of counts.
#' @param margin_pct Should percentages be printed in the margins?
#' @param margin_denom Should denominators be printed in the margins?
#' @param inside_pct Should percentages be printed inside the table?
#' @param inside_denom Should denominators be printed inside the table?
#' @importFrom htmlTable htmlTable
#' @export
#' @examples
#' mock_calc <- calc_cc_allcases_by_site_age(mock,
#'   condition = 'Streptococcus agalactiae')
#' html_table_site_age(mock_calc)
html_table_site_age <- function(x,
  margin_pct = TRUE,
  margin_denom = FALSE,
  inside_pct = FALSE,
  inside_denom = FALSE
) {
  check_champs_object(x)

  if (is.null(x$numerator) || is.null(x$denominator))
    stop("Currently only tables can be output that have both",
      " a numerator and denominator.")

  tbl <- get_table(x,
    margin_pct = margin_pct,
    margin_denom = margin_denom,
    inside_pct = inside_pct,
    inside_denom = inside_denom
  )
  htmlTable::htmlTable(tbl,
    rnames = FALSE,
    total = TRUE,
    align = c("l", rep("c", ncol(tbl) - 1)),
    css.table = "font-family: Lato",
    css.cell = "padding: 5px;"
  )
}

# # doc <- officer::read_pptx()
#
# #' Print a computed CHAMPS table to a PowerPoint presentation
# #'
# #' @param x A computed CHAMPS object.
# #' @param doc A PowerPoint presentation object.
# #' @param margin_pct Should percentages be printed in the margins?
# #' @param margin_denom Should denominators be printed in the margins?
# #' @param inside_pct Should percentages be printed inside the table?
# #' @param inside_denom Should denominators be printed inside the table?
# #' @importFrom officer add_slide ph_with ph_location_type
# #' @export
# write_ppt_slide <- function(x, doc,
#   margin_pct = TRUE,
#   margin_denom = FALSE,
#   inside_pct = FALSE,
#   inside_denom = FALSE
# ) {
#   check_champs_object(x)
#
#   tbl <- get_table(x,
#     margin_pct = margin_pct,
#     margin_denom = margin_denom,
#     inside_pct = inside_pct,
#     inside_denom = inside_denom
#   )
#
#   doc <- officer::add_slide(doc)
#   officer::ph_with(x = doc, value = tbl,
#     location = officer::ph_location_type(type = "body"))
# }
#
#

# #' Experimental html table
# #'
# #' @param x Object of class "cc_allcases_by_site_casetype".
# #' @importFrom grDevices colorRampPalette
# #' @importFrom formattable formatter
# #' @importFrom formattable style
# #' @importFrom kableExtra kable
# #' @importFrom kableExtra kable_styling
# #' @importFrom kableExtra add_header_above
# #' @importFrom kableExtra row_spec
# #' @importFrom kableExtra column_spec
# #' @importFrom kableExtra save_kable
# #' @importFrom utils browseURL
# #' @export
# write_html_table_cc_allcases_by_site_age <- function(x) {
#   if (!inherits(x, "cc_allcases_by_site_age"))
#    stop("Experimental table only works for cc_allcases_by_site_age")
#
#   nn <- as.data.frame.matrix(x$numerator)
#   dd <- as.data.frame.matrix(x$denominator)
#   nms <- names(nn)
#   nms[nms == "Sum"] <- "Total"
#   nr <- nrow(nn)
#   nc <- ncol(nn)
#
#   a <- unlist(nn) / unlist(dd)
#   a[is.nan(a)] <- 0
#   rng <- range(a)
#   cols <- grDevices::colorRampPalette(c("white", "red"))(101)
#
#   tbl <- lapply(seq_len(nc), function(ii) {
#     lapply(seq_len(nr), function(jj) {
#       list(nn = nn[jj, ii], dd = dd[jj, ii])
#     })
#   })
#   names(tbl) <- nms
#   tbl <- dplyr::as_tibble(tbl)
#   rnms <- rownames(nn)
#   rnms[length(rnms)] <- "Total"
#   tbl <- dplyr::bind_cols(dplyr::tibble(`Case Type` = rnms), tbl)
#
#   fn <- function(x) {
#     sapply(x, function(a) paste0(a$nn, "/", a$dd))
#   }
#
#   fmt <- formattable::formatter("span",
#     style = function(x) {
#       nns <- sapply(x, function(a) a$nn)
#       dds <- sapply(x, function(a) a$dd)
#       vals <- floor(100 * (nns / dds) / rng[2]) + 1
#       vals[is.nan(vals)] <- 1
#       formattable::style(
#         display = "block",
#         padding = "0 4px",
#         color = "black !important",
#         `text-align` = "right",
#         `border-radius` = "2px",
#         `background-color` = cols[vals])
#     },
#     `data-toggle` = "popover",
#     `data-trigger` = "hover",
#     `data-placement` = "right",
#     `data-content` = function(x) {
#       nns <- sapply(x, function(a) a$nn)
#       dds <- sapply(x, function(a) a$dd)
#       vals <- round(100 * (nns / dds), 1)
#       vals[is.nan(vals)] <- 0
#       paste0(vals, "%")
#     },
#     fn
#   )
#
#   tbl %>%
#     dplyr::mutate_at(nms, fmt) %>%
#     kableExtra::kable("html", escape = FALSE) %>%
#     kableExtra::kable_styling(c("hover", "condensed"), full_width = FALSE) %>%
#     kableExtra::column_spec(2:ncol(tbl), width = "120px") %>%
#     kableExtra::add_header_above(c(" ", "Site" = ncol(tbl) - 2, " ")) %>%
#     kableExtra::row_spec(0, align = "c") %>%
#     kableExtra::row_spec(nrow(tbl), bold = TRUE,
#       extra_css = "border-top: 1px solid rgb(119, 119, 119);") %>%
#     kableExtra::column_spec(ncol(tbl), bold = TRUE, border_left = TRUE) %>%
#     kableExtra::column_spec(1, border_right = TRUE) %>%
#     print_and_browse()
# }
#
# print_and_browse <- function(x) {
#   x <- paste0("<script>
# $(document).ready(function() {
#     $('[data-toggle=\"popover\"]').popover();
# });
# </script>
# ", x)
#   tf <- tempfile(fileext = ".html")
#   kableExtra::save_kable(x, file = tf, self_contained = TRUE)
#   utils::browseURL(tf)
# }
