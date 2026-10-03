# Display encodings follow the manuscript: communities retain their colors;
# focal outcomes are diamonds, and the active comparison has dark outlines.
network_display <- function(network, metadata, focal, neighborhood, gss_example = FALSE) {
  ids <- colnames(network$adjacency)
  community <- as.character(network$membership[ids])
  groups <- sort(unique(community))
  palette <- c("#86A5C3", "#C2A16D", "#AA8BB7", "#76AAA2")
  if (length(groups) > length(palette))
    palette <- c(palette, grDevices::hcl.colors(length(groups) - length(palette), "Dark 3"))
  colors <- stats::setNames(palette[seq_along(groups)], groups)
  role <- ifelse(ids %in% focal, "Focal outcome",
    ifelse(ids %in% neighborhood, "Unreported neighbor", "Other analyzable outcome"))
  field <- function(name) {
    if (!name %in% names(metadata)) return(rep("", length(ids)))
    values <- as.character(metadata[[name]][match(ids, metadata$item)])
    values[is.na(values)] <- ""
    values
  }
  wrap <- function(x) paste(htmltools::htmlEscape(strwrap(x, width = 58)), collapse = "<br>")
  labels <- field("label"); labels[!nzchar(labels)] <- ids[!nzchar(labels)]
  position <- rep("top center", length(ids))
  if (isTRUE(gss_example)) {
    # Label placement uses the fixed archived map, never fitted predictor results.
    positions <- c(Happy="top right", Trust="middle left", Fair="bottom left",
      Helpful="middle left", Life="top right", Hapmar="top right", Health="bottom right",
      Finrela="bottom center", Finalter="middle right", Satjob="bottom right", News="middle left",
      Natheal="top left", Natcity="middle right", Nateduc="top left", Natrace="bottom left",
      Natfare="middle right", Natenvir="bottom left", Natarms="top left", Natdrug="top left",
      Premarsx="middle left", Homosex="bottom left", Xmarsex="middle right", Pornlaw="middle right",
      Courts="bottom right", Reliten="top left", Attend="bottom center", Getahead="bottom right",
      Coneduc="top left", Contv="middle left", Conpress="bottom left", Conmedic="bottom left",
      Confinan="bottom left", Conjudge="bottom left", Conbus="bottom center", Consci="bottom right",
      Confed="middle right", Conclerg="top right")
    at <- match(ids, names(positions)); position[!is.na(at)] <- positions[at[!is.na(at)]]
  }
  hover <- vapply(seq_along(ids), function(i) {
    parts <- c(paste0("<b>", htmltools::htmlEscape(ids[i]), "</b> · ", wrap(labels[i])),
      paste("Community", htmltools::htmlEscape(community[i]), "·", role[i]))
    for (entry in list(c("question", "Question"), c("response_options", "Responses"),
        c("respondent_scope", "Respondents"), c("coding_note", "Display coding"))) {
      value <- field(entry[1])[i]
      if (nzchar(value)) parts <- c(parts, paste0("<b>", entry[2], ":</b> ", wrap(value)))
    }
    paste(parts, collapse = "<br>")
  }, character(1))
  data.frame(item = ids, community = community, role = role,
    color = unname(colors[community]), symbol = ifelse(ids %in% focal, "diamond", "circle"),
    size = ifelse(ids %in% focal, 18, ifelse(ids %in% neighborhood, 12, 10)),
    outline = ifelse(ids %in% neighborhood, "#365D61", "#FFFFFF"),
    outline_width = ifelse(ids %in% neighborhood, 2, .5),
    label = ifelse(ids %in% focal, paste0("<b>", htmltools::htmlEscape(ids), "</b>"), htmltools::htmlEscape(ids)),
    position = position, hover = hover, stringsAsFactors = FALSE)
}
