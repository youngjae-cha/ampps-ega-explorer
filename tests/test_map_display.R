# Preserve the full empirical map while changing only comparison emphasis.
app_env <- new.env(parent = globalenv())
sys.source("app.R", envir = app_env, chdir = TRUE)
shiny::testServer(app_env$server, {
  session$setInputs(source_mode = "demo", focal = c("Happy", "Trust", "Fair"),
    boundary = "community", result_scope = "local", plot_metric = "abs_t",
    multiplicity = FALSE, interpretation = "", annotation_focal = "Happy")
  a <- result()
  nodes <- app_env$network_display(a$network, a$metadata, focal(), neighborhood())
  stopifnot(nrow(nodes) == 44L, length(unique(nodes$color)) == 4L,
    identical(sort(nodes$item[nodes$symbol == "diamond"]), sort(focal())),
    sum(nodes$outline_width == 2 & nodes$symbol == "circle") == 8L,
    all(nzchar(nodes$label)), inherits(output$network_plot, "json"))
  for (id in c("Happy", "Trust", "Fair", "Health", "Life", "Hapmar", "Satjob"))
    stopifnot(grepl("<b>Question:</b>", nodes$hover[nodes$item == id], fixed = TRUE),
      grepl("<b>Responses:</b>", nodes$hover[nodes$item == id], fixed = TRUE))
  baseline <- a$landscape; coordinates <- a$network$coordinates
  session$setInputs(boundary = "ring25")
  ring <- app_env$network_display(result()$network, result()$metadata, focal(), neighborhood())
  stopifnot(identical(nodes$color, ring$color), identical(nodes$label, ring$label),
    identical(coordinates, result()$network$coordinates), identical(baseline, result()$landscape),
    sum(ring$outline_width == 2) == 14L)
  archive <- tempfile(fileext = ".zip"); stage <- tempfile(); dir.create(stage)
  app_env$export_bundle(archive, payload())
  utils::unzip(archive, exdir = stage)
  exported <- read.csv(file.path(stage, "item_annotations.csv"))
  stopifnot(sum(nzchar(exported$question)) == 11L,
    all(c("response_options", "wording_source") %in% names(exported)))
  unlink(c(archive, stage), recursive = TRUE)
})
# Uploaded names and markup must not borrow GSS wording or enter the tooltip as HTML.
net <- app_env$DEMO_NETWORK
meta <- data.frame(item = colnames(net$adjacency), label = "<script>test</script>")
x <- app_env$network_display(net, meta, "Happy", "Happy")
stopifnot(all(grepl("&lt;script&gt;", x$hover, fixed = TRUE)),
  !any(grepl("<b>Question:</b>", x$hover, fixed = TRUE)))
cat("PASS: four stable community colors; 44 labels; three diamonds; eight outlined neighbors; supplied item wordings in tooltips and export; unchanged fits and coordinates; escaped upload labels.\n")
