plot_workflow <- function() {
  requireNamespace("tidygraph")
  requireNamespace("ggraph")

  # create the links between objects and calls
  workflow_network <- self$calls %>% pmap_df(function(id, call, ...) {
    workflow_network <- bind_rows(
      tibble(
        from = call$input_ids,
        to = id
      ),
      tibble(
        from = id,
        to = call$output_ids
      )
    )

    workflow_network
  })

  # create the nodes = objects and calls
  workflow_nodes <- bind_rows(
    self$calls %>%
      mutate(status = map(call, "call_status") %>% invoke_map_chr(runs_exited = self$runs_exited)) %>%
      rename(object = call),
    self$objects %>%
      mutate(status = map(object, "status") %>% invoke_map_chr())
  ) %>%
    mutate(
      label = map_chr(object, "label")
    )

  # construct the graph
  workflow_graph <- igraph::graph_from_data_frame(workflow_network, vertices = workflow_nodes) %>%
    tidygraph::as_tbl_graph()

  # add fontawesome font if not available yet
  load_fontawesome()

  # status colors
  status_colors <- c(finished = "#3D9970", present = "#2ECC40", not_present = "#FFDC00", unfinished = "#FF851B")
  scale_color_status <- scale_color_manual(values = status_colors, limits = names(status_colors))

  # get layout
  layout <- igraph::layout_with_sugiyama(workflow_graph)
  workflow_graph <- workflow_graph %>%
    mutate(x = layout$layout[, 1], y =  layout$layout[, 2])

  workflow_graph %>%
    mutate(label = label) %>%
    ggraph::ggraph(x = workflow_graph$x, y = workflow_graph$y) +
    ggraph::geom_edge_fan(color = "lightgrey") +
    geom_label(mapping = aes(x = x, y = y, label = label, color = status), size = 5, family="Font Awesome 5 Free", label.size = 0) +
    geom_text(mapping = aes(x = x, y = y, label = name), color = "black", vjust = 2, size = 4) +
    scale_x_continuous(expand = c(0.5, 0)) +
    scale_color_status +
    ggraph::theme_graph() +
    theme(legend.position = "top")
}
