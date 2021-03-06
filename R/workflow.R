#' A workflow
#'
#' @param ... Extra arguments to the call, such as inputs, outputs, script, ...
#' @rdname workflow
Workflow <- R6Class(
  "Workflow",
  public = list(
    calls = list(),
    workflow_graph = NULL,
    call_dependencies = NULL,
    execution = NULL,
    initialize = function(...) {
      self$calls <- map(list(...), "calls") %>% flatten()

      # check call ids
      call_ids <- map(self$calls, "id")
      if (any(duplicated(call_ids))) {
        stop("Duplicated call ids: ", unique(call_ids[duplicated(call_ids)]) %>% glue::glue_collapse(", "))
      }

      # create the tibble of different calls and their status
      self$execution <- tibble(
        call = self$calls
      ) %>%
        mutate(id = map_chr(call, "id"))

      # get all (non-duplicated) objects
      objects_dupl <- map(self$calls, function(call) {
        c(call$inputs %>% map("individual") %>% flatten(), call$outputs %>% map("individual") %>% flatten())
      }) %>% flatten() %>% unname()
      object_ids <- objects_dupl %>% map_chr("id")
      objects <- objects_dupl[!duplicated(object_ids)]

      # create nodes of the workflow, which can be calls or objects
      workflow_nodes <- bind_rows(
        tibble(
          object = self$calls,
          type = "call"
        ),
        tibble(
          object = objects,
          type = "object"
        )
      ) %>%
        mutate(
          id = map_chr(object, "id")
        ) %>%
        select(id, type, object)

      # create the directed network between the nodes
      workflow_network <- self$calls %>% map(function(call) {
        workflow_network <- bind_rows(
          tibble(
            from = call$inputs %>% map("individual") %>% flatten() %>% map_chr("id"),
            to = call$id
          ),
          tibble(
            from = call$id,
            to = call$outputs %>% map("individual") %>% flatten() %>% map_chr("id")
          )
        )

        workflow_network
      }) %>% bind_rows()

      requireNamespace("igraph", quietly = TRUE)

      # create the workflow graph
      self$workflow_graph <- workflow_graph <- igraph::graph_from_data_frame(workflow_network, vertices = workflow_nodes) %>%
        tidygraph::as_tbl_graph()

      # get the dependencies between calls
      self$call_dependencies <- tibble(
        id = workflow_nodes %>% filter(type == "call") %>% pull(id)
      ) %>%
        mutate(
          dependencies = igraph::ego(workflow_graph, id, order = 99999999, mode = "in", mindist = 1) %>% map(names)
        ) %>%
        tidyr::unnest(dependency = dependencies) %>%
        filter(dependency %in% (workflow_nodes %>% filter(type == "call") %>% pull(id)))
    },
    plot = function() {
      requireNamespace("ggraph")

      layout <- igraph::layout_with_sugiyama(self$workflow_graph)
      workflow_graph <- self$workflow_graph %>%
        mutate(x = layout$layout[, 1], y =  layout$layout[, 2])

      workflow_graph %>%
        ggraph::ggraph() +
        ggraph::geom_node_point() +
        ggraph::geom_edge_link() +
        ggraph::geom_node_label(aes(label = name, fill = type)) +
        ggraph::theme_graph()
    },
    run = function(poll_time = 0.1) {
      self$execution <- self$execution %>% mutate(status = map_chr(call, "status"))

      execution_running <- tibble(id = character(), call = list())
      execution_pending <- self$execution
      execution_finished <- tibble(id = character(), status = character())

      while(nrow(execution_pending) > 0 || nrow(execution_running) > 0) {
        # end successful processes
        execution_finished_now <- execution_running %>%
          mutate(status = map_chr(call, "status")) %>%
          filter(status != "running")

        if (nrow(execution_finished_now) > 0) {
          execution_finished_now %>%
            pull(call) %>%
            map("wait") %>%
            invoke_map()

          execution_finished <- bind_rows(execution_finished, execution_finished_now)
          execution_running <- execution_running %>% filter(!id %in% execution_finished_now$id)
        }

        # get ready processes (with inputs which were succesful)
        execution_ready <- self$call_dependencies %>%
          filter(id %in% execution_pending$id) %>%
          left_join(execution_finished, c("dependency"="id")) %>%
          group_by(id) %>%
          summarise(
            status = case_when(
              any(is.na(status)) ~ "pending",
              any(status == "failed") ~ "dependency_error",
              all(status %in% c("cached", "succeeded")) ~ "ready",
              TRUE ~ "running"
            )
          ) %>%
          tidyr::complete(id = execution_pending$id, fill = list(status = "ready")) %>%
          filter(status == "ready") %>%
          left_join(execution_pending %>% select(id, call), "id")

        # start ready processes
        execution_ready$call %>% map("start") %>% invoke_map()
        execution_running <- bind_rows(
          execution_running,
          execution_ready
        )
        execution_pending <- execution_pending %>% filter(!id %in% execution_ready$id)

        Sys.sleep(poll_time)
      }

      # get status of all calls and return if all successful
      self$execution <- self$execution %>% mutate(status = map_chr(call, "status"))
      if (all(self$execution$status %in% c("cached", "succeeded"))) {
        cat_rule(crayon_ok("\U2714 Workflow successfully executed"))
      } else {
        cat_rule(crayon_error("\U2714 Some errors during workflow execution"))
      }
    },
    reset = function() {
      map(self$calls, "reset") %>% invoke_map()
      invisible()
    }
  )
)

#' @export
#' @rdname workflow
workflow <- Workflow$new








# plot_workflow <- function() {
#   requireNamespace("tidygraph")
#   requireNamespace("ggraph")
#
#   # create the links between objects and calls
#   workflow_network <- self$calls %>% pmap_df(function(id, call, ...) {
#     workflow_network <- bind_rows(
#       tibble(
#         from = call$input_ids,
#         to = id
#       ),
#       tibble(
#         from = id,
#         to = call$output_ids
#       )
#     )
#
#     workflow_network
#   })
#
#   # create the nodes = objects and calls
#   workflow_nodes <- bind_rows(
#     self$calls %>%
#       mutate(status = map(call, "call_status") %>% invoke_map_chr(runs_exited = self$runs_exited)) %>%
#       rename(object = call),
#     self$objects %>%
#       mutate(status = map(object, "status") %>% invoke_map_chr())
#   ) %>%
#     mutate(
#       label = map_chr(object, "label")
#     )
#
#   # construct the graph
#   workflow_graph <- igraph::graph_from_data_frame(workflow_network, vertices = workflow_nodes) %>%
#     tidygraph::as_tbl_graph()
#
#   # add fontawesome font if not available yet
#   load_fontawesome()
#
#   # status colors
#   status_colors <- c(finished = "#3D9970", present = "#2ECC40", not_present = "#FFDC00", unfinished = "#FF851B")
#   scale_color_status <- scale_color_manual(values = status_colors, limits = names(status_colors))
#
#   # get layout
#   layout <- igraph::layout_with_sugiyama(workflow_graph)
#   workflow_graph <- workflow_graph %>%
#     mutate(x = layout$layout[, 1], y =  layout$layout[, 2])
#
#   workflow_graph %>%
#     mutate(label = label) %>%
#     ggraph::ggraph(x = workflow_graph$x, y = workflow_graph$y) +
#     ggraph::geom_edge_fan(color = "lightgrey") +
#     geom_label(mapping = aes(x = x, y = y, label = label, color = status), size = 5, family="Font Awesome 5 Free", label.size = 0) +
#     geom_text(mapping = aes(x = x, y = y, label = name), color = "black", vjust = 2, size = 4) +
#     scale_x_continuous(expand = c(0.5, 0)) +
#     scale_color_status +
#     ggraph::theme_graph() +
#     theme(legend.position = "top")
# }
