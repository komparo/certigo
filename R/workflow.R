plot_workflow <- function() {
  requireNamespace("tidygraph")
  requireNamespace("ggraph")
  requireNamespace("sysfonts")

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
  if(!"fontawesome" %in% sysfonts::font_families()) {
    sysfonts::font_add("fontawesome", system.file("fonts/fontawesome_5_solid.otf", package = "certigo"))
  }

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
    geom_label(mapping = aes(x = x, y = y, label = label, color = status), size = 5, family="fontawesome", label.size = 0) +
    geom_text(mapping = aes(x = x, y = y, label = name), color = "black", vjust = 2, size = 4) +
    scale_x_continuous(expand = c(0.5, 0)) +
    scale_color_status +
    ggraph::theme_graph() +
    theme(legend.position = "top")
}

calculate_object_dependencies <- function(calls) {
  object_dependency_network <- calls$call %>% map(function(call) {
    tidyr::crossing(
      from = call$input_ids,
      to = call$output_ids
    )
  }) %>%
    bind_rows() %>%
    igraph::graph_from_data_frame()

  igraph::connect(object_dependency_network, 9999999) %>%
    igraph::as_data_frame() %>%
    group_by(from) %>%
    summarise(to = list(to)) %>%
    deframe()
}




#' A certigo workflow
#' @export
Workflow <- R6Class(
  "Workflow",
  public = list(
    calls = NULL,
    objects = NULL,
    runs_exited = NULL,
    runs_active = tibble(id = character(), process = list()),
    initialize = function(calls_raw, runs_exited = tibble(id = character(), digest = character())) {
      # create calls tibble
      self$calls <- set_names(calls_raw, map_chr(calls_raw, "id")) %>% enframe("id", "call")

      # create objects from the calls, removing duplicates
      self$objects <- self$calls$call %>%
        map("objects") %>%
        unlist() %>%
        tibble(object = .) %>%
        mutate(id = map_chr(object, "id")) %>%
        distinct(id, .keep_all = TRUE)

      # add the objects to the inputs and outputs of call
      self$calls$call <- map(self$calls$call, function(call) {
        call$objects <- NULL
        call$inputs <- self$objects %>% slice(match(call$input_ids, id)) %>% pull(object)
        call$outputs <- self$objects %>% slice(match(call$output_ids, id)) %>% pull(object)
        call
      })

      # add runs_exited
      self$runs_exited <- runs_exited

      # precalculate the dependencies between all inputs towards other inputs and outputs
      self$object_dependencies <- calculate_object_dependencies(self$calls)
    },

    run_calls = function() {
      # start the ready runs first
      self$start_ready_runs()

      # start polling for whether the runs have exited
      while(nrow(self$runs_active) > 0) {
        Sys.sleep(0.1)

        runs_exited_current <- self$poll()

        # when runs have exited, check whether new runs need to be started
        if (nrow(runs_exited_current) > 0) {
          self$start_ready_runs()
        }
      }

      # all calls have finished, check their status and exit
      self$calls <- self$calls %>%
        mutate(
          output_status = map(call, "output_status") %>% invoke_map_chr()
        )

      if (all(self$calls$output_status == "present")) {
        cat_line(crayon::green(cli::rule(glue::glue("{cli::symbol$tick} All calls finished"))))
      } else {
        cat_line(crayon::red(cli::rule(glue::glue("{cli::symbol$cross} Not all calls finished sucessfully"))))
      }
    },

    poll = function() {
      # get exited processes
      runs_exited_current <- self$runs_active %>%
        mutate(
          alive = map_lgl(process, ~.$is_alive())
        ) %>%
        filter(!alive)

      # extract relevant information from each process
      runs_exited_current <- runs_exited_current %>%
        mutate(
          process_status = map_int(process, ~.$get_exit_status()),
          stdout = map_chr(process, ~glue::glue_collapse(paste0("", .$read_all_output_lines()), "\n")),
          stderr = map_chr(process, ~glue::glue_collapse(paste0("", .$read_all_error_lines()), "\n")),
          output_status = map(call, "output_status") %>% invoke_map_chr(),
          call_status = map(call, "call_status") %>% invoke_map_chr(runs_exited = self$runs_exited),
          digest = map_chr(call, "digest")
        )

      # cleanup all exited processes
      runs_exited_current$process %>% map("kill_tree") %>% invoke_map()
      runs_exited_current$process <- NULL

      # show messages on exited runs
      if (nrow(runs_exited_current) > 0) {
        pwalk(runs_exited_current, function(id, output_status, process_status, ...) {
          process_status_text <- if(process_status == 0) {
            crayon::green(glue::glue("process {cli::symbol$tick}"))
          } else {
            crayon::red(glue::glue("process {cli::symbol$cross}"))
          }
          output_status_text <- if(output_status == "present") {
            crayon::green(glue::glue("output {cli::symbol$tick}"))
          } else {
            crayon::red(glue::glue("output {cli::symbol$cross}"))
          }

          run_text <- col_split(id, glue("{process_status_text} | {output_status_text}"))

          cat_line(run_text)
        })
      }

      self$runs_active <- self$runs_active %>% filter(!id %in% runs_exited_current$id)

      self$runs_exited <- bind_rows(self$runs_exited %>% filter(!id %in% runs_exited_current$id), runs_exited_current)

      runs_exited_current
    },

    object_dependencies = NULL,

    start_ready_runs = function() {
      # check which calls are ready to be run:
      # - not running
      calls_ready <- self$calls %>%
        filter(!id %in% self$runs_active$id)

      # - unfinished call
      calls_ready <- calls_ready %>% mutate(
        output_status = map(call, "output_status") %>% invoke_map_chr(),
        call_status = map(call, "call_status") %>% invoke_map_chr(runs_exited = self$runs_exited)
      ) %>%
        filter(
          call_status == "unfinished"
        )

      # - available input and non-waiting input
      waiting_output_ids <- unlist(calls_ready$call %>% map(function(call) {map_chr(call$outputs, "id")}))
      waiting_input_ids <- unname(unlist(self$object_dependencies[waiting_output_ids])) %>% c(waiting_output_ids)

      calls_ready <- calls_ready %>%
        mutate(
          input_status = map(call, "input_status") %>% invoke_map_chr(waiting_input_ids = waiting_input_ids)
        ) %>%
        filter(input_status == "ready")

      # start the runs
      runs_started <- calls_ready %>%
        mutate(
          process = map(call, "run") %>% invoke_map()
        )

      # print that the runs have started
      pwalk(runs_started, function(id, ...) {
        cat_line(col_split(id, crayon::blue(paste0("started ", symbol$play))))
      })

      # add to active runs
      self$runs_active <- bind_rows(self$runs_active, runs_started)

      # remove running runs from exited runs
      self$runs_exited <- self$runs_exited %>% filter(!id %in% self$runs_active$id)
    },
    plot_workflow = plot_workflow
  )
)
