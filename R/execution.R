start_ready_runs <- function(calls, runs_active, runs_exited, object_dependencies = get_object_dependencies(calls)) {
  # check which calls are ready to be run:
  # - not running
  calls_ready <- calls %>%
    filter(!id %in% runs_active$id)

  # - unfinished call or unavailable output
  calls_ready <- calls_ready %>% mutate(
      output_status = map(call, "output_status") %>% invoke_map_chr(),
      call_status = map(call, "call_status") %>% invoke_map_chr(runs_exited = runs_exited)
    ) %>%
    filter(
      call_status == "unfinished" | output_status == "unavailable"
    )

  # - available input and non-waiting input
  possibly_new_output_ids <- unlist(calls_ready$call %>% map(function(call) {map_chr(call$outputs, "id")}))
  waiting_input_ids <- unname(unlist(object_dependencies[possibly_new_output_ids]))

  calls_ready <- calls_ready %>%
    filter(!id %in% runs_active$id) %>%
    mutate(
      input_status = map(call, "input_status") %>% invoke_map_chr(waiting_input_ids = waiting_input_ids)
    ) %>%
    filter(input_status == "ready")

  # start the runs
  runs_started <- calls_ready %>%
    mutate(
      process = map(call, "run") %>% invoke_map()
    )

  pmap(runs_started, function(id, ...) {
    cat_line(col_split(id, crayon::blue(paste0("started ", symbol$play))))
  })

  runs_active <- bind_rows(runs_active, runs_started)

  runs_active
}


run_calls <- function(calls, runs_exited = tibble(id = character(), digest = character())) {
  # initially no runs are active
  runs_active <- tibble(id = character(), process = list())

  # precalculate object dependency network
  object_dependencies <- get_object_dependencies(calls)

  # start up initial runes
  runs_active <- start_ready_runs(calls, runs_active, runs_exited, object_dependencies)

  # remove previous runs of the currently running
  runs_exited <- runs_exited %>% filter(!id %in% runs_active$id)

  # poll running runs and start new ones when available
  while(nrow(runs_active) > 0) {
    Sys.sleep(0.1)

    polling_result <- poll_runs(runs_active)

    runs_active <- polling_result$runs_active
    runs_exited <- bind_rows(
      runs_exited,
      polling_result$runs_exited
    )

    if (nrow(polling_result$runs_exited) > 0) {
      # new runs have exited, check whether new runs needs to be started
      runs_active <- start_ready_runs(calls, runs_active, runs_exited, object_dependencies)
      runs_exited <- runs_exited %>% filter(!id %in% runs_active$id)
    }
  }

  # check the output status of all calls to print workflow success
  calls <- calls %>%
    mutate(
      output_status = map(call, "output_status") %>% invoke_map_chr()
    )

  if (all(calls$output_status == "available")) {
    cat_line(crayon::green(cli::rule(glue::glue("{cli::symbol$tick} All calls finished"))))
  } else {
    cat_line(crayon::red(cli::rule(glue::glue("{cli::symbol$cross} Not all calls finished sucessfully"))))
  }

  runs_exited
}




poll_runs <- function(runs_active) {
  runs_exited <- runs_active %>%
    mutate(
      alive = map_lgl(process, ~.$is_alive())
    ) %>%
    filter(!alive)

  runs_exited <- runs_exited %>%
    mutate(
      process_status = map_int(process, ~.$get_exit_status()),
      stdout = map_chr(process, ~glue::glue_collapse(paste0("", .$read_all_output_lines()), "\n")),
      stderr = map_chr(process, ~glue::glue_collapse(paste0("", .$read_all_error_lines()), "\n")),
      output_status = map(call, "output_status") %>% invoke_map_chr(),
      call_status = map(call, "call_status") %>% invoke_map_chr(),
      digest = map(call, "digest") %>% invoke_map_chr()
    )

  # cleanup all exited processes
  runs_exited$process %>% map("kill_tree") %>% invoke_map()
  runs_exited$process <- NULL

  # show messages on exited runs
  if (nrow(runs_exited) > 0) {
    pmap(runs_exited, function(id, output_status, process_status, ...) {
      process_status_text <- if(process_status == 0) {
        crayon::green(glue::glue("process {cli::symbol$tick}"))
      } else {
        crayon::red(glue::glue("process {cli::symbol$cross}"))
      }
      output_status_text <- if(output_status == "available") {
        crayon::green(glue::glue("output {cli::symbol$tick}"))
      } else {
        crayon::red(glue::glue("output {cli::symbol$cross}"))
      }

      run_text <- col_split(id, glue("{process_status_text} | {output_status_text}"))

      cat_line(run_text)
    })
  }

  lst(
    runs_exited = runs_exited,
    runs_active = runs_active %>% filter(!id %in% runs_exited)
  )
}





get_object_dependencies <- function(calls) {
  object_dependency_network <- calls$call %>% map(function(call) {
    tidyr::crossing(
      from = map_chr(call$inputs, "id"),
      to = map_chr(call$outputs, "id")
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



process_calls_raw <- function(calls_raw) {
  set_names(calls_raw, map_chr(calls_raw, "id")) %>% enframe("id", "call")
}
