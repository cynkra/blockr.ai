#' @rdname new_llm_block
#' @export
llm_block_server <- function(x) {
  UseMethod("llm_block_server", x)
}

#' @rdname new_llm_block
#' @export
llm_block_server.llm_block_proxy <- function(x) {

  result_ptype <- result_ptype(x)
  result_base_class <- last(class(result_ptype))

  function(id, data = NULL, ...args = list()) {
    moduleServer(
      id,
      function(input, output, session) {

        observeEvent(
          get_board_option_or_default("dark_mode"),
          shinyAce::updateAceEditor(
            session,
            "code_editor",
            theme = switch(
              get_board_option_or_default("dark_mode"),
              light = "katzenmilch",
              dark = "dracula"
            )
          )
        )

        r_datasets <- reactive(
          c(
            if (is.reactive(data) && !is.null(data())) list(data = data()),
            if (is.reactivevalues(...args)) reactiveValuesToList(...args)
          )
        )

        rv_code <- reactiveVal()
        rv_expl <- reactiveVal(x[["explanation"]])
        rv_cond <- reactiveValues(
          error = character(),
          warning = character(),
          message = character()
        )

        observeEvent(
          input$ask,
          {
            dat <- r_datasets()

            req(
              input$question,
              dat,
              length(dat) > 0,
              all(lengths(dat) > 0)
            )

            result <- query_llm_with_retry(
              datasets = dat,
              user_prompt = input$question,
              system_prompt = system_prompt(x, dat),
              max_retries = x[["max_retries"]],
              progress = TRUE
            )

            if ("error" %in% names(result)) {
              rv_cond$error <- result$error
              rv_cond$warning <- character()
            } else if (!inherits(result$value, result_base_class)) {
              rv_cond$error <- character()
              rv_cond$warning <- paste0(
                "Expecting code to evaluate to an object inheriting from `",
                result_base_class, "`."
              )
            } else {
              rv_cond$error <- character()
              rv_cond$warning <- character()
            }

            rv_code(result$code)
            rv_expl(result$explanation)
          }
        )

        observeEvent(
          rv_code(),
          shinyAce::updateAceEditor(
            session,
            "code_editor",
            value = style_code(rv_code())
          )
        )

        observeEvent(
          input$code_editor,
          {
            res <- try_eval_code(input$code_editor, r_datasets())
            if (inherits(res, "try-error")) {
              rv_cond$error <- paste0(
                "Encountered an error evaluating code: ", res
              )
            } else {
              rv_code(input$code_editor)
              rv_cond$error <- character()
            }
          }
        )

        output$explanation <- renderUI(markdown(rv_expl()))

        output$result_is_available <- reactive(
          length(rv_code()) > 0 && any(nzchar(rv_code()))
        )

        outputOptions(
          output,
          "result_is_available",
          suspendWhenHidden = FALSE
        )

        list(
          expr = reactive(str2expression(rv_code())),
          state = list(
            question = reactive(input$question),
            code = rv_code,
            explanation = rv_expl,
            max_retries = x[["max_retries"]]
          ),
          cond = rv_cond
        )
      }
    )
  }
}
