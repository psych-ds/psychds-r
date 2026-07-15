# step3_single_file.R
# ---------------------------------------------------------------------------
# Simplified single-file experience for Step 3 (Standardize Filenames).
#
# When exactly one CSV/TSV is selected, the full multi-file keyword builder is
# overkill and is where new users get most confused about Psych-DS naming. This
# swaps in a guided panel that teaches what a filename means, nudges the user to
# name their study, and lets them add any number of additional keyword-value
# pairs (condition, session, task, ...). It produces the SAME one-element
# file_mappings shape the rest of Step 3 expects, then hands off to the normal
# proceedToFinalStep().
#
# Wiring (see PR notes):
#   - global.R sources this file.
#   - step3UI adds uiOutput(ns("single_file_panel")) and wraps the full
#     multi-file body in a conditionalPanel keyed on output$single_file_mode.
#   - step3Server calls step3SingleFileServer(...) once, near the end of its
#     moduleServer body (after proceedToFinalStep() is defined).
# ---------------------------------------------------------------------------

# Legal Psych-DS filename: keyword-value pairs joined by "_", then _data.ext
.PSYCHDS_NAME_REGEX <- "^([a-z]+-[a-zA-Z0-9]+)(_[a-z]+-[a-zA-Z0-9]+)*_data\\.(csv|tsv)$"

# Strip a keyword VALUE to the allowed alphanumeric set.
.clean_value <- function(x) {
  if (is.null(x)) return("")
  gsub("[^A-Za-z0-9]", "", x)
}

# Strip a keyword NAME to allowed lowercase letters.
.clean_keyword <- function(x) {
  if (is.null(x)) return("")
  gsub("[^a-z]", "", tolower(x))
}

# UI for one additional keyword row (inserted/removed dynamically).
#' @noRd
.single_extra_row <- function(ns, rid) {
  div(
    id = ns(paste0("row_", rid)),
    style = "display:flex; gap:8px; align-items:flex-end; flex-wrap:wrap; margin-bottom:8px; padding:8px; background:#fff; border:1px solid #eee; border-radius:6px;",
    div(style = "min-width:160px;",
        selectInput(ns(paste0(rid, "_key")), "Keyword",
                    choices = c("condition", "session", "task", "location",
                                "subject", "run", "custom\u2026" = "__custom__"),
                    width = "100%")),
    conditionalPanel(
      condition = sprintf("input['%s'] == '__custom__'", ns(paste0(rid, "_key"))),
      div(style = "min-width:150px;",
          textInput(ns(paste0(rid, "_keycustom")), "Custom keyword",
                    value = "", placeholder = "lowercase", width = "100%"))
    ),
    div(style = "min-width:150px;",
        textInput(ns(paste0(rid, "_val")), "Value",
                  value = "", placeholder = "e.g. control", width = "100%")),
    div(actionButton(ns(paste0(rid, "_remove")), icon("xmark"),
                     class = "btn btn-sm btn-default", title = "Remove this keyword"))
  )
}

#' Build the simplified single-file panel UI
#' @noRd
step3SingleFileUI <- function(ns, ext = "csv") {
  tagList(
    div(
      style = "padding:16px 18px; background:#f0f9ff; border:1px solid #bee5eb; border-radius:8px; margin-bottom:16px;",
      h4(icon("wand-magic-sparkles"), " You've selected one data file \u2014 let's name it",
         style = "margin-top:0;"),
      p("Psych-DS filenames are built from ", tags$strong("keyword-value pairs"),
        " so that every part of the name means something. Instead of a name like ",
        tags$code("347B.csv"), ", you describe the file, e.g. ",
        tags$code(paste0("study-memory_data.", ext)), "."),
      p("Each pair is ", tags$code("keyword-value"), ", pairs are joined with ",
        tags$code("_"), ", and the name always ends in ",
        tags$code(paste0("_data.", ext)),
        ". Keywords are lowercase letters; values are letters and numbers only ",
        "(no spaces or punctuation).")
    ),

    # The one piece every dataset needs: a study name.
    div(
      style = "margin-bottom:14px;",
      tags$label("Name your study", `for` = ns("single_study"),
                 style = "font-weight:bold; display:block; margin-bottom:4px;"),
      div(style = "font-size:13px; color:#555; margin-bottom:6px;",
          "The one keyword every dataset should have is ", tags$code("study"),
          ". Give this study a short name \u2014 for example ", tags$code("memory"),
          ", ", tags$code("stroop"), ", or ", tags$code("faceRatings"), "."),
      div(style = "display:flex; align-items:center; gap:8px;",
          span(tags$code("study-"), style = "font-family:monospace;"),
          div(style = "flex:1;",
              textInput(ns("single_study"), label = NULL, value = "",
                        placeholder = "YourStudyName", width = "100%"))
      )
    ),

    # Additional keywords (any number).
    div(
      style = "margin-bottom:14px; background:#fafafa; border:1px solid #e5e5e5; border-radius:6px; padding:12px 14px;",
      tags$strong("Add more keywords (optional)"),
      div(style = "font-size:13px; color:#555; margin:6px 0;",
          "Planning to add more data files later? Think about what will be ",
          tags$strong("different"), " between this file and the next one \u2014 that ",
          "difference becomes a keyword. Common ones: ",
          tags$code("condition"), ", ", tags$code("session"), ", ", tags$code("task"),
          ", ", tags$code("location"), ", ", tags$code("subject"),
          ". Add one row per keyword; later files just change the value."),
      # Rows get inserted here.
      div(id = ns("single_extra_container")),
      actionButton(ns("single_add_keyword"),
                   tagList(icon("plus"), "Add keyword"),
                   class = "btn btn-sm btn-info")
    ),

    # Live preview of the resulting filename.
    div(
      style = "margin:8px 0 16px; padding:14px; background:#f8f9fa; border:1px solid #ced4da; border-radius:6px; text-align:center; min-height:44px;",
      uiOutput(ns("single_preview"))
    ),

    div(
      style = "display:flex; justify-content:space-between; margin-top:10px;",
      actionButton(ns("single_back"), "Back", class = "btn btn-default"),
      actionButton(ns("single_continue"),
                   "Continue to save your dataset \u2014 no files will be saved yet",
                   class = "btn btn-primary")
    )
  )
}

#' Wire up the single-file panel inside step3Server's moduleServer.
#'
#' @param input,output,session the module's reactives
#' @param state global state reactiveValues
#' @param file_mappings the reactiveVal holding the mappings list
#' @param proceed_fn proceedToFinalStep (called on continue)
#' @noRd
step3SingleFileServer <- function(input, output, session, state, file_mappings, proceed_fn) {
  ns <- session$ns

  # Dynamic additional-keyword rows.
  row_seq <- reactiveVal(0)
  rows    <- reactiveVal(character(0))

  # Mode flag for the conditionalPanel wrap in step3UI.
  output$single_file_mode <- reactive({
    df <- state$data_files
    !is.null(df) && length(df) == 1
  })
  outputOptions(output, "single_file_mode", suspendWhenHidden = FALSE)

  # Extension of the one selected file (csv/tsv), default csv.
  single_ext <- reactive({
    df <- state$data_files
    if (is.null(df) || length(df) < 1) return("csv")
    e <- tolower(tools::file_ext(df[[1]]))
    if (e %in% c("csv", "tsv")) e else "csv"
  })

  # A change of selected file resets the panel's dynamic rows so the freshly
  # rendered (empty) container and our row bookkeeping stay in sync.
  observeEvent(state$data_files, {
    rows(character(0))
    row_seq(0)
  }, ignoreNULL = FALSE)

  # Render the guided panel only in single-file mode (empty otherwise).
  output$single_file_panel <- renderUI({
    df <- state$data_files
    if (is.null(df) || length(df) != 1) return(NULL)
    step3SingleFileUI(ns, ext = single_ext())
  })

  # Add an additional-keyword row.
  observeEvent(input$single_add_keyword, {
    n <- row_seq() + 1
    row_seq(n)
    rid <- paste0("xk", n)
    rows(c(rows(), rid))
    insertUI(
      selector = paste0("#", ns("single_extra_container")),
      where = "beforeEnd",
      ui = .single_extra_row(ns, rid)
    )
    # Remove handler for this row (self-destructs after one click).
    observeEvent(input[[paste0(rid, "_remove")]], {
      removeUI(selector = paste0("#", ns(paste0("row_", rid))))
      rows(setdiff(rows(), rid))
    }, ignoreInit = TRUE, once = TRUE)
  })

  # Compose the candidate filename + validation from current inputs.
  single_name <- reactive({
    ext <- single_ext()

    pairs <- list()
    study <- .clean_value(input$single_study)
    if (nzchar(study)) pairs <- c(pairs, list(list(name = "study", value = study)))

    for (rid in rows()) {
      key <- input[[paste0(rid, "_key")]]
      if (!is.null(key) && identical(key, "__custom__")) {
        key <- .clean_keyword(input[[paste0(rid, "_keycustom")]])
      } else {
        key <- .clean_keyword(key)
      }
      val <- .clean_value(input[[paste0(rid, "_val")]])
      if (nzchar(key) && nzchar(val)) {
        pairs <- c(pairs, list(list(name = key, value = val)))
      }
    }

    if (length(pairs) == 0) {
      return(list(ok = FALSE, name = "", ext = ext, pairs = list(),
                  msg = "Enter a study name to build your filename."))
    }

    names_vec <- vapply(pairs, function(p) p$name, character(1))
    dup <- unique(names_vec[duplicated(names_vec)])
    if (length(dup) > 0) {
      return(list(ok = FALSE, name = "", ext = ext, pairs = pairs,
                  msg = paste0("Each keyword can be used only once \u2014 you've repeated: ",
                               paste(dup, collapse = ", "), ".")))
    }

    name <- paste0(
      paste(vapply(pairs, function(p) paste0(p$name, "-", p$value), character(1)),
            collapse = "_"),
      "_data.", ext
    )
    list(ok = grepl(.PSYCHDS_NAME_REGEX, name), name = name, ext = ext,
         pairs = pairs, msg = NULL)
  })

  output$single_preview <- renderUI({
    res <- single_name()
    if (!isTRUE(res$ok)) {
      hint <- if (!is.null(res$msg)) res$msg else "Keep going\u2026"
      return(div(style = "color:#999; font-size:13px;", hint))
    }
    tagList(
      div(style = "font-size:12px; color:#666; margin-bottom:4px;", "Your file will be named:"),
      div(style = "font-family:monospace; font-size:16px; color:#2c3e50;", res$name)
    )
  })

  # Back -> Step 2 (mirrors the existing input$back handler).
  observeEvent(input$single_back, {
    state$current_step <- 2
    session$sendCustomMessage("updateSidebarStep", list(step = 2))
  })

  # Continue -> build the single mapping, hand off to the normal save flow.
  observeEvent(input$single_continue, {
    res <- single_name()
    if (!isTRUE(res$ok)) {
      showModal(modalDialog(
        title = "Check your keywords",
        div(
          p(if (!is.null(res$msg) &&
                !identical(res$msg, "Enter a study name to build your filename."))
              res$msg
            else
              "Give your study a short name so we can build a valid Psych-DS filename."),
          p(style = "color:#666;",
            "A dataset only needs the ", tags$code("study"), " keyword to be valid \u2014 for example ",
            tags$code(paste0("study-memory_data.", res$ext)), ".")
        ),
        easyClose = TRUE, footer = modalButton("OK")
      ))
      return()
    }

    keywords <- lapply(res$pairs, function(p) {
      list(name = p$name, value = p$value, id = p$name)
    })
    df <- state$data_files
    mapping <- list(list(
      original = df[[1]],
      new = res$name,
      keywords = keywords,
      values = list(),
      partial_keywords = keywords
    ))
    file_mappings(mapping)
    proceed_fn()
  })
}
