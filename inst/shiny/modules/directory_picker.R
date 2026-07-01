# directory_picker.R
# ---------------------------------------------------------------------------
# Lightweight, files-free directory picker for the Psych-DS Shiny app.
#
# Drop-in replacement for the shinyFiles-based directoryInputUI() /
# directoryInputServer(). Same function names, same arguments, same return
# value (a reactive that yields the chosen path), so step1Server() and the
# directoryInputUI(ns("project_dir")) call site need no changes.
#
# WHY: shinyFiles' shinyDirChoose() unconditionally runs its fileGetter() on
# every folder you open -- dir_ls() + fs::file_info() (a full stat on EVERY
# entry) + dir.exists() -- and only filters afterwards. On a folder with ~100k
# files that stat-storm blocks the single-threaded R process and ships a
# 100k-row payload the browser then has to render, which is what froze the
# weaker test machines. A folder picker never needs file metadata, so this
# module enumerates ONLY one level of subdirectories (list.dirs(recursive =
# FALSE)) and never touches files at all.
# ---------------------------------------------------------------------------

#' Directory Input UI (files-free)
#'
#' @param id Module ID
#' @param value Initial path value
#' @param placeholder Placeholder text for the path field
#' @noRd
directoryInputUI <- function(id, value = "", placeholder = "Project directory path") {
  ns <- NS(id)
  tagList(
    div(
      class = "directory-input",
      textInput(
        ns("path"),
        label = NULL,
        value = value,
        placeholder = placeholder,
        width = "100%"
      ),
      actionButton(
        ns("browse"),
        label = "...",
        title = "Browse for a project directory",
        class = "browse-btn"
      )
    ),
    # Pre-empt the "where are my files?" confusion right at the field.
    div(
      style = "font-size:12px; color:#888; margin-top:4px;",
      "Choose or type the folder that holds your data. You're selecting a ",
      "folder, not individual files."
    )
  )
}

#' List immediate subdirectories cheaply (no file metadata, no recursion)
#'
#' This is the whole performance trick: one directory read, directories only.
#' We never call fs::file_info() and never enumerate files.
#'
#' @param path Directory to inspect
#' @param show_hidden Whether to include dot-directories
#' @return Character vector of absolute subdirectory paths
#' @noRd
list_subdirs <- function(path, show_hidden = FALSE) {
  if (is.null(path) || is.na(path) || path == "" || !dir.exists(path)) {
    return(character(0))
  }
  dirs <- tryCatch(
    list.dirs(path, full.names = TRUE, recursive = FALSE),
    error = function(e) character(0)
  )
  if (length(dirs) == 0) return(dirs)
  if (!show_hidden) {
    dirs <- dirs[!startsWith(basename(dirs), ".")]
  }
  sort(dirs)
}

#' Available volume roots (Home, filesystem/Windows drive roots)
#' @noRd
get_path_volumes <- function() {
  vols <- c(Home = path.expand("~"))
  if (.Platform$OS.type == "windows") {
    win_vols <- tryCatch(shinyFiles::getVolumes()(), error = function(e) NULL)
    if (!is.null(win_vols)) vols <- c(vols, win_vols)
  } else {
    vols <- c(vols, Root = "/")
  }
  vols
}

#' Normalize and validate a candidate directory path
#'
#' Returns the normalized path if it exists and is a directory, else NULL.
#' @noRd
.resolve_dir <- function(path) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  path <- path.expand(path)             # handle a leading ~/
  if (!dir.exists(path)) return(NULL)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Directory Input Server (files-free)
#'
#' @param id Module ID
#' @param state Global state reactiveValues
#' @param session Parent session (unused; kept for signature compatibility)
#' @return A reactive returning the currently selected directory path
#' @noRd
directoryInputServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    path_value <- reactiveVal("")              # the committed selection
    nav_path   <- reactiveVal(path.expand("~")) # where the browser is currently pointed

    # Restore a previously chosen directory from global state
    observe({
      if (!is.null(state$project_dir) && state$project_dir != "" && path_value() == "") {
        updateTextInput(session, "path", value = state$project_dir)
        path_value(state$project_dir)
      }
    })

    # Commit a chosen directory: update field, state, and close the modal.
    commit_selection <- function(chosen) {
      updateTextInput(session, "path", value = chosen)
      path_value(chosen)
      state$project_dir <- chosen
      removeModal()
    }

    # Live browser body: current location + subfolder list. Reactive on
    # nav_path(), so navigating updates it in place inside the open modal.
    output$dir_list <- renderUI({
      cur <- nav_path()
      subdirs <- list_subdirs(cur)

      list_body <- if (length(subdirs) == 0) {
        div(
          style = "color:#999; padding:14px; text-align:center;",
          "No subfolders here \u2014 use \u201cSelect this folder\u201d to choose this directory."
        )
      } else {
        lapply(subdirs, function(d) {
          div(
            class = "dir-row",
            style = paste0(
              "padding:6px 10px; cursor:pointer; border-bottom:1px solid #eee; ",
              "display:flex; align-items:center;"
            ),
            onmouseover = "this.style.backgroundColor='#f5f5f5'",
            onmouseout  = "this.style.backgroundColor=''",
            onclick = sprintf(
              "Shiny.setInputValue('%s', %s, {priority:'event'})",
              ns("navigate"),
              jsonlite::toJSON(d, auto_unbox = TRUE)
            ),
            icon("folder", style = "color:#f0ad4e; margin-right:8px;"),
            span(basename(d))
          )
        })
      }

      tagList(
        div(
          style = "font-size:12px; color:#666; margin-bottom:4px;",
          "Current location:"
        ),
        div(
          style = paste0(
            "padding:6px 8px; background:#f8f9fa; border:1px solid #ddd; ",
            "border-radius:4px; margin-bottom:8px; word-break:break-all; font-family:monospace;"
          ),
          cur
        ),
        div(
          style = paste0(
            "max-height:300px; overflow-y:auto; border:1px solid #ddd; ",
            "border-radius:4px; background:#fff;"
          ),
          list_body
        )
      )
    })

    open_modal <- function() {
      showModal(modalDialog(
        title = "Select a project directory",

        # Explain why no files are listed -- the source of the confusion.
        div(
          style = paste0(
            "background:#eef5fb; border:1px solid #cfe2f3; border-radius:4px; ",
            "padding:10px 12px; margin-bottom:12px; font-size:13px; color:#31708f;"
          ),
          icon("circle-info", style = "margin-right:6px;"),
          HTML(paste0(
            "This browser lists <strong>folders only</strong>. Psych-DS treats an ",
            "entire folder as your dataset, so individual files (CSVs, JSON, etc.) ",
            "aren't shown here \u2014 that's expected. Open the folder you want, then ",
            "click \u201cSelect this folder.\u201d"
          ))
        ),

        # Direct-path entry: jump the browser there, or select it outright.
        div(
          textInput(
            ns("jump_path"),
            label = "Or enter a path directly:",
            value = nav_path(),
            width = "100%"
          ),
          div(
            style = "margin-top:-6px; margin-bottom:12px;",
            actionButton(ns("go_path"), "Open here", class = "btn-default btn-sm"),
            actionButton(ns("use_path"), "Use this path", class = "btn-primary btn-sm"),
            span(
              style = "margin-left:8px; font-size:12px; color:#888;",
              "\u201cOpen here\u201d browses from this path; \u201cUse this path\u201d selects it."
            )
          )
        ),

        uiOutput(ns("dir_list")),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("go_up"), HTML("&uarr; Up"), class = "btn-default"),
          modalButton("Cancel"),
          actionButton(ns("select_current"), "Select this folder", class = "btn-primary")
        )
      ))
    }

    # Open the browser, starting from the most sensible existing location
    observeEvent(input$browse, {
      start <- .resolve_dir(input$path)
      if (is.null(start)) start <- .resolve_dir(path_value())
      if (is.null(start)) start <- path.expand("~")
      nav_path(start)
      open_modal()
    })

    # Descend into a clicked subfolder
    observeEvent(input$navigate, {
      target <- .resolve_dir(input$navigate)
      if (!is.null(target)) nav_path(target)
    })

    # Go up one level (stops at filesystem root)
    observeEvent(input$go_up, {
      parent <- .resolve_dir(dirname(nav_path()))
      if (!is.null(parent)) nav_path(parent)
    })

    # Direct path -> jump the browser to that location (opening level)
    observeEvent(input$go_path, {
      target <- .resolve_dir(input$jump_path)
      if (!is.null(target)) {
        nav_path(target)
      } else {
        showNotification("That path doesn't exist or isn't a folder.", type = "warning")
      }
    })

    # Direct path -> select it outright, no browsing needed
    observeEvent(input$use_path, {
      chosen <- .resolve_dir(input$jump_path)
      if (!is.null(chosen)) {
        commit_selection(chosen)
      } else {
        showNotification("That path doesn't exist or isn't a folder.", type = "warning")
      }
    })

    # Commit the folder the browser is currently pointed at
    observeEvent(input$select_current, {
      commit_selection(nav_path())
    })

    # Honor manual typing/pasting in the main field
    observeEvent(input$path, {
      if (!is.null(input$path) && input$path != "" && input$path != path_value()) {
        path_value(input$path)
        state$project_dir <- input$path
      }
    })

    return(path_value)
  })
}