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
#
# MODAL LAYOUT (rev 2, after supervisor feedback):
#   [info box: folders only; pick the folder, files come next]
#   Enter a path directly or navigate to a folder below:
#   [ path text box ................................ ] [Show below]
#   [ folder navigator                                ]
#   Current location: /the/folder/Select-this-folder/will/commit
#   footer: [Up] [Cancel] [Select this folder]
#
# Notable change: there is no longer a one-click "Use this path" commit.
# Typing a path and clicking "Show below" only POINTS the browser there;
# committing always goes through the explicit "Select this folder" button,
# with the would-be selection printed directly above it. This makes it much
# harder to accidentally select an entire home directory (which triggered
# OS permission prompts for Music/Photos/etc. on macOS during testing).
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
#'
#' Windows drives are probed with dir.exists() over drive letters, so this
#' module has no shinyFiles dependency at all.
#' @noRd
get_path_volumes <- function() {
  vols <- c(Home = path.expand("~"))
  if (.Platform$OS.type == "windows") {
    drives <- paste0(LETTERS, ":/")
    drives <- drives[dir.exists(drives)]
    if (length(drives) > 0) {
      names(drives) <- paste0(substr(drives, 1, 2))
      vols <- c(vols, drives)
    }
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

#' macOS user folders gated by TCC (Transparency, Consent, and Control)
#' @noRd
.protected_macos_dirs <- function() {
  h <- path.expand("~")
  file.path(h, c("Desktop", "Documents", "Downloads",
                 "Pictures", "Music", "Movies", "Library"))
}

#' Should this selection get a warning before we commit it?
#'
#' Returns NULL for a normal folder, or a human-readable reason string when
#' the selection is likely to trigger OS permission prompts/errors or a
#' massive scan. NOTE: there is no reliable way to *probe* protection status
#' from R -- on macOS, attempting the read IS what triggers the permission
#' prompt, and file.access()/access(2) reports success even for TCC-blocked
#' folders. So this is a known-locations heuristic; the downstream file
#' scanner should still skip-and-report unreadable folders as the safety net.
#' @noRd
.risky_selection <- function(path) {
  clean <- function(p) {
    p <- normalizePath(p, winslash = "/", mustWork = FALSE)
    if (nchar(p) > 1) p <- sub("/+$", "", p)
    p
  }
  p <- clean(path)
  h <- clean(path.expand("~"))

  if (identical(p, "/") || grepl("^[A-Za-z]:/?$", p)) {
    return(paste0(
      "This is an entire disk. Scanning it will take a very long time and ",
      "will run into system folders your operating system protects."
    ))
  }
  if (identical(p, h)) {
    return(paste0(
      "This is your entire home folder. Scanning it makes the app try to ",
      "read protected folders like Desktop, Documents, and Photos, which ",
      "your operating system may block with permission errors."
    ))
  }

  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    lib <- clean(file.path(h, "Library"))
    if (identical(p, lib) || startsWith(p, paste0(lib, "/"))) {
      return(paste0(
        "This folder is inside macOS's Library area, which holds app and ",
        "system data. macOS protects much of it, so scanning here will ",
        "trigger repeated permission requests or errors."
      ))
    }
    prot <- vapply(.protected_macos_dirs(), clean, character(1))
    if (any(startsWith(prot, paste0(p, "/")))) {
      return(paste0(
        "This folder contains system-protected folders (like Desktop, ",
        "Documents, or Photos). Scanning it may trigger permission ",
        "requests or errors from your operating system."
      ))
    }
  } else if (startsWith(h, paste0(p, "/"))) {
    return(paste0(
      "This folder contains your entire home folder. Scanning it may run ",
      "into folders your operating system protects."
    ))
  }

  NULL
}


# ===========================================================================
# Reusable modal directory browser (rev 3)
#
# bindDirectoryBrowser() is the single shared picker used by ALL directory
# choosers in the app: Step 1 (via directoryInputServer below), Step 3's save
# location, the Validate tab, the Data Dictionary tab, the Dataset Explorer,
# and the OSF upload tab. Call it once inside a moduleServer(), pointing it at
# an existing actionButton (browse_button) and textInput (target_input) in
# that module's namespace. It opens the files-free browser modal on click and
# writes the committed path into target_input via updateTextInput().
#
# Replaces every shinyFiles shinyDirChoose()/parseDirPath() pairing, so the
# app has ONE picker with ONE behavior: no per-file stat storm, and the same
# protected-folder warning everywhere.
# ===========================================================================

#' Wire the shared directory-browser modal to a browse button + text input
#'
#' @param input,output,session The calling module's input/output/session
#' @param browse_button Id (in the caller's namespace) of the actionButton
#'   that opens the browser
#' @param target_input Id of the textInput that receives the committed path
#' @param state Optional reactiveValues; with state_field, mirrors the
#'   committed path into state[[state_field]]
#' @param state_field Optional name of the state field to mirror into
#' @param on_commit Optional function(path) called after a path is committed
#' @param on_cancel Optional function() called after the user cancels. Supply
#'   this (with easy_close = FALSE) when the browser is launched from inside
#'   another modal: Shiny shows only one modal at a time, so the parent dialog
#'   must be reopened by on_commit/on_cancel when the browser closes.
#' @param easy_close Whether clicking outside dismisses the browser. Set FALSE
#'   for modal-hosted callers, where a stray dismissal would lose the parent
#'   dialog with no way to restore it.
#' @param host_modal Set TRUE when the browser is launched from inside another
#'   modal. Commit/cancel then skip removeModal() entirely and rely on
#'   on_commit/on_cancel calling showModal() to reopen the parent dialog:
#'   showModal() replaces the current modal synchronously, whereas
#'   removeModal() only starts Bootstrap's fade-out and defers the real DOM
#'   cleanup to the animation's "hidden" event -- cleanup that lands a few
#'   hundred ms later and tears down whichever modal is showing by then,
#'   i.e. the freshly reopened parent dialog.
#' @param title Modal title
#' @noRd
bindDirectoryBrowser <- function(input, output, session,
                                 browse_button, target_input,
                                 state = NULL, state_field = NULL,
                                 on_commit = NULL,
                                 on_cancel = NULL,
                                 easy_close = TRUE,
                                 host_modal = FALSE,
                                 title = "Select a project directory") {
  ns  <- session$ns
  pfx <- paste0(browse_button, "_pk_")
  id  <- function(x) paste0(pfx, x)

  nav_path <- reactiveVal(path.expand("~"))

  commit_selection <- function(chosen) {
    # Modal-hosted callers reopen their parent dialog in on_commit; that
    # showModal() replaces the browser synchronously, and issuing a
    # removeModal() here as well would race Bootstrap's deferred fade-out
    # cleanup and tear the reopened dialog back down (see host_modal docs).
    if (!host_modal) removeModal()
    updateTextInput(session, target_input, value = chosen)
    if (!is.null(state) && !is.null(state_field)) state[[state_field]] <- chosen
    if (is.function(on_commit)) on_commit(chosen)
  }

  output[[id("dirlist")]] <- renderUI({
    cur <- nav_path()
    subdirs <- list_subdirs(cur)

    list_body <- if (length(subdirs) == 0) {
      div(
        style = "color:#999; padding:14px; text-align:center;",
        "Click \u201cSelect this folder\u201d to move on, or navigate to a different folder."
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
            ns(id("navigate")),
            jsonlite::toJSON(d, auto_unbox = TRUE)
          ),
          icon("folder", style = "color:#f0ad4e; margin-right:8px;"),
          span(basename(d))
        )
      })
    }

    tagList(
      div(
        style = paste0(
          "max-height:300px; overflow-y:auto; border:1px solid #ddd; ",
          "border-radius:4px; background:#fff;"
        ),
        list_body
      ),
      div(
        style = "margin-top:10px; text-align:right; font-size:12px; color:#666;",
        "Current location:"
      ),
      div(
        style = paste0(
          "text-align:right; word-break:break-all; font-family:monospace; ",
          "font-size:12px; color:#333; margin-top:2px;"
        ),
        cur
      )
    )
  })

  open_modal <- function() {
    showModal(modalDialog(
      title = title,
      div(
        style = paste0(
          "background:#eef5fb; border:1px solid #cfe2f3; border-radius:4px; ",
          "padding:10px 12px; margin-bottom:12px; font-size:13px; color:#31708f;"
        ),
        icon("circle-info", style = "margin-right:6px;"),
        HTML(paste0(
          "This browser lists <strong>folders only</strong>. Choose the subfolder ",
          "that contains all the data you want to include. It's okay if that ",
          "folder also contains other things; you'll select the specific data ",
          "files next."
        ))
      ),
      div(
        style = "margin-bottom:12px;",
        div(
          style = "margin-bottom:4px;",
          tags$label("Enter a path directly or navigate to a folder below:")
        ),
        div(
          style = "display:flex; gap:8px; align-items:flex-start;",
          div(
            style = "flex:1;",
            textInput(ns(id("jump")), label = NULL,
                      value = nav_path(), width = "100%")
          ),
          actionButton(ns(id("show_below")), "Show below", class = "btn-default")
        )
      ),
      uiOutput(ns(id("dirlist"))),
      size = "l",
      easyClose = easy_close,
      footer = tagList(
        actionButton(ns(id("go_up")), HTML("&uarr; Up"), class = "btn-default"),
        if (is.null(on_cancel)) modalButton("Cancel") else
          actionButton(ns(id("cancel")), "Cancel", class = "btn-default"),
        actionButton(ns(id("select_current")), "Select this folder",
                     class = "btn-primary")
      )
    ))
  }

  observeEvent(input[[browse_button]], {
    start <- .resolve_dir(input[[target_input]])
    if (is.null(start)) start <- path.expand("~")
    nav_path(start)
    open_modal()
  })

  observeEvent(input[[id("navigate")]], {
    target <- .resolve_dir(input[[id("navigate")]])
    if (!is.null(target)) nav_path(target)
  })

  observeEvent(input[[id("go_up")]], {
    parent <- .resolve_dir(dirname(nav_path()))
    if (!is.null(parent)) nav_path(parent)
  })

  observeEvent(input[[id("show_below")]], {
    target <- .resolve_dir(input[[id("jump")]])
    if (!is.null(target)) {
      nav_path(target)
    } else {
      showNotification("That path doesn't exist or isn't a folder.", type = "warning")
    }
  })

  observeEvent(input[[id("select_current")]], {
    chosen <- nav_path()
    reason <- .risky_selection(chosen)
    if (!is.null(reason)) {
      showModal(modalDialog(
        title = "Heads up about this folder",
        div(
          style = "font-size:13px;",
          p(HTML(sprintf(
            "You're about to select <code>%s</code>.",
            htmltools::htmlEscape(chosen)
          ))),
          p(reason),
          p(paste0(
            "Psych-DS works best when you choose the project folder that ",
            "holds just this dataset."
          ))
        ),
        footer = tagList(
          actionButton(ns(id("risky_back")), "Go back and choose a subfolder",
                       class = "btn-primary"),
          actionButton(ns(id("risky_confirm")), "Select it anyway",
                       class = "btn-default")
        ),
        easyClose = FALSE
      ))
    } else {
      commit_selection(chosen)
    }
  })

  observeEvent(input[[id("cancel")]], {
    if (!host_modal) removeModal()
    if (is.function(on_cancel)) on_cancel()
  })

  observeEvent(input[[id("risky_back")]], {
    open_modal()
  })

  observeEvent(input[[id("risky_confirm")]], {
    commit_selection(nav_path())
  })

  invisible(NULL)
}

#' Directory Input Server (files-free)
#'
#' Thin wrapper over bindDirectoryBrowser() so Step 1 keeps its original
#' signature and behavior (returns a reactive path, mirrors into state).
#'
#' @param id Module ID
#' @param state Global state reactiveValues
#' @param session Parent session (unused; kept for signature compatibility)
#' @param state_field Field of `state` to mirror the selection into
#' @return A reactive returning the currently selected directory path
#' @noRd
directoryInputServer <- function(id, state, session, state_field = "project_dir") {
  moduleServer(id, function(input, output, session) {

    path_value <- reactiveVal("")   # the committed selection

    # Restore a previously chosen directory from global state
    observe({
      prior <- state[[state_field]]
      if (!is.null(prior) && prior != "" && path_value() == "") {
        updateTextInput(session, "path", value = prior)
        path_value(prior)
      }
    })

    # All browsing/committing lives in the shared core
    bindDirectoryBrowser(
      input, output, session,
      browse_button = "browse",
      target_input  = "path",
      state = state, state_field = state_field,
      on_commit = function(p) path_value(p)
    )

    # Honor manual typing/pasting in the main field
    observeEvent(input$path, {
      if (!is.null(input$path) && input$path != "" && input$path != path_value()) {
        path_value(input$path)
        state[[state_field]] <- input$path
      }
    })

    return(path_value)
  })
}
