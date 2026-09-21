#' Psych-DS App UI
#'
#' This file defines the main UI for the Psych-DS app.
#' It loads the modular UI components and handles the top-level UI structure.

# Load UI modules
source("modules/ui_modules.R")

# Cache-bust the stylesheet with its modification time: the RStudio viewer
# caches css/styles.css aggressively, so without a changing query string,
# style edits show up in the browser but not the viewer.
.css_version <- tryCatch(
  as.integer(file.mtime(file.path("www", "css", "styles.css"))),
  error = function(e) NA_integer_
)
if (is.na(.css_version)) .css_version <- as.integer(Sys.time())

# Main UI definition
ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = span("Psych-DS", style = "font-size: 24px;"),
    titleWidth = 200
  ),

  # Sidebar with fixed width
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "sidebar",
      menuItem("Welcome", tabName = "welcome", icon = icon("home"), selected = TRUE),
      menuItem("Create Dataset", tabName = "create", icon = icon("plus-circle")),
      menuItem("Validate Dataset", tabName = "validate", icon = icon("check-circle")),
      menuItem("Update Dictionary", tabName = "dictionary", icon = icon("book")),
      menuItem("Dataset Explorer", tabName = "explorer", icon = icon("table"))
    )
  ),

  # Body
  dashboardBody(
    useShinyjs(),

    # Pin the Psych-DS logo (top-left) and the sidebar while the page
    # scrolls. The header's navbar itself is hidden in styles.css, leaving
    # a logo-only header.
    tags$script(HTML("$(document).ready(function(){ $('body').addClass('fixed'); });")),

    # External resources
    tags$head(
      # Include the external CSS file with cache-busting
      tags$link(rel = "stylesheet", type = "text/css",
               href = paste0("css/styles.css?v=", .css_version)),
      # All app CSS lives in www/css/styles.css (single source of truth);
      # do not add inline tags$style() blocks here.
      # Include SortableJS library for drag-and-drop functionality (bundled locally)
      tags$script(src = "js/sortable.min.js"),
      
      # Include EventEmitter for validator event handling (bundled locally)
      tags$script(src = "js/eventemitter3.min.js"),
      
      # Include jsonld library for validation
      tags$script(src = "js/jsonld.min.js"),

      # Include the validator.js module
      tags$script(src = "js/validator.js", type = "module"),
      tags$script(src = "js/validator-utils.js")
    ),

    # JavaScript handlers for UI interactions
    
    #' UI refresh handler - forces redraw of UI elements
    #' Used when dynamic elements need to be refreshed
    tags$script(HTML("
      Shiny.addCustomMessageHandler('refreshUI', function(message) {
        // Force a redraw by slightly resizing elements
        $('.step-circle').each(function() {
          var $this = $(this);
          var w = $this.width();
          $this.width(w+1);
          setTimeout(function() { $this.width(w); }, 50);
        });
      });
    ")),
    
    #' Sidebar step indicator handler - updates step indicators in sidebar
    tags$script(HTML("
      // Track the highest step reached (for controlling clickability)
      var highestStepReached = 1;
      var currentStep = 1;
      
      // Create step indicators and inject them after Create Dataset menu item
      $(document).on('shiny:connected', function() {
        // Create the step indicators container (initially hidden)
        var indicatorsHtml = '<div id=\"create-step-indicators\" style=\"display: none;\">' +
          '<button id=\"step-ind-1\" class=\"step-indicator active clickable\" type=\"button\">1</button>' +
          '<span class=\"step-separator\">—</span>' +
          '<button id=\"step-ind-2\" class=\"step-indicator\" type=\"button\">2</button>' +
          '<span class=\"step-separator\">—</span>' +
          '<button id=\"step-ind-3\" class=\"step-indicator\" type=\"button\">3</button>' +
          '</div>';
        
        // Insert after the Create Dataset menu item (second item in menu, index 1)
        var createDatasetItem = $('.sidebar-menu li').eq(1);
        if (createDatasetItem.length) {
          $(indicatorsHtml).insertAfter(createDatasetItem);
        }
        
        // Click handlers for step indicators
        $(document).on('click', '#step-ind-1', function() {
          if (highestStepReached >= 1) {
            Shiny.setInputValue('sidebar_goto_step', 1, {priority: 'event'});
          }
        });
        
        $(document).on('click', '#step-ind-2', function() {
          if (highestStepReached >= 2) {
            Shiny.setInputValue('sidebar_goto_step', 2, {priority: 'event'});
          }
        });
        
        $(document).on('click', '#step-ind-3', function() {
          if (highestStepReached >= 3) {
            Shiny.setInputValue('sidebar_goto_step', 3, {priority: 'event'});
          }
        });
        
        // Show/hide based on active tab - check on initialization
        setTimeout(function() {
          var activeTab = $('.sidebar-menu .active[data-value]').data('value');
          if (activeTab === 'create') {
            $('#create-step-indicators').show();
          }
        }, 100);
        
        // Listen for tab changes
        $(document).on('click', '.sidebar-menu a', function() {
          var tabName = $(this).data('value');
          $('#create-step-indicators').toggle(tabName === 'create');
        });
      });
      
      Shiny.addCustomMessageHandler('updateSidebarStep', function(message) {
        currentStep = message.step;
        
        // Update highest step reached
        if (currentStep > highestStepReached) {
          highestStepReached = currentStep;
        }
        
        // Update step indicators
        for (var i = 1; i <= 3; i++) {
          var indicator = document.getElementById('step-ind-' + i);
          if (indicator) {
            indicator.classList.remove('active', 'completed', 'clickable');
            
            if (i < currentStep) {
              // Previous steps are completed and clickable
              indicator.classList.add('completed', 'clickable');
            } else if (i === currentStep) {
              // Current step is active and clickable
              indicator.classList.add('active', 'clickable');
            } else if (i <= highestStepReached) {
              // Future steps that have been reached are clickable
              indicator.classList.add('clickable');
            }
            // Steps not yet reached remain unclickable (default state)
          }
        }
      });
    ")),

    tags$script(HTML("
      Shiny.addCustomMessageHandler('scrollToElement', function(message) {
        var element = document.getElementById(message.elementId);
        if (element) {
          element.scrollIntoView({
            behavior: 'smooth',
            block: 'center'
          });
        }
      });
    ")),
    
    #' File download handler - allows downloading files generated by the app
    tags$script(HTML("
      Shiny.addCustomMessageHandler('downloadFile', function(message) {
        var link = document.createElement('a');
        link.href = window.URL.createObjectURL(new Blob([message.filepath]));
        link.download = message.filename;
        link.click();
      });
    ")),
    
    #' Tab change handler - allows programmatically changing tabs
    tags$script(HTML("
      Shiny.addCustomMessageHandler('changeTab', function(message) {
        // Find the sidebar menu item and trigger a click
        var tabItem = $('.sidebar-menu a[data-value=\"' + message.tabName + '\"]');
        if (tabItem.length) {
          tabItem.click();
        }
      });
      Shiny.addCustomMessageHandler('scrollToTop', function(message) {
        window.scrollTo({top: 0, behavior: 'smooth'});
      });
    ")),

    # Application tab items
    tabItems(
      # Welcome Tab (Landing Page)
      tabItem(
        tabName = "welcome",
        welcomeUI("welcome_page")
      ),

      # Create Dataset Tab
      tabItem(
        tabName = "create",
        uiOutput("create_dataset_ui")
      ),

      # Validate Dataset Tab
      tabItem(
        tabName = "validate",
        validateUI("validate_dataset")
      ),

      # Update Dictionary Tab
      tabItem(
        tabName = "dictionary",
        dataDictionaryUI("data_dictionary")
      ),

      # Dataset Explorer Tab
      tabItem(
        tabName = "explorer",
        datasetExplorerUI("dataset_explorer")
      )
    )
  )
)
