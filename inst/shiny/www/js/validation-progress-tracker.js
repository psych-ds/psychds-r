// ValidationProgressTracker implementation for Shiny integration
window.ValidationProgressTracker = class ValidationProgressTracker {
    constructor(emitter) {
      this.emitter = emitter;
      this.steps = [
        {
          key: "start",
          message: {
            imperative: "Start validation",
            pastTense: "Validation started"
          },
          subSteps: []
        },
        {
          key: "check-folder",
          message: {
            imperative: "Find project folder",
            pastTense: "Project folder found"
          },
          subSteps: [
            {
              key: "build-tree",
              message: {
                imperative: "Crawl project folder and construct file tree",
                pastTense: "Project folder crawled and file tree constructed"
              }
            }
          ]
        },
        {
          key: "find-metadata",
          message: {
            imperative: "Find metadata file",
            pastTense: 'Metadata file "dataset_description.json" found in the root folder'
          },
          subSteps: []
        },
        {
          key: "find-data-dir",
          message: {
            imperative: `Find "data" subfolder`,
            pastTense: `"data" subfolder found in the root folder`
          },
          subSteps: []
        },
        {
          key: "parse-metadata",
          message: {
            imperative: 'Parse "dataset_description.json" metadata file',
            pastTense: 'Successfully parsed "dataset_description.json" metadata file'
          },
          subSteps: [
            {
              key: "metadata-utf8",
              message: {
                imperative: "Check metadata file for utf-8 encoding",
                pastTense: "Metadata file is utf-8 encoded"
              }
            },
            {
              key: "metadata-json",
              message: {
                imperative: "Parse metadata file as JSON",
                pastTense: "Metadata file parsed successfully"
              }
            },
            {
              key: "metadata-jsonld",
              message: {
                imperative: "Validate metadata file as JSON-LD",
                pastTense: "Metadata file is valid JSON-LD"
              }
            },
            {
              key: "metadata-fields",
              message: {
                imperative: `Check metadata file for required "name", "description", and "variableMeasured" fields`,
                pastTense: `Metadata file contains required "name", "description", and "variableMeasured" fields.`
              }
            },
            {
              key: "metadata-type",
              message: {
                imperative: 'Check metadata file for field "@type" with value "Dataset"',
                pastTense: 'Metadata file has "@type" field with value "Dataset"'
              }
            }
          ]
        },
        {
          key: "check-for-csv",
          message: {
            imperative: `Check for CSV data files in "data" subfolder`,
            pastTense: `CSV data files found in "data" subfolder`
          },
          subSteps: []
        },
        {
          key: "validate-csvs",
          message: {
            imperative: `Check that all CSV data files are valid`,
            pastTense: `All CSV data files are valid`
          },
          subSteps: [
            {
              key: "csv-keywords",
              message: {
                imperative: `Check filename for keyword formatting `,
                pastTense: `Filename uses valid keyword formatting`
              }
            },
            {
              key: "csv-parse",
              message: {
                imperative: `Parse data file as CSV`,
                pastTense: `Data file successfully parsed as CSV`
              }
            },
            {
              key: "csv-header",
              message: {
                imperative: `Check for header line`,
                pastTense: `Header line found`
              }
            },
            {
              key: "csv-header-repeat",
              message: {
                imperative: `Check for redundant column names`,
                pastTense: `No redundant column names found`
              }
            },
            {
              key: "csv-nomismatch",
              message: {
                imperative: `Check all lines for equal number of cells`,
                pastTense: `All lines have equal number of cells`
              }
            },
            {
              key: "csv-rowid",
              message: {
                imperative: `Check for any row_id columns with non-unique values`,
                pastTense: `All row_id columns have unique values`
              }
            }
          ]
        },
        {
          key: "check-variableMeasured",
          message: {
            imperative: `Confirm that all column headers in CSV data files are found in "variableMeasured" metadata field`,
            pastTense: `All column headers in CSV data files were found in "variableMeasured" metadata field`
          },
          subSteps: []
        }
      ];
      
      this.stepStatus = new Map();
      this.initializeStepStatus();
      
      // Set up event listeners
      this.setupListeners();
      console.log("ValidationProgressTracker initialized with steps");
    }
    
    initializeStepStatus() {
      this.steps.forEach((superStep) => {
        this.stepStatus.set(superStep.key, { complete: false, success: false });
        superStep.subSteps.forEach((subStep) => {
          this.stepStatus.set(subStep.key, { complete: false, success: false });
        });
      });
    }
    
    setupListeners() {
      this.steps.forEach((superStep) => {
        if (superStep.subSteps.length === 0) {
          this.emitter.once(superStep.key, (data) => {
            this.updateStepStatus(superStep.key, data, superStep);
          });
        } else {
          superStep.subSteps.forEach((subStep) => {
            this.emitter.once(subStep.key, (data) => {
              this.updateStepStatus(subStep.key, data, superStep);
            });
          });
        }
      });
    }
    
    updateStepStatus(stepKey, data, superStep) {
      console.log(`Updating step status for ${stepKey}:`, data);
      this.stepStatus.set(stepKey, {
        complete: true,
        success: data.success,
        issue: data.issue
      });
      
      if (superStep && superStep.subSteps.length > 0) {
        this.updateSuperStepStatus(superStep);
      }
      
      // Emit stepStatusChange event
      this.emitter.emit("stepStatusChange", {
        stepStatus: Array.from(this.stepStatus.entries()),
        superStep: superStep
      });
    }
    
    updateSuperStepStatus(superStep) {
      const allSubStepsComplete = superStep.subSteps.every((subStep) => 
        this.stepStatus.get(subStep.key)?.complete
      );
      const allSubStepsSuccess = superStep.subSteps.every((subStep) => 
        this.stepStatus.get(subStep.key)?.success
      );
      
      this.stepStatus.set(superStep.key, {
        complete: allSubStepsComplete,
        success: allSubStepsSuccess,
        issue: undefined
      });
    }
    
    waitForCompletion() {
      return new Promise((resolve) => {
        this.emitter.once("complete", () => {
          resolve();
        });
        
        this.emitter.once("validation-halted", () => {
          resolve();
        });
      });
    }
  }