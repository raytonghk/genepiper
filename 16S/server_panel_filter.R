output$filterColumnType <- reactive("None")
outputOptions(output, "filterColumnType", suspendWhenHidden = FALSE)

### Filter Sample Column
observe({
  req(vals$phyloseq)
  updateSelectInput(session, "filterSampleColumn", choices = c("Please Select", variableNames(vals$phyloseq)))
})

# reset select column when command line entered
observe({
  req(input$filterSampleText != "")
  updateSelectInput(session, "filterSampleColumn", selected = "Please Select")
})

# reset command line when select column
observe({
  req(input$filterSampleColumn != "Please Select")
  updateTextInput(session, "filterSampleText", value = "")
})

# check filter column type
observe({
  req(vals$phyloseq, input$filterSampleColumn, input$filterSampleColumn != "Please Select")
  output$filterColumnType <- reactive(variableType(vals$phyloseq, input$filterSampleColumn))
})

### Filter Sample Checkbox
observe({
  req(vals$phyloseq, input$filterSampleColumn,
      input$filterSampleColumn != "Please Select",
      variableType(vals$phyloseq, input$filterSampleColumn) == "character"
  )
  updateCheckboxGroupInput(session, "filterSampleCheckbox",
                           choices = unique(get_variable(vals$phyloseq, input$filterSampleColumn)),
                           selected = unique(get_variable(vals$phyloseq, input$filterSampleColumn)))
})

### Filter Button
observeEvent(input$filterButton,
             {
               vals$filterSampleMessage <- NULL
               vals$filteredPhyloseq <- NULL
               tryCatch(
                 {
                   req(vals$phyloseq)
                   vals$filteredPhyloseq <- getFilteredPhyloseq(vals$phyloseq, input$filterSampleText, input$filterSampleColumn)
                 },
                 error = function(e) {
                   vals$filterSampleMessage <- "Error: Filter is not correct."
                 }
               )
             }
)

getFilteredPhyloseq <- function(phyloseq, filterText, filterColumn) {
  if(filterText == "" && filterColumn != "Please Select") {
    filterText <- filterTextByColumn(phyloseq, filterColumn)
  }
  eval(parse(text = paste0("subset_samples(phyloseq, ", filterText, ")")))
}

filterTextByColumn <- function(phyloseq, filterColumn, id = NULL) {
  variableType <- variableType(phyloseq, filterColumn)
  if(variableType == "numeric") {
    filterTextByNumericColumn(filterColumn, id = id)
  } else if(variableType == "character") {
    filterTextByCharacterColumn(filterColumn, id = id)
  }
}

filterTextByNumericColumn <- function(filterColumn, id = NULL) {
  paste(filterColumn, input[[paste0("filterSampleOperator", id)]], input[[paste0("filterSampleNumeric", id)]])
}

filterTextByCharacterColumn <- function(filterColumn, id = NULL) {
  paste(filterColumn, "%in% c(", paste(shQuote(input[[paste0("filterSampleCheckbox", id)]]), collapse = ", "), ")")
}

output$filterSampleMessage <- renderText(vals$filterSampleMessage)

### Filtered Phyloseq Details
output$filteredPhyloseqDetails <- renderPrint(showFilteredPhyloseq(vals$filteredPhyloseq))

showFilteredPhyloseq <- function(phyloseq) {
  cat("Filtered Phyloseq: \n\n")
  phyloseq::show(phyloseq)
}

### Filter Module
observe(
  {
    disable("addFilter")
    disable("filterModuleButton")
    req(vals$phyloseq)
    enable("addFilter")
    enable("filterModuleButton")
  }
)

observeEvent(
  input$addFilter,
  {
    filterNum <- input$addFilter
    insertUI("#filterPanelFooter", "beforeBegin", uiFilterModulePanel(filterNum))
    vals$filterStack <- c(vals$filterStack, filterNum)
    observeFilterColumn(filterNum)
    observeUpdateFilterSampleCheckbox(filterNum)
    observeUpdateFilterTaxaCheckbox(filterNum)
    observeRemoveFilterButton(filterNum)
  }
)

observeFilterColumn <- function(id) {
  output[[paste0("filterSampleColumnType", id)]] <- reactive("None")
  outputOptions(output, paste0("filterSampleColumnType", id), suspendWhenHidden = FALSE)
  
  observe(
    {
      req(vals$phyloseq, input[[paste0("filterSampleColumn", id)]], input[[paste0("filterSampleColumn", id)]] != "Please Select")
      output[[paste0("filterSampleColumnType", id)]] <- reactive(variableType(vals$phyloseq, input[[paste0("filterSampleColumn", id)]]))
    }
  )
}

observeUpdateFilterSampleCheckbox <- function(id) {
  observe(
    {
      req(vals$phyloseq, input[[paste0("filterSampleColumn", id)]],
          input[[paste0("filterSampleColumn", id)]] != "Please Select",
          variableType(vals$phyloseq, input[[paste0("filterSampleColumn", id)]]) == "character"
      )
      updateCheckboxGroupInput(session, paste0("filterSampleCheckbox", id),
                               choices = unique(get_variable(vals$phyloseq, input[[paste0("filterSampleColumn", id)]])),
                               selected = unique(get_variable(vals$phyloseq, input[[paste0("filterSampleColumn", id)]])))
    }
  )
}

observeUpdateFilterTaxaCheckbox <- function(id) {
  observe(
    {
      req(vals$phyloseq, input[[paste0("filterTaxaRank", id)]])
      updateCheckboxGroupInput(session, paste0("filterTaxaCheckbox", id),
                               choices = taxaNames(vals$phyloseq, input[[paste0("filterTaxaRank", id)]]),
                               selected = taxaNames(vals$phyloseq, input[[paste0("filterTaxaRank", id)]]))
    }
  )
}

observeRemoveFilterButton <- function(id) {
  observeEvent(
    input[[paste0("removeFilterButton", id)]],
    {
      vals$filterStack <- vals$filterStack[vals$filterStack != id]
      removeUI(paste0("#filter", id))
    }
  )
}

uiFilterModulePanel <- function(id) {
  tags$div(
    id = paste0("filter", id),
    selectInput(paste0("filterSubject", id), NULL, c("Sample", "Taxa")),
    filterSampleConditionalPanel(id),
    filterTaxaConditionalPanel(id),
    errorOutput(paste0("filterMessage", id)),
    actionButton(paste0("removeFilterButton", id), "-"),
    tags$hr()
  )
}

filterTaxaConditionalPanel <- function(id) {
  conditionalPanel(
    condition = paste0("input.filterSubject", id, "=='Taxa'"),
    tags$div(
      tags$div(
        class = "column-left",
        style = "width: auto; margin-right: 5px;",
        h5("By")
      ),
      tags$div(
        class = "column-right",
        selectInput(paste0("filterTaxaMethod", id), NULL, c("Count", "Rank"))
      )
    ),
    filterTaxaByCountConditionalPanel(id),
    filterTaxaByRankConditionalPanel(id)
  )
}

filterTaxaByCountConditionalPanel <- function(id) {
  conditionalPanel(
    condition = paste0("input.filterTaxaMethod", id, "=='Count'"),
    tags$div(
      tags$div(
        class = "column-left",
        style = "width: auto; margin-right: 5px;",
        h5("Prevalence Cutoff (%):")
      ),
      tags$div(
        class = "column-right",
        numericInput(paste0("prevalenceCutoff", id), NULL, 0, 0, 100, 10)
      )
    ),
    tags$div(
      tags$div(
        class = "column-left",
        style = "width: auto; margin-right: 5px;",
        h5("Abundance Cutoff:")
      ),
      tags$div(
        class = "column-right",
        numericInput(paste0("abundanceCutoff", id), NULL, 1, 0)
      )
    )
  )
}

filterTaxaByRankConditionalPanel <- function(id) {
  conditionalPanel(
    condition = paste0("input.filterTaxaMethod", id, "=='Rank'"),
    selectInput(paste0("filterTaxaRank", id), NULL, rank_names(vals$phyloseq)),
    checkboxGroupInput(paste0("filterTaxaCheckbox", id), NULL, NULL)
  )
}

filterSampleConditionalPanel <- function(id) {
  conditionalPanel(
    condition = paste0("input.filterSubject", id, "=='Sample'"),
    tags$div(
      tags$div(
        class = "column-left",
        style = "width: auto; margin-right: 5px;",
        h5("By")
      ),
      tags$div(
        class = "column-right",
        selectInput(paste0("filterSampleMethod", id), NULL, c("Column", "Formula"))
      )
    ),
    filterSampleByFormulaConditionalPanel(id),
    filterSampleByColumnConditionalPanel(id)
  )
}

filterSampleByColumnConditionalPanel <- function(id) {
  conditionalPanel(
    condition = paste0("input.filterSampleMethod", id, "=='Column'"),
    selectInput(paste0("filterSampleColumn", id), NULL, choices = c("Please Select", variableNames(vals$phyloseq))),
    conditionalPanel(
      condition = paste0("output.filterSampleColumnType", id, "=='numeric'"),
      selectInput(paste0("filterSampleOperator", id), NULL, choices = c("==", ">", ">=", "<", "<=")),
      numericInput(paste("filterSampleNumeric", id), NULL, 0)
    ),
    conditionalPanel(
      condition = paste0("output.filterSampleColumnType", id, "=='character'"),
      checkboxGroupInput(paste0("filterSampleCheckbox", id), "Please Select", NULL)
    ),
    errorOutput(paste0("filterSampleMessage", id))
  )
}

filterSampleByFormulaConditionalPanel <- function(id) {
  conditionalPanel(
    condition = paste0("input.filterSampleMethod", id, "=='Formula'"),
    textInput(paste0("filterSampleText", id), "Command Line", placeholder = "e.g. Blood == \'A\'")
  )
}

output$moduleFilteredPhyloseqDetails <- renderPrint(showFilteredPhyloseq(vals$moduleFilteredPhyloseq))

observeEvent(
  input$filterModuleButton,
  {
    vals$moduleFilteredPhyloseq <- NULL
    req(vals$filterStack, vals$phyloseq)
    ps <- vals$phyloseq
    
    for(id in vals$filterStack) {
      output[[paste0("filterMessage", id)]] <- NULL
      ps <- filterPhyloseq(ps, input[[paste0("filterSubject", id)]], id)
    }
    
    vals$moduleFilteredPhyloseq <- ps
  }
)

filterPhyloseq <- function(ps, filterSubject, id) {
  if(filterSubject == "Sample") {
    filterPhyloseqSample(ps, id)
  } else if (filterSubject == "Taxa") {
    filterPhyloseqTaxa(ps, id)
  }
}

filterPhyloseqSample <- function(ps, id) {
  if(input[[paste0("filterSampleMethod", id)]] == "Column") {
    filterPhyloseqSampleByColumn(ps, id)
  } else {
    filterPhyloseqSampleByFormula(ps, id)
  }
}

filterPhyloseqSampleByColumn <- function(ps, id) {
  if(input[[paste0("filterSampleColumn", id)]] != "Please Select") {
    filterText <- filterTextByColumn(ps, input[[paste0("filterSampleColumn", id)]], id)
    eval(parse(text = paste0("subset_samples(ps, ", filterText, ")")))
  } else {
    output[[paste0("filterMessage", id)]] <- renderText(HTML("Error: Column was empty."))
  }
}

filterPhyloseqSampleByFormula <- function(ps, id) {
  if(input[[paste0("filterSampleText", id)]] != "") {
    tryCatch(
      {
        eval(parse(text = paste0("subset_samples(ps, ", input[[paste0("filterSampleText", id)]], ")")))
      },
      error = function(e) {
        output[[paste0("filterMessage", id)]] <- renderText(HTML("Error: Filter text error."))
      }
    )
  } else {
    output[[paste0("filterMessage", id)]] <- renderText(HTML("Error: Filter text empty."))
  }
  
}

filterPhyloseqTaxa <- function(ps, id) {
  if(input[[paste0("filterTaxaMethod", id)]] == "Count") {
    filterPhyloseqTaxaByCount(ps, id)
  } else {
    filterPhyloseqTaxaByRank(ps, id)
  }
}

filterPhyloseqTaxaByCount <- function(ps, id) {
  filterPhyloseqTaxaByPrevalence(ps, input[[paste0("prevalenceCutoff", id)]]) %>%
    filterPhyloseqTaxaByAbundance("abundance", input[[paste0("abundanceCutoff", id)]])
}

filterPhyloseqTaxaByRank <- function(ps, id) {
  eval(parse(text = paste0("subset_taxa(ps, ", input[[paste0("filterTaxaRank", id)]], " %in% c(",
                           paste(shQuote(input[[paste0("filterTaxaCheckbox", id)]]), collapse = ", "), "))")))
}











