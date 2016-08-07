library(shiny)
library(shinyjs)
library(ggplot2)


fetch_obj_names <- function(class, compliment = F){
  which_obj_names <- sapply(as.list(.GlobalEnv), class = class, function(x, class){
    ifelse(class %in% class(x), TRUE, FALSE)
  })
  # compliment provides the opposite results if TRUE
  if(compliment == F){
    obj_names = names(as.list(.GlobalEnv))[which_obj_names]
  } else {
    obj_names = names(as.list(.GlobalEnv))[!which_obj_names]
  }
  return(obj_names)
}
code_str <<- ""


shinyServer(function(input, output) {
  # ---------------------------------------------------------------------------------------
  # Determine all objects in .GlobalEnv that are:
  # data.frames
  output$data_selection = renderUI({
    selectInput("data_selection", label = "Data", choices = fetch_obj_names("data.frame"), width = "240px")
  })
  # non data.frames
  output$label_selection = renderUI({
    selectInput("label_selection", label = "Labels", choices = fetch_obj_names("data.frame", T), width = "240px")
  })
  # model reults selection
  output$results_vis = renderUI({
    selectInput("results_vis", label = "Model Results:", choices = fetch_obj_names("data.frame", T), width = "240px")
  })
  # model result options
  output$perf_vis_opts = renderUI({
    selectInput("perf_vis_opts", label = "Options:", width = "240px",
                choices = c("ROC Curve",
                            "plot"))

    })
  # Run Model -----------------------------------------------------------------------------
  observeEvent(input$submit_model, {
    print("Model Submitted")
    print(code_str)
    eval(parse(text = code_str))
  })

  # Get Code Output -----------------------------------------------------------------------
  observe({

    if (input$model_type == 1){
      # Training & Testing ------------------------------------------------------------------
      code_str <<- paste0(input$model_name, " <- blkbox(data = Partition(",
                          input$data_selection, ", ", input$label_selection, ", ",
                          "size = ", input$partition_slider,
                          ifelse(is.na(input$partition_seed) || input$partition_seed_ask == F, "", paste0(", seed = ", input$partition_seed)),")",
                          ifelse(input$ntree != 500 && input$options == T, paste0(",\n       ntrees = ", input$ntree), ""),
                          ifelse(!is.na(input$mtry) && input$options == T, paste0(",\n       mTry = ", input$mtry), ""),
                          ifelse(input$svm_kernel != "linear" && input$options == T, paste0(",\n       Kernel = ", "'", input$svm_kernel, "'"), ""),
                          ifelse(!is.na(input$svm_gamma) && input$options == T, paste0(",\n       Gamma = ", input$svm_gamma), ""),
                          ifelse(!is.na(input$max_depth) && input$options == T, paste0(",\n       max.depth = ", input$max_depth), ""),
                          ifelse(input$exclude_alg == "0" && length(input$exclude_alg) == 1, "",
                                 paste0(",\n       exclude = c(", toString(paste0("'", input$exclude_alg[-which(input$exclude_alg %in% "0")])),")")),
                          ifelse(is.na(input$seed) || input$seed_ask == F, "", paste0(",\n       seed = ", input$seed)),")")

    } else if (input$model_type == 2){
      # Cross-fold Validation ---------------------------------------------------------------
      code_str <<- paste0(input$model_name, " <- blkboxCV(data = ", input$data_selection,
                          ", labels = ", input$label_selection,
                          ifelse(input$fold_number == 10, "", paste0(",\n       folds = ", input$fold_number)),
                          ifelse(input$cv_repeat_number == 1, "", paste0(",\n       repeats = ", input$cv_repeat_number)),
                          ifelse(input$cv_auc_slider == 0.5, "", paste0(",\n       AUC = ", input$cv_auc_slider)),
                          ifelse(input$cv_method == "GLM", "", paste0(",\n       Method = ", "'", input$cv_method, "'")),
                          ifelse(input$ntree != 500 && input$options == T, paste0(",\n       ntrees = ", input$ntree), ""),
                          ifelse(!is.na(input$mtry) && input$options == T, paste0(",\n       mTry = ", input$mtry), ""),
                          ifelse(input$svm_kernel != "linear" && input$options == T, paste0(",\n       Kernel = ", "'", input$svm_kernel, "'"), ""),
                          ifelse(!is.na(input$svm_gamma) && input$options == T, paste0(",\n       Gamma = ", input$svm_gamma), ""),
                          ifelse(!is.na(input$max_depth) && input$options == T, paste0(",\n       max.depth = ", input$max_depth), ""),
                          ifelse(input$exclude_alg == "0" && length(input$exclude_alg) == 1, "",
                                 paste0(",\n       exclude = c(", toString(paste0("'", input$exclude_alg[-which(input$exclude_alg %in% "0")])),")")),
                          ifelse(is.na(input$seed) || input$seed_ask == F, "", paste0(",\n       seed = ", input$seed)),")")

    } else if (input$model_type ==3 ){
      # Nested Cross-fold Validation -------------------------------------------------------
      code_str <<- paste0(input$model_name, " <- blkboxNCV(data = ", input$data_selection,
                          ", labels = ", input$label_selection,
                          ifelse(input$inner_folds == 10, "", paste0(",\n       folds = ", input$fold_number)),
                          ifelse(input$outer_folds == 10, "", paste0(",\n       folds = ", input$fold_number)),
                          ifelse(input$ncv_repeat_number == 1, "", paste0(",\n       repeats = ", input$ncv_repeat_number)),
                          ifelse(input$ncv_auc_slider == 0.5, "", paste0(",\n       AUC = ", input$ncv_auc_slider)),
                          ifelse(input$ncv_method == "GLM", "", paste0(",\n       Method = ", "'", input$ncv_method, "'")),
                          ifelse(input$ntree != 500 && input$options == T, paste0(",\n       ntrees = ", input$ntree), ""),
                          ifelse(!is.na(input$mtry) && input$options == T, paste0(",\n       mTry = ", input$mtry), ""),
                          ifelse(input$svm_kernel != "linear" && input$options == T, paste0(",\n       Kernel = ", "'", input$svm_kernel, "'"), ""),
                          ifelse(!is.na(input$svm_gamma) && input$options == T, paste0(",\n       Gamma = ", input$svm_gamma), ""),
                          ifelse(!is.na(input$max_depth) && input$options == T, paste0(",\n       max.depth = ", input$max_depth), ""),
                          ifelse(input$exclude_alg == "0" && length(input$exclude_alg) == 1, "",
                                 paste0(",\n       exclude = c(", toString(paste0("'", input$exclude_alg[-which(input$exclude_alg %in% "0")])),")")),
                          ifelse(is.na(input$seed) || input$seed_ask == F, "", paste0(",\n       seed = ", input$seed)),")")
    }

    output$code <- renderText({code_str})

  })

  observeEvent(input$viewpane_on, {
    show("viewpane")
    toggle("top_panel")
    toggle("visualis_opts")
    toggle("viewpane_on")
    toggle("model_on")
    toggle("get_code")
    toggle("submit_model")
    toggle("model_type")
    toggle("TT")
    toggle("CV")
    toggle("NCV")
    toggle("exclude_opts")
    toggle("hide_div")
    hide("code_well")
    hide("perf_vis_opts")
  })

  observeEvent(input$model_on, {
    hide("viewpane")
    toggle("top_panel")
    toggle("visualis_opts")
    toggle("model_on")
    toggle("viewpane_on")
    toggle("get_code")
    toggle("submit_model")
    toggle("model_type")
    toggle("TT")
    toggle("CV")
    toggle("NCV")
    toggle("exclude_opts")
    toggle("hide_div")
  })


  observeEvent(input$get_code, {
    toggle("code_well")
  })

  observeEvent(input$vis_type, {
    if(input$vis_type == "Model Performance"){
      show("perf_vis_opts")
    } else {
      hide("perf_vis_opts")
    }
  })
  observeEvent(input$vis_type, {
    if(input$vis_type != "--Choose Visualisation--"){
      show("xxx")
    } else {
      hide("xxx")
    }
  })


  # Performance Output of Model -----------------------------------------------------------


  output$temp_plot = renderPlot({
    if (input$vis_type == "Model Performance"){
      if (input$perf_vis_opts == "ROC Curve"){
        if(length(input$results_vis) < 6){
          vis <- eval(parse(text = paste0("blkboxROC(Performance(", input$results_vis, "))")))
        } else {
          vis <- eval(parse(text = paste0("blkboxROC(", input$results_vis, ")")))
        }
      } else {
        if(eval(parse(text = paste("length(", input$results_vis, ")"))) < 6){
          vis <- eval(parse(text = paste0("cv.plot(", input$results_vis, ", metric = 'AUROC')")))
        } else {
          vis <- eval(parse(text = paste0("ncv.plot(", input$results_vis, ")")))
        }
      }
      vis
    }
  })

  # Coming Soon ---------------------------------------------------------------------------

  # ---------------------------------------------------------------------------------------

})
