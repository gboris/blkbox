library(shiny)

shinyServer(function(input, output) {
  # ---------------------------------------------------------------------------------------

  # Run Model -----------------------------------------------------------------------------
  # Coming Soon


  # Get Code Output -----------------------------------------------------------------------

  observeEvent(input$get_code, {

    if (input$model_type == 1){
    # Training & Testing ------------------------------------------------------------------
      code <- paste0("blkbox(data = Partition(",
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
      code <- paste0("blkboxCV(data = ", input$data_selection,
                     ", labels = ", input$label_selection,
                     ifelse(input$fold_number == 10, "", paste0(",\n       folds = ", input$fold_number)),
                     ifelse(input$cv_repeat_number == 1, "", paste0(",\n       repeats = ", input$cv_repeat_number)),
                     ifelse(input$cv_auc_slider == 0.5, "", paste0(",\n       AUC = ", input$cv_auc_slider)),
                     ifelse(input$cv_method == "GLM", "", paste0(",\n       Method = ", "'", input$cv_method, "'"), ""),
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
      code <- paste0("blkboxCV(data = ", input$data_selection,
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

    output$code <- renderText({code})

  })

  # Performance Output of Model -----------------------------------------------------------
  # Coming Soon

  # ---------------------------------------------------------------------------------------
})
