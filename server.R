server <- function(input, output, session) {
    #----- General setup/navigation ----
    observe({
        data_ready <- FALSE
        if (input$data_option == "Use sample data" && input$select_sample_dat != "") {
            data_ready <- TRUE
        } else if (input$data_option == "Upload data" &&
                   !is.null(input$upload) &&
                   !is.null(input$select_freq) && input$select_freq != "" &&
                   !is.null(input$select_variables) && length(input$select_variables) > 0) {
            data_ready <- TRUE
        }
        
        if (data_ready) {
            shinyjs::show("tab1_contents")
            shinyjs::hide("welcome_panel")
        } else {
            shinyjs::hide("tab1_contents")
            shinyjs::show("welcome_panel")
        }
    })
    
    disableTabs <- function() {
        runjs("$('.nav-tabs li').addClass('disabled');")
        runjs(sprintf("$('.nav-tabs li a[data-value=\"%s\"]').parent().removeClass('disabled').addClass('active');", input$tabs))
    }
    observeEvent(input$tabs, { disableTabs() })
    observeEvent(input$next1, { updateTabsetPanel(session, "tabs", selected = "tab2") })
    observeEvent(input$back2, { updateTabsetPanel(session, "tabs", selected = "tab1") })
    observeEvent(input$next2, { updateTabsetPanel(session, "tabs", selected = "tab3") })
    observeEvent(input$back3, { updateTabsetPanel(session, "tabs", selected = "tab2") })
    observeEvent(input$next3, { updateTabsetPanel(session, "tabs", selected = "tab4") })
    observeEvent(input$back4, { updateTabsetPanel(session, "tabs", selected = "tab3") })
    
    observeEvent(input$freq_help, {
        showModal(modalDialog(
            title = "Frequency Format",
            
            tags$p(
                "Each row is a unique combination",
                " of categorical variables, along with a frequency column indicating how many times that combination occurs."
            ),
            
            tags$strong("Example:"), br(),
            tableOutput("example_table"),
            
            easyClose = TRUE,
            footer = modalButton("Close")
        ))
    })
    
    example_df <- data.frame(
        Gender = c("Male", "Female", "Male", "Female"),
        Age = c("Young", "Young", "Old", "Old"),
        freq = c(12, 15, 9, 11)
    )
    
    output$example_table <- renderTable({
        example_df
    }, rownames = FALSE)
    
    # ---- Code for App Behavior -----
    uploaded_data <- reactive({
        req(input$upload)
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ","),
               tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
               validate("Invalid file; Please upload a .csv or .tsv file"))
    })
    
    output$var_select_inputs <- renderUI({
        req(uploaded_data())
        choices <- names(uploaded_data())
        tagList(
            selectInput("select_freq", "Frequency Variable:", choices = choices),
            selectInput("select_variables", "Categorical Variables:", choices = choices, multiple = TRUE)
        )
    })
    
    
    
    select_dat <- reactive({
        if (input$data_option == "Use sample data") {
            req(input$select_sample_dat)
            data_sets[[input$select_sample_dat]]
        } else {
            req(uploaded_data(), input$select_freq, input$select_variables)
            form <- as.formula(paste(input$select_freq, "~", paste(input$select_variables, collapse = " + ")))
            xtabs(form, data = uploaded_data())
        }
    })

    
    output$table_preview <- renderUI({
        req(select_dat())
        color_table(select_dat(), legend = "")
    })
    
    preview_levels <- reactive({
        req(select_dat())
        detect_levels(select_dat(), is_table = TRUE)
    })
    
    output$level_preview <- renderPrint({
        req(preview_levels())
        cat(paste(lapply(names(preview_levels()), function(nm) {
            paste0(nm, ": ", paste(preview_levels()[[nm]], collapse = ", "))
        }), collapse = "\n\n"))
    })
    
    observeEvent(select_dat(), {
        updateSelectInput(session, "select_factors", choices = names(preview_levels()))
    })
    
    mod_dat <- reactive({
        if (is.null(input$select_factors) || length(input$select_factors) == 0) {
            select_dat()
        } else {
            xtabs(convert_xtabs_formula(input$select_factors), data = select_dat())
        }
    })
    
    dat_factors <- reactive({
        names(detect_levels(mod_dat()))
    })
    
    output$preview_vars <- renderText({
        fcts <- paste(dat_factors(), collapse = ", ")
        paste("Factors:", fcts)
    })
    
    reasonable_formulas <- reactive({
        show_all_formulas(dat_factors())
    })
    
    output$select_formula_ui <- renderUI({
        req(input$customize_formula_options == "Select model type")
        selectInput("selected_formula", "Choose a model type:", choices = reasonable_formulas())
    })
    
    output$formula_preview <- renderText({
        req(input$customize_formula_options == "Select model type")
        if(is.null(input$selected_formula)) "Please specify a default formula"
        paste0("Formula: ", as.character(input$selected_formula))
    })
    
    mod_dat_form <- reactive({
        if (input$customize_formula_options == "Write custom formula") {
            req(input$custom_formula)
            f <- try(as.formula(input$custom_formula), silent = TRUE)
            validate(need(!inherits(f, "try-error"), "Please input a valid formula (e.g., Freq ~ A + B)."))
            f
        } else if (input$customize_formula_options == "Select model type") {
            req(input$selected_formula)
            as.formula(input$selected_formula)
        } else {
            convert_loglm_formula(dat_factors())
        }
    })
    
    observeEvent(input$customize_formula_options, {
        if (input$customize_formula_options != "No customization") {
            updateSelectInput(session, "residual_type", choices = c("pearson", "deviance", "rstandard"))
        } else {
            updateSelectInput(session, "residual_type", choices = c("pearson", "deviance", "ft"))
        }
    })
    
    selected_shading <- reactive({
        switch(input$shading_type,
               "Friendly" = vcd::shading_Friendly(),
               "Friendly2" = vcd::shading_Friendly2(),
               "sieve" = vcd::shading_sieve(),
               "custom" = vcd::shading_binary(col = c(input$color_1, input$color_2)))
    })
    
    output$mosaic_plot <- renderPlot({
        req(mod_dat())
        customized <- input$customize_formula_options != "No customization"
        args <- list(gp = selected_shading(), split_vertical = "V" %in% input$split)
        
        mod.glm <- NULL
        if (customized) {
            df <- as.data.frame(mod_dat())
            mod.glm <- glm(mod_dat_form(), data = df, family = poisson())
            args$x <- mod.glm
            args$data <- df
        } else {
            args$x <- mod_dat()
        }
        
        # Always input appropriate residuals
        args$residuals_type <- input$residual_type
        
        if (input$show_residuals) {
            args$labeling <- labeling_residuals()
        }
        
        # Helper function to make plots output more flexibly
        wrap_title <- function(x, width = 60) {
            paste(strwrap(x, width = width), collapse = "\n")
        }
        
        main_txt <- NULL
        if (input$show_formula) main_txt <- paste(deparse(mod_dat_form()), collapse = "")
        if (input$show_g_square & customized) {
            g2_val <- paste0("G² = ", round(deviance(mod.glm), 2))
            main_txt <- if (is.null(main_txt)) g2_val else paste(main_txt, g2_val, sep = ", ")
        }
        
        if (!is.null(main_txt)) {
            args$main <- wrap_title(main_txt, 60)
            args$main_gp <- grid::gpar(fontsize = 12)
        }
        
        suppressWarnings(do.call(vcd::mosaic, args))
    })
}
