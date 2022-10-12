if(interactive()){
  shinyServer(function(input, output, session){
    
    
    values <- reactiveValues(df_data = NA, backup = NA)
    lista_laco_vetor <- reactiveValues(lista = c())
    table <- reactiveValues(table_arvore = NA)
    final_tree <- reactiveValues(combinacao = NA, box = NA)
    grafico <- reactiveValues(graf = NA)
    
    density_tools <- reactiveValues(varx = NA, varfator = NA)
    observeEvent(input$h_density_var_x, {
      density_tools$varx <- input$h_density_var_x
    })
    observeEvent(input$h_density_var_x, {
      density_tools$varfator <- input$h_density_var_fator
    })
    
    dis_marg_tools <- reactiveValues(varx = NA, 
                                     vary = NA, 
                                     varsep = NA, 
                                     dis_tipo = NA)
    
    observeEvent(input$h_dis_marg_var_x, {
      dis_marg_tools$varx <- input$h_dis_marg_var_x 
    })
    observeEvent(input$h_dis_marg_var_y, {
      dis_marg_tools$vary <- input$h_dis_marg_var_y
    })
    observeEvent(input$h_dis_marg_var_sep, {
      dis_marg_tools$varsep <- input$h_dis_marg_var_sep
    })
    observeEvent(input$h_dis_marg_tipo, {
      dis_marg_tools$dis_tipo <- input$h_dis_marg_tipo
    })
    
    bd_tools <- reactiveValues(varx = NULL)
    observeEvent(input$h_bd_var_x, {
      bd_tools$varx <- input$h_bd_var_x
    })
    
    observeEvent(input$base_user, {
      values$df_data <- read.csv(input$base_user$datapath,
                                 header = input$header,
                                 sep = input$sep
      )
      values$df_data$Class <- ifelse(values$df_data$Class == 'malignant', 1, 0)
      # values$df_data$Class <- values$df_data$Class %>% as.factor()
      
      # levels(values$df_data$Class)[match("malignant",
      #                                    levels(values$df_data$Class))] <- 1
      # levels(values$df_data$Class)[match("benign",
      #                                    levels(values$df_data$Class))] <- 0
      # values$df_data <- subset(values$df_data,
      #                          select = -c(1))
      # values$df_data <- values$df_data %>% select(-'Id')
      
      splits <- sample(
        c(rep(0, 0.8*nrow(values$df_data)),
          rep(1, 0.2*nrow(values$df_data)), 1))
      values$df_data <- values$df_data %>% mutate(split = splits)
      
      
      values$backup <- read.csv(input$base_user$datapath,
                                 header = input$header,
                                 sep = input$sep
      )
      values$backup$Class <- ifelse(values$df_data$Class == 'malignant', 1, 0)
      # values$backup$Class <- values$backup$Class %>% as.factor()
      # levels(values$backup$Class)[match("malignant",
      #                                    levels(values$backup$Class))] <- 1
      # levels(values$backup$Class)[match("benign",
      #                                    levels(values$backup$Class))] <- 0
      # values$backup <- subset(values$backup,
      #                          select = -c(1))
      # values$backup <- values$backup %>% select(-'Id')
      
      splits <- sample(
        c(rep(0, 0.8*nrow(values$backup)),
          rep(1, 0.2*nrow(values$backup)), 1))
      values$backup <- values$backup %>% mutate(split = splits)
    })
    observeEvent(input$lista_laco, {
      
      lista_laco_vetor$lista <- as.numeric(unlist(strsplit(input$lista_laco,",")))
      
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,inputId = "variable_y", label = "Selecione a variavel de resposta: ",
                           data = values$df_data)
    })
    observeEvent(input$criar_arvore, {
      table$table_arvore <-  algor_laco_arvore(values$df_data, 'Class ~ .', lista_laco_vetor$lista)
    })
    observeEvent(input$comb, {
      final_tree$combinacao <-  input$comb %>% as.numeric()
    })
    observeEvent(input$tab2, {
      final_tree$box <-  algor_final_arvore(table$table_arvore, 
                                                    final_tree$combinacao, 
                                                    values$df_data, 
                                                    'Class ~ .')
    })
    observe({
      req(input$base_user)
      if(input$tipo_graph == 'corr'){
        #req(input$criar_graph)
        grafico$graf <-  algor_graph_corr(values$df_data)
      } 
      else if(input$tipo_graph == 'density') {
        req(input$h_density_var_x, input$h_density_var_fator, 
            #input$criar_graph
            )
        grafico$graf <-  algor_graph_density(values$df_data, 
                                             density_tools$varx, 
                                             density_tools$varfator)
      } 
      else if(input$tipo_graph == 'dis_marg') {
        req(input$h_dis_marg_var_sep, 
            input$h_dis_marg_tipo, 
            input$h_dis_marg_var_y, 
            input$h_dis_marg_var_x,
            #input$criar_graph
            )
        
        grafico$graf <- algor_graph_marg_dist(
          values$df_data,
          var_x = dis_marg_tools$varx,
          var_y = dis_marg_tools$vary,
          # base %>%
          #   mutate(x = rexp(dim(base)[1]),
          #          y = rnorm(dim(base)[1], 10, 3)),
          #var_x = 'x',
          #var_y = 'y',
          var_sep = dis_marg_tools$varsep,
          tipo = dis_marg_tools$dis_tipo)
        
        # grafico$graf <-  algor_graph_marg_dist(values$df_data,
        #                                        dis_marg_tools$varx,
        #                                        dis_marg_tools$vary,
        #                                        dis_marg_tools$varsep)
        
        
      } 
      else if(input$tipo_graph == 'bd'){
        req(input$h_bd_var_x, input$variable_y, 
            #input$criar_graph
            )
        grafico$graf <-  algor_graph_badrate(values$df_data, input$variable_y, bd_tools$varx)
      }
    }) # NOTE - Arrumar variacao dos graficos
    
    observeEvent(input$tipo_graph, {
      if(input$tipo_graph == 'corr'){
        show("selects_para_corr")
        hide("selects_para_density")
        hide("selects_para_dis_marg")
        hide("selects_para_bd")
      } 
      else if(input$tipo_graph == 'density'){
        hide("selects_para_corr")
        show("selects_para_density")
        hide("selects_para_dis_marg")
        hide("selects_para_bd")
      }
      else if(input$tipo_graph == 'dis_marg'){
        hide("selects_para_corr")
        hide("selects_para_density")
        show("selects_para_dis_marg")
        hide("selects_para_bd")
      }
      else if(input$tipo_graph == 'bd'){
        hide("selects_para_corr")
        hide("selects_para_density")
        hide("selects_para_dis_marg")
        show("selects_para_bd")
      }
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,inputId = "h_density_var_x", label = "Selecione a variavel x: ",
                           data = values$df_data)
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,
                           inputId = "h_density_var_fator", 
                           label = "Selecione a variavel de separacao (Normalmente fator): ",
                           data = values$df_data)
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,
                           inputId = "h_dis_marg_var_x", 
                           label = "Selecione a variavel x: ",
                           data = values$df_data)
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,
                           inputId = "h_dis_marg_var_y", 
                           label = "Selecione a variavel y: ",
                           data = values$df_data)
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,
                           inputId = "h_dis_marg_var_sep", 
                           label = "Selecione a variavel de separacao (Normalmente var resposta): ",
                           data = values$df_data)
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,inputId = "h_bd_var_x", label = "Selecione a variavel x: ",
                           data = values$df_data)
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,inputId = "h_bd_var_y", label = "Selecione a variavel resposta: ",
                           data = values$df_data)
    })
    
    
    observeEvent(input$b_retirar, {
        show("base_user_retirar_wt")
        hide("base_user_modificar_wt")
        hide("base_user_resetar_wt")
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,
                           inputId = "var_to_retirar", 
                           label = "Selecione a variavel para retirar: ",
                           data = values$df_data)
    })
    observeEvent(input$b_retirar_confirmar, {
      req(input$var_to_retirar)
      values$df_data <- values$df_data %>% select(-input$var_to_retirar) 
    })
    observeEvent(input$b_retirar_confirmar, {
      updateVarSelectInput(session,
                           inputId = "var_to_retirar", 
                           label = "Selecione a variavel para retirar: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "variable_y", 
                           label = "Selecione a variavel de resposta: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_density_var_x", 
                           label = "Selecione a variavel x: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_density_var_fator", 
                           label = "Selecione a variavel de separacao (Normalmente fator): ",
                           data = values$df_data)
      # updateVarSelectInput(session,
      #                      inputId = "h_dis_marg_tipo", 
      #                      label = "Selecione os graficos marginais: ",
      #                      data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_dis_marg_var_x", 
                           label = "Selecione a variavel x: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_dis_marg_var_y", 
                           label = "Selecione a variavel y: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_dis_marg_var_sep", 
                           label = "Selecione a variavel de separacao (Normalmente var resposta): ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_bd_var_x", 
                           label = "Selecione a variavel x: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "var_modificar", 
                           label = "Selecione a variavel",
                           data = values$df_data)
    })
    
   
    observeEvent(input$b_modificar , {
      show("base_user_modificar_wt")
      hide("base_user_retirar_wt")
      hide("base_user_resetar_wt")
    })
    observeEvent(input$base_mod, {
      show("base_user_modificar_wt_column_base")
      hide("base_user_modificar_wt_column_coluna")
    })
    observeEvent(input$tirar_na, {
      req(input$base_user)
      values$df_data <- values$df_data[complete.cases(values$df_data),] 
      values$df_data <- values$df_data %>% na.omit()
    })
    observeEvent(input$col_mod, {
      hide("base_user_modificar_wt_column_base")
      show("base_user_modificar_wt_column_coluna")
    })
    observeEvent(input$base_user, {
      updateVarSelectInput(session,
                           inputId = "var_modificar", 
                           label = "Selecione a variavel",
                           data = values$df_data)
    })
    observeEvent(input$pad, {
      req(input$var_modificar)
      #values$df_data <- values$df_data %>% mutate(input$var_modificar = algor_pad(values$df_data %>% select(input$var_modificar)))
      values$df_data[[input$var_modificar]] <- algor_pad(values$df_data[[input$var_modificar]])
    })
    observeEvent(input$norm, {
      req(input$var_modificar)
      values$df_data[[input$var_modificar]] <-  algor_norm(values$df_data[[input$var_modificar]])
    })
    observeEvent(input$fat, {
      req(input$var_modificar)
      values$df_data[[input$var_modificar]] <-  as.factor(values$df_data[[input$var_modificar]])
    })
    observeEvent(input$num, {
      req(input$var_modificar)
      values$df_data[[input$var_modificar]] <-  as.numeric(values$df_data[[input$var_modificar]])
    })
    observeEvent(input$num, {
      req(input$var_modificar)
      values$df_data[[input$var_modificar]] <-  as.numeric(values$df_data[[input$var_modificar]])
    })
    observeEvent(input$clust,{
      req(input$var_modificar)
      l <- reactiveValues() 
      showModal(modalDialog(
        tags$h2('Informacoes necessarias: '),
        numericInput('agrup_clust', 'Agrupar de quanto em quanto? ', value = 1),
        footer=tagList(
          actionButton('submit', 'Submit'),
          modalButton('cancel')
        )
      ))
      
      observeEvent(input$submit, {
        removeModal()
        l$agrup <- 1
      })
      
      observeEvent(input$agrup_clust,{
        values$df_data[[input$var_modificar]] <- round(values$df_data[[input$var_modificar]]/1)*1
      })
      
    })
    output$var_modificar_summary <-  DT::renderDataTable({
      req(input$var_modificar)
      DT::datatable(values$df_data %>% select(input$var_modificar) %>% summary(),
                    width = '5px', 
                    options = list(searching = FALSE,
                                   scrollX = TRUE))
    })
    
    
    observeEvent(input$b_resetar, {
      show("base_user_resetar_wt")
      hide("base_user_modificar_wt")
      hide("base_user_retirar_wt")
    })
    observeEvent(input$b_resetar_confirmar, {
      values$df_data <- values$backup
    })
    observeEvent(input$b_resetar_confirmar, {
      updateVarSelectInput(session,
                           inputId = "var_to_retirar", 
                           label = "Selecione a variavel para retirar: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "variable_y", 
                           label = "Selecione a variavel de resposta: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_density_var_x", 
                           label = "Selecione a variavel x: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_density_var_fator", 
                           label = "Selecione a variavel de separacao (Normalmente fator): ",
                           data = values$df_data)
      # updateVarSelectInput(session,
      #                      inputId = "h_dis_marg_tipo", 
      #                      label = "Selecione os graficos marginais: ",
      #                      data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_dis_marg_var_x", 
                           label = "Selecione a variavel x: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_dis_marg_var_y", 
                           label = "Selecione a variavel y: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_dis_marg_var_sep", 
                           label = "Selecione a variavel de separacao (Normalmente var resposta): ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "h_bd_var_x", 
                           label = "Selecione a variavel x: ",
                           data = values$df_data)
      updateVarSelectInput(session,
                           inputId = "var_modificar", 
                           label = "Selecione a variavel",
                           data = values$df_data)
    })
    
    observeEvent(input$button_ida_analise, {
      updateTabsetPanel(session, "inTabset", selected = 'tab2')
    })
    observeEvent(input$button_ida_engenharia, {
      updateTabsetPanel(session, "inTabset", selected = 'tab3')
    })
    observeEvent(input$button_volta_criacao_qdo_analise, {
      updateTabsetPanel(session, "inTabset", selected = 'tab1')
    })
    observeEvent(input$button_volta_criacao_qdo_engenharia, {
      updateTabsetPanel(session, "inTabset", selected = 'tab1')
    })
    
    output$tabela_base <-  DT::renderDataTable({
      req(input$base_user)
      DT::datatable(values$df_data,
                    width = '5px', 
                    options = list(searching = FALSE,
                                   scrollX = TRUE))
    })
    output$tabela_arvore <-  DT::renderDataTable({
      req(input$criar_arvore)
      DT::datatable(table$table_arvore %>% round(3),
                    width = '5px', 
                    options = list(searching = FALSE,
                                   scrollX = TRUE))
    })
    output$graph <- renderPlot({
      grafico$graf
    })
    #output$plot_absoluto_macro <- renderPlot()
    
    
    
    WIDGET_ID = 'MODELSTUDIO'
    observeEvent(input$button_ida_analise, {
      final_tree$box <-  algor_final_arvore(table$table_arvore, 
                                            final_tree$combinacao, 
                                            values$df_data, 
                                            'Class ~ .')
      explainer <- DALEX::explain(final_tree$box$arvore, 
                                  data = values$df_data, 
                                  y = base$Class %>% as.numeric() - 1, 
                                  label = 'DT')
      
      ms <- modelStudio::modelStudio(explainer, 
                                     widget_id = WIDGET_ID, 
                                     show_info = FALSE)
      ms$elementId <- NULL
      output[[WIDGET_ID]] <- renderD3({
        ms
      })
      output$dashboard <- renderUI({
        d3Output(WIDGET_ID, width=ms$width, height=ms$height)
      })
      
      
    })
    
    
    
  
  })
}