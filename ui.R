navbarPage(title = 'Workflow for CART (classification and regression trees)',
           useShinyjs(),
           #theme = shinytheme("superhero"),
           #theme = 'bootstrap (2).css',
           id = 'navbar',
           position = 'static-top',
           inverse = TRUE,
           tags$style("#inTabset { display:none; }"), 
           tags$style(type="text/css",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }"
           ),
           tabsetPanel(id = "inTabset",
                       # Primeiro tabPanel  -------
                       tabPanel(
                         title = 'Construcao do modelo',
                         value = 'tab1',
                         
                         # Fluid Row ---------
                         fluidRow(
                           # Coluna com tamanho 3 -------
                           column(width = 3,
                                  bs4DashSidebar(
                                    # Botao de separador de colunas -------
                                    radioButtons("sep", "Separador de colunas na base",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ","),
                                    # Botao de check de header -------
                                    checkboxInput("header", "Arquivo com header? ", TRUE),
                                    
                                    # input da base -------
                                    fileInput(
                                      inputId = 'base_user',
                                      label = 'Carregue a base de dados: ',
                                      width = '250px',
                                      buttonLabel = 'Pesquisar',
                                      placeholder = '...'
                                    ),
                                    
                                    # Input para selecionar variavel y -------
                                    varSelectInput("variable_y", "Selecione a variavel de resposta: ", NULL),
                                    
                                    # Input para selecionar class ou reg -------
                                    selectizeInput(
                                      inputId = 'class_reg',
                                      label = 'Selecione o tipo de arvore a ser construida: ',
                                      choices = c('Classificacao' = 'class_tree',
                                                  'Regressao' = 'reg_tree'),
                                      options = list(maxItens = 1),
                                      width = '250px'),
                                    
                                    # Input para lista de laco -------
                                    selectizeInput(
                                      inputId = "lista_laco",
                                      label = "Selecione os min_bucket para otimizacao",
                                      choices = c('5','10', '25', '50', '100', '250', '500', '1000', '1500', '2000', '3000', '5000', '7500', '10000'),
                                      selected = NULL,
                                      multiple = TRUE,
                                      width = "100%",
                                      options = list(
                                        'plugins' = list('remove_button'),
                                        'create' = TRUE,
                                        'persist' = TRUE
                                      )),
                                    
                                    # Botao para criar arvore -------
                                    actionButton("criar_arvore", "Criar arvore")
                                  )),
                           
                           # Coluna com tamanho 9 ---------
                           column(width = 9,
                                  bs4DashBody(
                                    # Fluid Row ---------
                                    fluidRow( 
                                      #  Coluna com tamanho 12 -------
                                      column(
                                        width = 12,
                                        # Output da tabela base -------
                                        dataTableOutput(outputId = 'tabela_base'
                                        )),
                                      #  Coluna com tamanho 12 -------
                                      column(
                                        width = 12,
                                        # Output da tabela arvore -------
                                        dataTableOutput(outputId = 'tabela_arvore'
                                        )),
                                      # Coluna com tamanho 6 -------
                                      column(
                                        width = 6,
                                        # Input com ccp otimo -------
                                        #varSelectInput("best_ccp", "Ccp otimo: ", NULL)),
                                        textInput(
                                          inputId = 'comb',
                                          label = 'Combinacao otima',
                                          value = "",
                                          width = '250px',
                                          placeholder = NULL)),
                                      
                                      # Coluna com tamanho 6 -------
                                      column(
                                        width = 6,
                                        # Input de texto, do min bucket otimo -------
                                        #varSelectInput("best_min_bucket", "Min Bucket otimo: ", NULL)),
                                        # textInput(
                                        #   inputId = 'Min Bucket otimo',
                                        #   label = 'best_min_bucket',
                                        #   value = "",
                                        #   width = '250px',
                                        #   placeholder = NULL)
                                        ),
                                      
                                      # Coluna com tamanho 12 ------- 
                                      
                                      column(
                                        width = 6,
                                        # Botao para pagina analise modelo  -------
                                        actionButton("button_ida_analise", "Pagina de analise do modelo")),
                                      
                                      column(
                                        width = 6,
                                        # Botao para pagina engenheria base  -------
                                        actionButton("button_ida_engenharia", "Pagina da engenharia da base")),
                                      
                                      solidHeader = TRUE,
                                      collapsible = FALSE,
                                      height = 300,
                                      width = 9,
                                      title = ''),
                                  )
                           )
                           
                           #####
                         )),
                       
                       # Segundo tabPanel -------
                       tabPanel(
                         title = 'Analise do modelo',
                         value = 'tab2',
                         fluidRow(column(width = 2,
                                         actionButton("button_volta_criacao_qdo_analise", "Pagina Criacao modelo")),
                                  column(width = 10, uiOutput('dashboard'))),
                         # Botao volta criacao -------
                         
                         
                       ),
                       
                       # Terceiro tabPanel -------
                       tabPanel(
                         title = 'Engenharia base',
                         value = 'tab3',
                         fluidRow(
                           box(
                             title = 'Graficos',
                             solidHeader = TRUE,
                             collapsible = FALSE,
                             fluidRow(
                               # Input para selecionar grafico ----
                               selectizeInput(
                                 inputId = 'tipo_graph',
                                 label = 'Selecione o tipo de grafico a ser construido: ',
                                 choices = c(
                                   '...' = '',
                                   'Correlacao' = 'corr',
                                   'Densidade' = 'density',
                                   'Distribuicao Marginal' = 'dis_marg',
                                   'BadRate' = 'bd'),
                                 selected = NULL,
                                 options = list(maxItens = 1),
                                 width = '250px'),
                               #####
                             ),
                             fluidRow(
                               # Hidden para corr ----
                               hidden(
                                 div(id = "selects_para_corr"
                                 )
                               ),
                               # Hidden para density ----
                               hidden(
                                 div(id = "selects_para_density",
                                     
                                     varSelectInput("h_density_var_x", 
                                                    "Selecione a variavel x: ", 
                                                    NULL),
                                     
                                     varSelectInput("h_density_var_fator", 
                                                    "Selecione a variavel de separacao (Normalmente fator): ", 
                                                    NULL)
                                     
                                 )
                               ),
                               # Hidden para dis_marg ----
                               hidden(
                                 div(id = "selects_para_dis_marg", 
                                     
                                     selectizeInput(
                                       inputId = 'h_dis_marg_tipo',
                                       label = 'Selecione os graficos marginais: ',
                                       choices = c(
                                         '...' = '',
                                         'Densidade' = 'density',
                                         'Histograma' = 'histogram',
                                         'Boxplot' = 'boxplot',
                                         'Violino' = 'violin',
                                         'Densigrama' = 'densigram'),
                                       selected = NULL,
                                       options = list(maxItens = 1),
                                       width = '250px'),
                                     
                                     varSelectInput("h_dis_marg_var_x", 
                                                    "Selecione a variavel x: ", 
                                                    NULL),
                                     
                                     varSelectInput("h_dis_marg_var_y", 
                                                    "Selecione a variavel y: ", 
                                                    NULL),
                                     
                                     varSelectInput("h_dis_marg_var_sep", 
                                                    "Selecione a variavel de separacao (Normalmente var resposta): ", 
                                                    NULL)
                                     
                                     
                                 )
                               ),
                               # Hidden para bd ----
                               hidden(
                                 div(id = "selects_para_bd",
                                     
                                     varSelectInput("h_bd_var_x", 
                                                    "Selecione a variavel x: ", 
                                                    NULL)
                                    
                                 )
                               )
                             ),
                             fluidRow(
                               #actionButton("criar_graph", "Criar grafico"),
                               plotOutput(outputId = 'graph')
                               #####
                             )
                               ),
                           box(
                             title = 'Criacao',
                             solidHeader = TRUE,
                             collapsible = FALSE,
                             fluidRow(
                               # Botoes ----
                               column(width = 3#,
                                      # Botao de adicionar ----
                                      #actionButton("b_adicionar", "Adicionar")
                                      ),
                               column(width = 9, 
                                      # Botao de modificar ----
                                      actionButton("b_modificar", "Modificar"))
                             ),
                             fluidRow(column (width = 12)),
                             fluidRow(
                               column(width = 6,
                                      # Botao de retirar ----
                                      actionButton("b_retirar", "Retirar")),
                               column(width = 6, 
                                      # Botao de resetar ----
                                      actionButton("b_resetar", "Resetar"))
                               #####
                             ),
                             box(title = 'Work Table',
                                 solidHeader = TRUE,
                                 collapsible = FALSE,
                                 width = 12,
                                 #####
                                 # Hidden work table retirar ----
                                 hidden(
                                   div(id = "base_user_retirar_wt",
                                       varSelectInput("var_to_retirar", 
                                                      "Selecione a variavel para retirar: ", 
                                                      NULL),
                                       actionButton("b_retirar_confirmar", "Confirmar"))
                                   ),
                                 # Hidden work table modificar ----
                                 hidden(
                                   div(id = "base_user_modificar_wt",
                                       fluidRow(actionButton('base_mod', 'Modificar base'),
                                                actionButton('col_mod', 'Modificar coluna'),
                                                hr()),
                                       fluidRow(column(
                                         width = 6,
                                         # Hidden para modificar base ----
                                         hidden(
                                           div(id = "base_user_modificar_wt_column_base",
                                               # Botao para tirar na ----
                                               actionButton('tirar_na', 'Retirar os Nas'))
                                         ),
                                         # Hidden para modificar coluna ----
                                         hidden(
                                           div(id = "base_user_modificar_wt_column_coluna",
                                               # Botao para selecionar a var que vai modificar ----
                                               varSelectInput("var_modificar", 
                                                              "Selecione a variavel", 
                                                              NULL),
                                               # Botao para padronizar ----
                                               actionButton('pad', 'Padronizar'),
                                               hr(),
                                               # Botao para normalizar ----
                                               actionButton('norm', 'Normalizar'),
                                               hr(),
                                               # Botao para fatorizar ----
                                               actionButton('fat', 'Mudar para fator'),
                                               hr(),
                                               # Botao para mudar para numerico ----
                                               actionButton('num', 'Mudar para numerico'),
                                               hr()#,
                                               # Botao para clusterizar ----
                                               # actionButton('clust', 'Clusterizar'),
                                               # hr()
                                           )
                                         )
                                       ),
                                       #####
                                       column(
                                         width = 6,
                                         dataTableOutput(outputId = 'var_modificar_summary')
                                         #summary
                                       )
                                       ),
                                       fluidRow()
                                   )
                                 ),
                                 # Hidden work table resetar ----
                                 hidden(
                                   div(id = "base_user_resetar_wt",
                                       actionButton("b_resetar_confirmar", "Resetar"))
                                 )
                             )
                           )
                         ),
                         fluidRow(
                           # Botao volta criacao -------
                           actionButton("button_volta_criacao_qdo_engenharia", "Pagina Criacao modelo")  
                         )
                       )
                       #####
           )
)

