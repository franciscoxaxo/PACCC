# ==============================================================================
# APLICACIÓN SHINY: CURADOR Y RECLASIFICADOR PACCC
# Arquitectura Basada en Proyectos - Soporte Total de ID (Primary Key)
# ==============================================================================

paquetes_requeridos <- c("shiny", "bslib", "DT", "dplyr", "tidytext", 
                         "ggplot2", "stringr", "shinyjs", "plotly", 
                         "wordcloud2", "tidyr", "visNetwork", "readr", "officer")

paquetes_faltantes <- paquetes_requeridos[!(paquetes_requeridos %in% installed.packages()[,"Package"])]
if(length(paquetes_faltantes)) {
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(shinyjs)
library(plotly)
library(wordcloud2)
library(tidyr)
library(visNetwork)
library(readr)
library(officer) # <- NUEVA LIBRERÍA PARA EXPORTAR A WORD

# ==============================================================================
# FUNCIONES DE LECTURA ROBUSTA
# ==============================================================================

leer_csv_robusto <- function(ruta) {
  correcciones_mojibake <- c(
    "Ã¡" = "á", "Ã©" = "é", "Ã\u00ad" = "í", "Ã³" = "ó", "Ãº" = "ú", "Ã±" = "ñ", 
    "Ã " = "à", "Ã¨" = "è", "Ã¬" = "ì", "Ã²" = "ò", "Ã¹" = "ù",
    "Ã\u0081" = "Á", "Ã‰" = "É", "Ã\u008d" = "Í", "Ã“" = "Ó", "Ãš" = "Ú", "Ã‘" = "Ñ",
    "Âº" = "º", "Â¿" = "¿", "Â¡" = "¡", "Ã¼" = "ü", "Ãœ" = "Ü", 
    "â€œ" = "\"", "â€\u009d" = "\"", "â€˜" = "'", "â€™" = "'", "â€“" = "-", "â€”" = "-",
    "ï»¿" = "", "\ufeff" = "" 
  )
  
  encodings <- readr::guess_encoding(ruta)
  enc_prob <- if (nrow(encodings) > 0) encodings$encoding[1] else "UTF-8"
  if (is.na(enc_prob) || enc_prob == "unknown") enc_prob <- "latin1"
  
  texto_crudo <- tryCatch({ readr::read_file(ruta, locale = readr::locale(encoding = enc_prob)) }, 
                          error = function(e) { tryCatch({ readr::read_file(ruta, locale = readr::locale(encoding = "UTF-8")) }, 
                                                         error = function(e2) { readr::read_file(ruta, locale = readr::locale(encoding = "latin1")) }) })
  
  texto_limpio <- stringr::str_replace_all(texto_crudo, correcciones_mojibake)
  texto_limpio <- stringr::str_remove_all(texto_limpio, "\\x00")
  
  primera_linea <- strsplit(texto_limpio, "\n")[[1]][1]
  n_comas <- stringr::str_count(primera_linea, ",")
  n_ptocoma <- stringr::str_count(primera_linea, ";")
  separador_usado <- if(n_ptocoma > n_comas) ";" else ","
  
  temp_file <- tempfile(fileext = ".csv")
  readr::write_file(texto_limpio, temp_file)
  
  df <- suppressWarnings(readr::read_delim(
    temp_file, delim = separador_usado, show_col_types = FALSE, na = c("", "NA"),
    escape_double = TRUE, trim_ws = TRUE, name_repair = "minimal"
  ))
  unlink(temp_file)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  names(df) <- gsub("^\uFEFF", "", names(df))
  names(df) <- gsub("^ï»¿", "", names(df))
  names(df) <- gsub('^"|"$', "", names(df))
  
  # Forzar que la columna se llame ID si viene con algún espacio raro
  idx_id <- grep("^ID$", toupper(trimws(names(df))))
  if(length(idx_id) > 0) names(df)[idx_id[1]] <- "ID"
  
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      df[[col]] <- stringr::str_replace_all(df[[col]], "[\r\n]+", " ")
      df[[col]] <- stringr::str_squish(df[[col]])
    }
  }
  return(df)
}

# GENERADOR AUTOMÁTICO DE IDs FALTANTES (Ancla del Sistema)
fill_missing_ids <- function(df, df_ref = NULL) {
  if (!"ID" %in% names(df)) df$ID <- NA_character_
  
  # Forzar a numérico internamente para encontrar el máximo
  ids_numericos <- suppressWarnings(as.numeric(df$ID))
  idx_na <- which(is.na(ids_numericos))
  
  if (length(idx_na) > 0) {
    max_ref <- 0
    if (!is.null(df_ref) && "ID" %in% names(df_ref)) {
      max_ref <- max(c(0, suppressWarnings(as.numeric(df_ref$ID))), na.rm = TRUE)
    }
    max_df <- max(c(0, ids_numericos), na.rm = TRUE)
    max_tot <- max(max_ref, max_df)
    
    df$ID[idx_na] <- as.character(seq(max_tot + 1, max_tot + length(idx_na)))
  }
  
  # Mover ID a la primera columna siempre
  df <- df %>% relocate(ID)
  # Asegurar que sea tratado como texto de forma consistente en cruces
  df$ID <- as.character(df$ID) 
  return(df)
}

resolve_status <- function(st_vec, rule) {
  st_clean <- tolower(trimws(st_vec))
  n_total <- length(st_clean[st_clean != ""])
  if(n_total == 0) return("Excluido")
  n_inc <- sum(grepl("inclu", st_clean), na.rm=TRUE)
  n_exc <- sum(grepl("exclu", st_clean), na.rm=TRUE)
  if(rule == "majority") {
    if(n_inc > n_total/2) return("Aprobado")
    else if(n_exc > n_total/2) return("Rechazado")
    else return("CONFLICTO")
  } else { return(tools::toTitleCase(st_clean[1])) }
}

resolve_metadata <- function(vals, rule) {
  v_clean <- unique(trimws(vals[!is.na(vals) & trimws(vals) != ""]))
  if(length(v_clean) == 0) return(NA_character_)
  if(length(v_clean) == 1) return(v_clean)
  if(rule == "union") {
    all_terms <- unlist(strsplit(v_clean, ";\\s*"))
    return(paste(unique(trimws(all_terms)), collapse = "; "))
  } else { return(paste0("[CONFLICTO: ", paste(v_clean, collapse = " vs "), "]")) }
}

# ==============================================================================
# 1. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- page_sidebar(
  shinyjs::useShinyjs(),
  title = "Curador y Reclasificador PACCC",
  theme = bs_theme(version = 5, bootswatch = "flatly"), 
  tags$head(tags$style(HTML(".jump-box input { height: 30px; text-align: center; }"))),
  
  sidebar = sidebar(
    title = "Gestor de Proyectos",
    width = 370,
    
    h6("Identificación de Evaluador", class = "text-info"),
    textInput("reviewer_name", label=NULL, value="Revisor_A", placeholder="Sin espacios"),
    hr(),
    
    h6("1. Crear Nuevo Proyecto", class = "text-primary"),
    textInput("new_proj_name", "Nombre del Proyecto:", placeholder = "Ej: PACCC_2026"),
    fileInput("upload_csv", "Subir Sistematización PACCC (CSV):", accept = c(".csv")),
    actionButton("create_proj_btn", "Crear e Iniciar", class = "btn-primary", icon = icon("rocket")),
    
    hr(),
    h6("2. Cargar Proyecto Existente", class = "text-success"),
    selectInput("existing_proj", "Seleccionar Proyecto:", choices = NULL),
    actionButton("load_proj_btn", "Cargar Proyecto", class = "btn-success", icon = icon("folder-open")),
    
    hr(),
    uiOutput("active_proj_ui"),
    
    conditionalPanel(
      condition = "output.project_is_active == true",
      hr(),
      h6("3. Actualizar Datos del Proyecto", class = "text-warning"),
      fileInput("update_csv", "Subir nuevo CSV para cruzar:", accept = c(".csv")),
      actionButton("update_proj_btn", "Sincronizar Filas Nuevas", class = "btn-warning text-dark", width = "100%", icon = icon("sync"))
    )
  ),
  
  navset_card_underline(
    title = "Flujo de Trabajo Colaborativo",
    nav_panel("Capa 1: Base Bruta", icon = icon("table"), DTOutput("results_table")),
    nav_panel("Capa 1.5: Setear Reclasificación", icon = icon("list-check"), br(),
              fluidRow(
                column(5, card(card_header("1. Editar o Crear Variable", class = "bg-primary text-white"), card_body(
                  selectInput("q_select", "Seleccionar Variable:", choices = c("--- CREAR NUEVA ---")),
                  conditionalPanel(condition = "input.q_select == '--- CREAR NUEVA ---'",
                                   textInput("q_name_new", "Nombre de la Nueva Variable:")),
                  selectInput("q_type", "Tipo de Campo:", choices = c("Text Field", "Single Choice", "Multiple Choice")),
                  conditionalPanel(condition = "input.q_type != 'Text Field'", hr(), h6("Opciones de Clasificación"), 
                                   textInput("c_name", "Nombre Opción"), textInput("c_acronym", "Acrónimo"), 
                                   div(style="display:flex; gap:10px;",
                                       actionButton("add_choice_btn", "Agregar Opción", class = "btn-light btn-sm mt-2"),
                                       actionButton("clear_choices_btn", "Limpiar", class = "btn-warning btn-sm mt-2")
                                   ),
                                   br(), br(), DTOutput("current_choices_tbl")), 
                  hr(), 
                  fluidRow(
                    column(6, actionButton("delete_selected_q_btn", "Eliminar Variable", class = "btn-danger", width = "100%", icon = icon("trash"))),
                    column(6, actionButton("save_question_btn", "Guardar Variable", class = "btn-success", width = "100%", icon = icon("save")))
                  )
                ))),
                # AQUI AGREGAMOS EL BOTÓN DE EXPORTACIÓN A WORD
                column(7, card(card_header("2. Diccionario de Variables Actual", class = "bg-info text-white"), card_body(
                  downloadButton("export_word_dict_btn", "Exportar a Word", class = "btn-secondary mb-3", icon = icon("file-word")),
                  DTOutput("questions_set_tbl")
                )))
              )
    ),
    nav_panel("Capa 2: Curación de Medidas", icon = icon("user-edit"), br(), uiOutput("curation_ui")),
    nav_panel("Capa 2.5: Fusión de Evaluaciones", icon = icon("handshake"), br(),
              fluidRow(
                column(4, card(card_header("Subir Evaluaciones"), card_body(
                  fileInput("consensus_files", "Sube los CSV de los revisores:", multiple = TRUE, accept = ".csv"),
                  radioButtons("meta_rule", "Regla ante Disenso:", choices = c("A. Unión Inclusiva" = "union", "B. Marcar [CONFLICTO]" = "conflict"), selected = "union"),
                  actionButton("run_consensus_btn", "Generar Dataset Maestro", class="btn-success", width="100%")
                ))),
                column(8, card(card_header("Dataset Maestro Consolidado"), card_body(
                  DTOutput("consensus_table"), hr(), downloadButton("export_consensus_btn", "Descargar Dataset Maestro", class="btn-dark text-white")
                )))
              )
    ),
    nav_panel("Capa 3: Análisis Visual", icon = icon("chart-pie"), br(),
              layout_sidebar(
                sidebar = sidebar(
                  title = "Filtros y Controles",
                  selectizeInput("plan_filter", "Filtrar por Plan(es):", choices = NULL, multiple = TRUE, options = list(placeholder = 'Todos los planes')), hr(),
                  selectInput("viz_mode", "Seleccionar Gráfico:", 
                              choices = c("1. Nube de Palabras Global" = "wordcloud", "2. Nube de Palabras Únicas (Muerte Cruzada)" = "wordcloud_unique",
                                          "3. Flujo Dinámico de Relaciones (Sankey)" = "sankey", "4. Gráfico de Frecuencias" = "freq", "5. Red Plan vs Actores" = "network")), hr(),
                  conditionalPanel(condition = "input.viz_mode == 'wordcloud' || input.viz_mode == 'wordcloud_unique'",
                                   h6("Filtros NLP"), textInput("custom_stopwords", "Excluir palabras:", value = "para, el, la,esta,busca,medida,estos,este,sobre,tiene,estas,asimismo,entre,través,manera, los, las, con, de, en, del, a, y, o, por, se, su, sus, como, al, una, un, que")),
                  conditionalPanel(condition = "input.viz_mode == 'wordcloud_unique'", hr(), h6("Muerte Cruzada"),
                                   selectInput("unique_group_col", "Agrupar por:", choices = c("Plan", "Eje", "Área")), selectInput("unique_group_val", "Mostrar exclusivas de:", choices = NULL)),
                  conditionalPanel(condition = "input.viz_mode == 'sankey'", hr(), h6("Ejes Sankey"),
                                   selectInput("sankey_source", "Origen:", choices = NULL), selectInput("sankey_target", "Destino:", choices = NULL),
                                   radioButtons("sankey_label_format", "Formato de Etiquetas:", choices = c("Original" = "orig", "Nombre Completo" = "full", "Solo Acrónimo" = "acro"), selected = "orig")),
                  conditionalPanel(condition = "input.viz_mode == 'freq'", hr(), h6("Configuración de Frecuencias"), selectInput("freq_col", "Variable a contar:", choices = NULL)),
                  conditionalPanel(condition = "input.viz_mode == 'network'", hr(), h6("Configuración de Red"),
                                   numericInput("top_actors_n", "Cantidad Máxima de Actores (Top):", value = 10, min = 1),
                                   checkboxInput("shared_actors_only", "Mostrar SOLO actores compartidos (>1 Plan)", value = FALSE),
                                   radioButtons("net_dir", "Estructura Visual:", choices = c("Orgánico (Fuerza central)" = "force", "Jerarquía: Plan -> Actores" = "LR", "Jerarquía: Actores -> Plan" = "RL"), selected = "force"))
                ),
                conditionalPanel(condition = "input.viz_mode == 'wordcloud'", fluidRow(column(12, card(card_header("Nube de Análisis de Texto"), wordcloud2Output("nlp_wordcloud", height = "650px"))))),
                conditionalPanel(condition = "input.viz_mode == 'wordcloud_unique'", fluidRow(column(12, card(card_header("Vocabulario Exclusivo"), wordcloud2Output("nlp_wordcloud_unique", height = "650px"))))),
                conditionalPanel(condition = "input.viz_mode == 'sankey'", fluidRow(column(12, card(card_header("Flujo Dinámico"), plotlyOutput("sankey_plot", height = "650px"))))),
                conditionalPanel(condition = "input.viz_mode == 'freq'", fluidRow(column(12, card(card_header("Distribución de Frecuencias (Top 20)"), plotlyOutput("freq_plot", height = "650px"))))),
                conditionalPanel(condition = "input.viz_mode == 'network'", fluidRow(column(12, card(card_header("Red de Colaboradores Externos"), visNetworkOutput("network_plot", height = "650px")))))
              )
    )
  )
)

# ==============================================================================
# 2. LÓGICA DEL SERVIDOR (Server)
# ==============================================================================
server <- function(input, output, session) {
  
  workspace <- "PACCC_Workspace"
  if(!dir.exists(workspace)) dir.create(workspace)
  
  actualizar_lista_proyectos <- function() {
    dirs <- list.dirs(workspace, full.names = FALSE, recursive = FALSE)
    updateSelectInput(session, "existing_proj", choices = dirs)
  }
  actualizar_lista_proyectos()
  
  active_project <- reactiveVal(NULL)
  raw_data <- reactiveVal(NULL)
  curated_data <- reactiveVal(NULL)
  consensus_data <- reactiveVal(NULL)
  current_row <- reactiveVal(1)
  
  current_choices <- reactiveVal(data.frame(Choice=character(), Acronimo=character(), stringsAsFactors=FALSE))
  questions_set <- reactiveVal(data.frame(Pregunta=character(), Tipo=character(), Opciones_Acronimos=character(), stringsAsFactors=FALSE))
  choices_list_master <- reactiveVal(list())
  
  output$project_is_active <- reactive({ !is.null(active_project()) })
  outputOptions(output, "project_is_active", suspendWhenHidden = FALSE)
  
  output$active_proj_ui <- renderUI({
    if(is.null(active_project())) { HTML("<div class='alert alert-warning'>Ningún proyecto activo</div>") }
    else { HTML(paste0("<div class='alert alert-success'><strong>Proyecto Activo:</strong><br>", active_project(), "</div>")) }
  })
  
  # === CREAR PROYECTO ===
  observeEvent(input$create_proj_btn, {
    req(input$new_proj_name, input$upload_csv)
    proj_dir <- file.path(workspace, gsub("[^A-Za-z0-9_]", "_", input$new_proj_name))
    
    if(dir.exists(proj_dir)) { showNotification("El proyecto ya existe.", type="error"); return() }
    
    dir.create(proj_dir)
    dir.create(file.path(proj_dir, "config"))
    
    df_new <- leer_csv_robusto(input$upload_csv$datapath)
    df_new <- fill_missing_ids(df_new) # Garantiza que ID exista y sea correlativo 1...n
    
    if(!"status" %in% names(df_new)) df_new$status <- "Pendiente"
    
    write_excel_csv2(df_new, file.path(proj_dir, "datos_brutos.csv"), na = "")
    
    # ID ya no es una variable curable, es la llave del sistema
    base_cols <- setdiff(names(df_new), c("ID", "status"))
    initial_q_set <- data.frame(Pregunta = base_cols, Tipo = "Text Field", Opciones_Acronimos = "N/A (Texto Libre)", stringsAsFactors = FALSE)
    write_excel_csv2(initial_q_set, file.path(proj_dir, "config", "diccionario_variables.csv"), na = "")
    
    actualizar_lista_proyectos()
    updateSelectInput(session, "existing_proj", selected = basename(proj_dir))
    showNotification("Proyecto creado. Sincronizando...", type="message")
    
    shinyjs::click("load_proj_btn")
  })
  
  # === CARGAR PROYECTO (BASADO EN ID ABSOLUTO) ===
  observeEvent(input$load_proj_btn, {
    req(input$existing_proj)
    proj_dir <- file.path(workspace, input$existing_proj)
    active_project(basename(proj_dir))
    
    df_bruta <- leer_csv_robusto(file.path(proj_dir, "datos_brutos.csv"))
    df_bruta <- fill_missing_ids(df_bruta) # Garantía
    
    rev_name <- gsub(" ", "_", input$reviewer_name)
    file_curado <- file.path(proj_dir, paste0("dataset_curado_", rev_name, ".csv"))
    
    df_final <- df_bruta
    if (file.exists(file_curado)) {
      df_curado <- leer_csv_robusto(file_curado)
      df_curado <- fill_missing_ids(df_curado)
      
      # Filtro de Fantasmas: Conservar en curado SOLO los IDs que existen en el Bruto oficial
      df_curado <- df_curado %>% filter(ID %in% df_bruta$ID)
      df_curado <- df_curado %>% distinct(ID, .keep_all = TRUE)
      
      # Identificar medidas genuinamente nuevas por ID
      nuevas <- df_bruta %>% filter(!ID %in% df_curado$ID)
      
      if (nrow(nuevas) > 0) {
        df_final <- bind_rows(df_curado, nuevas)
        showNotification(paste("Se sincronizaron", nrow(nuevas), "medidas nuevas por ID."), type="message")
      } else {
        df_final <- df_curado
        showNotification("Proyecto cargado. Sesión restaurada con éxito.", type="message")
      }
      
      # Guardar purga
      write_excel_csv2(df_final, file_curado, na = "")
      
    } else {
      showNotification("Proyecto cargado exitosamente.", type = "message")
    }
    
    conf_file <- file.path(proj_dir, "config", "diccionario_variables.csv")
    if(file.exists(conf_file)) {
      qs <- leer_csv_robusto(conf_file)
      questions_set(qs)
      c_list <- list()
      for(var_name in qs$Pregunta) {
        safe_name <- gsub("[^A-Za-z0-9_]", "_", var_name)
        opt_file <- file.path(proj_dir, "config", paste0("opciones_", safe_name, ".csv"))
        if(file.exists(opt_file)) {
          c_list[[var_name]] <- leer_csv_robusto(opt_file)
        }
      }
      choices_list_master(c_list)
      updateSelectInput(session, "q_select", choices = c("--- CREAR NUEVA ---", qs$Pregunta))
    }
    
    raw_data(df_bruta)
    # Reordenar por ID para que la interfaz siempre sea ascendente y limpia
    df_final <- df_final %>% arrange(as.numeric(ID))
    curated_data(df_final)
    
    estado_limpio <- tolower(trimws(df_final$status))
    primer_pendiente <- which(estado_limpio == "pendiente")
    if (length(primer_pendiente) > 0) {
      current_row(primer_pendiente[1])
    } else {
      revisados <- which(estado_limpio != "pendiente")
      if (length(revisados) > 0) current_row(max(revisados)) else current_row(1)
    }
    
    if("Plan" %in% names(df_final)) { updateSelectizeInput(session, "plan_filter", choices = unique(na.omit(df_final$Plan))) }
    todas_columnas <- names(df_final)
    updateSelectInput(session, "freq_col", choices = todas_columnas, selected = if("Eje" %in% todas_columnas) "Eje" else todas_columnas[1])
    updateSelectInput(session, "sankey_source", choices = todas_columnas, selected = if("Eje" %in% todas_columnas) "Eje" else todas_columnas[1])
    updateSelectInput(session, "sankey_target", choices = todas_columnas, selected = if("Área" %in% todas_columnas) "Área" else todas_columnas[2])
  })
  
  # === ACTUALIZAR PROYECTO (BASADO EN ID) ===
  observeEvent(input$update_proj_btn, {
    req(active_project(), input$update_csv)
    proj_dir <- file.path(workspace, active_project())
    
    df_nuevo <- leer_csv_robusto(input$update_csv$datapath)
    df_viejo <- leer_csv_robusto(file.path(proj_dir, "datos_brutos.csv"))
    
    df_viejo <- fill_missing_ids(df_viejo)
    df_nuevo <- fill_missing_ids(df_nuevo, df_viejo) # Todo lo que venga sin ID se le asigna el siguiente consecutivo
    
    # Exclusivamente empalmar las filas cuyo ID no exista en el bruto original
    df_realmente_nuevo <- df_nuevo %>% filter(!ID %in% df_viejo$ID)
    
    if(nrow(df_realmente_nuevo) > 0) {
      if(!"status" %in% names(df_realmente_nuevo)) df_realmente_nuevo$status <- "Pendiente"
      df_combinado <- bind_rows(df_viejo, df_realmente_nuevo) %>% distinct(ID, .keep_all = TRUE)
      write_excel_csv2(df_combinado, file.path(proj_dir, "datos_brutos.csv"), na = "")
    }
    
    shinyjs::click("load_proj_btn") 
  })
  
  # === CAPA 1.5: GESTIÓN DE RÚBRICA Y EXPORTACIÓN ===
  observeEvent(questions_set(), {
    qs <- questions_set()
    if (nrow(qs) > 0) {
      updateSelectInput(session, "q_select", choices = c("--- CREAR NUEVA ---", qs$Pregunta))
    } else {
      updateSelectInput(session, "q_select", choices = c("--- CREAR NUEVA ---"))
    }
  })
  
  observeEvent(input$q_select, {
    req(input$q_select)
    if(input$q_select == "--- CREAR NUEVA ---") {
      updateTextInput(session, "q_name_new", value = "")
      updateSelectInput(session, "q_type", selected = "Text Field")
      current_choices(data.frame(Choice=character(), Acronimo=character(), stringsAsFactors=FALSE))
    } else {
      qs <- questions_set()
      idx <- which(qs$Pregunta == input$q_select)
      if(length(idx) > 0) {
        updateTextInput(session, "q_name_new", value = input$q_select)
        updateSelectInput(session, "q_type", selected = qs$Tipo[idx])
        c_list <- choices_list_master()
        if(!is.null(c_list[[input$q_select]])) {
          current_choices(c_list[[input$q_select]])
        } else {
          current_choices(data.frame(Choice=character(), Acronimo=character(), stringsAsFactors=FALSE))
        }
      }
    }
  })
  
  observeEvent(input$clear_choices_btn, {
    current_choices(data.frame(Choice=character(), Acronimo=character(), stringsAsFactors=FALSE))
  })
  
  observeEvent(input$add_choice_btn, {
    req(input$c_name, input$c_acronym)
    new_choice <- data.frame(Choice = input$c_name, Acronimo = input$c_acronym, stringsAsFactors = FALSE)
    current_choices(bind_rows(current_choices(), new_choice))
    updateTextInput(session, "c_name", value = ""); updateTextInput(session, "c_acronym", value = "")
  })
  output$current_choices_tbl <- renderDT({ datatable(current_choices(), options = list(dom = 't'), rownames = FALSE) })
  
  observeEvent(input$save_question_btn, {
    req(active_project())
    is_new <- input$q_select == "--- CREAR NUEVA ---"
    var_name <- if(is_new) input$q_name_new else input$q_select
    req(var_name != "")
    acronimos_str <- "N/A (Texto Libre)"
    if(input$q_type != "Text Field" && nrow(current_choices()) > 0) {
      acronimos_str <- paste(current_choices()$Acronimo, collapse = " | ")
    }
    qs <- questions_set()
    if (var_name %in% qs$Pregunta) {
      idx <- which(qs$Pregunta == var_name)
      qs$Tipo[idx] <- input$q_type
      qs$Opciones_Acronimos[idx] <- acronimos_str
    } else {
      new_q <- data.frame(Pregunta = var_name, Tipo = input$q_type, Opciones_Acronimos = acronimos_str, stringsAsFactors = FALSE)
      qs <- bind_rows(qs, new_q)
    }
    questions_set(qs)
    
    proj_dir <- file.path(workspace, active_project())
    write_excel_csv2(qs, file.path(proj_dir, "config", "diccionario_variables.csv"), na = "")
    
    if(input$q_type != "Text Field" && nrow(current_choices()) > 0) {
      temp_list <- choices_list_master()
      temp_list[[var_name]] <- current_choices()
      choices_list_master(temp_list)
      safe_name <- gsub("[^A-Za-z0-9_]", "_", var_name)
      write_excel_csv2(current_choices(), file.path(proj_dir, "config", paste0("opciones_", safe_name, ".csv")), na = "")
    }
    
    updateSelectInput(session, "q_select", selected = "--- CREAR NUEVA ---")
    showNotification("Variable actualizada y guardada en el proyecto.", type = "message")
  })
  
  observeEvent(input$delete_selected_q_btn, {
    req(active_project())
    if(input$q_select == "--- CREAR NUEVA ---") return()
    
    var_name <- input$q_select
    qs <- questions_set()
    qs <- qs[qs$Pregunta != var_name, ]
    questions_set(qs)
    
    c_list <- choices_list_master()
    c_list[[var_name]] <- NULL
    choices_list_master(c_list)
    
    proj_dir <- file.path(workspace, active_project())
    write_excel_csv2(qs, file.path(proj_dir, "config", "diccionario_variables.csv"), na = "")
    
    updateSelectInput(session, "q_select", selected = "--- CREAR NUEVA ---")
    showNotification(paste("Variable", var_name, "eliminada."), type="message")
  })
  
  output$questions_set_tbl <- renderDT({ datatable(questions_set(), options = list(pageLength = 10), rownames = FALSE) })
  
  # === NUEVO: LOGICA EXPORTAR WORD ===============================
  output$export_word_dict_btn <- downloadHandler(
    filename = function() {
      paste0("Diccionario_Variables_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(questions_set())
      qs <- questions_set()
      c_master <- choices_list_master()
      
      # Generar doc. de officer
      doc <- read_docx()
      doc <- body_add_par(doc, "Diccionario de Variables y Opciones", style = "heading 1")
      
      # Info del Proyecto
      if(!is.null(active_project())) {
        doc <- body_add_par(doc, paste("Proyecto activo:", active_project()), style = "Normal")
        doc <- body_add_par(doc, "", style = "Normal") 
      }
      
      # Iterar sobre las variables
      for (i in seq_len(nrow(qs))) {
        q_name <- qs$Pregunta[i]
        q_type <- qs$Tipo[i]
        
        doc <- body_add_par(doc, paste("Variable:", q_name), style = "heading 2")
        doc <- body_add_par(doc, paste("Tipo de dato:", q_type), style = "Normal")
        
        # Si tiene opciones adjuntas (Single o Multiple Choice)
        if (q_type != "Text Field") {
          choices_df <- c_master[[q_name]]
          if (!is.null(choices_df) && nrow(choices_df) > 0) {
            doc <- body_add_par(doc, "Opciones de clasificación disponibles:", style = "Normal")
            # Agregar las opciones como tabla al documento de Word
            doc <- body_add_table(doc, value = choices_df, style = "table_template")
          } else {
            doc <- body_add_par(doc, "Sin opciones registradas.", style = "Normal")
          }
        }
        # Añadir un espacio tras cada variable
        doc <- body_add_par(doc, "", style = "Normal")
      }
      
      print(doc, target = file)
    }
  )
  # ===============================================================
  
  # === CAPA 2: EVALUACIÓN ===
  output$results_table <- renderDT({ req(curated_data()); datatable(curated_data(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  
  observeEvent(input$jump_btn, { val <- isolate(input$jump_row_val); if(is.numeric(val) && val >= 1 && val <= nrow(curated_data())) current_row(val) })
  observeEvent(input$jump_next_btn, { if (current_row() < nrow(curated_data())) current_row(current_row() + 1) })
  observeEvent(input$prev_btn, { if (current_row() > 1) { current_row(current_row() - 1) } })
  
  output$curation_ui <- renderUI({
    df <- curated_data()
    raw <- raw_data() 
    if (is.null(df)) return(h5("Sube o Carga un Proyecto en la barra lateral.", class="text-danger"))
    
    idx <- current_row()
    row_data <- df[idx, ]
    
    # Match estricto por ID
    raw_row <- raw[which(raw$ID == row_data$ID), ]
    if(nrow(raw_row) == 0) {
      raw_row <- setNames(data.frame(matrix(ncol = ncol(raw), nrow = 1)), names(raw))
    } else {
      raw_row <- raw_row[1, ]
    }
    
    val_header <- function(col) {
      if (col %in% names(row_data) && !is.na(row_data[[col]]) && trimws(row_data[[col]]) != "") {
        return(row_data[[col]])
      }
      return("N/A")
    }
    
    static_ui <- div(
      style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #dee2e6;",
      h5(paste("ID:", val_header("ID"), "-", val_header("Medida")), class="text-primary"),
      p(strong("Plan/Comuna: "), val_header("Plan"), " | ", strong("Eje: "), val_header("Eje"), " | ", strong("Área: "), val_header("Área"), style="margin-bottom: 5px; font-size: 0.9em;"),
      p(strong("Objetivo: "), val_header("Objetivo"), style="margin-bottom: 5px; font-size: 0.9em;"),
      p(strong("Descripción: "), val_header("Descripción"), style="margin-bottom: 0px; font-size: 0.9em;")
    )
    
    safe_raw <- function(col) if(col %in% names(raw_row) && !is.na(raw_row[[col]])) raw_row[[col]] else "Sin datos originales"
    
    ui_text <- list()
    ui_select <- list()
    
    q_set <- questions_set()
    c_master <- choices_list_master()
    long_fields <- c("Objetivo", "Descripción", "Acciones", "Potenciales barreras y obstáculos", "Tecnología, infraestructura y recursos necesarios", "Consideraciones de Genero", "Vinculación normativa")
    
    for (i in 1:nrow(q_set)) {
      col_name <- q_set$Pregunta[i]
      q_type <- q_set$Tipo[i]
      input_id <- paste0("dyn_q_", idx, "_", gsub("[^A-Za-z0-9]", "_", col_name))
      current_val <- if (col_name %in% names(row_data)) row_data[[col_name]] else ""
      if (is.na(current_val) || is.null(current_val)) current_val <- "" 
      
      if (q_type == "Text Field") {
        if (col_name %in% long_fields || nchar(as.character(current_val)) > 60) {
          ui_text[[length(ui_text) + 1]] <- textAreaInput(input_id, col_name, value = current_val, rows = 4, width = "100%")
        } else {
          ui_text[[length(ui_text) + 1]] <- textInput(input_id, col_name, value = current_val, width = "100%")
        }
      } else {
        choices_df <- c_master[[col_name]]
        choice_opts <- c()
        if (!is.null(choices_df) && nrow(choices_df) > 0) choice_opts <- setNames(choices_df$Acronimo, paste0(choices_df$Choice, " (", choices_df$Acronimo, ")"))
        
        static_box <- div(style="color: #555; background: #e9ecef; padding: 6px 10px; border-radius: 4px; margin-bottom: 5px; font-size: 0.9em; border: 1px solid #ced4da;", safe_raw(col_name))
        
        if (q_type == "Single Choice") {
          choice_opts <- c("Seleccionar..." = "", choice_opts)
          sel_val <- if(current_val %in% choice_opts) current_val else ""
          ui_select[[length(ui_select) + 1]] <- div(style="margin-bottom: 15px;", tags$label(col_name, class="control-label", style="font-weight: bold;"), static_box, selectInput(input_id, label = NULL, choices = choice_opts, selected = sel_val, width = "100%"))
        } else if (q_type == "Multiple Choice") {
          sel_vals <- if(current_val == "") character(0) else strsplit(as.character(current_val), ";\\s*")[[1]]
          sel_vals <- sel_vals[sel_vals %in% choice_opts]
          ui_select[[length(ui_select) + 1]] <- div(style="margin-bottom: 15px;", tags$label(col_name, class="control-label", style="font-weight: bold;"), static_box, selectizeInput(input_id, label = NULL, choices = choice_opts, selected = sel_vals, multiple = TRUE, width = "100%"))
        }
      }
    }
    
    form_layout <- fluidRow(
      column(7, h6("✏️ Campos de Texto Editables", class="text-secondary"), tagList(ui_text)),
      column(5, h6("📌 Clasificadores", class="text-secondary"), tagList(ui_select), hr(),
             div(style="margin-bottom: 15px;", tags$label("Estado de Revisión Final:", class="control-label", style="font-weight: bold; color: #d35400;"),
                 selectInput(paste0("cur_status_", idx), label=NULL, choices = c("Pendiente", "Revisado", "Con Observaciones"), selected = row_data$status, width="100%"))
      )
    )
    
    card(
      card_header(class = "bg-primary text-white", div(style = "display: flex; justify-content: space-between; align-items: center;", span(paste("Evaluación de Medidas PACCC - ID:", val_header("ID"))), div(class = "jump-box", style = "display: flex; align-items: center; gap: 8px;", span("Ir a fila:"), numericInput("jump_row_val", label = NULL, value = idx, min = 1, max = nrow(df), width = "70px"), span(paste("de", nrow(df))), actionButton("jump_btn", "Ir", class = "btn-light btn-sm"), actionButton("jump_next_btn", ">>", class = "btn-secondary btn-sm")))),
      card_body(static_ui, hr(), form_layout),
      card_footer(fluidRow(column(4, actionButton("prev_btn", "Anterior", icon = icon("arrow-left"), width = "100%")), column(8, actionButton("save_next_btn", "Guardar Cambios y Avanzar", class = "btn-success", icon = icon("save"), width = "100%"))))
    )
  })
  
  observeEvent(input$save_next_btn, {
    req(active_project())
    df <- curated_data(); idx <- current_row()
    status_val <- input[[paste0("cur_status_", idx)]]
    if (!is.null(status_val)) { df$status[idx] <- status_val }
    
    q_set <- questions_set()
    if (nrow(q_set) > 0) {
      for (i in 1:nrow(q_set)) {
        col_name <- q_set$Pregunta[i]
        input_id <- paste0("dyn_q_", idx, "_", gsub("[^A-Za-z0-9]", "_", col_name))
        val <- input[[input_id]]
        if (!is.null(val)) {
          if (length(val) > 1) val <- paste(val, collapse = "; ") 
          df[idx, col_name] <- val
        }
      }
    }
    curated_data(df)
    rev_name <- gsub(" ", "_", input$reviewer_name)
    file_path <- file.path(workspace, active_project(), paste0("dataset_curado_", rev_name, ".csv"))
    write_excel_csv2(df, file_path, na = "")
    
    if (idx < nrow(df)) current_row(idx + 1)
  })
  
  # === CAPA 2.5 CONSENSO ===
  observeEvent(input$run_consensus_btn, {
    req(input$consensus_files)
    df_list <- lapply(input$consensus_files$datapath, function(p) { leer_csv_robusto(p) })
    df_all <- bind_rows(df_list)
    base_cols <- names(raw_data())
    dyn_cols <- setdiff(names(df_all), c(base_cols, "status"))
    master_df <- df_all %>%
      group_by(ID) %>%
      summarise(across(any_of(base_cols), ~ first(na.omit(.))), status = "Consolidado", across(any_of(dyn_cols), ~ resolve_metadata(., rule = input$meta_rule)), .groups = "drop")
    consensus_data(master_df)
    curated_data(master_df)
  })
  
  output$consensus_table <- renderDT({ req(consensus_data()); datatable(consensus_data(), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE) })
  output$export_consensus_btn <- downloadHandler(filename = function() { "dataset_PACCC_MAESTRO.csv" }, content = function(file) { write_excel_csv2(consensus_data(), file, na = "") })
  
  # === CAPA 3 ANALÍTICAS ===
  viz_data <- reactive({
    req(curated_data())
    df <- curated_data()
    if (!is.null(input$plan_filter) && length(input$plan_filter) > 0 && "Plan" %in% names(df)) {
      df <- df %>% filter(Plan %in% input$plan_filter)
    }
    return(df)
  })
  
  observeEvent(viz_data(), {
    df <- viz_data()
    actor_col <- "Colaboradores externos"
    if("Plan" %in% names(df) && actor_col %in% names(df)) {
      total_actores <- df %>% 
        select(all_of(actor_col)) %>% 
        tidyr::drop_na() %>% 
        tidyr::separate_rows(!!sym(actor_col), sep = ";\\s*") %>% 
        mutate(!!sym(actor_col) := str_to_title(trimws(!!sym(actor_col)))) %>%
        filter(!!sym(actor_col) != "") %>% 
        pull(!!sym(actor_col)) %>% 
        n_distinct()
      updateNumericInput(session, "top_actors_n", value = total_actores, max = total_actores)
    }
  })
  
  output$nlp_wordcloud <- renderWordcloud2({
    df <- viz_data()
    if(is.null(df) || !"Descripción" %in% names(df)) return(NULL)
    user_stops <- input$custom_stopwords %>% str_split(",") %>% unlist() %>% str_trim() %>% tolower()
    custom_stops <- data.frame(word = unique(user_stops))
    tokens <- df %>% select(ID, Descripción) %>% unnest_tokens(word, Descripción) %>% anti_join(custom_stops, by = "word") %>% filter(nchar(word) > 3) %>% filter(!str_detect(word, "^[0-9]+$"))
    freqs <- tokens %>% count(word, sort = TRUE) %>% head(100)
    if(nrow(freqs) == 0) return(NULL)
    wordcloud2(freqs, size = 0.6)
  })
  
  observe({
    df <- viz_data()
    req(input$unique_group_col %in% names(df))
    opciones <- unique(na.omit(df[[input$unique_group_col]]))
    updateSelectInput(session, "unique_group_val", choices = opciones)
  })
  
  output$nlp_wordcloud_unique <- renderWordcloud2({
    df <- viz_data()
    req(input$unique_group_col, input$unique_group_val)
    if(!"Descripción" %in% names(df) || !input$unique_group_col %in% names(df)) return(NULL)
    user_stops <- input$custom_stopwords %>% str_split(",") %>% unlist() %>% str_trim() %>% tolower()
    custom_stops <- data.frame(word = unique(user_stops))
    tokens_grouped <- df %>% select(all_of(c(input$unique_group_col, "Descripción"))) %>% rename(Grupo = !!sym(input$unique_group_col)) %>% filter(!is.na(Grupo) & Grupo != "") %>% unnest_tokens(word, Descripción) %>% anti_join(custom_stops, by = "word") %>% filter(nchar(word) > 3) %>% filter(!str_detect(word, "^[0-9]+$"))
    word_distribution <- tokens_grouped %>% group_by(word) %>% summarise(n_grupos = n_distinct(Grupo))
    exclusive_words <- word_distribution %>% filter(n_grupos == 1) %>% pull(word)
    final_tokens <- tokens_grouped %>% filter(Grupo == input$unique_group_val) %>% filter(word %in% exclusive_words)
    freqs <- final_tokens %>% count(word, sort = TRUE) %>% head(80)
    if(nrow(freqs) == 0) return(NULL)
    wordcloud2(freqs, size = 0.6, color = "random-light", backgroundColor = "#2c3e50")
  })
  
  output$sankey_plot <- renderPlotly({
    df <- viz_data()
    v_orig <- input$sankey_source
    v_dest <- input$sankey_target
    if(is.null(df) || nrow(df) == 0 || is.null(v_orig) || is.null(v_dest)) return(NULL)
    if(!(v_orig %in% names(df) && v_dest %in% names(df))) return(NULL)
    
    sankey_df <- df %>% select(all_of(c(v_orig, v_dest))) %>% tidyr::drop_na() %>% 
      tidyr::separate_rows(!!sym(v_orig), sep = ";\\s*") %>% 
      tidyr::separate_rows(!!sym(v_dest), sep = ";\\s*") %>% 
      filter(!!sym(v_orig) != "", !!sym(v_dest) != "") %>% 
      count(!!sym(v_orig), !!sym(v_dest), name = "value")
    
    if(nrow(sankey_df) == 0) return(NULL)
    
    orig_nodes <- paste0(sankey_df[[v_orig]], " (Orig)")
    dest_nodes <- paste0(sankey_df[[v_dest]], " (Dest)")
    all_nodes <- unique(c(orig_nodes, dest_nodes))
    sankey_df$source <- match(orig_nodes, all_nodes) - 1
    sankey_df$target <- match(dest_nodes, all_nodes) - 1
    
    clean_nodes <- gsub(" \\(Orig\\)| \\(Dest\\)", "", all_nodes)
    
    if(!is.null(input$sankey_label_format)) {
      if(input$sankey_label_format == "full") {
        clean_nodes <- sub("\\s*\\([^)]+\\)$", "", clean_nodes)
      } else if(input$sankey_label_format == "acro") {
        paren_idx <- grepl("\\([^)]+\\)$", clean_nodes)
        clean_nodes[paren_idx] <- sub("^.*\\(([^)]+)\\)$", "\\1", clean_nodes[paren_idx])
      }
    }
    
    plot_ly(type = "sankey", orientation = "h", 
            node = list(label = clean_nodes, pad = 15, thickness = 20, line = list(color = "black", width = 0.5)), 
            link = list(source = sankey_df$source, target = sankey_df$target, value = sankey_df$value))
  })
  
  output$freq_plot <- renderPlotly({
    df <- viz_data()
    req(input$freq_col %in% names(df))
    freq_df <- df %>% select(all_of(input$freq_col)) %>% tidyr::drop_na() %>% tidyr::separate_rows(!!sym(input$freq_col), sep = ";\\s*") %>% filter(!!sym(input$freq_col) != "") %>% mutate(Categoria = trimws(!!sym(input$freq_col))) %>% count(Categoria, name = "Conteo") %>% arrange(desc(Conteo)) %>% head(20)
    if(nrow(freq_df) == 0) return(NULL)
    freq_df$Categoria_Corta <- str_trunc(freq_df$Categoria, 20, "right")
    freq_df <- freq_df %>% arrange(Conteo)
    freq_df$Categoria_Corta <- factor(freq_df$Categoria_Corta, levels = unique(freq_df$Categoria_Corta))
    plot_ly(freq_df, x = ~Conteo, y = ~Categoria_Corta, type = 'bar', orientation = 'h', marker = list(color = '#3498db')) %>% layout(xaxis = list(title = "Cantidad de Medidas"), yaxis = list(title = input$freq_col), margin = list(l = 150))
  })
  
  output$network_plot <- renderVisNetwork({
    df <- viz_data()
    actor_col <- "Colaboradores externos"
    if(!"Plan" %in% names(df) || !actor_col %in% names(df)) return(NULL)
    
    net_df <- df %>% select(Plan, all_of(actor_col)) %>% tidyr::drop_na() %>% 
      tidyr::separate_rows(!!sym(actor_col), sep = ";\\s*") %>% 
      mutate(!!sym(actor_col) := str_to_title(trimws(!!sym(actor_col)))) %>% 
      filter(!!sym(actor_col) != "") 
    
    if(!is.null(input$shared_actors_only) && input$shared_actors_only) {
      actores_compartidos <- net_df %>% 
        group_by(!!sym(actor_col)) %>% 
        summarise(n_planes = n_distinct(Plan)) %>% 
        filter(n_planes > 1) %>% 
        pull(!!sym(actor_col))
      
      net_df <- net_df %>% filter(!!sym(actor_col) %in% actores_compartidos)
    }
    
    net_df <- net_df %>% count(Plan, !!sym(actor_col), name = "peso")
    if(nrow(net_df) == 0) return(NULL)
    
    actor_freq <- net_df %>% count(!!sym(actor_col), wt = peso, sort = TRUE)
    limite <- input$top_actors_n
    if(is.null(limite) || is.na(limite)) limite <- nrow(actor_freq)
    top_actors <- head(actor_freq[[actor_col]], limite)
    
    net_df <- net_df %>% filter(!!sym(actor_col) %in% top_actors)
    
    planes_nodos <- data.frame(id = unique(net_df$Plan), label = unique(net_df$Plan), group = "Plan", font.size = 20, stringsAsFactors = FALSE)
    actores_nodos <- data.frame(id = unique(net_df[[actor_col]]), label = unique(net_df[[actor_col]]), group = "Actor", font.size = 14, stringsAsFactors = FALSE)
    nodes <- bind_rows(planes_nodos, actores_nodos)
    
    if(!is.null(input$net_dir) && input$net_dir == "RL") { 
      edges <- data.frame(from = net_df[[actor_col]], to = net_df$Plan, value = net_df$peso, stringsAsFactors = FALSE)
    } else { 
      edges <- data.frame(from = net_df$Plan, to = net_df[[actor_col]], value = net_df$peso, stringsAsFactors = FALSE) 
    }
    
    g <- visNetwork(nodes, edges, width = "100%") %>% 
      visGroups(groupname = "Plan", shape = "square", color = "#e74c3c") %>% 
      visGroups(groupname = "Actor", shape = "dot", color = "#2ecc71") %>% 
      visEdges(color = list(color = "#BDC3C7", opacity = 0.5), smooth = list(enabled = TRUE, type = "continuous")) %>% 
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE)
    
    if(is.null(input$net_dir) || input$net_dir == "force") { 
      g <- g %>% visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -80, centralGravity = 0.01, springLength = 150), stabilization = list(enabled = TRUE, iterations = 300))
    } else { 
      g <- g %>% visHierarchicalLayout(direction = input$net_dir, levelSeparation = 300) 
    }
    g
  })
}

shinyApp(ui = ui, server = server)