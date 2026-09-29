# ==============================================================================
# APLICACIÓN SHINY: CURADOR Y RECLASIFICADOR PACCC
# Arquitectura Basada en Proyectos - Soporte Total de ID (Primary Key)
# ==============================================================================

paquetes_requeridos <- c("shiny", "bslib", "DT", "dplyr", "tidytext", 
                         "ggplot2", "stringr", "shinyjs", "plotly", 
                         "wordcloud2", "tidyr", "visNetwork", "readr", "officer",
                         "quanteda", "quanteda.textstats", "htmlwidgets", "webshot2",
                         "devtools", "leaflet", "sf", "chilemapas") # <--- AÑADIDOS PARA EL MAPA

paquetes_faltantes <- paquetes_requeridos[!(paquetes_requeridos %in% installed.packages()[,"Package"])]
if(length(paquetes_faltantes)) {
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

if (!"chorddiag" %in% installed.packages()[,"Package"]) {
  devtools::install_github("mattflor/chorddiag")
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
library(officer) 
library(quanteda)
library(quanteda.textstats)
library(htmlwidgets)
library(webshot2)
library(chorddiag)
library(leaflet)    # <--- NUEVO
library(sf)         # <--- NUEVO
library(chilemapas) # <--- NUEVO

# ==============================================================================
# DICCIONARIO DE FALSOS VERBOS
# ==============================================================================
falsos_verbos <- c(
  "circular", "solar", "lugar", "mujer", "primer", "tercer", "taller", 
  "cualquier", "mar", "hogar", "celular", "familiar", "particular", 
  "titular", "alquiler", "líder", "chofer", "carácter", "ayer", 
  "bienestar", "super", "estándar", "militar", "similar"
)

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
    escape_double = TRUE, trim_ws = TRUE, name_repair = "minimal",
    col_types = readr::cols(.default = "c") 
  ))
  unlink(temp_file)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  names(df) <- gsub("^\uFEFF", "", names(df))
  names(df) <- gsub("^ï»¿", "", names(df))
  names(df) <- gsub('^"|"$', "", names(df))
  
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

fill_missing_ids <- function(df, df_ref = NULL) {
  if (!"ID" %in% names(df)) df$ID <- NA_character_
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
  df <- df %>% relocate(ID)
  df$ID <- as.character(df$ID) 
  return(df)
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
  tags$head(tags$style(HTML(".jump-box input { height: 30px; text-align: center; } .leaflet-container { background: #ffffff !important; }"))),
  
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
                              choices = c("1. Nube de Palabras Global" = "wordcloud", 
                                          "2. Nube de Palabras Únicas (Keyness)" = "wordcloud_unique", 
                                          "3. Flujo Dinámico de Relaciones (Sankey)" = "sankey", 
                                          "4. Gráfico de Frecuencias" = "freq", 
                                          "5. Red Plan vs Actores" = "network",
                                          "6. Diagrama de Cuerdas (Co-ocurrencia)" = "chord",
                                          "7. Correlograma (Matriz de Correlaciones)" = "corrmatrix",
                                          "8. Mapa Territorial RM (Frecuencia)" = "map")), hr(), # <--- AÑADIDO MAPA AQUÍ
                  
                  conditionalPanel(condition = "input.viz_mode == 'wordcloud'",
                                   radioButtons("wc_count_mode", "Modo de Cálculo:", 
                                                choices = c("Ponderado por Dispersión (Anti-sesgo)" = "pond", "Frecuencia Absoluta (Clásico)" = "abs"), 
                                                selected = "pond"),
                                   hr()),
                  
                  # === FILTROS NLP ===
                  conditionalPanel(condition = "input.viz_mode == 'wordcloud' || input.viz_mode == 'wordcloud_unique'",
                                   radioButtons("wc_view_mode", "Modo de Vista:", choices = c("Nube de Palabras" = "cloud", "Heatmap (Plan vs Palabra)" = "heatmap"), inline = TRUE),
                                   hr(),
                                   
                                   radioButtons("ngram_mode", "Composición de Palabras:", 
                                                choices = c("Solo Palabras (Unigramas)" = "unigrams", 
                                                            "Pares (Bigramas)" = "bigrams", 
                                                            "Ambos mezclados" = "both"), 
                                                selected = "unigrams"),
                                   hr(),
                                   
                                   radioButtons("verb_filter", "Filtrar acciones / verbos:", 
                                                choices = c("Todas las palabras" = "all", "Solo verbos (infinitivos)" = "only_verbs", "Excluir verbos (infinitivos)" = "no_verbs"), selected = "all"),
                                   h6("Filtros de Exclusión"), textInput("custom_stopwords", "Excluir palabras:", value = "para, el, la,esta,busca,medida,estos,este,sobre,tiene,estas,asimismo,entre,través,manera, los, las, con, de, en, del, a, y, o, por, se, su, sus, como, al, una, un, que")),
                  
                  conditionalPanel(condition = "input.viz_mode == 'wordcloud_unique'", hr(), h6("Análisis Keyness"),
                                   selectInput("unique_group_col", "Agrupar por:", choices = c("Plan", "Eje", "Área")), selectInput("unique_group_val", "Mostrar exclusivas de:", choices = NULL)),
                  
                  conditionalPanel(condition = "input.viz_mode == 'sankey'", hr(), h6("Ejes Sankey"),
                                   selectInput("sankey_source", "Origen:", choices = NULL), selectInput("sankey_target", "Destino:", choices = NULL),
                                   radioButtons("sankey_label_format", "Formato de Etiquetas:", choices = c("Original" = "orig", "Nombre Completo" = "full", "Solo Acrónimo" = "acro"), selected = "orig"),
                                   checkboxInput("show_na_sankey", "Incluir vacíos/nulos (como 'NA/NC')", value = FALSE)),
                  
                  conditionalPanel(condition = "input.viz_mode == 'freq'", hr(), h6("Configuración de Frecuencias"), 
                                   selectInput("freq_col", "Variable a contar:", choices = NULL),
                                   checkboxInput("weight_multiple", "Ponderar múltiples equitativamente (ej. 1/N si hay 2 categorías)", value = FALSE),
                                   checkboxInput("show_na", "Incluir vacíos/nulos (como 'NA/NC')", value = FALSE)),
                  
                  conditionalPanel(condition = "input.viz_mode == 'network'", hr(), h6("Configuración de Red"),
                                   numericInput("top_actors_n", "Cantidad Máxima de Actores (Top):", value = 10, min = 1),
                                   checkboxInput("shared_actors_only", "Mostrar SOLO actores compartidos (>1 Plan)", value = FALSE),
                                   radioButtons("net_dir", "Estructura Visual:", choices = c("Orgánico (Fuerza central)" = "force", "Jerarquía: Plan -> Actores" = "LR", "Jerarquía: Actores -> Plan" = "RL"), selected = "force")),
                  
                  conditionalPanel(condition = "input.viz_mode == 'chord'", hr(), h6("Configuración de Cuerdas"),
                                   selectInput("chord_col", "Variable a analizar (con múltiples datos por celda):", choices = NULL),
                                   checkboxInput("chord_show_na", "Incluir vacíos/nulos", value = FALSE)),
                  
                  # === CONFIGURACIÓN CORRELOGRAMA ===
                  conditionalPanel(condition = "input.viz_mode == 'corrmatrix'", hr(), h6("Configuración de Correlaciones"),
                                   selectizeInput("corr_cols", "Variables a cruzar:", choices = NULL, multiple = TRUE),
                                   numericInput("corr_min_freq", "Frecuencia mínima (casos por categoría):", value = 3, min = 2),
                                   sliderInput("corr_threshold", "Ocultar si correlación máxima es menor a:", min = 0.0, max = 0.8, value = 0.15, step = 0.05))
                ),
                
                # =========================================================
                # VISTAS CON BOTONES DE DESCARGA PNG
                # =========================================================
                conditionalPanel(condition = "input.viz_mode == 'wordcloud'", 
                                 fluidRow(column(12, card(
                                   card_header(
                                     div(style="display: flex; justify-content: space-between; align-items: center;",
                                         span("Análisis de Texto Global"),
                                         div(
                                           conditionalPanel("input.wc_view_mode == 'cloud'", downloadButton("dl_wc_global", "Descargar PNG", class="btn-sm btn-outline-primary")),
                                           conditionalPanel("input.wc_view_mode == 'heatmap'", downloadButton("dl_hm_global", "Descargar PNG", class="btn-sm btn-outline-primary"))
                                         )
                                     )
                                   ), 
                                   card_body(
                                     conditionalPanel("input.wc_view_mode == 'cloud'", wordcloud2Output("nlp_wordcloud", height = "650px")),
                                     conditionalPanel("input.wc_view_mode == 'heatmap'", plotlyOutput("nlp_heatmap", height = "650px"))
                                   )
                                 )))),
                
                conditionalPanel(condition = "input.viz_mode == 'wordcloud_unique'", 
                                 fluidRow(column(12, card(
                                   card_header(
                                     div(style="display: flex; justify-content: space-between; align-items: center;",
                                         span("Vocabulario Exclusivo (Keyness Chi-Cuadrado)"),
                                         div(
                                           conditionalPanel("input.wc_view_mode == 'cloud'", downloadButton("dl_wc_unique", "Descargar PNG", class="btn-sm btn-outline-primary")),
                                           conditionalPanel("input.wc_view_mode == 'heatmap'", downloadButton("dl_hm_unique", "Descargar PNG", class="btn-sm btn-outline-primary"))
                                         )
                                     )
                                   ), 
                                   card_body(
                                     conditionalPanel("input.wc_view_mode == 'cloud'", wordcloud2Output("nlp_wordcloud_unique", height = "650px")),
                                     conditionalPanel("input.wc_view_mode == 'heatmap'", plotlyOutput("nlp_heatmap_unique", height = "650px"))
                                   )
                                 )))),
                
                conditionalPanel(condition = "input.viz_mode == 'sankey'", fluidRow(column(12, card(
                  card_header(
                    div(style="display: flex; justify-content: space-between; align-items: center;",
                        span("Flujo Dinámico"), downloadButton("dl_sankey", "Descargar PNG", class="btn-sm btn-outline-primary")
                    )),
                  plotlyOutput("sankey_plot", height = "650px")
                )))),
                
                conditionalPanel(condition = "input.viz_mode == 'freq'", fluidRow(column(12, card(
                  card_header(
                    div(style="display: flex; justify-content: space-between; align-items: center;",
                        span("Distribución de Frecuencias (Top 20)"), downloadButton("dl_freq", "Descargar PNG", class="btn-sm btn-outline-primary")
                    )),
                  plotlyOutput("freq_plot", height = "650px")
                )))),
                
                conditionalPanel(condition = "input.viz_mode == 'network'", fluidRow(column(12, card(
                  card_header(
                    div(style="display: flex; justify-content: space-between; align-items: center;",
                        span("Red de Colaboradores Externos"), downloadButton("dl_network", "Descargar PNG", class="btn-sm btn-outline-primary")
                    )),
                  visNetworkOutput("network_plot", height = "650px")
                )))),
                
                conditionalPanel(condition = "input.viz_mode == 'chord'", fluidRow(column(12, card(
                  card_header(
                    div(style="display: flex; justify-content: space-between; align-items: center;",
                        span("Diagrama de Cuerdas (Co-ocurrencia en misma celda)"), 
                        downloadButton("dl_chord", "Descargar PNG", class="btn-sm btn-outline-primary")
                    )),
                  card_body(
                    chorddiagOutput("chord_plot", height = "650px")
                  )
                )))),
                
                # === CARD CORRELOGRAMA ===
                conditionalPanel(condition = "input.viz_mode == 'corrmatrix'", fluidRow(column(12, card(
                  card_header(
                    div(style="display: flex; justify-content: space-between; align-items: center;",
                        span("Matriz de Correlaciones (Co-ocurrencia / Exclusión)"), 
                        downloadButton("dl_corr", "Descargar PNG", class="btn-sm btn-outline-primary")
                    )),
                  card_body(
                    plotlyOutput("corr_plot", height = "750px")
                  )
                )))),
                
                # === NUEVA CARD: MAPA TERRITORIAL ===
                conditionalPanel(condition = "input.viz_mode == 'map'", fluidRow(column(12, card(
                  card_header(
                    div(style="display: flex; justify-content: space-between; align-items: center;",
                        span("Distribución Territorial de Medidas (Región Metropolitana)"), 
                        downloadButton("dl_map", "Descargar PNG", class="btn-sm btn-outline-primary")
                    )),
                  card_body(
                    leafletOutput("map_plot", height = "650px")
                  )
                ))))
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
  
  observeEvent(input$create_proj_btn, {
    req(input$new_proj_name, input$upload_csv)
    proj_dir <- file.path(workspace, gsub("[^A-Za-z0-9_]", "_", input$new_proj_name))
    
    if(dir.exists(proj_dir)) { showNotification("El proyecto ya existe.", type="error"); return() }
    
    dir.create(proj_dir)
    dir.create(file.path(proj_dir, "config"))
    
    df_new <- leer_csv_robusto(input$upload_csv$datapath)
    df_new <- fill_missing_ids(df_new)
    if(!"status" %in% names(df_new)) df_new$status <- "Pendiente"
    
    write_excel_csv2(df_new, file.path(proj_dir, "datos_brutos.csv"), na = "")
    
    base_cols <- setdiff(names(df_new), c("ID", "status"))
    initial_q_set <- data.frame(Pregunta = base_cols, Tipo = "Text Field", Opciones_Acronimos = "N/A (Texto Libre)", stringsAsFactors = FALSE)
    write_excel_csv2(initial_q_set, file.path(proj_dir, "config", "diccionario_variables.csv"), na = "")
    
    actualizar_lista_proyectos()
    updateSelectInput(session, "existing_proj", selected = basename(proj_dir))
    showNotification("Proyecto creado. Sincronizando...", type="message")
    
    shinyjs::click("load_proj_btn")
  })
  
  observeEvent(input$load_proj_btn, {
    req(input$existing_proj)
    proj_dir <- file.path(workspace, input$existing_proj)
    active_project(basename(proj_dir))
    
    df_bruta <- leer_csv_robusto(file.path(proj_dir, "datos_brutos.csv"))
    df_bruta <- fill_missing_ids(df_bruta)
    
    rev_name <- gsub(" ", "_", input$reviewer_name)
    file_curado <- file.path(proj_dir, paste0("dataset_curado_", rev_name, ".csv"))
    
    df_final <- df_bruta
    if (file.exists(file_curado)) {
      df_curado <- leer_csv_robusto(file_curado)
      df_curado <- fill_missing_ids(df_curado)
      df_curado <- df_curado %>% filter(ID %in% df_bruta$ID) %>% distinct(ID, .keep_all = TRUE)
      
      nuevas <- df_bruta %>% filter(!ID %in% df_curado$ID)
      if (nrow(nuevas) > 0) {
        df_final <- bind_rows(df_curado, nuevas)
        showNotification(paste("Se sincronizaron", nrow(nuevas), "medidas nuevas por ID."), type="message")
      } else {
        df_final <- df_curado
        showNotification("Proyecto cargado. Sesión restaurada con éxito.", type="message")
      }
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
        if(file.exists(opt_file)) c_list[[var_name]] <- leer_csv_robusto(opt_file)
      }
      choices_list_master(c_list)
      updateSelectInput(session, "q_select", choices = c("--- CREAR NUEVA ---", qs$Pregunta))
    }
    
    raw_data(df_bruta)
    df_final <- df_final %>% arrange(as.numeric(ID))
    curated_data(df_final)
    
    estado_limpio <- tolower(trimws(df_final$status))
    primer_pendiente <- which(estado_limpio == "pendiente")
    if (length(primer_pendiente) > 0) current_row(primer_pendiente[1])
    else { revisados <- which(estado_limpio != "pendiente"); if (length(revisados) > 0) current_row(max(revisados)) else current_row(1) }
    
    if("Plan" %in% names(df_final)) { updateSelectizeInput(session, "plan_filter", choices = unique(na.omit(df_final$Plan))) }
    todas_columnas <- names(df_final)
    updateSelectInput(session, "freq_col", choices = todas_columnas, selected = if("Eje" %in% todas_columnas) "Eje" else todas_columnas[1])
    updateSelectInput(session, "sankey_source", choices = todas_columnas, selected = if("Eje" %in% todas_columnas) "Eje" else todas_columnas[1])
    updateSelectInput(session, "sankey_target", choices = todas_columnas, selected = if("Área" %in% todas_columnas) "Área" else todas_columnas[2])
    updateSelectInput(session, "chord_col", choices = todas_columnas, selected = if("Colaboradores externos" %in% todas_columnas) "Colaboradores externos" else todas_columnas[1])
    
    # Inicializar columnas del gráfico de correlación
    cols_clasificadoras <- intersect(c("Eje", "Área", "Tipo de medida", "Colaboradores externos"), todas_columnas)
    if(length(cols_clasificadoras) == 0) cols_clasificadoras <- setdiff(todas_columnas, c("ID", "status"))[1:min(3, length(todas_columnas))]
    updateSelectizeInput(session, "corr_cols", choices = todas_columnas, selected = cols_clasificadoras)
  })
  
  observeEvent(input$update_proj_btn, {
    req(active_project(), input$update_csv)
    proj_dir <- file.path(workspace, active_project())
    
    df_nuevo <- leer_csv_robusto(input$update_csv$datapath)
    df_viejo <- leer_csv_robusto(file.path(proj_dir, "datos_brutos.csv"))
    
    df_viejo <- fill_missing_ids(df_viejo)
    df_nuevo <- fill_missing_ids(df_nuevo, df_viejo)
    
    df_realmente_nuevo <- df_nuevo %>% filter(!ID %in% df_viejo$ID)
    if(nrow(df_realmente_nuevo) > 0) {
      if(!"status" %in% names(df_realmente_nuevo)) df_realmente_nuevo$status <- "Pendiente"
      df_combinado <- bind_rows(df_viejo, df_realmente_nuevo) %>% distinct(ID, .keep_all = TRUE)
      write_excel_csv2(df_combinado, file.path(proj_dir, "datos_brutos.csv"), na = "")
    }
    shinyjs::click("load_proj_btn") 
  })
  
  observeEvent(questions_set(), {
    qs <- questions_set()
    if (nrow(qs) > 0) updateSelectInput(session, "q_select", choices = c("--- CREAR NUEVA ---", qs$Pregunta))
    else updateSelectInput(session, "q_select", choices = c("--- CREAR NUEVA ---"))
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
        if(!is.null(c_list[[input$q_select]])) current_choices(c_list[[input$q_select]])
        else current_choices(data.frame(Choice=character(), Acronimo=character(), stringsAsFactors=FALSE))
      }
    }
  })
  
  observeEvent(input$clear_choices_btn, { current_choices(data.frame(Choice=character(), Acronimo=character(), stringsAsFactors=FALSE)) })
  
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
    if(input$q_type != "Text Field" && nrow(current_choices()) > 0) acronimos_str <- paste(current_choices()$Acronimo, collapse = " | ")
    qs <- questions_set()
    if (var_name %in% qs$Pregunta) {
      idx <- which(qs$Pregunta == var_name)
      qs$Tipo[idx] <- input$q_type; qs$Opciones_Acronimos[idx] <- acronimos_str
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
    req(active_project()); if(input$q_select == "--- CREAR NUEVA ---") return()
    var_name <- input$q_select; qs <- questions_set()
    qs <- qs[qs$Pregunta != var_name, ]; questions_set(qs)
    c_list <- choices_list_master(); c_list[[var_name]] <- NULL; choices_list_master(c_list)
    proj_dir <- file.path(workspace, active_project())
    write_excel_csv2(qs, file.path(proj_dir, "config", "diccionario_variables.csv"), na = "")
    updateSelectInput(session, "q_select", selected = "--- CREAR NUEVA ---")
    showNotification(paste("Variable", var_name, "eliminada."), type="message")
  })
  output$questions_set_tbl <- renderDT({ datatable(questions_set(), options = list(pageLength = 10), rownames = FALSE) })
  
  # === CAPA 2: EVALUACIÓN ===
  output$results_table <- renderDT({ req(curated_data()); datatable(curated_data(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) })
  
  observeEvent(input$jump_btn, { val <- isolate(input$jump_row_val); if(is.numeric(val) && val >= 1 && val <= nrow(curated_data())) current_row(val) })
  observeEvent(input$jump_next_btn, { if (current_row() < nrow(curated_data())) current_row(current_row() + 1) })
  observeEvent(input$prev_btn, { if (current_row() > 1) { current_row(current_row() - 1) } })
  
  output$curation_ui <- renderUI({
    df <- curated_data(); raw <- raw_data() 
    if (is.null(df)) return(h5("Sube o Carga un Proyecto en la barra lateral.", class="text-danger"))
    
    idx <- current_row()
    
    if (idx > nrow(df)) {
      return(card(
        card_header(class = "bg-success text-white", "¡Revisión Completada!"),
        card_body(
          h2("🎉 ¡Terminamos!", class="text-center text-success mt-4"), 
          h5("Has llegado al final del dataset.", class="text-center mb-4")
        ),
        card_footer(
          fluidRow(
            column(4, actionButton("prev_btn_end", "Volver a la última medida", icon = icon("arrow-left"), width = "100%")),
            column(4, actionButton("jump_first_pending_btn_end", "Ir al 1er Pendiente", class="btn-warning", icon = icon("search"), width="100%")),
            column(4, actionButton("back_to_start_btn", "Ir al inicio", class="btn-primary", icon = icon("home"), width="100%"))
          )
        )
      ))
    }
    
    row_data <- df[idx, ]
    raw_row <- raw[which(raw$ID == row_data$ID), ]
    if(nrow(raw_row) == 0) { raw_row <- setNames(data.frame(matrix(ncol = ncol(raw), nrow = 1)), names(raw)) } 
    else { raw_row <- raw_row[1, ] }
    
    val_header <- function(col) { if (col %in% names(row_data) && !is.na(row_data[[col]]) && trimws(row_data[[col]]) != "") return(row_data[[col]]); return("N/A") }
    
    static_ui <- div(
      style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #dee2e6;",
      h5(paste("ID:", val_header("ID"), "-", val_header("Medida")), class="text-primary"),
      p(strong("Plan/Comuna: "), val_header("Plan"), " | ", strong("Eje: "), val_header("Eje"), " | ", strong("Área: "), val_header("Área"), style="margin-bottom: 5px; font-size: 0.9em;"),
      p(strong("Objetivo: "), val_header("Objetivo"), style="margin-bottom: 5px; font-size: 0.9em;"),
      p(strong("Descripción: "), val_header("Descripción"), style="margin-bottom: 0px; font-size: 0.9em;")
    )
    
    safe_raw <- function(col) if(col %in% names(raw_row) && !is.na(raw_row[[col]])) raw_row[[col]] else "Sin datos originales"
    
    ui_text <- list(); ui_select <- list()
    q_set <- questions_set(); c_master <- choices_list_master()
    long_fields <- c("Objetivo", "Descripción", "Acciones", "Potenciales barreras y obstáculos", "Tecnología, infraestructura y recursos necesarios", "Consideraciones de Genero", "Vinculación normativa")
    
    for (i in 1:nrow(q_set)) {
      col_name <- q_set$Pregunta[i]; q_type <- q_set$Tipo[i]
      input_id <- paste0("dyn_q_", idx, "_", gsub("[^A-Za-z0-9]", "_", col_name))
      current_val <- if (col_name %in% names(row_data)) row_data[[col_name]] else ""
      if (is.na(current_val) || is.null(current_val)) current_val <- "" 
      
      if (q_type == "Text Field") {
        if (col_name %in% long_fields || nchar(as.character(current_val)) > 60) ui_text[[length(ui_text) + 1]] <- textAreaInput(input_id, col_name, value = current_val, rows = 4, width = "100%")
        else ui_text[[length(ui_text) + 1]] <- textInput(input_id, col_name, value = current_val, width = "100%")
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
      card_header(class = "bg-primary text-white", div(style = "display: flex; justify-content: space-between; align-items: center;", 
                                                       span(paste("Evaluación de Medidas PACCC - ID:", val_header("ID"))), 
                                                       div(class = "jump-box", style = "display: flex; align-items: center; gap: 8px;", 
                                                           actionButton("jump_first_pending_btn", "1er Pendiente", class = "btn-warning btn-sm", icon = icon("search")),
                                                           span(" | Ir a fila:"), numericInput("jump_row_val", label = NULL, value = idx, min = 1, max = nrow(df), width = "70px"), 
                                                           span(paste("de", nrow(df))), actionButton("jump_btn", "Ir", class = "btn-light btn-sm"), actionButton("jump_next_btn", ">>", class = "btn-secondary btn-sm")))),
      card_body(static_ui, hr(), form_layout),
      card_footer(fluidRow(column(4, actionButton("prev_btn", "Anterior", icon = icon("arrow-left"), width = "100%")), column(8, actionButton("save_next_btn", "Guardar Cambios y Avanzar", class = "btn-success", icon = icon("save"), width = "100%"))))
    )
  })
  
  observeEvent(input$save_next_btn, {
    req(active_project()); df <- curated_data(); idx <- current_row()
    status_val <- input[[paste0("cur_status_", idx)]]; if (!is.null(status_val)) { df$status[idx] <- status_val }
    
    q_set <- questions_set()
    if (nrow(q_set) > 0) {
      for (i in 1:nrow(q_set)) {
        col_name <- q_set$Pregunta[i]; input_id <- paste0("dyn_q_", idx, "_", gsub("[^A-Za-z0-9]", "_", col_name))
        val <- input[[input_id]]
        if (!is.null(val)) { if (length(val) > 1) val <- paste(val, collapse = "; "); df[idx, col_name] <- val }
      }
    }
    curated_data(df)
    rev_name <- gsub(" ", "_", input$reviewer_name)
    file_path <- file.path(workspace, active_project(), paste0("dataset_curado_", rev_name, ".csv"))
    write_excel_csv2(df, file_path, na = "")
    
    current_row(idx + 1)
  })
  
  observeEvent(input$prev_btn_end, { current_row(nrow(curated_data())) })
  observeEvent(input$back_to_start_btn, { current_row(1) })
  
  observeEvent(c(input$jump_first_pending_btn, input$jump_first_pending_btn_end), {
    df <- curated_data()
    pendientes <- which(tolower(trimws(df$status)) == "pendiente")
    if (length(pendientes) > 0) {
      current_row(pendientes[1])
    } else {
      showNotification("¡Excelente! No hay ninguna medida en estado 'Pendiente'.", type="message")
    }
  })
  
  observeEvent(input$run_consensus_btn, {
    req(input$consensus_files)
    df_list <- lapply(input$consensus_files$datapath, function(p) { leer_csv_robusto(p) })
    df_all <- bind_rows(df_list)
    base_cols <- names(raw_data())
    dyn_cols <- setdiff(names(df_all), c(base_cols, "status"))
    master_df <- df_all %>% group_by(ID) %>% summarise(across(any_of(base_cols), ~ first(na.omit(.))), status = "Consolidado", across(any_of(dyn_cols), ~ resolve_metadata(., rule = input$meta_rule)), .groups = "drop")
    consensus_data(master_df); curated_data(master_df)
  })
  
  output$consensus_table <- renderDT({ req(consensus_data()); datatable(consensus_data(), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE) })
  output$export_consensus_btn <- downloadHandler(filename = function() { "dataset_PACCC_MAESTRO.csv" }, content = function(file) { write_excel_csv2(consensus_data(), file, na = "") })
  
  # === CAPA 3 ANALÍTICAS ===
  viz_data <- reactive({
    req(curated_data())
    df <- curated_data()
    if (!is.null(input$plan_filter) && length(input$plan_filter) > 0 && "Plan" %in% names(df)) df <- df %>% filter(Plan %in% input$plan_filter)
    return(df)
  })
  
  observeEvent(viz_data(), {
    df <- viz_data()
    actor_col <- "Colaboradores externos"
    if("Plan" %in% names(df) && actor_col %in% names(df)) {
      total_actores <- df %>% select(all_of(actor_col)) %>% tidyr::drop_na() %>% tidyr::separate_rows(!!sym(actor_col), sep = ";\\s*") %>% mutate(!!sym(actor_col) := str_to_title(trimws(!!sym(actor_col)))) %>% filter(!!sym(actor_col) != "") %>% pull(!!sym(actor_col)) %>% n_distinct()
      updateNumericInput(session, "top_actors_n", value = total_actores, max = total_actores)
    }
  })
  
  filtrar_verbos <- function(tokens, mode) {
    if(is.null(mode) || mode == "all") return(tokens)
    
    if (mode == "only_verbs") {
      tokens <- tokens %>% filter(str_detect(word, "[aei]r$") & !word %in% falsos_verbos)
    } else if (mode == "no_verbs") {
      tokens <- tokens %>% filter(!(str_detect(word, "[aei]r$") & !word %in% falsos_verbos))
    }
    return(tokens)
  }
  
  generar_tokens <- function(df, text_col, group_col, stopwords_str, verb_filter, ngram_type) {
    user_stops <- stopwords_str %>% str_split(",") %>% unlist() %>% str_trim() %>% tolower()
    custom_stops <- data.frame(word = unique(user_stops))
    
    df_clean <- df %>% 
      select(all_of(c(group_col, text_col))) %>% 
      rename(Grupo = !!sym(group_col), Texto = !!sym(text_col)) %>% 
      filter(!is.na(Grupo) & Grupo != "")
    
    res <- tibble(Grupo = character(), word = character())
    
    if (ngram_type %in% c("unigrams", "both")) {
      t1 <- df_clean %>% 
        unnest_tokens(word, Texto) %>% 
        anti_join(custom_stops, by = "word") %>% 
        filter(nchar(word) > 3, !str_detect(word, "^[0-9]+$"))
      
      t1 <- filtrar_verbos(t1, verb_filter)
      res <- bind_rows(res, t1)
    }
    
    if (ngram_type %in% c("bigrams", "both")) {
      t2 <- df_clean %>% 
        unnest_tokens(word, Texto, token = "ngrams", n = 2) %>% 
        filter(!is.na(word)) %>% 
        tidyr::separate(word, c("w1", "w2"), sep = " ", remove = FALSE) %>% 
        filter(!w1 %in% custom_stops$word, !w2 %in% custom_stops$word) %>% 
        filter(nchar(w1) > 2, nchar(w2) > 2) %>% 
        filter(!str_detect(w1, "^[0-9]+$"), !str_detect(w2, "^[0-9]+$"))
      
      if (verb_filter == "only_verbs") {
        t2 <- t2 %>% filter((str_detect(w1, "[aei]r$") & !w1 %in% falsos_verbos) | (str_detect(w2, "[aei]r$") & !w2 %in% falsos_verbos))
      } else if (verb_filter == "no_verbs") {
        t2 <- t2 %>% filter(!(str_detect(w1, "[aei]r$") & !w1 %in% falsos_verbos) & !(str_detect(w2, "[aei]r$") & !w2 %in% falsos_verbos))
      }
      
      t2 <- t2 %>% select(Grupo, word)
      res <- bind_rows(res, t2)
    }
    return(res)
  }
  
  # -- Wordcloud Global --
  wc_global_reac <- reactive({
    df <- viz_data()
    idx_desc <- grep("descripci", tolower(names(df)))
    if(length(idx_desc) == 0) return(NULL)
    
    nombre_col_real <- names(df)[idx_desc[1]] 
    group_col <- if("Plan" %in% names(df)) "Plan" else "ID" 
    
    tokens <- generar_tokens(df, nombre_col_real, group_col, input$custom_stopwords, input$verb_filter, input$ngram_mode)
    if(nrow(tokens) == 0) return(NULL)
    
    total_grupos <- n_distinct(tokens$Grupo)
    
    word_metrics <- tokens %>% 
      group_by(word) %>% 
      summarise(frecuencia_absoluta = n(), grupos_distintos = n_distinct(Grupo)) 
    
    if (!is.null(input$wc_count_mode) && input$wc_count_mode == "pond") {
      word_metrics <- word_metrics %>% mutate(peso_dispersion = grupos_distintos / total_grupos, freq_final = frecuencia_absoluta * peso_dispersion) %>% filter(grupos_distintos > 1 | total_grupos == 1)
    } else {
      word_metrics <- word_metrics %>% mutate(freq_final = frecuencia_absoluta)
    }
    
    freqs <- word_metrics %>% arrange(desc(freq_final)) %>% head(100) %>% select(word, freq = freq_final)
    freqs <- as.data.frame(freqs) 
    if(nrow(freqs) == 0) return(NULL)
    
    freqs$freq <- round(freqs$freq, 1)
    wordcloud2(freqs, size = 0.6)
  })
  
  # -- Heatmap Global --
  hm_global_reac <- reactive({
    df <- viz_data()
    idx_desc <- grep("descripci", tolower(names(df)))
    if(length(idx_desc) == 0) return(NULL)
    
    nombre_col_real <- names(df)[idx_desc[1]]
    group_col <- if("Plan" %in% names(df)) "Plan" else "ID"
    
    tokens <- generar_tokens(df, nombre_col_real, group_col, input$custom_stopwords, input$verb_filter, input$ngram_mode)
    if(nrow(tokens) == 0) return(NULL)
    
    top_words <- tokens %>% count(word, sort = TRUE) %>% head(20) %>% pull(word)
    if(length(top_words) == 0) return(NULL)
    
    heat_df <- tokens %>% filter(word %in% top_words) %>% count(Grupo, word) %>% tidyr::complete(Grupo, word, fill = list(n = 0))
    plot_ly(heat_df, x = ~Grupo, y = ~word, z = ~n, type = "heatmap", colors = colorRamp(c("#f7fbff", "#08306b"))) %>%
      layout(title = "Frecuencia de Top Términos", xaxis = list(title = group_col), yaxis = list(title = "Término")) %>%
      config(displayModeBar = FALSE)
  })
  
  # -- Wordcloud Unique --
  wc_unique_reac <- reactive({
    df_global <- curated_data() 
    req(input$unique_group_col, input$unique_group_val)
    
    idx_desc <- grep("descripci", tolower(names(df_global)))
    if(length(idx_desc) == 0 || !input$unique_group_col %in% names(df_global)) return(NULL)
    
    nombre_col_real <- names(df_global)[idx_desc[1]]
    tokens_grouped <- generar_tokens(df_global, nombre_col_real, input$unique_group_col, input$custom_stopwords, input$verb_filter, input$ngram_mode)
    if(nrow(tokens_grouped) == 0) return(NULL)
    
    word_counts <- tokens_grouped %>% count(Grupo, word)
    if(nrow(word_counts) == 0) return(NULL)
    
    mi_dfm <- word_counts %>% cast_dfm(document = Grupo, term = word, value = n)
    stat_keyness <- tryCatch({ textstat_keyness(mi_dfm, target = as.character(input$unique_group_val)) }, error = function(e) { return(NULL) })
    
    if(is.null(stat_keyness) || nrow(stat_keyness) == 0) return(NULL)
    
    freqs <- stat_keyness %>% filter(chi2 > 0 & p < 0.05) %>% arrange(desc(chi2)) %>% head(80) %>% select(word = feature, freq = chi2) %>% mutate(freq = round(freq, 1))
    freqs <- as.data.frame(freqs)
    if(nrow(freqs) == 0) return(NULL)
    
    wordcloud2(freqs, size = 0.6, color = "random-light", backgroundColor = "#2c3e50")
  })
  
  # -- Heatmap Unique --
  hm_unique_reac <- reactive({
    df_global <- curated_data()
    req(input$unique_group_col, input$unique_group_val)
    
    idx_desc <- grep("descripci", tolower(names(df_global)))
    if(length(idx_desc) == 0 || !input$unique_group_col %in% names(df_global)) return(NULL)
    
    nombre_col_real <- names(df_global)[idx_desc[1]]
    tokens_grouped <- generar_tokens(df_global, nombre_col_real, input$unique_group_col, input$custom_stopwords, input$verb_filter, input$ngram_mode)
    if(nrow(tokens_grouped) == 0) return(NULL)
    
    word_counts <- tokens_grouped %>% count(Grupo, word)
    if(nrow(word_counts) == 0) return(NULL)
    
    mi_dfm <- word_counts %>% cast_dfm(document = Grupo, term = word, value = n)
    stat_keyness <- tryCatch({ textstat_keyness(mi_dfm, target = as.character(input$unique_group_val)) }, error = function(e) { return(NULL) })
    
    if(is.null(stat_keyness) || nrow(stat_keyness) == 0) return(NULL)
    
    top_words <- stat_keyness %>% filter(chi2 > 0 & p < 0.05) %>% arrange(desc(chi2)) %>% head(20) %>% pull(feature)
    if(length(top_words) == 0) return(NULL)
    
    heat_df <- tokens_grouped %>% filter(word %in% top_words) %>% count(Grupo, word) %>% tidyr::complete(Grupo, word, fill = list(n = 0))
    plot_ly(heat_df, x = ~Grupo, y = ~word, z = ~n, type = "heatmap", colors = colorRamp(c("#fdfbfb", "#e74c3c"))) %>%
      layout(title = paste("Keyness (Distribución real):", input$unique_group_val), xaxis = list(title = input$unique_group_col), yaxis = list(title = "Término Clave")) %>%
      config(displayModeBar = FALSE)
  })
  
  # -- Sankey --
  sankey_reac <- reactive({
    df <- viz_data()
    v_orig <- input$sankey_source
    v_dest <- input$sankey_target
    if(is.null(df) || nrow(df) == 0 || is.null(v_orig) || is.null(v_dest)) return(NULL)
    if(!(v_orig %in% names(df) && v_dest %in% names(df))) return(NULL)
    
    sankey_df <- df %>% select(all_of(c(v_orig, v_dest))) %>%
      mutate(across(everything(), as.character)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.) | trimws(.) == "" | tolower(trimws(.)) %in% c("na", "nc", "n/a", "sin informacion"), "NA/NC", trimws(.)))) %>%
      tidyr::separate_rows(!!sym(v_orig), sep = ";\\s*") %>% 
      tidyr::separate_rows(!!sym(v_dest), sep = ";\\s*") %>%
      mutate(across(everything(), ~ ifelse(is.na(.) | trimws(.) == "", "NA/NC", trimws(.))))
    
    if(is.null(input$show_na_sankey) || !input$show_na_sankey) sankey_df <- sankey_df %>% filter(!!sym(v_orig) != "NA/NC", !!sym(v_dest) != "NA/NC")
    sankey_df <- sankey_df %>% count(!!sym(v_orig), !!sym(v_dest), name = "value")
    if(nrow(sankey_df) == 0) return(NULL)
    
    orig_nodes <- paste0(sankey_df[[v_orig]], " (Orig)")
    dest_nodes <- paste0(sankey_df[[v_dest]], " (Dest)")
    all_nodes <- unique(c(orig_nodes, dest_nodes))
    sankey_df$source <- match(orig_nodes, all_nodes) - 1
    sankey_df$target <- match(dest_nodes, all_nodes) - 1
    clean_nodes <- gsub(" \\(Orig\\)| \\(Dest\\)", "", all_nodes)
    
    if(!is.null(input$sankey_label_format)) {
      c_master <- choices_list_master()
      mapped_nodes <- clean_nodes
      for (i in seq_along(clean_nodes)) {
        val <- clean_nodes[i]; found <- FALSE
        for (q_name in names(c_master)) {
          opts <- c_master[[q_name]]
          if (!is.null(opts) && nrow(opts) > 0) {
            idx_acro <- which(opts$Acronimo == val)
            if (length(idx_acro) > 0) {
              if (input$sankey_label_format == "full") mapped_nodes[i] <- opts$Choice[idx_acro[1]]
              else if (input$sankey_label_format == "orig") mapped_nodes[i] <- val
              found <- TRUE; break
            }
            idx_choice <- which(opts$Choice == val)
            if (length(idx_choice) > 0) {
              if (input$sankey_label_format == "acro") mapped_nodes[i] <- opts$Acronimo[idx_choice[1]]
              else if (input$sankey_label_format == "orig") mapped_nodes[i] <- val
              found <- TRUE; break
            }
          }
        }
        if (!found) {
          if (input$sankey_label_format == "full") mapped_nodes[i] <- sub("\\s*\\([^)]+\\)$", "", val)
          else if (input$sankey_label_format == "acro") if (grepl("\\([^)]+\\)$", val)) mapped_nodes[i] <- sub("^.*\\(([^)]+)\\)$", "\\1", val)
        }
      }
      clean_nodes <- mapped_nodes
    }
    
    plot_ly(type = "sankey", orientation = "h", 
            node = list(label = clean_nodes, pad = 15, thickness = 20, line = list(color = "black", width = 0.5)), 
            link = list(source = sankey_df$source, target = sankey_df$target, value = sankey_df$value)) %>% 
      config(displayModeBar = FALSE)
  })
  
  # -- Frecuencias --
  freq_reac <- reactive({
    df <- viz_data()
    req(input$freq_col %in% names(df))
    
    freq_df <- df %>% select(all_of(input$freq_col)) %>% 
      mutate(Categoria = as.character(!!sym(input$freq_col))) %>% 
      mutate(Categoria = ifelse(is.na(Categoria) | trimws(Categoria) == "" | tolower(trimws(Categoria)) %in% c("na", "nc", "n/a", "sin informacion"), "NA/NC", trimws(Categoria)))
    
    if (!is.null(input$weight_multiple) && input$weight_multiple) {
      freq_df <- freq_df %>% mutate(peso_fila = 1 / (str_count(Categoria, ";") + 1))
    } else {
      freq_df <- freq_df %>% mutate(peso_fila = 1)
    }
    
    freq_df <- freq_df %>% 
      tidyr::separate_rows(Categoria, sep = ";\\s*") %>% 
      mutate(Categoria = ifelse(is.na(Categoria) | trimws(Categoria) == "", "NA/NC", trimws(Categoria)))
    
    if (is.null(input$show_na) || !input$show_na) freq_df <- freq_df %>% filter(Categoria != "NA/NC") 
    
    freq_df <- freq_df %>% count(Categoria, wt = peso_fila, name = "Conteo") %>% arrange(desc(Conteo)) %>% head(20)
    if(nrow(freq_df) == 0) return(NULL)
    
    freq_df$Categoria_Corta <- str_trunc(freq_df$Categoria, 20, "right")
    freq_df <- freq_df %>% arrange(Conteo)
    freq_df$Categoria_Corta <- factor(freq_df$Categoria_Corta, levels = unique(freq_df$Categoria_Corta))
    
    plot_ly(freq_df, x = ~Conteo, y = ~Categoria_Corta, type = 'bar', orientation = 'h', 
            text = ~round(Conteo, 2), textposition = 'auto', marker = list(color = '#3498db')) %>% 
      layout(xaxis = list(title = "Cantidad de Medidas"), yaxis = list(title = input$freq_col), margin = list(l = 150)) %>% 
      config(displayModeBar = FALSE)
  })
  
  # -- Network --
  network_reac <- reactive({
    df <- viz_data()
    actor_col <- "Colaboradores externos"
    if(!"Plan" %in% names(df) || !actor_col %in% names(df)) return(NULL)
    
    net_df <- df %>% select(Plan, all_of(actor_col)) %>% tidyr::drop_na() %>% 
      tidyr::separate_rows(!!sym(actor_col), sep = ";\\s*") %>% 
      mutate(!!sym(actor_col) := str_to_title(trimws(!!sym(actor_col)))) %>% filter(!!sym(actor_col) != "") 
    
    if(!is.null(input$shared_actors_only) && input$shared_actors_only) {
      actores_compartidos <- net_df %>% group_by(!!sym(actor_col)) %>% summarise(n_planes = n_distinct(Plan)) %>% filter(n_planes > 1) %>% pull(!!sym(actor_col))
      net_df <- net_df %>% filter(!!sym(actor_col) %in% actores_compartidos)
    }
    
    net_df <- net_df %>% count(Plan, !!sym(actor_col), name = "peso")
    if(nrow(net_df) == 0) return(NULL)
    
    actor_freq <- net_df %>% count(!!sym(actor_col), wt = peso, sort = TRUE)
    limite <- input$top_actors_n
    if(is.null(limite) || is.na(limite)) limite <- nrow(actor_freq)
    top_actors <- head(actor_freq[[actor_col]], limite)
    
    net_df <- net_df %>% filter(!!sym(actor_col) %in% top_actors)
    actores_frecuencias <- net_df %>% group_by(!!sym(actor_col)) %>% summarise(n_planes = n_distinct(Plan))
    actores_comunes <- actores_frecuencias %>% filter(n_planes > 1) %>% pull(!!sym(actor_col))
    
    planes_nodos <- data.frame(id = unique(net_df$Plan), label = unique(net_df$Plan), group = "Plan", font.size = 20, stringsAsFactors = FALSE)
    actores_ids <- unique(net_df[[actor_col]])
    actores_nodos <- data.frame(id = actores_ids, label = actores_ids, stringsAsFactors = FALSE) %>% 
      mutate(group = ifelse(id %in% actores_comunes, "Actor_Comun", "Actor"), font.size = 14)
    
    nodes <- bind_rows(planes_nodos, actores_nodos)
    
    if(!is.null(input$net_dir) && input$net_dir == "RL") { 
      edges <- data.frame(from = net_df[[actor_col]], to = net_df$Plan, value = net_df$peso, stringsAsFactors = FALSE)
    } else { 
      edges <- data.frame(from = net_df$Plan, to = net_df[[actor_col]], value = net_df$peso, stringsAsFactors = FALSE) 
    }
    
    g <- visNetwork(nodes, edges, width = "100%") %>% 
      visGroups(groupname = "Plan", shape = "square", color = "#e74c3c") %>% 
      visGroups(groupname = "Actor", shape = "dot", color = "#2ecc71") %>% 
      visGroups(groupname = "Actor_Comun", shape = "dot", color = "#3498db") %>% 
      visEdges(color = list(color = "#BDC3C7", opacity = 0.5), smooth = list(enabled = TRUE, type = "continuous")) %>% 
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE)
    
    if(is.null(input$net_dir) || input$net_dir == "force") { 
      g <- g %>% visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -80, centralGravity = 0.01, springLength = 150), stabilization = list(enabled = TRUE, iterations = 300))
    } else { 
      g <- g %>% visHierarchicalLayout(direction = input$net_dir, levelSeparation = 300) 
    }
    g
  })
  
  # -- Diagrama de Cuerdas (Chorddiag) --
  chord_data_reac <- reactive({
    df <- viz_data()
    req(input$chord_col %in% names(df))
    
    df_pairs <- df %>% 
      select(ColumnaObjetivo = all_of(input$chord_col)) %>% 
      mutate(row_id = row_number()) %>% 
      mutate(ColumnaObjetivo = as.character(ColumnaObjetivo))
    
    df_pairs <- df_pairs %>% 
      mutate(ColumnaObjetivo = ifelse(is.na(ColumnaObjetivo) | trimws(ColumnaObjetivo) == "" | 
                                        tolower(trimws(ColumnaObjetivo)) %in% c("na", "nc", "n/a"), 
                                      "NA/NC", trimws(ColumnaObjetivo)))
    
    if (is.null(input$chord_show_na) || !input$chord_show_na) {
      df_pairs <- df_pairs %>% filter(ColumnaObjetivo != "NA/NC")
    }
    
    df_pairs <- df_pairs %>% 
      tidyr::separate_rows(ColumnaObjetivo, sep = ";\\s*") %>% 
      mutate(ColumnaObjetivo = trimws(ColumnaObjetivo)) %>% 
      filter(ColumnaObjetivo != "")
    
    df_pairs <- df_pairs %>% 
      group_by(row_id) %>% 
      filter(n() > 1) %>% 
      ungroup()
    
    if(nrow(df_pairs) == 0) return(NULL)
    
    co_oc <- df_pairs %>% 
      inner_join(df_pairs, by = "row_id", relationship = "many-to-many") %>% 
      filter(ColumnaObjetivo.x < ColumnaObjetivo.y) %>% 
      count(origen = ColumnaObjetivo.x, destino = ColumnaObjetivo.y, name = "value") %>% 
      arrange(desc(value))
    
    if(nrow(co_oc) == 0) return(NULL)
    return(co_oc)
  })
  
  chord_matrix_reac <- reactive({
    co_oc <- chord_data_reac()
    req(!is.null(co_oc) && nrow(co_oc) > 0)
    
    nodos <- unique(c(co_oc$origen, co_oc$destino))
    mat <- matrix(0, nrow = length(nodos), ncol = length(nodos), dimnames = list(nodos, nodos))
    
    for(i in 1:nrow(co_oc)) {
      mat[co_oc$origen[i], co_oc$destino[i]] <- co_oc$value[i]
      mat[co_oc$destino[i], co_oc$origen[i]] <- co_oc$value[i]
    }
    return(mat)
  })
  
  output$chord_plot <- renderChorddiag({
    mat <- chord_matrix_reac()
    req(mat)
    
    chorddiag(mat, 
              type = "directional", 
              showTicks = FALSE, 
              groupnamePadding = 15, 
              groupnameFontsize = 13, 
              margin = 120)
  })
  
  # ====================================================================
  # CORRELOGRAMA INTERACTIVO (MATRIZ DE CORRELACIONES PLOTLY)
  # ====================================================================
  corr_plot_reac <- reactive({
    df <- viz_data()
    req(input$corr_cols, length(input$corr_cols) >= 1)
    
    cols_sel <- intersect(input$corr_cols, names(df))
    if(length(cols_sel) == 0) return(NULL)
    
    # 1. Binarización correcta separando los ";"
    df_dummies <- list()
    for(col in cols_sel) {
      sub_df <- df %>% 
        select(ID, val = all_of(col)) %>% 
        filter(!is.na(val) & trimws(val) != "" & !tolower(trimws(val)) %in% c("na", "nc", "n/a")) %>% 
        tidyr::separate_rows(val, sep = ";\\s*") %>% 
        mutate(val = trimws(val)) %>% 
        filter(val != "")
      
      if(nrow(sub_df) > 0) {
        prefix <- if(length(cols_sel) > 1) paste0(str_trunc(col, 8, "right", ellipsis = ""), ": ") else ""
        sub_df <- sub_df %>% 
          mutate(cat_name = paste0(prefix, val), present = 1) %>% 
          select(ID, cat_name, present) %>% 
          distinct(ID, cat_name, .keep_all = TRUE) %>% 
          tidyr::pivot_wider(names_from = cat_name, values_from = present, values_fill = list(present = 0))
        
        df_dummies[[col]] <- sub_df
      }
    }
    
    if(length(df_dummies) == 0) return(NULL)
    
    mat_bin <- df %>% select(ID)
    for(d in df_dummies) { mat_bin <- left_join(mat_bin, d, by = "ID") }
    mat_bin <- mat_bin %>% select(-ID) %>% mutate(across(everything(), ~ as.numeric(ifelse(is.na(.), 0, .))))
    
    # 2. Filtrado por frecuencia mínima
    frecuencias <- colSums(mat_bin, na.rm = TRUE)
    freq_minima <- if(!is.null(input$corr_min_freq)) input$corr_min_freq else 2
    cols_validas <- frecuencias >= freq_minima & frecuencias < nrow(mat_bin)
    mat_bin <- mat_bin[, cols_validas, drop = FALSE]
    if(ncol(mat_bin) < 2) return(NULL)
    
    # 3. Calcular Correlación de Pearson (Phi para variables binarias)
    R <- cor(mat_bin)
    diag_R <- diag(R)
    diag(R) <- 0 # Ignorar la diagonal para el filtro de máximos
    
    # 4. Filtro Anti-Ruido
    umbral <- input$corr_threshold
    max_cor <- apply(abs(R), 1, max, na.rm = TRUE)
    vars_validas <- names(max_cor)[max_cor >= umbral & !is.na(max_cor)]
    
    if(length(vars_validas) < 2) return(NULL)
    
    R <- cor(mat_bin[, vars_validas, drop = FALSE])
    
    # Preparar el texto para el hover de Plotly
    hover_text <- matrix(NA, nrow = nrow(R), ncol = ncol(R))
    for(i in 1:nrow(R)) {
      for(j in 1:ncol(R)) {
        hover_text[i, j] <- paste0(
          "<b>", rownames(R)[i], "</b><br>vs<br><b>", colnames(R)[j], "</b><br><br>",
          "Correlación (r): <b>", round(R[i, j], 3), "</b>"
        )
      }
    }
    
    # 5. Renderizar Heatmap
    p <- plot_ly(
      x = colnames(R), y = rownames(R), z = R,
      type = "heatmap",
      zmin = -1, zmax = 1,
      colorscale = list(c(0, "#e74c3c"), c(0.5, "#ffffff"), c(1, "#3498db")), # Rojo - Blanco - Azul
      hoverinfo = "text",
      text = hover_text,
      showscale = TRUE
    ) %>% layout(
      title = list(text = paste("Matriz de Correlación (Filtro |r| >", umbral, ")"), font = list(size = 16)),
      xaxis = list(tickangle = -45, title = "", tickfont = list(size = 10)),
      yaxis = list(title = "", tickfont = list(size = 10)),
      margin = list(b = 150, l = 150, t = 50, r = 50)
    ) %>% config(displayModeBar = FALSE)
    
    return(p)
  })
  
  output$corr_plot <- renderPlotly({ corr_plot_reac() })
  
  output$dl_corr <- downloadHandler(
    filename = function() { paste0("correlograma_", Sys.Date(), ".png") },
    content = function(file) { generar_png_desde_html(corr_plot_reac(), file) }
  )
  
  # ====================================================================
  # MAPA TERRITORIAL (LEAFLET + CHILEMAPAS)
  # ====================================================================
  # ====================================================================
  # MAPA TERRITORIAL (LEAFLET + CHILEMAPAS)
  # ====================================================================
  map_reac <- reactive({
    df <- viz_data()
    req("Plan" %in% names(df))
    
    # 1. Contar la frecuencia de medidas por plan/comuna
    datos_frecuencia <- df %>%
      count(Plan, name = "cantidad_medidas")
    
    # 2. Preparar el mapa base de la RM
    mapa_rm <- mapa_comunas %>%
      filter(codigo_region == "13") %>%
      st_as_sf() %>%
      st_transform(crs = 4326) %>%
      left_join(codigos_territoriales %>% select(codigo_comuna, nombre_oficial = nombre_comuna), by = "codigo_comuna")
    
    # 3. Cruzar mapa con frecuencias y crear las etiquetas interactivas
    mapa_rm_datos <- mapa_rm %>%
      left_join(datos_frecuencia, by = c("nombre_oficial" = "Plan")) %>%
      mutate(
        etiqueta = paste0(
          "<b>", nombre_oficial, "</b><br>Cantidad de Medidas: ", 
          ifelse(is.na(cantidad_medidas), "0 (Sin datos)", cantidad_medidas)
        )
      )
    
    # 4. Paleta de verdes
    paleta <- colorNumeric(
      palette = "Greens", 
      domain = mapa_rm_datos$cantidad_medidas,
      na.color = "#e8e8e8"
    )
    
    # Extraemos los valores omitiendo los NA exclusivamente para la leyenda
    valores_leyenda <- na.omit(mapa_rm_datos$cantidad_medidas)
    
    # 5. Dibujar mapa aislado (fondo blanco puro vía CSS)
    leaflet(mapa_rm_datos) %>%
      addPolygons(
        fillColor = ~paleta(cantidad_medidas),
        weight = 1.2,
        color = "#888888",
        fillOpacity = 0.9,
        highlightOptions = highlightOptions(weight = 3, color = "#000000", bringToFront = TRUE),
        label = ~lapply(etiqueta, HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "8px 12px", "box-shadow" = "3px 3px rgba(0,0,0,0.25)"),
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = paleta,
        values = valores_leyenda, # Al pasar 'valores_leyenda', Leaflet detecta 0 NAs y oculta esa fila
        title = "Cantidad de<br>Medidas",
        position = "bottomright"
      )
  })
  
  output$map_plot <- renderLeaflet({ map_reac() })
  
  output$dl_map <- downloadHandler(
    filename = function() { paste0("mapa_territorial_", Sys.Date(), ".png") },
    content = function(file) { generar_png_desde_html(map_reac(), file) }
  )
  
  # ====================================================================
  # DESCARGAS A PNG (webshot2)
  # ====================================================================
  generar_png_desde_html <- function(widget_obj, file_path) {
    req(widget_obj)
    temp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(widget_obj, temp_html, selfcontained = TRUE)
    webshot2::webshot(temp_html, file = file_path, delay = 1.5, vwidth = 1200, vheight = 800)
  }
  
  output$nlp_wordcloud <- renderWordcloud2({ wc_global_reac() })
  output$dl_wc_global <- downloadHandler(
    filename = function() { "wordcloud_global.png" },
    content = function(file) { generar_png_desde_html(wc_global_reac(), file) }
  )
  
  output$nlp_heatmap <- renderPlotly({ hm_global_reac() })
  output$dl_hm_global <- downloadHandler(
    filename = function() { "heatmap_global.png" },
    content = function(file) { generar_png_desde_html(hm_global_reac(), file) }
  )
  
  observe({
    df <- viz_data()
    req(input$unique_group_col %in% names(df))
    opciones <- unique(na.omit(df[[input$unique_group_col]]))
    updateSelectInput(session, "unique_group_val", choices = opciones)
  })
  
  output$nlp_wordcloud_unique <- renderWordcloud2({ wc_unique_reac() })
  output$dl_wc_unique <- downloadHandler(
    filename = function() { paste0("wordcloud_unique_", input$unique_group_val, ".png") },
    content = function(file) { generar_png_desde_html(wc_unique_reac(), file) }
  )
  
  output$nlp_heatmap_unique <- renderPlotly({ hm_unique_reac() })
  output$dl_hm_unique <- downloadHandler(
    filename = function() { paste0("heatmap_unique_", input$unique_group_val, ".png") },
    content = function(file) { generar_png_desde_html(hm_unique_reac(), file) }
  )
  
  output$sankey_plot <- renderPlotly({ sankey_reac() })
  output$dl_sankey <- downloadHandler(
    filename = function() { "sankey_dinamico.png" },
    content = function(file) { generar_png_desde_html(sankey_reac(), file) }
  )
  
  output$freq_plot <- renderPlotly({ freq_reac() })
  output$dl_freq <- downloadHandler(
    filename = function() { "distribucion_frecuencias.png" },
    content = function(file) { generar_png_desde_html(freq_reac(), file) }
  )
  
  output$network_plot <- renderVisNetwork({ network_reac() })
  output$dl_network <- downloadHandler(
    filename = function() { "red_colaboradores.png" },
    content = function(file) { generar_png_desde_html(network_reac(), file) }
  )
  
  output$dl_chord <- downloadHandler(
    filename = function() { paste0("diagrama_cuerdas_interactivo_", Sys.Date(), ".png") },
    content = function(file) {
      mat <- chord_matrix_reac()
      req(mat)
      widget_obj <- chorddiag(mat, 
                              type = "directional", 
                              showTicks = FALSE, 
                              groupnamePadding = 15, 
                              groupnameFontsize = 13, 
                              margin = 120)
      generar_png_desde_html(widget_obj, file)
    }
  )
}

shinyApp(ui = ui, server = server)