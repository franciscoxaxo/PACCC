# ==============================================================================
# APLICACIÓN SHINY: CURADOR Y RECLASIFICADOR PACCC
# Arquitectura Basada en Proyectos - Soporte Total de ID (Primary Key)
# ==============================================================================
paquetes_requeridos <- c("shiny", "bslib", "DT", "dplyr", "tidytext", 
                         "stringr", "shinyjs", "plotly", "wordcloud2", 
                         "tidyr", "visNetwork", "readr", "quanteda", 
                         "quanteda.textstats", "htmlwidgets", "webshot2",
                         "leaflet", "sf", "chilemapas", "writexl") 

# Descomentar si necesitas instalar las librerías la primera vez:
# lapply(paquetes_requeridos, function(x) if(!require(x, character.only = TRUE)) install.packages(x))

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidytext)
library(stringr)
library(shinyjs)
library(plotly)
library(wordcloud2)
library(tidyr)
library(visNetwork)
library(readr)
library(quanteda)
library(quanteda.textstats)
library(htmlwidgets)
library(webshot2)
library(leaflet)
library(sf)
library(chilemapas)
library(writexl)

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
# DICCIONARIO DE ZONAS RM (CON CÓDIGOS INE PARA EVITAR ERRORES DE TEXTO)
# ==============================================================================
zonas_rm <- data.frame(
  codigo_comuna = c(
    "13101", "13104", "13107", "13108", "13125", "13127", 
    "13123", "13114", "13132", "13115", "13120", "13113", 
    "13118", "13122", "13110",                            
    "13130", "13129", "13121", "13116", "13109", "13131", "13111", "13105", "13112", 
    "13106", "13102", "13119",                            
    "13126", "13117", "13124", "13103", "13128",          
    "13301", "13302", "13303",                            
    "13201", "13202", "13203",                            
    "13401", "13402", "13403", "13404",                   
    "13501", "13502", "13503", "13504", "13505",          
    "13601", "13602", "13603", "13604", "13605"           
  ),
  nombre_comuna = c(
    "Santiago", "Conchalí", "Huechuraba", "Independencia", "Quilicura", "Recoleta",
    "Providencia", "Las Condes", "Vitacura", "Lo Barnechea", "Ñuñoa", "La Reina",
    "Macul", "Peñalolén", "La Florida",
    "San Miguel", "San Joaquín", "Pedro Aguirre Cerda", "Lo Espejo", "La Cisterna", "San Ramón", "La Granja", "El Bosque", "La Pintana",
    "Estación Central", "Cerrillos", "Maipú",
    "Quinta Normal", "Lo Prado", "Pudahuel", "Cerro Navia", "Renca",
    "Colina", "Lampa", "Tiltil",
    "Puente Alto", "Pirque", "San José de Maipo",
    "San Bernardo", "Buin", "Paine", "Calera de Tango",
    "Melipilla", "Alhué", "Curacaví", "María Pinto", "San Pedro",
    "Talagante", "El Monte", "Isla de Maipo", "Padre Hurtado", "Peñaflor"
  ),
  zona = c(
    "Centro", rep("Norte", 5), rep("Nororiente", 6), rep("Suroriente", 3),
    rep("Sur", 9), rep("Surponiente", 3), rep("Norponiente", 5),
    rep("Chacabuco", 3), rep("Cordillera", 3), rep("Maipo", 4),
    rep("Melipilla", 5), rep("Talagante", 5)
  ),
  stringsAsFactors = FALSE
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
  withMathJax(),
  title = "Curador y Reclasificador PACCC",
  theme = bs_theme(version = 5, bootswatch = "flatly"), 
  
  tags$head(
    tags$style(HTML(".jump-box input { height: 30px; text-align: center; } .leaflet-container { background: #ffffff !important; }"))
  ),
  
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
    
    nav_panel("Capa 0: Info General", icon = icon("info-circle"), br(),
              fluidRow(
                column(3, card(card_header("Gestión de Columnas", class = "bg-primary text-white"), card_body(
                  textInput("new_info_col", "Nombre de Columna:", placeholder = "Ej: Presupuesto"),
                  selectInput("new_info_type", "Tipo de Dato:", choices = c("Texto", "Número", "Dinero", "Fecha", "Lista")),
                  
                  conditionalPanel(
                    condition = "input.new_info_type == 'Lista'",
                    textInput("new_info_options", "Opciones de Lista (sep. por punto y coma):", placeholder = "Alto; Medio; Bajo")
                  ),
                  conditionalPanel(
                    condition = "input.new_info_type == 'Número' || input.new_info_type == 'Dinero'",
                    textInput("new_info_nulls", "Textos aceptados en esta col. (sep. por ;):", placeholder = "S/I; Comprometido")
                  ),
                  
                  actionButton("add_info_col_btn", "Guardar / Actualizar Columna", class = "btn-info text-white", width = "100%", icon = icon("plus")),
                  hr(),
                  actionButton("save_info_btn", "Guardar Datos (CSV)", class = "btn-success", width = "100%", icon = icon("save")),
                  br(),
                  p(style="font-size: 0.85em; color: #666; margin-top: 15px;", 
                    "Tip: Haz doble clic en una celda de la tabla para editar."),
                  p(style="font-size: 0.85em; color: #2c3e50; font-weight: bold;", 
                    "💡 Dinamismo: Si agregas valores directo en el CSV/Excel, o si escribes aquí el nombre de una Lista existente y le pones nuevas opciones, la app actualizará los menús automáticamente.")
                ))),
                column(9, card(card_header("Información de los Planes (Editable)", class = "bg-dark text-white"), card_body(
                  DTOutput("info_gral_table")
                )))
              )
    ),
    
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
                  selectizeInput("plan_filter", "Filtrar por Plan(es):", choices = NULL, multiple = TRUE, options = list(placeholder = 'Todos los planes')), 
                  hr(),
                  
                  downloadButton("dl_data_excel", "📥 Descargar Excel (Datos)", class="btn-success text-white w-100", style="font-weight: bold; margin-bottom: 15px;"),
                  hr(),
                  
                  selectInput("viz_mode", "Seleccionar Gráfico:", 
                              choices = c("1. Análisis de Texto (NLP)" = "nlp", 
                                          "2. Distribución de Frecuencias" = "freq", 
                                          "3. Flujo Dinámico (Sankey)" = "sankey", 
                                          "4. Red Plan vs Actores" = "network",
                                          "5. Matriz de Correlaciones" = "corrmatrix",
                                          "6. Mapa Territorial RM" = "map")), hr(),
                  
                  conditionalPanel(condition = "input.viz_mode == 'nlp'",
                                   radioButtons("nlp_type", "Tipo de Nube:", choices = c("1. Frecuencia Global" = "global", "2. Exclusividad (Keyness)" = "unique")),
                                   hr(),
                                   radioButtons("ngram_mode", "Composición de Palabras:", 
                                                choices = c("Solo Palabras (Unigramas)" = "unigrams", 
                                                            "Pares (Bigramas)" = "bigrams", 
                                                            "Ambos mezclados" = "both"), 
                                                selected = "unigrams"),
                                   hr(),
                                   radioButtons("verb_filter", "Filtrar acciones / verbos:", 
                                                choices = c("Todas las palabras" = "all", "Solo verbos (infinitivos)" = "only_verbs", "Excluir verbos (infinitivos)" = "no_verbs"), selected = "all"),
                                   h6("Filtros de Exclusión"), 
                                   textInput("custom_stopwords", "Excluir palabras:", value = "para, el, la,esta,busca,medida,estos,este,sobre,tiene,estas,asimismo,entre,través,manera, los, las, con, de, en, del, a, y, o, por, se, su, sus, como, al, una, un, que"),
                                   
                                   conditionalPanel(condition = "input.nlp_type == 'global'", hr(),
                                                    radioButtons("wc_count_mode", "Modo de Cálculo:", choices = c("Ponderado por Dispersión (Anti-sesgo)" = "pond", "Frecuencia Absoluta (Clásico)" = "abs"), selected = "pond")
                                   ),
                                   
                                   conditionalPanel(condition = "input.nlp_type == 'unique'", hr(), h6("Configuración Keyness"),
                                                    selectInput("unique_group_col", "Agrupar por:", choices = c("Plan", "Eje", "Área")), 
                                                    selectInput("unique_group_val", "Mostrar exclusivas de:", choices = NULL)
                                   )
                  ),
                  
                  conditionalPanel(condition = "input.viz_mode == 'sankey'", hr(), h6("Ejes Sankey"),
                                   selectInput("sankey_source", "Origen:", choices = NULL), selectInput("sankey_target", "Destino:", choices = NULL),
                                   radioButtons("sankey_label_format", "Formato de Etiquetas:", choices = c("Original" = "orig", "Nombre Completo" = "full", "Solo Acrónimo" = "acro"), selected = "orig"),
                                   checkboxInput("show_na_sankey", "Incluir vacíos/nulos (como 'NA/NC')", value = FALSE)),
                  
                  conditionalPanel(condition = "input.viz_mode == 'freq'", hr(), h6("Configuración de Frecuencias"), 
                                   selectInput("freq_col", "Variable a contar:", choices = NULL),
                                   checkboxInput("weight_multiple", "Ponderar múltiples equitativamente (ej. 1/N si hay 2 categorías)", value = FALSE),
                                   checkboxInput("show_na", "Incluir vacíos/nulos (como 'NA/NC')", value = FALSE)),
                  
                  conditionalPanel(condition = "input.viz_mode == 'network'", hr(), h6("Configuración de Red"),
                                   selectizeInput("network_search_actor", "🔍 Buscar/Filtrar por Actor:", choices = NULL, options = list(placeholder = 'Escriba un actor...')),
                                   numericInput("top_actors_n", "Cantidad Máxima de Actores (Top):", value = 15, min = 1),
                                   checkboxInput("shared_actors_only", "Mostrar SOLO actores compartidos (>1 Plan)", value = FALSE)),
                  
                  conditionalPanel(condition = "input.viz_mode == 'corrmatrix'", hr(), h6("Configuración de Correlaciones"),
                                   selectizeInput("corr_cols", "Variables a cruzar:", choices = NULL, multiple = TRUE),
                                   numericInput("corr_min_freq", "Frecuencia mínima (casos por categoría):", value = 3, min = 2),
                                   sliderInput("corr_threshold", "Ocultar si correlación máxima es menor a:", min = 0.0, max = 0.8, value = 0.15, step = 0.05)),
                  
                  conditionalPanel(condition = "input.viz_mode == 'map'", hr(), h6("Configuración de Mapa"),
                                   selectInput("map_var", "Variable a Mapear:", choices = c("Cantidad de Medidas" = "medidas")),
                                   radioButtons("map_grouping", "Agrupar el territorio por:", 
                                                choices = c("1. Zonas/Provincias (Promedios/Resumen)" = "zona", "2. Comunas (Medidas exactas)" = "comuna"), 
                                                selected = "zona"),
                                   radioButtons("map_scale", "Escala de Colores (Numérica):", 
                                                choices = c("Lineal" = "lin", "Logarítmica" = "log"), 
                                                selected = "lin", inline = TRUE),
                                   checkboxInput("map_always_labels", "Etiquetas siempre visibles", value = FALSE))
                ),
                
                # =========================================================
                # VISTAS (MAIN PANELS)
                # =========================================================
                
                conditionalPanel(condition = "input.viz_mode == 'nlp'", 
                                 fluidRow(column(12, card(
                                   card_header(
                                     div(style="display: flex; justify-content: space-between; align-items: center;",
                                         span(textOutput("nlp_title_ui", inline=TRUE)),
                                         downloadButton("dl_nlp", "Descargar PNG", class="btn-sm btn-outline-primary")
                                     )
                                   ), 
                                   card_body(wordcloud2Output("nlp_wordcloud_ui", height = "650px"))
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
                
                conditionalPanel(condition = "input.viz_mode == 'network'", 
                                 fluidRow(column(12, uiOutput("network_kpi_ui"))),
                                 fluidRow(column(12, card(
                                   card_header(
                                     div(style="display: flex; justify-content: space-between; align-items: center;",
                                         span("Red de Colaboradores Externos"), downloadButton("dl_network", "Descargar PNG", class="btn-sm btn-outline-primary")
                                     )),
                                   visNetworkOutput("network_plot", height = "650px")
                                 )))),
                
                conditionalPanel(condition = "input.viz_mode == 'corrmatrix'", fluidRow(column(12, card(
                  card_header(
                    div(style="display: flex; justify-content: space-between; align-items: center;",
                        span("Matriz de Correlaciones (Co-ocurrencia / Exclusión)"), 
                        downloadButton("dl_corr", "Descargar PNG", class="btn-sm btn-outline-primary")
                    )),
                  card_body(plotlyOutput("corr_plot", height = "750px"))
                )))),
                
                conditionalPanel(condition = "input.viz_mode == 'map'", fluidRow(column(12, card(
                  card_header(
                    div(style="display: flex; justify-content: space-between; align-items: center;",
                        span("Distribución Territorial de Medidas (Región Metropolitana)"), 
                        downloadButton("dl_map", "Descargar PNG", class="btn-sm btn-outline-primary")
                    )),
                  card_body(leafletOutput("map_plot", height = "650px"))
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
  
  info_gral_data <- reactiveVal(data.frame(Plan = character(), stringsAsFactors = FALSE))
  info_gral_dict <- reactiveVal(data.frame(Columna=character(), Tipo=character(), Opciones=character(), stringsAsFactors=FALSE))
  
  current_choices <- reactiveVal(data.frame(Choice=character(), Acronimo=character(), stringsAsFactors=FALSE))
  questions_set <- reactiveVal(data.frame(Pregunta=character(), Tipo=character(), Opciones_Acronimos=character(), stringsAsFactors=FALSE))
  choices_list_master <- reactiveVal(list())
  
  output$project_is_active <- reactive({ !is.null(active_project()) })
  outputOptions(output, "project_is_active", suspendWhenHidden = FALSE)
  
  output$active_proj_ui <- renderUI({
    if(is.null(active_project())) { HTML("<div class='alert alert-warning'>Ningún proyecto activo</div>") }
    else { HTML(paste0("<div class='alert alert-success'><strong>Proyecto Activo:</strong><br>", active_project(), "</div>")) }
  })
  
  # ==========================================================================
  # LÓGICA DE INFO GENERAL (CAPA 0)
  # ==========================================================================
  
  output$info_gral_table <- renderDT({
    df <- info_gral_data()
    req(df)
    dict <- info_gral_dict()
    
    if (nrow(dict) > 0) {
      for(i in 1:nrow(dict)) {
        col <- dict$Columna[i]
        t <- dict$Tipo[i]
        if (col %in% names(df) && t == "Lista") {
          niveles_dict <- strsplit(dict$Opciones[i], ";")[[1]] %>% trimws()
          niveles_dict <- niveles_dict[niveles_dict != ""]
          
          valores_actuales <- unique(na.omit(as.character(df[[col]])))
          valores_actuales <- valores_actuales[trimws(valores_actuales) != ""]
          
          niveles_finales <- unique(c(niveles_dict, valores_actuales))
          df[[col]] <- factor(as.character(df[[col]]), levels = niveles_finales)
        }
      }
    }
    
    cols_dinero <- which(names(df) %in% dict$Columna[dict$Tipo == "Dinero"]) - 1
    cols_numero <- which(names(df) %in% dict$Columna[dict$Tipo == "Número"]) - 1
    
    col_defs <- list()
    
    if (length(cols_dinero) > 0) {
      col_defs[[length(col_defs) + 1]] <- list(
        targets = cols_dinero,
        render = JS(
          "function(data, type, row, meta) {",
          "  if(type === 'display' && data !== null && data !== '') {",
          "    var str = data.toString().trim();",
          "    if(/^[0-9.,-]+$/.test(str)) {",
          "      var num = parseFloat(str.replace(/\\./g, '').replace(',', '.'));",
          "      if(!isNaN(num)) return '$ ' + num.toLocaleString('es-CL');",
          "    }",
          "  }",
          "  return data;",
          "}"
        )
      )
    }
    if (length(cols_numero) > 0) {
      col_defs[[length(col_defs) + 1]] <- list(
        targets = cols_numero,
        render = JS(
          "function(data, type, row, meta) {",
          "  if(type === 'display' && data !== null && data !== '') {",
          "    var str = data.toString().trim();",
          "    if(/^[0-9.,-]+$/.test(str)) {",
          "      var num = parseFloat(str.replace(/\\./g, '').replace(',', '.'));",
          "      if(!isNaN(num)) return num.toLocaleString('es-CL');",
          "    }",
          "  }",
          "  return data;",
          "}"
        )
      )
    }
    
    # 2. CONFIGURACIÓN EXPLÍCITA DE MENÚS DESPLEGABLES (LISTAS) PARA DATATABLES
    if (nrow(dict) > 0) {
      list_rows <- which(dict$Tipo == "Lista" & dict$Columna %in% names(df))
      if (length(list_rows) > 0) {
        for (r in list_rows) {
          col_name <- dict$Columna[r]
          col_idx <- which(names(df) == col_name) - 1
          opciones_str <- dict$Opciones[r]
          
          opts_vec <- strsplit(opciones_str, ";")[[1]] %>% trimws()
          opts_vec <- opts_vec[opts_vec != ""]
          opts_js <- paste0('"', opts_vec, '"', collapse = ",")
          
          col_defs[[length(col_defs) + 1]] <- list(
            targets = col_idx,
            createdCell = JS(paste0(
              "function (td, cellData, rowData, row, col) {",
              "  $(td).attr('data-type', 'select');",
              "  $(td).attr('data-options', '[", opts_js, "]');",
              "}"
            ))
          )
        }
      }
    }
    
    datatable(df, editable = list(target = "cell"), rownames = FALSE, 
              options = list(pageLength = 100, scrollX = TRUE, columnDefs = col_defs))
  })
  
  observeEvent(input$info_gral_table_cell_edit, {
    info <- input$info_gral_table_cell_edit
    df <- info_gral_data()
    dict <- info_gral_dict()
    
    if (nrow(dict) > 0) {
      for(i in 1:nrow(dict)) {
        col <- dict$Columna[i]
        if (col %in% names(df) && dict$Tipo[i] == "Lista") {
          niveles_dict <- strsplit(dict$Opciones[i], ";")[[1]] %>% trimws()
          niveles_dict <- niveles_dict[niveles_dict != ""]
          
          valores_actuales <- unique(na.omit(as.character(df[[col]])))
          valores_actuales <- valores_actuales[trimws(valores_actuales) != ""]
          
          niveles_finales <- unique(c(niveles_dict, valores_actuales))
          df[[col]] <- factor(as.character(df[[col]]), levels = niveles_finales)
        }
      }
    }
    
    df <- DT::editData(df, info, rownames = FALSE)
    info_gral_data(df)
  })
  
  observeEvent(input$add_info_col_btn, {
    new_col <- trimws(input$new_info_col)
    t <- input$new_info_type
    opt <- if(t == "Lista") input$new_info_options else if(t %in% c("Número", "Dinero")) input$new_info_nulls else ""
    req(new_col, new_col != "")
    
    df <- info_gral_data()
    dict <- info_gral_dict()
    
    if(!new_col %in% names(df)) {
      if(t == "Fecha") {
        df[[new_col]] <- as.Date(NA)
      } else if(t == "Lista") {
        niveles <- strsplit(opt, ";")[[1]] %>% trimws()
        df[[new_col]] <- factor(rep(NA_character_, nrow(df)), levels = niveles)
      } else {
        df[[new_col]] <- ""
      }
      info_gral_data(df)
      
      new_dict_row <- data.frame(Columna=new_col, Tipo=t, Opciones=opt, stringsAsFactors=FALSE)
      dict <- bind_rows(dict, new_dict_row)
      info_gral_dict(dict)
      
      if (!is.null(active_project())) {
        proj_dir <- file.path(workspace, active_project())
        write_excel_csv2(dict, file.path(proj_dir, "config", "diccionario_info_general.csv"), na = "")
        write_excel_csv2(df, file.path(proj_dir, "info_general.csv"), na = "")
      }
      
      updateTextInput(session, "new_info_col", value = "")
      if(t == "Lista") updateTextInput(session, "new_info_options", value = "")
      if(t %in% c("Número", "Dinero")) updateTextInput(session, "new_info_nulls", value = "")
      showNotification(paste("Columna", new_col, "agregada con éxito como", t), type = "message")
      
    } else {
      idx <- which(dict$Columna == new_col)
      if(length(idx) > 0 && dict$Tipo[idx] == "Lista" && t == "Lista") {
        opciones_actuales <- strsplit(dict$Opciones[idx], ";")[[1]] %>% trimws()
        opciones_nuevas <- strsplit(opt, ";")[[1]] %>% trimws()
        opciones_combinadas <- unique(c(opciones_actuales, opciones_nuevas))
        opciones_combinadas <- opciones_combinadas[opciones_combinadas != ""]
        
        if(length(opciones_combinadas) > length(opciones_actuales)) {
          dict$Opciones[idx] <- paste(opciones_combinadas, collapse = "; ")
          info_gral_dict(dict)
          
          if (!is.null(active_project())) {
            proj_dir <- file.path(workspace, active_project())
            write_excel_csv2(dict, file.path(proj_dir, "config", "diccionario_info_general.csv"), na = "")
          }
          showNotification(paste("Nuevas opciones agregadas a la lista:", new_col), type = "message")
          updateTextInput(session, "new_info_col", value = "")
          updateTextInput(session, "new_info_options", value = "")
        } else {
          showNotification("Esas opciones ya existen en la lista.", type = "warning")
        }
      } else {
        showNotification("La columna ya existe y no es posible modificar su tipo base.", type = "error")
      }
    }
  })
  
  observeEvent(input$save_info_btn, {
    req(active_project(), info_gral_data())
    file_path <- file.path(workspace, active_project(), "info_general.csv")
    write_excel_csv2(info_gral_data(), file_path, na = "")
    showNotification("Información General guardada con éxito en el proyecto.", type = "message")
  })
  
  # ==========================================================================
  # CREACIÓN Y CARGA DE PROYECTOS
  # ==========================================================================
  
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
    
    planes_unicos <- if("Plan" %in% names(df_new)) unique(na.omit(df_new$Plan)) else character(0)
    df_info_new <- data.frame(Plan = planes_unicos, stringsAsFactors = FALSE)
    write_excel_csv2(df_info_new, file.path(proj_dir, "info_general.csv"), na = "")
    
    dict_new <- data.frame(Columna="Plan", Tipo="Texto", Opciones="", stringsAsFactors=FALSE)
    write_excel_csv2(dict_new, file.path(proj_dir, "config", "diccionario_info_general.csv"), na = "")
    
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
    
    info_file <- file.path(proj_dir, "info_general.csv")
    dict_info_file <- file.path(proj_dir, "config", "diccionario_info_general.csv")
    
    dict <- data.frame(Columna=character(), Tipo=character(), Opciones=character(), stringsAsFactors=FALSE)
    if(file.exists(dict_info_file)) { dict <- leer_csv_robusto(dict_info_file) }
    
    if (file.exists(info_file)) {
      df_info <- leer_csv_robusto(info_file)
      cambios_dict <- FALSE
      
      if(nrow(dict) > 0) {
        for(i in 1:nrow(dict)) {
          col <- dict$Columna[i]; t <- dict$Tipo[i]; opt <- dict$Opciones[i]
          
          if(col %in% names(df_info)) {
            if(t == "Lista") {
              niveles_dict <- strsplit(opt, ";")[[1]] %>% trimws()
              niveles_data <- unique(na.omit(as.character(df_info[[col]])))
              niveles_data <- niveles_data[trimws(niveles_data) != ""]
              niveles_combinados <- unique(c(niveles_dict, niveles_data))
              
              if(length(niveles_combinados) > length(niveles_dict)) {
                dict$Opciones[i] <- paste(niveles_combinados, collapse = "; ")
                cambios_dict <- TRUE
              }
            }
            df_info[[col]] <- as.character(df_info[[col]])
          }
        }
      }
      info_gral_dict(dict)
      if(cambios_dict) { write_excel_csv2(dict, dict_info_file, na = "") }
      info_gral_data(df_info)
    } else {
      planes_unicos <- if("Plan" %in% names(df_bruta)) unique(na.omit(df_bruta$Plan)) else character(0)
      info_gral_data(data.frame(Plan = planes_unicos, stringsAsFactors = FALSE))
      info_gral_dict(dict)
    }
    
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
  
  # === ACTUALIZAR OPCIONES MAPA SEGUN CAPA 0 ===
  observe({
    req(info_gral_data())
    df_info <- info_gral_data()
    cols <- setdiff(names(df_info), "Plan")
    opciones <- c("Cantidad de Medidas" = "medidas")
    if (length(cols) > 0) { opciones <- c(opciones, cols) }
    
    sel <- input$map_var
    if (is.null(sel) || !(sel %in% opciones)) sel <- "medidas"
    updateSelectInput(session, "map_var", choices = opciones, selected = sel)
  })
  
  observeEvent(input$map_var, {
    if(!is.null(input$map_var) && input$map_var != "medidas" && input$map_grouping == "zona") {
      updateRadioButtons(session, "map_grouping", selected = "comuna")
      showNotification("Vista cambiada a Comuna para visualizar el detalle del Plan.", type = "message")
    }
  })
  
  observeEvent(viz_data(), {
    df <- viz_data()
    actor_col <- "Colaboradores externos"
    if("Plan" %in% names(df) && actor_col %in% names(df)) {
      actores <- df %>% select(all_of(actor_col)) %>% tidyr::drop_na() %>% 
        tidyr::separate_rows(!!sym(actor_col), sep = ";\\s*") %>% 
        mutate(!!sym(actor_col) := str_to_title(trimws(!!sym(actor_col)))) %>% 
        filter(!!sym(actor_col) != "") %>% pull(!!sym(actor_col)) %>% unique() %>% sort()
      
      updateSelectizeInput(session, "network_search_actor", choices = c("--- Mostrar Todos ---" = "", actores), server = TRUE)
      
      if (is.null(input$network_search_actor) || input$network_search_actor == "") {
        updateNumericInput(session, "top_actors_n", value = min(15, length(actores)), max = length(actores))
      }
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
  
  wc_global_data_reac <- reactive({
    df <- viz_data(); idx_desc <- grep("descripci", tolower(names(df)))
    if(length(idx_desc) == 0) return(NULL)
    nombre_col_real <- names(df)[idx_desc[1]]; group_col <- if("Plan" %in% names(df)) "Plan" else "ID" 
    
    tokens <- generar_tokens(df, nombre_col_real, group_col, input$custom_stopwords, input$verb_filter, input$ngram_mode)
    if(nrow(tokens) == 0) return(NULL)
    
    total_grupos <- n_distinct(tokens$Grupo)
    word_metrics <- tokens %>% group_by(word) %>% summarise(frecuencia_absoluta = n(), grupos_distintos = n_distinct(Grupo)) 
    
    if (!is.null(input$wc_count_mode) && input$wc_count_mode == "pond") {
      word_metrics <- word_metrics %>% mutate(peso_dispersion = grupos_distintos / total_grupos, freq_final = frecuencia_absoluta * peso_dispersion) %>% filter(grupos_distintos > 1 | total_grupos == 1)
    } else {
      word_metrics <- word_metrics %>% mutate(freq_final = frecuencia_absoluta)
    }
    
    freqs <- word_metrics %>% arrange(desc(freq_final)) %>% select(word, freq = freq_final, frecuencia_absoluta, grupos_distintos)
    freqs$freq <- round(freqs$freq, 1); as.data.frame(freqs)
  })
  
  wc_unique_data_reac <- reactive({
    df_global <- curated_data(); req(input$unique_group_col, input$unique_group_val)
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
    
    freqs <- stat_keyness %>% filter(chi2 > 0 & p < 0.05) %>% arrange(desc(chi2)) %>% select(word = feature, freq = chi2, p_value = p) %>% mutate(freq = round(freq, 1))
    as.data.frame(freqs)
  })
  
  output$nlp_title_ui <- renderText({
    if(is.null(input$nlp_type) || input$nlp_type == "global") { "Nube de Palabras: Frecuencia Global" }
    else { paste("Nube de Palabras (Keyness Chi-Cuadrado):", input$unique_group_val) }
  })
  
  output$nlp_wordcloud_ui <- renderWordcloud2({
    if(!is.null(input$nlp_type) && input$nlp_type == "unique") {
      freqs <- wc_unique_data_reac(); req(freqs)
      wordcloud2(head(freqs[, c("word", "freq")], 80), size = 0.6, color = "random-light", backgroundColor = "#2c3e50")
    } else {
      freqs <- wc_global_data_reac(); req(freqs)
      wordcloud2(head(freqs[, c("word", "freq")], 100), size = 0.6)
    }
  })
  
  sankey_data_reac <- reactive({
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
    sankey_df %>% count(Origen = !!sym(v_orig), Destino = !!sym(v_dest), name = "value")
  })
  
  output$sankey_plot <- renderPlotly({
    sankey_df <- sankey_data_reac()
    req(sankey_df, nrow(sankey_df) > 0)
    
    v_orig <- "Origen"
    v_dest <- "Destino"
    
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
  
  freq_data_reac <- reactive({
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
    
    freq_df %>% count(Categoria, wt = peso_fila, name = "Conteo") %>% arrange(desc(Conteo))
  })
  
  output$freq_plot <- renderPlotly({
    freq_df <- freq_data_reac()
    req(freq_df)
    freq_df <- head(freq_df, 20)
    if(nrow(freq_df) == 0) return(NULL)
    
    freq_df$Categoria_Corta <- str_trunc(freq_df$Categoria, 20, "right")
    freq_df <- freq_df %>% arrange(Conteo)
    freq_df$Categoria_Corta <- factor(freq_df$Categoria_Corta, levels = unique(freq_df$Categoria_Corta))
    
    plot_ly(freq_df, x = ~Conteo, y = ~Categoria_Corta, type = 'bar', orientation = 'h', 
            text = ~round(Conteo, 2), textposition = 'auto', marker = list(color = '#3498db')) %>% 
      layout(xaxis = list(title = "Cantidad de Medidas"), yaxis = list(title = input$freq_col), margin = list(l = 150)) %>% 
      config(displayModeBar = FALSE)
  })
  
  network_kpi_data <- reactive({
    df <- viz_data()
    actor_col <- "Colaboradores externos"
    req("Plan" %in% names(df), actor_col %in% names(df))
    
    p_totales <- n_distinct(df$Plan, na.rm = TRUE)
    if(p_totales == 0) return(NULL)
    
    net_df <- df %>% 
      select(Plan, all_of(actor_col)) %>% 
      mutate(Actor = as.character(!!sym(actor_col))) %>%
      mutate(Actor = ifelse(is.na(Actor) | trimws(Actor) == "" | tolower(trimws(Actor)) %in% c("na", "nc", "n/a", "sin informacion"), NA_character_, trimws(Actor)))
    
    planes_con_actores <- net_df %>% filter(!is.na(Actor)) %>% pull(Plan) %>% unique()
    p_aislados <- p_totales - length(planes_con_actores)
    
    edges_df <- net_df %>% filter(!is.na(Actor)) %>% 
      tidyr::separate_rows(Actor, sep = ";\\s*") %>% mutate(Actor = str_to_title(trimws(Actor))) %>% filter(Actor != "")
    
    e_totales <- nrow(edges_df)
    actores_counts <- edges_df %>% group_by(Actor) %>% summarise(n_planes = n_distinct(Plan))
    actores_compartidos <- actores_counts %>% filter(n_planes > 1) %>% pull(Actor)
    e_compartidos <- edges_df %>% filter(Actor %in% actores_compartidos) %>% nrow()
    
    if(e_totales > 0) {
      peso_red <- e_compartidos / e_totales
      penalizacion <- 1 - (p_aislados / p_totales)
      tasa <- peso_red * penalizacion
    } else { tasa <- 0 }
    
    list(tasa = tasa, e_totales = e_totales, e_compartidos = e_compartidos, p_totales = p_totales, p_aislados = p_aislados)
  })
  
  output$network_kpi_ui <- renderUI({
    res <- network_kpi_data(); req(res)
    withMathJax(
      card(class = "bg-light mb-3",
           card_header(class = "bg-dark text-white", icon("project-diagram"), " Índice de Integración Bipartita (Tasa de Simbiosis)"),
           card_body(
             fluidRow(
               column(4, div(style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
                             h2(paste0(round(res$tasa * 100, 1), "%"), class = "text-primary", style="font-size: 3.5rem; font-weight: bold; margin-bottom: 0;"),
                             p(class="text-muted mt-2", style="font-size: 1.1rem;", "Nivel de interdependencia global"))),
               column(8, p("Métrica de cohesión del ecosistema (núcleo compartido vs periferia)."),
                      div(style = "background: #fff; padding: 10px; border-radius: 5px; border: 1px solid #ddd; text-align: center; margin-bottom: 15px;",
                          "$$Tasa = \\left( \\frac{\\sum E_{comp}}{\\sum E_{tot}} \\right) \\times \\left( 1 - \\frac{P_{aislados}}{P_{tot}} \\right)$$"),
                      tags$ul(style="font-size: 0.9em; color: #444;",
                              tags$li(strong(paste0("E_comp (", res$e_compartidos, "):")), " Colaboraciones de agentes que conectan >1 plan."),
                              tags$li(strong(paste0("E_tot (", res$e_totales, "):")), " Total de colaboraciones de la red."),
                              tags$li(strong(paste0("P_aislados (", res$p_aislados, "):")), " Planes sin colaboradores externos."),
                              tags$li(strong(paste0("P_tot (", res$p_totales, "):")), " Total de planes en el universo visualizado.")))
             )
           )
      )
    )
  })
  
  network_data_reac <- reactive({
    df <- viz_data()
    actor_col <- "Colaboradores externos"
    if(!"Plan" %in% names(df) || !actor_col %in% names(df)) return(NULL)
    
    net_df <- df %>% select(Plan, all_of(actor_col)) %>% tidyr::drop_na() %>% 
      tidyr::separate_rows(!!sym(actor_col), sep = ";\\s*") %>% 
      mutate(!!sym(actor_col) := str_to_title(trimws(!!sym(actor_col)))) %>% filter(!!sym(actor_col) != "") 
    
    search_actor <- input$network_search_actor
    if(!is.null(search_actor) && search_actor != "") {
      planes_vinculados <- net_df %>% filter(!!sym(actor_col) == search_actor) %>% pull(Plan) %>% unique()
      net_df <- net_df %>% filter(Plan %in% planes_vinculados)
    }
    
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
    
    if(!is.null(search_actor) && search_actor != "") {
      top_actors <- unique(c(top_actors, search_actor))
    }
    
    net_df %>% filter(!!sym(actor_col) %in% top_actors)
  })
  
  output$network_plot <- renderVisNetwork({
    net_df <- network_data_reac()
    req(net_df)
    actor_col <- "Colaboradores externos"
    
    search_actor <- input$network_search_actor
    if (is.null(search_actor)) {
      search_actor <- ""
    }
    
    actores_frecuencias <- net_df %>% group_by(!!sym(actor_col)) %>% summarise(n_planes = n_distinct(Plan))
    actores_comunes <- actores_frecuencias %>% filter(n_planes > 1) %>% pull(!!sym(actor_col))
    
    planes_nodos <- data.frame(id = unique(net_df$Plan), label = unique(net_df$Plan), group = "Plan", font.size = 20, stringsAsFactors = FALSE)
    actores_ids <- unique(net_df[[actor_col]])
    
    actores_nodos <- data.frame(id = actores_ids, label = actores_ids, stringsAsFactors = FALSE) %>% 
      mutate(
        es_buscado = (search_actor != "" & id == search_actor),
        group = case_when(
          es_buscado ~ "Actor_Buscado",
          id %in% actores_comunes ~ "Actor_Comun",
          TRUE ~ "Actor"
        ),
        font.size = ifelse(es_buscado, 28, 14)
      ) %>%
      select(-es_buscado)
    
    nodes <- bind_rows(planes_nodos, actores_nodos)
    
    edges <- data.frame(from = net_df$Plan, to = net_df[[actor_col]], value = net_df$peso, stringsAsFactors = FALSE) 
    
    g <- visNetwork(nodes, edges, width = "100%") %>% 
      visGroups(groupname = "Plan", shape = "square", color = "#e74c3c") %>% 
      visGroups(groupname = "Actor", shape = "dot", color = "#2ecc71") %>% 
      visGroups(groupname = "Actor_Comun", shape = "dot", color = "#3498db") %>% 
      visGroups(groupname = "Actor_Buscado", shape = "star", color = "#f1c40f") %>% 
      visEdges(color = list(color = "#BDC3C7", opacity = 0.5), smooth = list(enabled = TRUE, type = "continuous")) %>% 
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>% 
      visPhysics(solver = "forceAtlas2Based", 
                 forceAtlas2Based = list(gravitationalConstant = -80, centralGravity = 0.01, springLength = 150), 
                 stabilization = list(enabled = TRUE, iterations = 300))
    g
  })
  
  corr_data_reac <- reactive({
    df <- viz_data()
    req(input$corr_cols, length(input$corr_cols) >= 1)
    
    cols_sel <- intersect(input$corr_cols, names(df))
    if(length(cols_sel) == 0) return(NULL)
    
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
    
    frecuencias <- colSums(mat_bin, na.rm = TRUE)
    freq_minima <- if(!is.null(input$corr_min_freq)) input$corr_min_freq else 2
    cols_validas <- frecuencias >= freq_minima & frecuencias < nrow(mat_bin)
    mat_bin <- mat_bin[, cols_validas, drop = FALSE]
    if(ncol(mat_bin) < 2) return(NULL)
    
    R <- cor(mat_bin)
    
    umbral <- input$corr_threshold
    diag_R <- diag(R)
    diag(R) <- 0 
    max_cor <- apply(abs(R), 1, max, na.rm = TRUE)
    vars_validas <- names(max_cor)[max_cor >= umbral & !is.na(max_cor)]
    
    if(length(vars_validas) < 2) return(NULL)
    
    cor(mat_bin[, vars_validas, drop = FALSE])
  })
  
  output$corr_plot <- renderPlotly({
    R <- corr_data_reac()
    req(R)
    umbral <- input$corr_threshold
    
    hover_text <- matrix(NA, nrow = nrow(R), ncol = ncol(R))
    for(i in 1:nrow(R)) {
      for(j in 1:ncol(R)) {
        hover_text[i, j] <- paste0(
          "<b>", rownames(R)[i], "</b><br>vs<br><b>", colnames(R)[j], "</b><br><br>",
          "Correlación (r): <b>", round(R[i, j], 3), "</b>"
        )
      }
    }
    
    plot_ly(x = colnames(R), y = rownames(R), z = R, type = "heatmap", zmin = -1, zmax = 1,
            colorscale = list(c(0, "#e74c3c"), c(0.5, "#ffffff"), c(1, "#3498db")), 
            hoverinfo = "text", text = hover_text, showscale = TRUE) %>% 
      layout(title = list(text = paste("Matriz de Correlación (Filtro |r| >", umbral, ")"), font = list(size = 16)),
             xaxis = list(tickangle = -45, title = "", tickfont = list(size = 10)),
             yaxis = list(title = "", tickfont = list(size = 10)), margin = list(b = 150, l = 150, t = 50, r = 50)) %>% 
      config(displayModeBar = FALSE)
  })
  
  # ====================================================================
  # MAPA TERRITORIAL AVANZADO (CON INTEGRACIÓN CAPA 0)
  # ====================================================================
  map_data_reac <- reactive({
    df <- viz_data()
    req("Plan" %in% names(df))
    
    df_info <- info_gral_data()
    dict_info <- info_gral_dict()
    var_selected <- input$map_var
    if(is.null(var_selected)) var_selected <- "medidas"
    
    normalizar <- function(x) { tolower(chartr("áéíóúüñÁÉÍÓÚÜÑ", "aeiouunAEIOUUN", trimws(x))) }
    
    df_counts <- df %>% count(Plan, name = "cantidad_medidas") %>% mutate(Plan_norm = normalizar(Plan))
    df_info_norm <- df_info %>% mutate(Plan_norm = normalizar(Plan))
    
    base_calc <- zonas_rm %>%
      mutate(nombre_norm = normalizar(nombre_comuna)) %>%
      left_join(df_counts, by = c("nombre_norm" = "Plan_norm")) %>%
      left_join(df_info_norm, by = c("nombre_norm" = "Plan_norm")) %>%
      mutate(cantidad_medidas = ifelse(is.na(cantidad_medidas), 0, as.numeric(cantidad_medidas)))
    
    if (var_selected == "medidas") {
      if (is.null(input$map_grouping) || input$map_grouping == "zona") {
        zonas_calc <- base_calc %>%
          group_by(zona) %>%
          summarise(
            total_comunas = n(),
            comunas_reportan = sum(cantidad_medidas > 0, na.rm = TRUE),
            total_medidas = sum(cantidad_medidas, na.rm = TRUE),
            valor_mapa = ifelse(comunas_reportan > 0, total_medidas / comunas_reportan, 0),
            .groups = "drop"
          )
        return(list(tipo = "zona", data = zonas_calc))
      } else {
        comunas_calc <- base_calc %>%
          mutate(valor_mapa = cantidad_medidas) %>%
          select(codigo_comuna, nombre_comuna, valor_mapa)
        return(list(tipo = "comuna", data = comunas_calc))
      }
    } else {
      t <- dict_info$Tipo[dict_info$Columna == var_selected]
      t <- if(length(t) > 0) t[1] else "Texto"
      
      base_df <- base_calc %>%
        select(codigo_comuna, nombre_comuna, zona, cantidad_medidas, val_raw = !!sym(var_selected)) %>%
        mutate(val_raw = as.character(val_raw))
      
      if(t %in% c("Número", "Dinero")) {
        base_df <- base_df %>%
          mutate(
            val_limpio = trimws(val_raw),
            val_str_calc = gsub("[\\$\\s]", "", val_limpio),
            val_str_calc = gsub("\\.", "", val_str_calc),
            val_str_calc = gsub(",", ".", val_str_calc),
            val_num_estricto = suppressWarnings(as.numeric(val_str_calc)),
            
            is_text = is.na(val_num_estricto) & !is.na(val_limpio) & val_limpio != "",
            val_cat = ifelse(is_text & !tolower(val_limpio) %in% c("na", "n/a", "nc"), val_limpio, NA_character_),
            val_num = ifelse(is_text, NA_real_, val_num_estricto)
          )
      } else if (t == "Lista") {
        base_df <- base_df %>% mutate(
          val_limpio = trimws(val_raw),
          val_num = ifelse(is.na(val_limpio) | val_limpio == "" | tolower(val_limpio) %in% c("na", "n/a", "s/i", "n/d"), 
                           NA_real_, 
                           stringr::str_count(val_limpio, ";") + 1),
          val_cat = NA_character_
        )
      } else {
        base_df <- base_df %>% mutate(
          val_cat = case_when(
            is.na(val_raw) | trimws(val_raw) == "" | tolower(trimws(val_raw)) %in% c("na", "n/a", "s/i", "n/d") ~ "NA (Sin Info)",
            tolower(trimws(val_raw)) %in% c("no", "falso", "false", "0") ~ "Falso",
            TRUE ~ "Verdadero"
          ),
          val_num = NA_real_
        )
      }
      
      if (input$map_grouping == "zona") {
        base_df <- base_df %>% group_by(zona) %>%
          summarise(
            cantidad_medidas = sum(cantidad_medidas, na.rm=TRUE),
            val_num = if(all(is.na(val_num))) NA_real_ else sum(val_num, na.rm=TRUE),
            val_cat = if(all(is.na(val_cat))) NA_character_ else paste(unique(na.omit(val_cat)), collapse=" + "),
            .groups = "drop"
          ) %>%
          mutate(codigo_comuna = zona, nombre_comuna = paste("Zona", zona)) 
      }
      
      return(list(tipo = if(input$map_grouping == "zona") "zona_capa0" else "comuna_capa0", data = base_df, var_type = t, var_name = var_selected))
    }
  })
  
  output$map_plot <- renderLeaflet({
    map_res <- map_data_reac()
    req(map_res)
    datos_mapa <- map_res$data
    
    mapa_rm <- mapa_comunas %>% filter(codigo_region == "13") %>%
      left_join(zonas_rm, by = "codigo_comuna") %>% st_as_sf() %>%
      st_transform(crs = 4326) %>% filter(!is.na(zona)) 
    
    etiquetas_visibles <- if (!is.null(input$map_always_labels) && input$map_always_labels) TRUE else FALSE
    
    trans_val <- function(x) {
      if(is.null(input$map_scale) || input$map_scale == "lin") return(x)
      sign_x <- sign(x)
      log10(abs(x) + 1) * sign_x
    }
    inverse_trans <- function(x) {
      if(is.null(input$map_scale) || input$map_scale == "lin") return(x)
      sign_x <- sign(x)
      (10^abs(x) - 1) * sign_x
    }
    
    formato_leyenda_limpio <- function(type, cuts) {
      vals <- inverse_trans(cuts)
      format(vals, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
    }
    
    if (map_res$tipo %in% c("zona", "comuna")) {
      if (map_res$tipo == "zona") {
        mapa_zonas <- mapa_rm %>% group_by(zona) %>% summarise(geometry = st_union(geometry), .groups = "drop")
        mapa_zonas_datos <- mapa_zonas %>% left_join(datos_mapa, by = "zona") %>%
          mutate(
            opacidad = ifelse(is.na(valor_mapa) | valor_mapa == 0, 0.15, 0.85),
            etiqueta = paste0("<div style='font-size: 14px;'><b>Zona: ", zona, "</b><br><hr style='margin: 4px 0;'>",
                              "Comunas Activadas: ", comunas_reportan, "<br>Promedio de Medidas: ", round(valor_mapa, 2), "<br>",
                              "Total Comunas en Zona: ", total_comunas, "</div>")
          )
        leyenda_titulo <- "Promedio de<br>Medidas"
      } else {
        mapa_zonas_datos <- mapa_rm %>% select(codigo_comuna, geometry) %>% left_join(datos_mapa, by = "codigo_comuna") %>%
          mutate(
            opacidad = ifelse(is.na(valor_mapa) | valor_mapa == 0, 0.15, 0.85),
            etiqueta = paste0("<div style='font-size: 14px;'><b>Comuna: ", nombre_comuna, "</b><br><hr style='margin: 4px 0;'>",
                              "Total de Medidas: ", valor_mapa, "</div>")
          )
        leyenda_titulo <- "Total de<br>Medidas"
      }
      
      vals_valid <- na.omit(mapa_zonas_datos$valor_mapa[mapa_zonas_datos$valor_mapa > 0])
      
      if(length(vals_valid) > 0) {
        rng <- range(trans_val(vals_valid), na.rm = TRUE)
        if(is.infinite(rng[1])) rng <- c(0, 1)
        if(rng[1] == rng[2]) rng[2] <- rng[1] + 1
        paleta <- colorNumeric(palette = "Greens", domain = rng, na.color = "#e8e8e8")
      } else {
        paleta <- function(x) "#e8e8e8"
      }
      
      mapa_zonas_datos <- mapa_zonas_datos %>%
        mutate(
          color_fill = case_when(
            is.na(valor_mapa) | valor_mapa == 0 ~ "#ffffff",
            TRUE ~ unname(paleta(trans_val(valor_mapa)))
          )
        )
      
      l <- leaflet(mapa_zonas_datos) %>%
        addPolygons(
          fillColor = ~color_fill, weight = 1.5, color = "#444444", fillOpacity = ~opacidad,
          highlightOptions = highlightOptions(weight = 3, color = "#000000", bringToFront = TRUE),
          label = ~lapply(etiqueta, HTML), 
          labelOptions = labelOptions(noHide = etiquetas_visibles, style = list("font-weight" = "normal", padding = "8px", "box-shadow" = "3px 3px rgba(0,0,0,0.25)"), textsize = "13px", direction = "auto")
        )
      
      if(length(vals_valid) > 0) {
        l <- l %>% addLegend(pal = paleta, values = trans_val(vals_valid), title = leyenda_titulo, 
                             labFormat = formato_leyenda_limpio, position = "bottomright")
      }
      l
      
    } else {
      t <- map_res$var_type
      if (map_res$tipo == "zona_capa0") {
        mapa_zonas <- mapa_rm %>% group_by(zona) %>% summarise(geometry = st_union(geometry), .groups = "drop")
        mapa_zonas_datos <- mapa_zonas %>% left_join(datos_mapa, by = c("zona" = "codigo_comuna")) 
      } else {
        mapa_zonas_datos <- mapa_rm %>% select(codigo_comuna, geometry) %>% left_join(datos_mapa, by = "codigo_comuna")
      }
      
      mapa_zonas_datos <- mapa_zonas_datos %>% mutate(
        opacidad = case_when(
          is.na(val_num) & is.na(val_cat) ~ 0.15,
          !is.na(val_num) & val_num == 0 ~ 0.15,
          t %in% c("Número", "Dinero") & !is.na(val_cat) ~ 0.15, 
          TRUE ~ 0.85
        )
      )
      
      colores_base <- c("#e67e22", "#e74c3c", "#2ecc71", "#f1c40f", "#95a5a6", "#d35400", "#16a085")
      
      if(t %in% c("Número", "Dinero", "Lista")) {
        nums_valid <- na.omit(mapa_zonas_datos$val_num[mapa_zonas_datos$val_num > 0])
        
        if(length(nums_valid) > 0) {
          rng <- range(trans_val(nums_valid), na.rm = TRUE)
          if(is.infinite(rng[1])) rng <- c(0, 1)
          if(rng[1] == rng[2]) rng[2] <- rng[1] + 1
          pal_num <- colorNumeric(palette = "Blues", domain = rng)
        } else {
          pal_num <- function(x) "#e8e8e8"
        }
        
        cats <- unique(unlist(strsplit(na.omit(mapa_zonas_datos$val_cat), " \\+ ")))
        cat_to_col <- setNames(rep_len(colores_base, length(cats)), cats)
        
        mapa_zonas_datos <- mapa_zonas_datos %>%
          mutate(
            color_fill = case_when(
              !is.na(val_num) & val_num == 0 ~ "#ffffff",
              !is.na(val_num) ~ unname(pal_num(trans_val(val_num))),
              !is.na(val_cat) & val_cat %in% names(cat_to_col) ~ unname(cat_to_col[val_cat]),
              TRUE ~ "#ffffff"
            ),
            val_str = case_when(
              !is.na(val_num) & !is.na(val_cat) & t == "Dinero" ~ paste0("$", format(val_num, big.mark = ".", decimal.mark=","), " [ + ", val_cat, " ]"),
              !is.na(val_num) & !is.na(val_cat) & t == "Lista" ~ paste0(val_num, " elemento(s) [ + ", val_cat, " ]"),
              !is.na(val_num) & !is.na(val_cat) ~ paste0(format(val_num, big.mark = ".", decimal.mark=","), " [ + ", val_cat, " ]"),
              !is.na(val_num) & t == "Dinero" ~ paste0("$", format(val_num, big.mark = ".", decimal.mark=",")),
              !is.na(val_num) & t == "Lista" ~ paste0(val_num, " elemento(s)"),
              !is.na(val_num) ~ format(val_num, big.mark = ".", decimal.mark=","),
              !is.na(val_cat) ~ val_cat,
              TRUE ~ "Sin Registro"
            ),
            etiqueta = paste0("<div style='font-size: 14px;'><b>", nombre_comuna, "</b><br><hr style='margin: 4px 0;'>",
                              "Variable: ", map_res$var_name, "<br>",
                              "Valor: <b>", val_str, "</b><br><br>",
                              "<i>Total Medidas PACCC: ", cantidad_medidas, "</i></div>")
          )
      } else {
        pal_cat <- colorFactor(c("#2ecc71", "#e74c3c", "#bdc3c7"), domain = c("Verdadero", "Falso", "NA (Sin Info)"))
        mapa_zonas_datos <- mapa_zonas_datos %>%
          mutate(
            color_fill = unname(pal_cat(val_cat)),
            etiqueta = paste0("<div style='font-size: 14px;'><b>", nombre_comuna, "</b><br><hr style='margin: 4px 0;'>",
                              "Estado: <b>", val_cat, "</b><br>",
                              "<i>", ifelse(val_cat == 'Verdadero' | val_cat == 'Falso', val_raw, ''), "</i></div>")
          )
      }
      
      l <- leaflet(mapa_zonas_datos) %>%
        addPolygons(
          fillColor = ~color_fill, weight = 1.5, color = "#444444", fillOpacity = ~opacidad,
          highlightOptions = highlightOptions(weight = 3, color = "#000000", bringToFront = TRUE),
          label = ~lapply(etiqueta, HTML), 
          labelOptions = labelOptions(noHide = etiquetas_visibles, style = list("font-weight" = "normal", padding = "8px", "box-shadow" = "3px 3px rgba(0,0,0,0.25)"), textsize = "13px", direction = "auto")
        )
      
      if(t %in% c("Número", "Dinero", "Lista")) {
        if(length(nums_valid) > 0) l <- l %>% addLegend(pal = pal_num, values = trans_val(nums_valid), title = map_res$var_name, labFormat = formato_leyenda_limpio, position = "bottomright")
        if(length(cats) > 0) l <- l %>% addLegend(colors = unname(cat_to_col[cats]), labels = cats, title = "Otros Valores", position = "bottomleft")
      } else {
        l <- l %>% addLegend(colors = c("#2ecc71", "#e74c3c", "#bdc3c7"), labels = c("Verdadero (Con Dato)", "Falso (No / 0)", "Sin Información"), title = "Estado", position = "bottomright")
      }
      l
    }
  })
  
  # ====================================================================
  # EXPORTACIÓN EXCEL Y PNG
  # ====================================================================
  output$dl_data_excel <- downloadHandler(
    filename = function() { paste0("Exportacion_Datos_", input$viz_mode, "_", Sys.Date(), ".xlsx") },
    content = function(file) {
      mode <- input$viz_mode; nlp_type <- input$nlp_type
      
      dataset_to_export <- switch(mode,
                                  "freq" = freq_data_reac(),
                                  "sankey" = sankey_data_reac(),
                                  "network" = network_data_reac(),
                                  "map" = map_data_reac()$data,
                                  "corrmatrix" = {
                                    r_mat <- corr_data_reac()
                                    if(!is.null(r_mat)) { cbind(Variable = rownames(as.data.frame(r_mat)), as.data.frame(r_mat)) } else NULL
                                  },
                                  "nlp" = {
                                    if(nlp_type == "global") wc_global_data_reac() else wc_unique_data_reac()
                                  }
      )
      req(dataset_to_export)
      writexl::write_xlsx(dataset_to_export, path = file)
    }
  )
  
  generar_png_desde_html <- function(widget_obj, file_path) {
    req(widget_obj)
    temp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(widget_obj, temp_html, selfcontained = TRUE)
    webshot2::webshot(temp_html, file = file_path, delay = 1.5, vwidth = 1200, vheight = 800)
  }
  
  output$dl_nlp <- downloadHandler(
    filename = function() { paste0("NubePalabras_", input$nlp_type, ".png") },
    content = function(file) {
      if(input$nlp_type == "global") {
        widget_obj <- wordcloud2(head(wc_global_data_reac()[, c("word", "freq")], 100), size = 0.6) 
      } else {
        widget_obj <- wordcloud2(head(wc_unique_data_reac()[, c("word", "freq")], 80), size = 0.6, color = "random-light", backgroundColor = "#2c3e50")
      }
      generar_png_desde_html(widget_obj, file) 
    }
  )
  
  observe({
    df <- viz_data(); req(input$unique_group_col %in% names(df))
    opciones <- unique(na.omit(df[[input$unique_group_col]]))
    updateSelectInput(session, "unique_group_val", choices = opciones)
  })
  
  output$dl_sankey <- downloadHandler(filename = function() { "sankey_dinamico.png" }, content = function(file) { })
  output$dl_freq <- downloadHandler(filename = function() { "distribucion_frecuencias.png" }, content = function(file) { })
  output$dl_network <- downloadHandler(filename = function() { "red_colaboradores.png" }, content = function(file) { })
  output$dl_corr <- downloadHandler(filename = function() { paste0("correlograma_", Sys.Date(), ".png") }, content = function(file) { })
  output$dl_map <- downloadHandler(filename = function() { paste0("mapa_territorial_", Sys.Date(), ".png") }, content = function(file) { })
}

shinyApp(ui = ui, server = server)