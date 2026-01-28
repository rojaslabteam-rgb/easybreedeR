# Shared language utilities for all EasybreedeR apps
# All translatable text for all apps is centralized here

# Supported Suite languages (lowercase ISO-like codes)
suite_supported_languages <- c("en", "zh", "pt")

# Null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Normalize arbitrary input to one of our codes (default en)
normalize_lang_code <- function(x) {
  x_raw <- as.character(x %||% "en")
  x <- tolower(trimws(x_raw))
  # Accept common display names as well as codes
  if (x %in% c("english", "eng", "en")) return("en")
  # If contains CJK characters, treat as Chinese
  if (grepl("[\u4e00-\u9fff]", x_raw)) return("zh")
  if (grepl("portugu", x)) return("pt")
  if (x %in% suite_supported_languages) return(x)
  return("en")
}

# Backwards-compatible alias expected by older code
# Some files (e.g. run_easybreedeR_Studio.R) call `language_code()`; keep that name
# as a thin wrapper to our canonical `normalize_lang_code()` so legacy callers keep working.
language_code <- function(x) {
  normalize_lang_code(x)
}

# Resolve language from Shiny session query (?lang=en|zh|pt), fallback if missing
resolve_suite_lang <- function(session, default = "en") {
  tryCatch({
    qs <- shiny::parseQueryString(session$clientData$url_search)
    normalize_lang_code(qs$lang %||% default)
  }, error = function(e) normalize_lang_code(default))
}

# Map suite lang to app-specific expectations
map_suite_lang_for_app <- function(lang_code, app) {
  # Return a normalized, lower-case language code tailored per-app when necessary.
  # Keep return values consistently lower-case: 'en', 'zh', 'pt'.
  code <- normalize_lang_code(lang_code)
  if (identical(app, "easyblup")) {
    # easyblup historically expected uppercase labels; standardize to lower-case here
    # and let easyblup itself uppercase if needed when rendering UI.
    if (code %in% c("en", "zh", "pt")) return(code)
    return("en")
  }
  if (identical(app, "datapreviewer")) {
    if (code %in% c("en", "zh", "pt")) return(code)
    return("en")
  }
  code
}

# ====== Centralized Translation Dictionary ======
# All translatable strings for all apps
TRANSLATIONS <- list(
  # === Suite Home Page ===
  suite_title = list(
    en = "easybreedeR Studio",
    zh = "易育 工作台",
    pt = "easybreedeR Studio"
  ),
  hero_kicker = list(
    en = "Every Giant Leap Starts With One Small Step",
    zh = "千里之行，始于足下",
    pt = "Cada Grande Salto Começa com Um Pequeno Passo"
  ),
  hero_title = list(
    en = "Every great analysis starts with a clean dataset",
    zh = "每一次出色的分析都始于一份干净的数据集",
    pt = "Toda grande análise começa com um conjunto de dados limpo"
  ),
  hero_subhead = list(
    en = "Launch a tool to explore data, check pedigrees, create BLUP cards, or open notebooks.",
    zh = "启动工具来探索数据、检查系谱、创建BLUP参数卡 或打开笔记本。",
    pt = "Inicie uma ferramenta para explorar dados, verificar pedigrees, executar BLUP/REML ou abrir notebooks."
  ),
  
  # === dataprevieweR ===
  datapreviewer_app_title = list(
    en = "datapreviewR: A data review and QC tool",
    zh = "数据预览R：数据审查与质控工具",
    pt = "datapreviewR: Uma ferramenta de revisão e controle de qualidade de dados"
  ),
  datapreviewer_app_name = list(
    en = "dataprevieweR",
    zh = "dataprevieweR",
    pt = "dataprevieweR"
  ),
  pediviewer_app_name = list(
    en = "pedivieweR",
    zh = "pedivieweR",
    pt = "pedivieweR"
  ),
  pediviewer_app_subtitle = list(
    en = "Pedigree Quality Control, Inbreeding Analysis & Interactive Visualization",
    zh = "系谱质量控制、近交分析与交互可视化",
    pt = "Controle de Qualidade de Pedigree, Análise de Endogamia e Visualização Interativa"
  ),
  genoviewer_app_name = list(
    en = "genovieweR",
    zh = "genovieweR",
    pt = "genovieweR"
  ),
  pediviewer_tab_network = list(
    en = "Network Visualization",
    zh = "关系网络可视化",
    pt = "Visualização de Rede"
  ),
  pediviewer_tab_data_preview = list(
    en = "Data Preview",
    zh = "数据预览",
    pt = "Pré-visualização de Dados"
  ),
  pediviewer_tab_qc_report = list(
    en = "QC Report",
    zh = "质控报告",
    pt = "Relatório de QC"
  ),
  pediviewer_data_upload = list(
    en = "Data Upload",
    zh = "数据上传",
    pt = "Upload de Dados"
  ),
  pediviewer_quick_stats = list(
    en = "Quick Stats",
    zh = "快速统计",
    pt = "Estatísticas Rápidas"
  ),
  pediviewer_inbreeding_analysis = list(
    en = "Inbreeding Analysis",
    zh = "近交分析",
    pt = "Análise de Endogamia"
  ),
  pediviewer_choose_pedigree_file = list(
    en = "Choose Pedigree File",
    zh = "选择系谱文件",
    pt = "Escolher Arquivo de Pedigree"
  ),
  pediviewer_separator = list(
    en = "Separator:",
    zh = "分隔符：",
    pt = "Separador:"
  ),
  pediviewer_auto_process = list(
    en = "🚀 Auto-detect, calculate F & visualize",
    zh = "🚀 自动检测、计算 F 并可视化",
    pt = "🚀 Auto-detectar, calcular F e visualizar"
  ),
  pediviewer_auto_process_help = list(
    en = "Automatically detect columns, calculate inbreeding, and generate visualization after upload",
    zh = "上传后自动检测列、计算近交并生成可视化",
    pt = "Detectar colunas automaticamente, calcular endogamia e gerar visualização após upload"
  ),
  pediviewer_column_mapping = list(
    en = "Column Mapping",
    zh = "列映射",
    pt = "Mapeamento de Colunas"
  ),
  pediviewer_process_data = list(
    en = "Process Data",
    zh = "处理数据",
    pt = "Processar Dados"
  ),
  pediviewer_search_individual = list(
    en = "🔍 Search Individual ID:",
    zh = "🔍 搜索个体ID：",
    pt = "🔍 Buscar ID do Indivíduo:"
  ),
  pediviewer_search_placeholder = list(
    en = "Enter individual ID to visualize pedigree",
    zh = "输入个体ID以可视化系谱",
    pt = "Insira o ID do indivíduo para visualizar o pedigree"
  ),
  pediviewer_search_depth = list(
    en = "Search Depth (generations):",
    zh = "搜索深度（世代）：",
    pt = "Profundidade de Busca (gerações):"
  ),
  pediviewer_visualize_btn = list(
    en = "Visualize",
    zh = "可视化",
    pt = "Visualizar"
  ),
  pediviewer_show_highest_f = list(
    en = "Show Highest Inbreeding",
    zh = "显示最高近交",
    pt = "Mostrar Maior Endogamia"
  ),
  pediviewer_refresh = list(
    en = "🔄 Refresh",
    zh = "🔄 刷新",
    pt = "🔄 Atualizar"
  ),
  pediviewer_network_legend = list(
    en = "📊 Network Legend:",
    zh = "📊 网络图例：",
    pt = "📊 Legenda da Rede:"
  ),
  pediviewer_legend_male = list(
    en = "Male (M)",
    zh = "雄性 (M)",
    pt = "Macho (M)"
  ),
  pediviewer_legend_female = list(
    en = "Female (F)",
    zh = "雌性 (F)",
    pt = "Fêmea (F)"
  ),
  pediviewer_legend_unknown = list(
    en = "Unknown",
    zh = "未知",
    pt = "Desconhecido"
  ),
  pediviewer_legend_target = list(
    en = "Target Individual",
    zh = "目标个体",
    pt = "Indivíduo Alvo"
  ),
  pediviewer_legend_hint = list(
    en = "💡 Node size represents inbreeding coefficient (F): Larger nodes = Higher inbreeding",
    zh = "💡 节点大小代表近交系数 (F)：越大表示近交越高",
    pt = "💡 O tamanho do nó representa o coeficiente de endogamia (F): maior nó = maior endogamia"
  ),
  pediviewer_download_relatives = list(
    en = "Download Relatives",
    zh = "下载亲属",
    pt = "Baixar Parentes"
  ),
  pediviewer_top10_inbred = list(
    en = "Top 10 Most Inbred:",
    zh = "近交程度最高的前10个体：",
    pt = "Top 10 Mais Endogâmicos:"
  ),
  pediviewer_top_impact_sires = list(
    en = "Top Impact Sires:",
    zh = "影响最大的公畜（Top）：",
    pt = "Principais Reprodutores (Impacto):"
  ),
  pediviewer_top_sires_view_label = list(
    en = "View:",
    zh = "显示方式：",
    pt = "Visualização:"
  ),
  pediviewer_top_sires_top_n_label = list(
    en = "Top N to display:",
    zh = "显示前 N 个：",
    pt = "Top N a exibir:"
  ),
  pediviewer_top_sires_view_table = list(
    en = "Table",
    zh = "表格",
    pt = "Tabela"
  ),
  pediviewer_top_sires_view_chart = list(
    en = "Bar Chart",
    zh = "柱状图",
    pt = "Gráfico de Barras"
  ),
  pediviewer_download_all_sire_progeny = list(
    en = "Download All Sire Progeny Counts",
    zh = "下载全部公畜后代数量",
    pt = "Baixar Contagem de Descendentes dos Reprodutores"
  ),
  pediviewer_no_sire_progeny_data = list(
    en = "No sire progeny data available",
    zh = "暂无公畜后代统计数据",
    pt = "Sem dados de descendência dos reprodutores"
  ),
  pediviewer_top_sires_plot_title = list(
    en = "Top Impact Sires",
    zh = "影响最大的公畜（Top）",
    pt = "Principais Reprodutores"
  ),
  pediviewer_sire_column = list(
    en = "Sire",
    zh = "公畜",
    pt = "Reprodutor"
  ),
  pediviewer_progeny_count_column = list(
    en = "Progeny Count",
    zh = "后代数量",
    pt = "Contagem de Descendentes"
  ),
  pediviewer_progeny_percent_column = list(
    en = "Progeny %",
    zh = "后代占比",
    pt = "Percentual de Descendentes"
  ),
  pediviewer_sire_progeny_ylabel = list(
    en = "Number of Progeny",
    zh = "后代数量",
    pt = "Número de Descendentes"
  ),
  pediviewer_download_all_f = list(
    en = "Download All F Values",
    zh = "下载全部 F 值",
    pt = "Baixar Todos os Valores de F"
  ),
  pediviewer_selected_animal_export = list(
    en = "Selected Animal Export",
    zh = "选中个体导出",
    pt = "Exportação de Animal Selecionado"
  ),
  pediviewer_download_selected_range = list(
    en = "📥 Download Selected Range",
    zh = "📥 下载选定范围",
    pt = "📥 Baixar Faixa Selecionada"
  ),
  pediviewer_export_scope_help = list(
    en = "Exports: the selected individual and all relatives within the current 'Search Depth (generations)'.",
    zh = "导出：所选个体及当前“搜索深度（世代）”内的所有亲属。",
    pt = "Exporta: o indivíduo selecionado e todos os parentes dentro da 'Profundidade de Busca (gerações)'."
  ),
  pediviewer_smart_visualization = list(
    en = "Smart Visualization",
    zh = "智能可视化",
    pt = "Visualização Inteligente"
  ),
  pediviewer_base_node_size = list(
    en = "Base Node Size:",
    zh = "基础节点大小：",
    pt = "Tamanho Base do Nó:"
  ),
  pediviewer_show_labels = list(
    en = "Show Labels",
    zh = "显示标签",
    pt = "Mostrar Rótulos"
  ),
  rcw_app_name = list(
    en = "R Canvas Workflow",
    zh = "R Canvas Workflow",
    pt = "R Canvas Workflow"
  ),
  rcw_app_subtitle = list(
    en = "R Code Notebook with Visual Pipeline",
    zh = "R 代码笔记本与可视化流程",
    pt = "Notebook de Código R com Pipeline Visual"
  ),
  
  # === RCW app labels ===
  rcw_left_header = list(
    en = "R Code Notebooks",
    zh = "R 代码笔记本",
    pt = "Cadernos de Código R"
  ),
  rcw_tip_drag = list(
    en = "Tip: Drag .R files to the canvas to build a workflow",
    zh = "提示：将 .R 文件拖拽到画布以构建工作流",
    pt = "Dica: Arraste arquivos .R para a tela para montar o fluxo"
  ),
  rcw_new_folder = list(
    en = "New Folder",
    zh = "新建文件夹",
    pt = "Nova Pasta"
  ),
  rcw_new_file = list(
    en = "New .R File",
    zh = "新建 .R 文件",
    pt = "Novo Arquivo .R"
  ),
  rcw_add_folder = list(
    en = "Add Folder",
    zh = "添加文件夹",
    pt = "Adicionar Pasta"
  ),
  rcw_run_pipeline = list(
    en = "Run Pipeline",
    zh = "运行流程",
    pt = "Executar Pipeline"
  ),
  rcw_clear = list(
    en = "Clear",
    zh = "清空",
    pt = "Limpar"
  ),
  rcw_tab_output = list(
    en = "Pipeline Output",
    zh = "流程输出",
    pt = "Saída do Pipeline"
  ),
  rcw_tab_export = list(
    en = "Export Rmd",
    zh = "导出 Rmd",
    pt = "Exportar Rmd"
  ),
  rcw_exec_results = list(
    en = "Execution Results",
    zh = "执行结果",
    pt = "Resultados da Execução"
  ),
  rcw_run_log = list(
    en = "Run Log",
    zh = "运行日志",
    pt = "Registro de Execução"
  ),
  rcw_export_title = list(
    en = "Export to R Markdown",
    zh = "导出为 R Markdown",
    pt = "Exportar para R Markdown"
  ),
  rcw_export_desc = list(
    en = "Export canvas workflow as R Markdown with hierarchical structure based on connections.",
    zh = "将画布工作流导出为 R Markdown，并按连接关系生成层级结构。",
    pt = "Exporte o fluxo da tela como R Markdown com estrutura hierárquica baseada nas conexões."
  ),
  rcw_download_rmd = list(
    en = "Download .Rmd",
    zh = "下载 .Rmd",
    pt = "Baixar .Rmd"
  ),
  rcw_export_svg_title = list(
    en = "Export to SVG",
    zh = "导出为 SVG",
    pt = "Exportar para SVG"
  ),
  rcw_export_svg_desc = list(
    en = "Export canvas workflow as an SVG file (includes all connections and nodes).",
    zh = "将画布工作流导出为 SVG 文件（包含所有连接线和节点）。",
    pt = "Exporte o fluxo da tela como um arquivo SVG (inclui todas as conexões e nós)."
  ),
  rcw_btn_export_svg = list(
    en = "Export SVG",
    zh = "导出 SVG",
    pt = "Exportar SVG"
  ),
  rcw_svg_export_generating = list(
    en = "Generating SVG file...",
    zh = "正在生成SVG文件...",
    pt = "Gerando arquivo SVG..."
  ),
  rcw_svg_export_success = list(
    en = "SVG file exported successfully!",
    zh = "SVG文件已成功导出！",
    pt = "Arquivo SVG exportado com sucesso!"
  ),
  rcw_svg_export_error_msg = list(
    en = "Error exporting SVG",
    zh = "导出SVG时出错",
    pt = "Erro ao exportar SVG"
  ),
  rcw_preview = list(
    en = "Preview",
    zh = "预览",
    pt = "Pré-visualização"
  ),
  
  # NOTE: RCW alias keys are defined after TRANSLATIONS is fully created below.
  datapreviewer_app_subtitle = list(
    en = "Data review and QC tool",
    zh = "数据审查与质控工具",
    pt = "Revisão e controle de qualidade de dados"
  ),
  datapreviewer_file_upload = list(
    en = "Phenotype File",
    zh = "选择表型文件",
    pt = "Escolher Arquivo de Fenótipo"
  ),
  datapreviewer_supported_types = list(
    en = " Supported file types: <strong>.csv</strong>, <strong>.tsv</strong>, <strong>.txt</strong>, <strong>.xlsx</strong>, <strong>.xls</strong>, <strong>.rds</strong><br> <em>Note: First row must contain column headers.</em>",
    zh = " 支持的文件类型：<strong>.csv</strong>、<strong>.tsv</strong>、<strong>.txt</strong>、<strong>.xlsx</strong>、<strong>.xls</strong>、<strong>.rds</strong><br> <em>注意：第一行必须包含列名（header）。</em>",
    pt = " Tipos de arquivo suportados: <strong>.csv</strong>, <strong>.tsv</strong>, <strong>.txt</strong>, <strong>.xlsx</strong>, <strong>.xls</strong>, <strong>.rds</strong><br> <em>Nota: A primeira linha deve conter cabeçalhos de coluna.</em>"
  ),
  datapreviewer_select_columns = list(
    en = "Select Column Names for Visualization (Multi-select supported)",
    zh = "选择用于可视化的列名（支持多选）",
    pt = "Selecionar Nomes de Colunas para Visualização (Seleção múltipla suportada)"
  ),
  datapreviewer_categorical_vars = list(
    en = "Select Categorical Variables (e.g., Variety, Farm, etc.)",
    zh = "选择分类变量（如品种、场等）",
    pt = "Selecionar Variáveis Categóricas (ex: Variedade, Fazenda, etc.)"
  ),
  datapreviewer_plot_type = list(
    en = "Plot Type",
    zh = "图表类型",
    pt = "Tipo de Gráfico"
  ),
  datapreviewer_plot_type_title = list(
    en = "Chart Type & Settings",
    zh = "图表类型与设置",
    pt = "Tipo de Gráfico e Configurações"
  ),
  datapreviewer_histogram = list(
    en = "Histogram",
    zh = "直方图",
    pt = "Histograma"
  ),
  datapreviewer_boxplot = list(
    en = "Boxplot",
    zh = "盒线图",
    pt = "Gráfico de Caixa"
  ),
  datapreviewer_hist_bin = list(
    en = "Histogram Bin Size",
    zh = "直方图 Bin 大小",
    pt = "Tamanho do Bin do Histograma"
  ),
  datapreviewer_color_customization = list(
    en = "Color Customization",
    zh = "颜色自定义",
    pt = "Personalização de Cores"
  ),
  datapreviewer_show_color_options = list(
    en = "Show Color Options",
    zh = "显示颜色选项",
    pt = "Mostrar Opções de Cor"
  ),
  datapreviewer_hide_color_options = list(
    en = "Hide Color Options",
    zh = "隐藏颜色选项",
    pt = "Ocultar Opções de Cor"
  ),
  datapreviewer_pre_filter_color = list(
    en = "Pre-Filter Color",
    zh = "质控前颜色",
    pt = "Cor Pré-Filtro"
  ),
  datapreviewer_post_filter_color = list(
    en = "Post-Filter Color",
    zh = "质控后颜色",
    pt = "Cor Pós-Filtro"
  ),
  datapreviewer_color_input_label = list(
    en = "Color (RGB hex, e.g., #FF0000):",
    zh = "颜色（RGB十六进制，如 #FF0000）：",
    pt = "Cor (hexadecimal RGB, ex: #FF0000):"
  ),
  datapreviewer_color_palette_label = list(
    en = "Choose from color palette:",
    zh = "从颜色调色板选择：",
    pt = "Escolher da paleta de cores:"
  ),
  datapreviewer_reset_colors = list(
    en = "Reset to Default Colors",
    zh = "重置为默认颜色",
    pt = "Restaurar Cores Padrão"
  ),
  datapreviewer_data_upload = list(
    en = "Data Upload",
    zh = "数据上传",
    pt = "Upload de Dados"
  ),
  datapreviewer_column_selection = list(
    en = "Column Selection",
    zh = "列选择",
    pt = "Seleção de Colunas"
  ),
  datapreviewer_apply_download = list(
    en = "Apply & Download",
    zh = "应用与下载",
    pt = "Aplicar & Baixar"
  ),
  datapreviewer_select_columns_first = list(
    en = "Please select columns first.",
    zh = "请先选择列。",
    pt = "Por favor, selecione as colunas primeiro."
  ),
  datapreviewer_qc_filter_options = list(
    en = "QC Filter Options",
    zh = "质控过滤选项",
    pt = "Opções de Filtro de Controle de Qualidade"
  ),
  datapreviewer_qc_mode = list(
    en = "QC Mode",
    zh = "质控模式",
    pt = "Modo de Controle de Qualidade"
  ),
  datapreviewer_uniform_qc = list(
    en = "Same Method for All Traits",
    zh = "所有性状使用相同方法",
    pt = "Mesmo Método para Todas as Características"
  ),
  datapreviewer_individual_qc = list(
    en = "Different Methods per Trait",
    zh = "每个性状使用不同方法",
    pt = "Métodos Diferentes por Característica"
  ),
  datapreviewer_filter_type = list(
    en = "Filter Type",
    zh = "过滤类型",
    pt = "Tipo de Filtro"
  ),
  datapreviewer_individual_qc_title = list(
    en = "Per-Trait QC Settings",
    zh = "按性状的质控设置",
    pt = "Configurações de QC por Característica"
  ),
  datapreviewer_threshold_range = list(
    en = "Threshold Range",
    zh = "阈值范围",
    pt = "Intervalo de Limite"
  ),
  datapreviewer_sd_multiplier = list(
    en = "SD Multiplier",
    zh = "标准差倍数",
    pt = "Multiplicador de DP"
  ),
  datapreviewer_iqr_multiplier = list(
    en = "IQR Multiplier",
    zh = "IQR 倍数",
    pt = "Multiplicador de IQR"
  ),
  datapreviewer_min_threshold = list(
    en = "Min Threshold",
    zh = "最小阈值",
    pt = "Limite Mínimo"
  ),
  datapreviewer_max_threshold = list(
    en = "Max Threshold",
    zh = "最大阈值",
    pt = "Limite Máximo"
  ),
  datapreviewer_trait_label = list(
    en = "Trait",
    zh = "性状",
    pt = "Característica"
  ),
  datapreviewer_apply_filter = list(
    en = "Apply QC Filter",
    zh = "应用质控过滤",
    pt = "Aplicar Filtro de Controle de Qualidade"
  ),
  datapreviewer_download_filtered = list(
    en = "Download Filtered Data",
    zh = "下载过滤后数据",
    pt = "Baixar Dados Filtrados"
  ),
  datapreviewer_download_plot = list(
    en = "Download Plot (PNG)",
    zh = "下载图表 (PNG)",
    pt = "Baixar Gráfico (PNG)"
  ),
  datapreviewer_unsupported_file = list(
    en = "Unsupported file type:",
    zh = "不支持的文件类型：",
    pt = "Tipo de arquivo não suportado:"
  ),
  datapreviewer_file_error = list(
    en = "Error reading file. Please check the file format.",
    zh = "读取文件错误。请检查文件格式。",
    pt = "Erro ao ler arquivo. Por favor, verifique o formato do arquivo."
  ),
  datapreviewer_missing_value_format_label = list(
    en = "Define Missing Values:",
    zh = "定义缺失值：",
    pt = "Definir Valores Ausentes:"
  ),
  datapreviewer_missing_value_format_help = list(
    en = "Select which values should be treated as missing values when reading the data (multiple selection supported)",
    zh = "选择在读取数据时应该被当作缺失值处理的值（支持多选）",
    pt = "Selecione quais valores devem ser tratados como ausentes ao ler os dados (seleção múltipla suportada)"
  ),
  
  # dataprevieweR: tab titles
  datapreviewer_data_preview = list(
    en = "Data Preview",
    zh = "数据预览",
    pt = "Visualização de Dados"
  ),
  datapreviewer_qc_results = list(
    en = "QC Results",
    zh = "质控结果",
    pt = "Resultados de QC"
  ),
  
  # === easyblup ===
  easyblup_app_title = list(
    en = "easyblup - BLUPF90 Parameter Generator",
    zh = "easyblup - BLUPF90参数文件生成器",
    pt = "easyblup - Gerador de Parâmetros BLUPF90"
  ),
  easyblup_app_name = list(
    en = "easyblup",
    zh = "easyblup",
    pt = "easyblup"
  ),
  rnotebook_app_name = list(
    en = "RNotebook",
    zh = "R 笔记本",
    pt = "RNotebook"
  ),
  language_button = list(
    en = "English / Chinese / Portuguese",
    zh = "英文 / 中文 / 葡萄牙语",
    pt = "Inglês / Chinês / Português"
  ),
  easyblup_app_subtitle = list(
    en = "BLUPF90 Parameter Generator",
    zh = "BLUPF90参数文件生成器",
    pt = "Gerador de Parâmetros BLUPF90"
  ),
  easyblup_upload_data = list(
    en = "Data Upload",
    zh = "上传数据文件",
    pt = "Upload de Dados"
  ),
  easyblup_phenotype = list(
    en = "Phenotype File",
    zh = "表型文件",
    pt = "Arquivo de Fenótipo"
  ),
  easyblup_pedigree = list(
    en = "Pedigree File",
    zh = "系谱文件",
    pt = "Arquivo de Pedigree"
  ),
  easyblup_genotype = list(
    en = "Genotype Files",
    zh = "基因型文件",
    pt = "Arquivos de Genótipo"
  ),
  easyblup_genotype_format = list(
    en = "Genotype Format",
    zh = "基因型格式",
    pt = "Formato de Genótipo"
  ),
  easyblup_genotype_format_plink = list(
    en = "PLINK (.ped/.map)",
    zh = "PLINK (.ped/.map)",
    pt = "PLINK (.ped/.map)"
  ),
  easyblup_genotype_format_blupf90 = list(
    en = "BLUPF90 (.txt)",
    zh = "BLUPF90 (.txt)",
    pt = "BLUPF90 (.txt)"
  ),
  easyblup_snp_file = list(
    en = "SNP marker file",
    zh = "SNP标记文件",
    pt = "Arquivo de marcador SNP"
  ),
  easyblup_clear_all = list(
    en = "Clear All",
    zh = "清空所有",
    pt = "Limpar Tudo"
  ),
  easyblup_model_builder = list(
    en = "Model Builder",
    zh = "模型构建",
    pt = "Construtor de Modelo"
  ),
  easyblup_traits = list(
    en = "🧬 Traits (y)",
    zh = "🧬 性状 (y)",
    pt = "🧬 Características (y)"
  ),
  easyblup_fixed_effects = list(
    en = "📊 Fixed Effects (b)",
    zh = "📊 固定效应 (b)",
    pt = "📊 Efeitos Fixos (b)"
  ),
  easyblup_animal_id = list(
    en = "🐄 Animal ID (a)",
    zh = "🐄 动物ID (a)",
    pt = "🐄 ID do Animal (a)"
  ),
  easyblup_random_effects = list(
    en = "🎲 Random Effects (r)",
    zh = "🎲 随机效应 (r)",
    pt = "🎲 Efeitos Aleatórios (r)"
  ),
  easyblup_optional_effects = list(
    en = "➕ Additional Effects (Optional)",
    zh = "➕ 附加效应 (可选)",
    pt = "➕ Efeitos Adicionais (Opcional)"
  ),
  easyblup_parameter_file = list(
    en = "Parameter File Preview & Editor",
    zh = "参数文件预览与编辑",
    pt = "Visualização e Editor de Arquivo de Parâmetros"
  ),
  easyblup_reset_param = list(
    en = "🔄 Reset to Default",
    zh = "🔄 重置为默认",
    pt = "🔄 Restaurar Padrão"
  ),
  easyblup_download_param = list(
    en = "📥 Download Parameter File",
    zh = "📥 下载参数文件",
    pt = "📥 Baixar Arquivo de Parâmetros"
  ),
  
  # === easyblup: Right Panel ===
  easyblup_basic_options = list(
    en = "Basic Options",
    zh = "基础选项",
    pt = "Opções Básicas"
  ),
  easyblup_remove_all_missing = list(
    en = "Remove rows with all missing values",
    zh = "移除全缺失的行",
    pt = "Remover linhas com todos os valores ausentes"
  ),
  easyblup_missing_in_weights = list(
    en = "Allow missing values in weights",
    zh = "允许权重中存在缺失值",
    pt = "Permitir valores ausentes nos pesos"
  ),
  easyblup_no_basic_statistics = list(
    en = "Skip basic statistics",
    zh = "跳过基础统计",
    pt = "Pular estatísticas básicas"
  ),
  easyblup_missing_value_symbol = list(
    en = "Missing value symbol",
    zh = "缺失值符号",
    pt = "Símbolo de valor ausente"
  ),
  
  easyblup_analysis_method_options = list(
    en = "Analysis Method Options",
    zh = "分析方法选项",
    pt = "Opções de Método de Análise"
  ),
  easyblup_method_label = list(
    en = "Method",
    zh = "方法",
    pt = "Método"
  ),
  easyblup_method_blup = list(
    en = "BLUP",
    zh = "BLUP",
    pt = "BLUP"
  ),
  easyblup_method_vce = list(
    en = "VCE",
    zh = "VCE",
    pt = "VCE"
  ),
  easyblup_sol_se = list(
    en = "Solution standard errors",
    zh = "估计解的标准误",
    pt = "Erros-padrão das soluções"
  ),
  easyblup_conv_crit = list(
    en = "Convergence criterion",
    zh = "收敛判据",
    pt = "Critério de convergência"
  ),
  easyblup_em_reml_rounds = list(
    en = "EM-REML rounds (n)",
    zh = "EM-REML 轮数 (n)",
    pt = "Rodadas EM-REML (n)"
  ),
  easyblup_em_reml_pure = list(
    en = "EM-REML pure",
    zh = "EM-REML 纯模式",
    pt = "EM-REML puro"
  ),
  easyblup_em_reml_ai_conv = list(
    en = "EM-REML AI convergence",
    zh = "EM-REML AI 收敛",
    pt = "Convergência EM-REML AI"
  ),
  easyblup_use_yams = list(
    en = "Use YAMS",
    zh = "使用 YAMS",
    pt = "Usar YAMS"
  ),
  easyblup_tuned_g2 = list(
    en = "Tuned G2",
    zh = "Tuned G2",
    pt = "Tuned G2"
  ),
  easyblup_maxrounds = list(
    en = "Max rounds",
    zh = "最大迭代次数",
    pt = "Máximo de iterações"
  ),
  easyblup_solv_method = list(
    en = "Solver method",
    zh = "求解器方法",
    pt = "Método do solver"
  ),
  easyblup_r_factor = list(
    en = "r_factor",
    zh = "r_factor",
    pt = "r_factor"
  ),
  easyblup_blksize_traits = list(
    en = "blksize (traits)",
    zh = "blksize（性状数）",
    pt = "blksize (características)"
  ),
  easyblup_residual_output = list(
    en = "Output residuals",
    zh = "输出残差",
    pt = "Exportar resíduos"
  ),
  easyblup_stdresidual_output = list(
    en = "Standardized residuals",
    zh = "标准化残差",
    pt = "Resíduos padronizados"
  ),
  easyblup_prior_solutions = list(
    en = "Use prior solutions",
    zh = "使用先验解",
    pt = "Usar soluções prévias"
  ),
  easyblup_set_eig = list(
    en = "set_eig",
    zh = "set_eig",
    pt = "set_eig"
  ),
  easyblup_auto_se_covar = list(
    en = "Auto-generate h² & r_g (heritability & correlation)",
    zh = "自动生成 h² 与 r_g（遗传力与相关）",
    pt = "Gerar automaticamente h² e r_g (herdabilidade e correlação)"
  ),
  
  easyblup_solution_output_options = list(
    en = "Solution Output Options",
    zh = "解的输出选项",
    pt = "Opções de Saída das Soluções"
  ),
  easyblup_origID_store_solutions = list(
    en = "Store solutions with original ID",
    zh = "以原始ID存储解",
    pt = "Armazenar soluções com ID original"
  ),
  
  easyblup_accuracy_reliability = list(
    en = "Accuracy & Reliability",
    zh = "准确度与可靠度",
    pt = "Acurácia e Confiabilidade"
  ),
  easyblup_acctype = list(
    en = "Accuracy type",
    zh = "准确度类型",
    pt = "Tipo de acurácia"
  ),
  easyblup_correct_accuracy_inb_direct0 = list(
    en = "Correct accuracy by inbreeding (direct = 0)",
    zh = "按近交校正准确度（direct = 0）",
    pt = "Corrigir acurácia pela endogamia (direct = 0)"
  ),
  
  easyblup_genomic_ssgblup = list(
    en = "Genomic / ssGBLUP",
    zh = "基因组 / ssGBLUP",
    pt = "Genômico / ssGBLUP"
  ),
  easyblup_snp_p_value = list(
    en = "Exact GWAS (snp_p_value)",
    zh = "精确GWAS（snp_p_value）",
    pt = "GWAS exato (snp_p_value)"
  ),
  easyblup_omit_ainv = list(
    en = "GBLUP mode (omit_ainv)",
    zh = "GBLUP 模式（omit_ainv）",
    pt = "Modo GBLUP (omit_ainv)"
  ),
  easyblup_tauomega = list(
    en = "TauOmega (tau omega)",
    zh = "TauOmega（tau omega）",
    pt = "TauOmega (tau ômega)"
  ),
  easyblup_alphabeta = list(
    en = "AlphaBeta (alpha beta)",
    zh = "AlphaBeta（alpha beta）",
    pt = "AlphaBeta (alfa beta)"
  ),
  
  easyblup_het_res_weights = list(
    en = "Heterogeneous Residuals & Weights",
    zh = "异方差残差与权重",
    pt = "Resíduos Heterogêneos e Pesos"
  ),
  easyblup_hetres_pos = list(
    en = "hetres_pos (column/effect for heterogeneous residuals)",
    zh = "hetres_pos（异方差残差的列/效应）",
    pt = "hetres_pos (coluna/efeito para resíduos heterogêneos)"
  ),
  easyblup_hetres_pol_preset_label = list(
    en = "hetres_pol (initial polynomial values)",
    zh = "hetres_pol（多项式初始值）",
    pt = "hetres_pol (valores polinomiais iniciais)"
  ),
  easyblup_hetres_pol_constant = list(
    en = "Constant: 0.1",
    zh = "常数：0.1",
    pt = "Constante: 0.1"
  ),
  easyblup_hetres_pol_linear = list(
    en = "Linear: 0.1 0.01",
    zh = "线性：0.1 0.01",
    pt = "Linear: 0.1 0.01"
  ),
  easyblup_hetres_pol_quadratic = list(
    en = "Quadratic: 0.1 0.01 0.001",
    zh = "二次：0.1 0.01 0.001",
    pt = "Quadrático: 0.1 0.01 0.001"
  )
)
# === RCW alias keys used by app ===
# Create alias keys after TRANSLATIONS is fully constructed to avoid
# referencing TRANSLATIONS during its own initialization.
TRANSLATIONS$rcw_left_notebooks <- TRANSLATIONS$rcw_left_header
TRANSLATIONS$rcw_left_tip_drag <- TRANSLATIONS$rcw_tip_drag
TRANSLATIONS$rcw_btn_new_folder <- TRANSLATIONS$rcw_new_folder
TRANSLATIONS$rcw_btn_new_file <- TRANSLATIONS$rcw_new_file
TRANSLATIONS$rcw_btn_add_folder <- TRANSLATIONS$rcw_add_folder
TRANSLATIONS$rcw_btn_run_pipeline <- TRANSLATIONS$rcw_run_pipeline
TRANSLATIONS$rcw_btn_clear <- TRANSLATIONS$rcw_clear
TRANSLATIONS$rcw_tab_pipeline_output <- TRANSLATIONS$rcw_tab_output
TRANSLATIONS$rcw_tab_export_rmd <- TRANSLATIONS$rcw_tab_export
TRANSLATIONS$rcw_right_exec_results <- TRANSLATIONS$rcw_exec_results
TRANSLATIONS$rcw_right_run_log <- TRANSLATIONS$rcw_run_log
TRANSLATIONS$rcw_right_export_rmd_title <- TRANSLATIONS$rcw_export_title
TRANSLATIONS$rcw_right_export_rmd_desc <- TRANSLATIONS$rcw_export_desc
TRANSLATIONS$rcw_right_export_svg_title <- TRANSLATIONS$rcw_export_svg_title
TRANSLATIONS$rcw_right_export_svg_desc <- TRANSLATIONS$rcw_export_svg_desc
TRANSLATIONS$rcw_right_preview <- TRANSLATIONS$rcw_preview
TRANSLATIONS$rcw_btn_download_rmd <- TRANSLATIONS$rcw_download_rmd
TRANSLATIONS$rcw_btn_export_svg <- TRANSLATIONS$rcw_btn_export_svg
TRANSLATIONS$rcw_svg_export_generating <- TRANSLATIONS$rcw_svg_export_generating
TRANSLATIONS$rcw_svg_export_success <- TRANSLATIONS$rcw_svg_export_success
TRANSLATIONS$rcw_svg_export_error_msg <- TRANSLATIONS$rcw_svg_export_error_msg

# === dataprevieweR alias keys used by app ===
# Map generic keys used in inst/dataprevieweR/app.R to the centralized datapreviewer_* entries
TRANSLATIONS$file_upload <- TRANSLATIONS$datapreviewer_file_upload
TRANSLATIONS$supported_types <- TRANSLATIONS$datapreviewer_supported_types
TRANSLATIONS$select_columns <- TRANSLATIONS$datapreviewer_select_columns
TRANSLATIONS$categorical_vars <- TRANSLATIONS$datapreviewer_categorical_vars
TRANSLATIONS$plot_type <- TRANSLATIONS$datapreviewer_plot_type
TRANSLATIONS$plot_type_title <- TRANSLATIONS$datapreviewer_plot_type_title
TRANSLATIONS$histogram <- TRANSLATIONS$datapreviewer_histogram
TRANSLATIONS$boxplot <- TRANSLATIONS$datapreviewer_boxplot
TRANSLATIONS$hist_bin <- TRANSLATIONS$datapreviewer_hist_bin
TRANSLATIONS$color_customization <- TRANSLATIONS$datapreviewer_color_customization
TRANSLATIONS$show_color_options <- TRANSLATIONS$datapreviewer_show_color_options
TRANSLATIONS$hide_color_options <- TRANSLATIONS$datapreviewer_hide_color_options
TRANSLATIONS$pre_filter_color <- TRANSLATIONS$datapreviewer_pre_filter_color
TRANSLATIONS$post_filter_color <- TRANSLATIONS$datapreviewer_post_filter_color
TRANSLATIONS$color_input_label <- TRANSLATIONS$datapreviewer_color_input_label
TRANSLATIONS$color_palette_label <- TRANSLATIONS$datapreviewer_color_palette_label
TRANSLATIONS$reset_colors <- TRANSLATIONS$datapreviewer_reset_colors
TRANSLATIONS$data_upload <- TRANSLATIONS$datapreviewer_data_upload
TRANSLATIONS$column_selection <- TRANSLATIONS$datapreviewer_column_selection
TRANSLATIONS$apply_download <- TRANSLATIONS$datapreviewer_apply_download
TRANSLATIONS$select_columns_first <- TRANSLATIONS$datapreviewer_select_columns_first
TRANSLATIONS$qc_filter_options <- TRANSLATIONS$datapreviewer_qc_filter_options
TRANSLATIONS$qc_mode <- TRANSLATIONS$datapreviewer_qc_mode
TRANSLATIONS$uniform_qc <- TRANSLATIONS$datapreviewer_uniform_qc
TRANSLATIONS$individual_qc <- TRANSLATIONS$datapreviewer_individual_qc
TRANSLATIONS$filter_type <- TRANSLATIONS$datapreviewer_filter_type
TRANSLATIONS$individual_qc_title <- TRANSLATIONS$datapreviewer_individual_qc_title
TRANSLATIONS$threshold_range <- TRANSLATIONS$datapreviewer_threshold_range
TRANSLATIONS$sd_multiplier <- TRANSLATIONS$datapreviewer_sd_multiplier
TRANSLATIONS$iqr_multiplier <- TRANSLATIONS$datapreviewer_iqr_multiplier
TRANSLATIONS$min_threshold <- TRANSLATIONS$datapreviewer_min_threshold
TRANSLATIONS$max_threshold <- TRANSLATIONS$datapreviewer_max_threshold
TRANSLATIONS$trait_label <- TRANSLATIONS$datapreviewer_trait_label
TRANSLATIONS$apply_filter <- TRANSLATIONS$datapreviewer_apply_filter
TRANSLATIONS$download_filtered <- TRANSLATIONS$datapreviewer_download_filtered
TRANSLATIONS$download_plot <- TRANSLATIONS$datapreviewer_download_plot
TRANSLATIONS$unsupported_file <- TRANSLATIONS$datapreviewer_unsupported_file
TRANSLATIONS$file_error <- TRANSLATIONS$datapreviewer_file_error
TRANSLATIONS$missing_value_format_label <- TRANSLATIONS$datapreviewer_missing_value_format_label
TRANSLATIONS$missing_value_format_help <- TRANSLATIONS$datapreviewer_missing_value_format_help
TRANSLATIONS$data_preview <- TRANSLATIONS$datapreviewer_data_preview
TRANSLATIONS$qc_results <- TRANSLATIONS$datapreviewer_qc_results
TRANSLATIONS$missing_value_modal_title <- TRANSLATIONS$datapreviewer_missing_value_modal_title
TRANSLATIONS$missing_value_modal_text <- TRANSLATIONS$datapreviewer_missing_value_modal_text
TRANSLATIONS$confirm_download_text <- TRANSLATIONS$datapreviewer_confirm_download_text
TRANSLATIONS$cancel_download_text <- TRANSLATIONS$datapreviewer_cancel_download_text

# Additional labels used in dataprevieweR UI not previously defined
TRANSLATIONS$data_summary <- list(
  en = "Data Summary",
  zh = "数据摘要",
  pt = "Resumo dos Dados"
)
TRANSLATIONS$original_dataset <- list(
  en = "Original dataset:",
  zh = "原始数据集：",
  pt = "Conjunto de dados original:"
)
TRANSLATIONS$rows <- list(
  en = "rows",
  zh = "行",
  pt = "linhas"
)
TRANSLATIONS$after_filtering <- list(
  en = "After filtering:",
  zh = "过滤后：",
  pt = "Após filtragem:"
)
TRANSLATIONS$filtered_out <- list(
  en = "Filtered out:",
  zh = "被过滤：",
  pt = "Filtradas:"
)
TRANSLATIONS$no_data_plot <- list(
  en = "No numeric data available for plotting",
  zh = "没有可用于绘图的数值型数据",
  pt = "Não há dados numéricos disponíveis para plotagem"
)

# Modal dialog labels for download workflow (with datapreviewer_ prefix)
TRANSLATIONS$datapreviewer_missing_value_modal_title <- list(
  en = "Download Filtered Data",
  zh = "下载过滤后的数据",
  pt = "Baixar Dados Filtrados"
)
TRANSLATIONS$datapreviewer_missing_value_modal_text <- list(
  en = "Define Missing Values:",
  zh = "定义缺失值：",
  pt = "Definir Valores Ausentes:"
)
TRANSLATIONS$datapreviewer_confirm_download_text <- list(
  en = "Confirm Download",
  zh = "确认下载",
  pt = "Confirmar Download"
)
TRANSLATIONS$datapreviewer_cancel_download_text <- list(
  en = "Cancel",
  zh = "取消",
  pt = "Cancelar"
)

# ====== Unified get_label Function ======
# This function works for all apps and automatically handles language mapping
get_label <- function(key, lang = "en", app = NULL) {
  # Normalize language code
  lang_normalized <- normalize_lang_code(lang)
  
  # Map to app-specific format if needed
  if (!is.null(app)) {
    lang_normalized <- map_suite_lang_for_app(lang_normalized, app)
  }
  
  # NOTE: Do not perform app-specific case conversions here; keep lang_normalized
  # as a lower-case code ('en','zh','pt'). Child apps may perform any
  # app-specific mapping or casing before use (e.g. easyblup may display 'EN'/'ZH').
  
  # Look up translation
  if (!key %in% names(TRANSLATIONS)) {
    return(key) # Return key if translation not found
  }
  
  trans <- TRANSLATIONS[[key]]
  
  # Return appropriate language version
  if (lang_normalized %in% names(trans)) {
    return(trans[[lang_normalized]])
  }
  
  # Fallback to English
  if ("en" %in% names(trans)) {
    return(trans[["en"]])
  }
  
  # Last resort: return first available
  if (length(trans) > 0) {
    return(trans[[1]])
  }
  
  return(key)
}
