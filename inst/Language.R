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
  if (identical(app, "dataviewer")) {
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
  # Suite Home: section titles and module/dependency labels
  suite_interactive_modules = list(
    en = "Interactive Modules",
    zh = "交互模块",
    pt = "Módulos Interativos"
  ),
  suite_dataviewer_desc = list(
    en = "Interactive data exploration and visualization",
    zh = "交互式数据探索与可视化",
    pt = "Exploração e visualização interativa de dados"
  ),
  suite_pediviewer_desc = list(
    en = "Pedigree viewer, pedigree quality control, inbreeding analysis, interactive visualization",
    zh = "系谱查看、系谱质控、近交分析与交互可视化",
    pt = "Visualizador de pedigree, controle de qualidade, análise de endogamia e visualização interativa"
  ),
  suite_genoviewer_desc = list(
    en = "Genotype visualization and analysis",
    zh = "基因型可视化与分析",
    pt = "Visualização e análise de genótipos"
  ),
  suite_easyblup_desc = list(
    en = "BLUPF90 parameter cards generator, genotype format conversion",
    zh = "BLUPF90 参数卡生成与基因型格式转换",
    pt = "Gerador de parâmetros BLUPF90 e conversão de formato de genótipos"
  ),
  suite_launch = list(
    en = "Launch",
    zh = "启动",
    pt = "Iniciar"
  ),
  suite_optional_dependencies = list(
    en = "Optional Dependencies",
    zh = "可选依赖",
    pt = "Dependências Opcionais"
  ),
  suite_plinkr_desc = list(
    en = "R wrapper for PLINK/PLINK2 with automatic output capture, genotype reading, and BLUPF90 format conversion",
    zh = "PLINK/PLINK2 的 R 封装，支持自动输出捕获、基因型读取与 BLUPF90 格式转换",
    pt = "Pacote R para PLINK/PLINK2 com captura de saída, leitura de genótipos e conversão para BLUPF90"
  ),
  suite_linkbreedeR_desc = list(
    en = "R package for running genetic analysis tools (BLUPF90+, PLINK, RENUMF90) directly from R",
    zh = "在 R 中直接运行遗传分析工具（BLUPF90+、PLINK、RENUMF90）的 R 包",
    pt = "Pacote R para executar ferramentas de análise genética (BLUPF90+, PLINK, RENUMF90) diretamente no R"
  ),
  suite_view_on_github = list(
    en = "View on GitHub",
    zh = "在 GitHub 上查看",
    pt = "Ver no GitHub"
  ),
  suite_required_dependencies = list(
    en = "Required Dependencies",
    zh = "必需依赖",
    pt = "Dependências Obrigatórias"
  ),
  suite_related_software = list(
    en = "Related Software",
    zh = "相关软件",
    pt = "Software Relacionado"
  ),
  
  # === datavieweR ===
  dataviewer_app_title = list(
    en = "dataviewR: A data review and QC tool",
    zh = "数据预览R：数据审查与质控工具",
    pt = "dataviewR: Uma ferramenta de revisão e controle de qualidade de dados"
  ),
  dataviewer_app_name = list(
    en = "datavieweR",
    zh = "datavieweR",
    pt = "datavieweR"
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
  genoviewer_app_subtitle = list(
    en = "Genotype Viewer and Quality Control",
    zh = "基因型查看与质量控制",
    pt = "Visualizador de Genótipos e Controle de Qualidade"
  ),
  genoviewer_show_hide_controls = list(
    en = "Show/Hide Controls",
    zh = "显示/隐藏控制",
    pt = "Mostrar/Ocultar Controles"
  ),
  genoviewer_show_hide_settings = list(
    en = "Show/Hide Settings",
    zh = "显示/隐藏设置",
    pt = "Mostrar/Ocultar Configurações"
  ),
  genoviewer_data_upload = list(
    en = "Data Upload",
    zh = "数据上传",
    pt = "Upload de Dados"
  ),
  genoviewer_genotype_format = list(
    en = "Genotype Format",
    zh = "基因型格式",
    pt = "Formato de Genótipo"
  ),
  genoviewer_chromosome = list(
    en = "Chromosome",
    zh = "染色体",
    pt = "Cromossomo"
  ),
  genoviewer_chromosome_help = list(
    en = "Maximum chromosome number to include (1 to selected number). Only chromosomes 1 to selected number will be included in analysis",
    zh = "包含的最大染色体编号（1 到所选数字）。仅 1 到所选编号的染色体会参与分析",
    pt = "Número máximo de cromossomos a incluir (1 ao número selecionado). Apenas cromossomos 1 ao selecionado serão incluídos na análise"
  ),
  genoviewer_show_summary_plots = list(
    en = "📊 Show Summary & Plots",
    zh = "📊 显示摘要与图表",
    pt = "📊 Mostrar Resumo e Gráficos"
  ),
  genoviewer_show_summary_help = list(
    en = "Generate basic visualizations from loaded data",
    zh = "根据加载的数据生成基本可视化",
    pt = "Gerar visualizações básicas a partir dos dados carregados"
  ),
  genoviewer_quality_control = list(
    en = "Quality Control",
    zh = "质量控制",
    pt = "Controle de Qualidade"
  ),
  genoviewer_qc_intro = list(
    en = "Set quality control thresholds and filter the data.",
    zh = "设置质控阈值并筛选数据。",
    pt = "Defina os limites de controle de qualidade e filtre os dados."
  ),
  genoviewer_run_qc = list(
    en = "🔍 Run Quality Control",
    zh = "🔍 运行质量控制",
    pt = "🔍 Executar Controle de Qualidade"
  ),
  genoviewer_data_preview = list(
    en = "Data Preview",
    zh = "数据预览",
    pt = "Pré-visualização de Dados"
  ),
  genoviewer_genotype_data_summary = list(
    en = "Genotype Data Summary",
    zh = "基因型数据摘要",
    pt = "Resumo dos Dados de Genótipo"
  ),
  genoviewer_browse = list(
    en = "Browse...",
    zh = "浏览...",
    pt = "Procurar..."
  ),
  genoviewer_no_file_selected = list(
    en = "No file selected",
    zh = "未选择文件",
    pt = "Nenhum arquivo selecionado"
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
  pediviewer_show_hide_data_controls = list(
    en = "Show/Hide Data Controls",
    zh = "显示/隐藏数据控制",
    pt = "Mostrar/Ocultar Controles de Dados"
  ),
  pediviewer_show_hide_analysis = list(
    en = "Show/Hide Analysis",
    zh = "显示/隐藏分析",
    pt = "Mostrar/Ocultar Análise"
  ),
  pediviewer_calculate_f_coefficients = list(
    en = "Calculate F Coefficients",
    zh = "计算 F 系数",
    pt = "Calcular Coeficientes F"
  ),
  pediviewer_auto_calculation_enabled = list(
    en = "✓ Auto-calculation enabled",
    zh = "✓ 已启用自动计算",
    pt = "✓ Cálculo automático ativado"
  ),
  pediviewer_auto_processing_enabled = list(
    en = "🔄 Auto-processing enabled - Data will be processed automatically when validation passes",
    zh = "🔄 已启用自动处理 - 验证通过后将自动处理数据",
    pt = "🔄 Processamento automático ativado - Os dados serão processados automaticamente quando a validação passar"
  ),
  pediviewer_download_pedigree_with_inbreeding = list(
    en = "📥 Download pedigree with inbreeding",
    zh = "📥 下载含近交系数的系谱",
    pt = "📥 Baixar pedigree com endogamia"
  ),
  pediviewer_format_label = list(
    en = "Format:",
    zh = "格式：",
    pt = "Formato:"
  ),
  pediviewer_log_format = list(
    en = "Log",
    zh = "日志",
    pt = "Log"
  ),
  pediviewer_table_format = list(
    en = "Table",
    zh = "表格",
    pt = "Tabela"
  ),
  pediviewer_download_qc_report = list(
    en = "📥 Download QC Report",
    zh = "📥 下载质控报告",
    pt = "📥 Baixar Relatório de QC"
  ),
  pediviewer_download_fixed_pedigree = list(
    en = "📥 Download Fixed Pedigree",
    zh = "📥 下载修复后的系谱",
    pt = "📥 Baixar Pedigree Corrigido"
  ),
  pediviewer_download_structure_report = list(
    en = "📥 Download Structure Report",
    zh = "📥 下载结构报告",
    pt = "📥 Baixar Relatório de Estrutura"
  ),
  pediviewer_pedigree_structure = list(
    en = "Pedigree Structure",
    zh = "系谱结构",
    pt = "Estrutura do Pedigree"
  ),
  pediviewer_inbreeding_trend = list(
    en = "Inbreeding Trend",
    zh = "近交趋势",
    pt = "Tendência de Endogamia"
  ),
  pediviewer_download_all_sires = list(
    en = "Download All Sires",
    zh = "下载全部父系",
    pt = "Baixar Todos os Pais"
  ),
  pediviewer_download_all_dams = list(
    en = "Download All Dams",
    zh = "下载全部母系",
    pt = "Baixar Todas as Mães"
  ),
  pediviewer_download_relatives = list(
    en = "Download Relatives",
    zh = "下载亲属",
    pt = "Baixar Parentes"
  ),
  pediviewer_clear_all = list(
    en = "Clear All",
    zh = "清空全部",
    pt = "Limpar Tudo"
  ),
  pediviewer_start_analysis = list(
    en = "🚀 Start Analysis",
    zh = "🚀 开始分析",
    pt = "🚀 Iniciar Análise"
  ),
  pediviewer_start_analysis_help = list(
    en = "Click to begin processing and analyzing the pedigree data after selecting column mappings.",
    zh = "选择列映射后点击开始处理并分析系谱数据。",
    pt = "Clique para iniciar o processamento e análise dos dados do pedigree após selecionar o mapeamento de colunas."
  ),
  pediviewer_highlight_generations = list(
    en = "🔍 Highlight Generations:",
    zh = "🔍 高亮世代：",
    pt = "🔍 Destacar Gerações:"
  ),
  pediviewer_top10_sire = list(
    en = "Top 10 Most Influential Sires",
    zh = "影响力最大的前10父系",
    pt = "Top 10 Pais Mais Influentes"
  ),
  pediviewer_top10_dam = list(
    en = "Top 10 Most Influential Dams",
    zh = "影响力最大的前10母系",
    pt = "Top 10 Mães Mais Influentes"
  ),
  pediviewer_animal_label = list(
    en = "🐾 Animal:",
    zh = "🐾 个体：",
    pt = "🐾 Animal:"
  ),
  dataviewer_app_subtitle = list(
    en = "Data review and QC tool",
    zh = "数据审查与质控工具",
    pt = "Revisão e controle de qualidade de dados"
  ),
  dataviewer_file_upload = list(
    en = "Phenotype File",
    zh = "选择表型文件",
    pt = "Escolher Arquivo de Fenótipo"
  ),
  dataviewer_supported_types = list(
    en = " Supported file types: <strong>.csv</strong>, <strong>.tsv</strong>, <strong>.txt</strong>, <strong>.xlsx</strong>, <strong>.xls</strong>, <strong>.rds</strong><br> <em>Note: First row must contain column headers.</em>",
    zh = " 支持的文件类型：<strong>.csv</strong>、<strong>.tsv</strong>、<strong>.txt</strong>、<strong>.xlsx</strong>、<strong>.xls</strong>、<strong>.rds</strong><br> <em>注意：第一行必须包含列名（header）。</em>",
    pt = " Tipos de arquivo suportados: <strong>.csv</strong>, <strong>.tsv</strong>, <strong>.txt</strong>, <strong>.xlsx</strong>, <strong>.xls</strong>, <strong>.rds</strong><br> <em>Nota: A primeira linha deve conter cabeçalhos de coluna.</em>"
  ),
  dataviewer_select_columns = list(
    en = "Select Column Names for Visualization (Multi-select supported)",
    zh = "选择用于可视化的列名（支持多选）",
    pt = "Selecionar Nomes de Colunas para Visualização (Seleção múltipla suportada)"
  ),
  dataviewer_categorical_vars = list(
    en = "Select Categorical Variables (e.g., Variety, Farm, etc.)",
    zh = "选择分类变量（如品种、场等）",
    pt = "Selecionar Variáveis Categóricas (ex: Variedade, Fazenda, etc.)"
  ),
  dataviewer_plot_type = list(
    en = "Plot Type",
    zh = "图表类型",
    pt = "Tipo de Gráfico"
  ),
  dataviewer_plot_type_title = list(
    en = "Chart Type & Settings",
    zh = "图表类型与设置",
    pt = "Tipo de Gráfico e Configurações"
  ),
  dataviewer_histogram = list(
    en = "Histogram",
    zh = "直方图",
    pt = "Histograma"
  ),
  dataviewer_boxplot = list(
    en = "Boxplot",
    zh = "盒线图",
    pt = "Gráfico de Caixa"
  ),
  dataviewer_qqplot = list(
    en = "Q-Q Plot",
    zh = "Q-Q 图",
    pt = "Gráfico Q-Q"
  ),
  dataviewer_hist_bin = list(
    en = "Histogram Bin Size",
    zh = "直方图 Bin 大小",
    pt = "Tamanho do Bin do Histograma"
  ),
  dataviewer_color_customization = list(
    en = "Color Customization",
    zh = "颜色自定义",
    pt = "Personalização de Cores"
  ),
  dataviewer_show_color_options = list(
    en = "Show Color Options",
    zh = "显示颜色选项",
    pt = "Mostrar Opções de Cor"
  ),
  dataviewer_hide_color_options = list(
    en = "Hide Color Options",
    zh = "隐藏颜色选项",
    pt = "Ocultar Opções de Cor"
  ),
  dataviewer_pre_filter_color = list(
    en = "Pre-Filter Color",
    zh = "质控前颜色",
    pt = "Cor Pré-Filtro"
  ),
  dataviewer_post_filter_color = list(
    en = "Post-Filter Color",
    zh = "质控后颜色",
    pt = "Cor Pós-Filtro"
  ),
  dataviewer_color_input_label = list(
    en = "Color (RGB hex, e.g., #FF0000):",
    zh = "颜色（RGB十六进制，如 #FF0000）：",
    pt = "Cor (hexadecimal RGB, ex: #FF0000):"
  ),
  dataviewer_color_palette_label = list(
    en = "Choose from color palette:",
    zh = "从颜色调色板选择：",
    pt = "Escolher da paleta de cores:"
  ),
  dataviewer_reset_colors = list(
    en = "Reset to Default Colors",
    zh = "重置为默认颜色",
    pt = "Restaurar Cores Padrão"
  ),
  dataviewer_data_upload = list(
    en = "Data Upload",
    zh = "数据上传",
    pt = "Upload de Dados"
  ),
  dataviewer_column_selection = list(
    en = "Column Selection",
    zh = "列选择",
    pt = "Seleção de Colunas"
  ),
  dataviewer_apply_download = list(
    en = "Apply & Download",
    zh = "应用与下载",
    pt = "Aplicar & Baixar"
  ),
  dataviewer_select_columns_first = list(
    en = "Please select columns first.",
    zh = "请先选择列。",
    pt = "Por favor, selecione as colunas primeiro."
  ),
  dataviewer_qc_filter_options = list(
    en = "QC Filter Options",
    zh = "质控过滤选项",
    pt = "Opções de Filtro de Controle de Qualidade"
  ),
  dataviewer_qc_mode = list(
    en = "QC Mode",
    zh = "质控模式",
    pt = "Modo de Controle de Qualidade"
  ),
  dataviewer_uniform_qc = list(
    en = "Same Method for All Traits",
    zh = "所有性状使用相同方法",
    pt = "Mesmo Método para Todas as Características"
  ),
  dataviewer_individual_qc = list(
    en = "Different Methods per Trait",
    zh = "每个性状使用不同方法",
    pt = "Métodos Diferentes por Característica"
  ),
  dataviewer_filter_type = list(
    en = "Filter Type",
    zh = "过滤类型",
    pt = "Tipo de Filtro"
  ),
  dataviewer_individual_qc_title = list(
    en = "Per-Trait QC Settings",
    zh = "按性状的质控设置",
    pt = "Configurações de QC por Característica"
  ),
  dataviewer_threshold_range = list(
    en = "Threshold Range",
    zh = "阈值范围",
    pt = "Intervalo de Limite"
  ),
  dataviewer_sd_multiplier = list(
    en = "SD Multiplier",
    zh = "标准差倍数",
    pt = "Multiplicador de DP"
  ),
  dataviewer_iqr_multiplier = list(
    en = "IQR Multiplier",
    zh = "IQR 倍数",
    pt = "Multiplicador de IQR"
  ),
  dataviewer_min_threshold = list(
    en = "Min Threshold",
    zh = "最小阈值",
    pt = "Limite Mínimo"
  ),
  dataviewer_max_threshold = list(
    en = "Max Threshold",
    zh = "最大阈值",
    pt = "Limite Máximo"
  ),
  dataviewer_trait_label = list(
    en = "Trait",
    zh = "性状",
    pt = "Característica"
  ),
  dataviewer_apply_filter = list(
    en = "Apply QC Filter",
    zh = "应用质控过滤",
    pt = "Aplicar Filtro de Controle de Qualidade"
  ),
  dataviewer_download_filtered = list(
    en = "Download Filtered Data",
    zh = "下载过滤后数据",
    pt = "Baixar Dados Filtrados"
  ),
  dataviewer_download_plot = list(
    en = "Download Plot (PNG)",
    zh = "下载图表 (PNG)",
    pt = "Baixar Gráfico (PNG)"
  ),
  dataviewer_unsupported_file = list(
    en = "Unsupported file type:",
    zh = "不支持的文件类型：",
    pt = "Tipo de arquivo não suportado:"
  ),
  dataviewer_file_error = list(
    en = "Error reading file. Please check the file format.",
    zh = "读取文件错误。请检查文件格式。",
    pt = "Erro ao ler arquivo. Por favor, verifique o formato do arquivo."
  ),
  dataviewer_missing_value_format_label = list(
    en = "Define Missing Values:",
    zh = "定义缺失值：",
    pt = "Definir Valores Ausentes:"
  ),
  dataviewer_missing_value_format_help = list(
    en = "Select which values should be treated as missing values when reading the data (multiple selection supported)",
    zh = "选择在读取数据时应该被当作缺失值处理的值（支持多选）",
    pt = "Selecione quais valores devem ser tratados como ausentes ao ler os dados (seleção múltipla suportada)"
  ),
  
  # datavieweR: tab titles
  dataviewer_data_preview = list(
    en = "Data Preview",
    zh = "数据预览",
    pt = "Visualização de Dados"
  ),
  dataviewer_qc_results = list(
    en = "QC Results",
    zh = "质控结果",
    pt = "Resultados de QC"
  ),
  dataviewer_normality_test_title = list(
    en = "Normality Test (Pre/Post QC)",
    zh = "正态性检验（质控前/后）",
    pt = "Teste de Normalidade (Pré/Pós QC)"
  ),
  dataviewer_normality_col_column = list(
    en = "Column",
    zh = "列",
    pt = "Coluna"
  ),
  dataviewer_normality_col_sample = list(
    en = "Sample",
    zh = "样本",
    pt = "Amostra"
  ),
  dataviewer_normality_col_n = list(
    en = "N",
    zh = "样本量",
    pt = "N"
  ),
  dataviewer_normality_col_method = list(
    en = "Method",
    zh = "方法",
    pt = "Método"
  ),
  dataviewer_normality_col_statistic = list(
    en = "Statistic",
    zh = "统计量",
    pt = "Estatística"
  ),
  dataviewer_normality_col_p_value = list(
    en = "P-value",
    zh = "P值",
    pt = "Valor-p"
  ),
  dataviewer_normality_col_normal = list(
    en = "Normal",
    zh = "正态",
    pt = "Normal"
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

# === datavieweR alias keys used by app ===
# Map generic keys used in inst/datavieweR/app.R to the centralized dataviewer_* entries
TRANSLATIONS$file_upload <- TRANSLATIONS$dataviewer_file_upload
TRANSLATIONS$supported_types <- TRANSLATIONS$dataviewer_supported_types
TRANSLATIONS$select_columns <- TRANSLATIONS$dataviewer_select_columns
TRANSLATIONS$categorical_vars <- TRANSLATIONS$dataviewer_categorical_vars
TRANSLATIONS$plot_type <- TRANSLATIONS$dataviewer_plot_type
TRANSLATIONS$plot_type_title <- TRANSLATIONS$dataviewer_plot_type_title
TRANSLATIONS$histogram <- TRANSLATIONS$dataviewer_histogram
TRANSLATIONS$boxplot <- TRANSLATIONS$dataviewer_boxplot
TRANSLATIONS$qqplot <- TRANSLATIONS$dataviewer_qqplot
TRANSLATIONS$hist_bin <- TRANSLATIONS$dataviewer_hist_bin
TRANSLATIONS$color_customization <- TRANSLATIONS$dataviewer_color_customization
TRANSLATIONS$show_color_options <- TRANSLATIONS$dataviewer_show_color_options
TRANSLATIONS$hide_color_options <- TRANSLATIONS$dataviewer_hide_color_options
TRANSLATIONS$pre_filter_color <- TRANSLATIONS$dataviewer_pre_filter_color
TRANSLATIONS$post_filter_color <- TRANSLATIONS$dataviewer_post_filter_color
TRANSLATIONS$color_input_label <- TRANSLATIONS$dataviewer_color_input_label
TRANSLATIONS$color_palette_label <- TRANSLATIONS$dataviewer_color_palette_label
TRANSLATIONS$reset_colors <- TRANSLATIONS$dataviewer_reset_colors
TRANSLATIONS$data_upload <- TRANSLATIONS$dataviewer_data_upload
TRANSLATIONS$column_selection <- TRANSLATIONS$dataviewer_column_selection
TRANSLATIONS$apply_download <- TRANSLATIONS$dataviewer_apply_download
TRANSLATIONS$select_columns_first <- TRANSLATIONS$dataviewer_select_columns_first
TRANSLATIONS$qc_filter_options <- TRANSLATIONS$dataviewer_qc_filter_options
TRANSLATIONS$qc_mode <- TRANSLATIONS$dataviewer_qc_mode
TRANSLATIONS$uniform_qc <- TRANSLATIONS$dataviewer_uniform_qc
TRANSLATIONS$individual_qc <- TRANSLATIONS$dataviewer_individual_qc
TRANSLATIONS$filter_type <- TRANSLATIONS$dataviewer_filter_type
TRANSLATIONS$individual_qc_title <- TRANSLATIONS$dataviewer_individual_qc_title
TRANSLATIONS$threshold_range <- TRANSLATIONS$dataviewer_threshold_range
TRANSLATIONS$sd_multiplier <- TRANSLATIONS$dataviewer_sd_multiplier
TRANSLATIONS$iqr_multiplier <- TRANSLATIONS$dataviewer_iqr_multiplier
TRANSLATIONS$min_threshold <- TRANSLATIONS$dataviewer_min_threshold
TRANSLATIONS$max_threshold <- TRANSLATIONS$dataviewer_max_threshold
TRANSLATIONS$trait_label <- TRANSLATIONS$dataviewer_trait_label
TRANSLATIONS$apply_filter <- TRANSLATIONS$dataviewer_apply_filter
TRANSLATIONS$download_filtered <- TRANSLATIONS$dataviewer_download_filtered
TRANSLATIONS$download_plot <- TRANSLATIONS$dataviewer_download_plot
TRANSLATIONS$unsupported_file <- TRANSLATIONS$dataviewer_unsupported_file
TRANSLATIONS$file_error <- TRANSLATIONS$dataviewer_file_error
TRANSLATIONS$missing_value_format_label <- TRANSLATIONS$dataviewer_missing_value_format_label
TRANSLATIONS$missing_value_format_help <- TRANSLATIONS$dataviewer_missing_value_format_help
TRANSLATIONS$data_preview <- TRANSLATIONS$dataviewer_data_preview
TRANSLATIONS$qc_results <- TRANSLATIONS$dataviewer_qc_results
TRANSLATIONS$missing_value_modal_title <- TRANSLATIONS$dataviewer_missing_value_modal_title
TRANSLATIONS$missing_value_modal_text <- TRANSLATIONS$dataviewer_missing_value_modal_text
TRANSLATIONS$confirm_download_text <- TRANSLATIONS$dataviewer_confirm_download_text
TRANSLATIONS$cancel_download_text <- TRANSLATIONS$dataviewer_cancel_download_text

# Additional labels used in datavieweR UI not previously defined
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

# Modal dialog labels for download workflow (with dataviewer_ prefix)
TRANSLATIONS$dataviewer_missing_value_modal_title <- list(
  en = "Download Filtered Data",
  zh = "下载过滤后的数据",
  pt = "Baixar Dados Filtrados"
)
TRANSLATIONS$dataviewer_missing_value_modal_text <- list(
  en = "Define Missing Values:",
  zh = "定义缺失值：",
  pt = "Definir Valores Ausentes:"
)
TRANSLATIONS$dataviewer_confirm_download_text <- list(
  en = "Confirm Download",
  zh = "确认下载",
  pt = "Confirmar Download"
)
TRANSLATIONS$dataviewer_cancel_download_text <- list(
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
