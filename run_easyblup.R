#!/usr/bin/env Rscript
# 启动 easyblup Shiny 应用

# 检查并安装必要的包
required_packages <- c("shiny", "bslib")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("正在安装 %s...\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# 确定应用路径
app_file <- "inst/easyblup/app.R"

# 如果当前目录没有，尝试项目根目录
if (!file.exists(app_file)) {
  # 尝试从脚本位置推断
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg)
    script_dir <- dirname(normalizePath(script_path))
    if (file.exists(file.path(script_dir, app_file))) {
      setwd(script_dir)
    }
  }
  
  # 如果还是找不到，使用绝对路径
  if (!file.exists(app_file)) {
    abs_path <- file.path("/Users/huangyitang/Downloads/easybreedeR-main-2", app_file)
    if (file.exists(abs_path)) {
      app_file <- abs_path
    } else {
      stop("无法找到应用文件。请确保在项目根目录运行此脚本，或手动指定路径。")
    }
  }
}

# 运行应用
cat("正在启动 easyblup 应用...\n")
cat("应用文件:", normalizePath(app_file), "\n")
cat("应用将在浏览器中自动打开\n")
cat("如果浏览器没有自动打开，请访问: http://127.0.0.1:3838\n")
cat("按 Ctrl+C 停止应用\n\n")

shiny::runApp(
  appDir = dirname(app_file),
  launch.browser = TRUE,
  port = 3838,
  host = "0.0.0.0"
)
