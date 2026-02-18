# C++ 源文件管理说明

## ✅ 简化后的架构（2026年2月更新）

### 📁 唯一源文件位置：`src/`
**所有 C++ 源文件只存在于包根目录的 `src/` 文件夹中：**
```
src/
├── genotype_qc.cpp          # 基因型质控函数
├── pedigree_qc.cpp          # 系谱质控函数
├── plink_blup_convert.cpp   # PLINK/BLUPF90 转换函数
└── RcppExports.cpp          # 自动生成的导出接口
```

### ❌ inst/ 下不再需要 .cpp 文件
**删除原因：**
- 所有 Shiny 应用通过 `asNamespace("easybreedeR")` 从已安装的包加载函数
- 没有任何代码使用 `sourceCpp()` 直接编译 inst/ 下的 .cpp 文件
- inst/ 下的 .cpp 副本是多余的历史遗留

### 🔄 正确的调用链
```
src/*.cpp
   ↓
R CMD INSTALL  (编译 C++ 代码)
   ↓
包命名空间 (所有导出函数可用)
   ↓
asNamespace("easybreedeR")
   ↓
Shiny 应用调用函数
```

**关键代码示例（来自 inst/genovieweR/app.R）：**
```r
# 从包命名空间加载函数
ns <- asNamespace("easybreedeR")
if (exists("gvr_marker_call_rate", mode = "function", envir = ns)) {
  gvr_marker_call_rate <- get("gvr_marker_call_rate", envir = ns)
}
```

---

## 🔧 开发工作流程

### 1. 修改 C++ 源文件
**直接在 `src/` 目录中编辑：**
```bash
# 编辑源文件
vim src/genotype_qc.cpp
```

### 2. 更新函数导出（如果需要）
**如果添加或修改了 `// [[Rcpp::export]]` 函数：**
```bash
# 重新生成 RcppExports.R 和 RcppExports.cpp
R -e "Rcpp::compileAttributes('.')"
```

### 3. 重新编译安装包
```bash
# 从源码安装包
R CMD INSTALL --preclean .

# 或者在 R 中
devtools::install()
```

### 4. 测试 Shiny 应用
```r
# 测试各个应用
easybreedeR::run_genovieweR()
easybreedeR::run_pedivieweR()
easybreedeR::run_easyblup()
```

---

## 📊 当前文件清单

### C++ 源文件
| 文件 | 行数 | 大小 | 导出函数数 |
|------|------|------|---------|
| `src/genotype_qc.cpp` | 457 | 16KB | 13 |
| `src/pedigree_qc.cpp` | 1636 | 52KB | 10 |
| `src/plink_blup_convert.cpp` | 587 | 20KB | 6 |
| `src/RcppExports.cpp` | 329 | ~12KB | (自动生成) |
| **总计** | **2680** | **88KB** | **29** |

### Shiny 应用文件结构
```
inst/
├── genovieweR/
│   └── app.R              # 从 asNamespace("easybreedeR") 加载
├── pedivieweR/
│   └── app.R              # 从 asNamespace("easybreedeR") 加载
├── easyblup/
│   └── app.R              # 从 asNamespace("easybreedeR") 加载
└── easybreedeR_Studio/
    └── app.R              # 整合所有功能
```

---

## ⚠️ 重要注意事项

### ✅ 正确做法
- ✅ 只在 `src/` 目录中管理 C++ 源文件
- ✅ 修改后运行 `Rcpp::compileAttributes('.')`（如果改了导出）
- ✅ 重新安装包：`R CMD INSTALL .`
- ✅ Shiny 应用会自动使用新编译的函数

### ❌ 错误做法
- ❌ 不要在 inst/ 下创建 .cpp 文件副本
- ❌ 不要在 Shiny 应用中使用 `sourceCpp()`
- ❌ 不要试图手动同步文件（inst/ 不需要 .cpp 文件）
- ❌ 不要直接修改 `RcppExports.cpp`（这是自动生成的）

---

## 🎯 架构优势

### 简化前（旧架构）
- ❌ src/ 和 inst/ 各有一份 .cpp 文件
- ❌ 需要手动同步（容易出错）
- ❌ 文件冗余，维护困难
- ❌ 可能出现版本不一致

### 简化后（新架构）
- ✅ 只有一份 C++ 源文件（src/）
- ✅ 无需同步，自动保持一致
- ✅ 清晰的调用链
- ✅ 符合 R 包开发规范

---

## 📚 相关文档

### R 包开发规范
- **src/ 目录**：存放需要编译的源代码（C, C++, Fortran等）
- **inst/ 目录**：存放安装时需要复制的文件（不包括源代码）
- **编译流程**：`R CMD INSTALL` 会自动编译 src/ 下的代码

### Rcpp 函数导出
```cpp
// [[Rcpp::export]]
NumericVector my_function(NumericVector x) {
  // ...
}
```
- 使用 `// [[Rcpp::export]]` 标记导出函数
- 运行 `Rcpp::compileAttributes()` 生成导出代码
- 函数会在包命名空间中可用

### Shiny 应用集成
```r
# 在 Shiny app 中使用包函数
ns <- asNamespace("easybreedeR")
if (exists("my_function", mode = "function", envir = ns)) {
  my_function <- get("my_function", envir = ns)
  result <- my_function(data)
}
```

---

## 🔍 常见问题

**Q: Shiny 应用如何知道使用哪个版本的 C++ 函数？**  
A: 它们使用已安装包中编译好的版本。每次重新安装包，所有应用自动使用新版本。

**Q: 如果我在 inst/ 下创建了 .cpp 文件会怎样？**  
A: 文件会被安装（复制）但不会被编译或使用，只会占用磁盘空间。

**Q: 我需要重启 R 才能使用新编译的函数吗？**  
A: 是的。重新安装包后，需要重启 R 会话或使用 `detach()` 和 `library()` 重新加载。

**Q: 如何验证 C++ 函数是否正确导出？**  
A: 安装包后运行：
```r
ls("package:easybreedeR")  # 查看所有导出函数
exists("gvr_marker_call_rate")  # 检查特定函数
```

---

## 🎉 总结

**新架构核心原则：**
1. **单一源代码**：只在 `src/` 管理 C++ 文件
2. **包集成**：所有应用通过包命名空间调用函数
3. **自动同步**：重新安装包即可更新所有应用
4. **符合规范**：遵循 R 包开发最佳实践

**工作流程简化为：**
```
编辑 src/*.cpp → Rcpp::compileAttributes() → R CMD INSTALL → 完成
```

🚀 **开发效率提升，维护成本降低！**


### 3. 重新生成 RcppExports
如果添加或修改了 `// [[Rcpp::export]]` 函数：
```r
library(Rcpp)
Rcpp::compileAttributes(".")
```

这会自动更新：
- `src/RcppExports.cpp`
- `R/RcppExports.R`

### 4. 重新build包
```bash
R CMD INSTALL .
# 或在 RStudio 中: Build > Install and Restart
```

## 📊 当前文件信息

### genotype_qc.cpp
- **行数**: ~630 行
- **功能**: 
  - 基因型质控统计（Call Rate, MAF, Het, HWE）
  - 样本亲缘关系估计（Method-of-Moments）
  - PCA 分析（使用 LAPACK）
- **最后更新**: 2026-02-17
- **修复**: HWE 卡方检验，MoM 亲缘关系，进度反馈

### pedigree_qc.cpp
- **行数**: 1636 行
- **功能**: 系谱数据质控和完整性检查
- **最后更新**: [查看 Git 历史]

### plink_blup_convert.cpp
- **行数**: 588 行
- **功能**: 
  - PLINK 等位基因编码转换
  - BLUPF90 格式转换
  - 基因型数据转换工具
- **导出函数**: 6 个
- **最后更新**: [查看 Git 历史]

## ⚠️ 重要提示

1. **不要直接修改 inst/ 中的 C++ 文件**
   - inst/ 中的文件会被 src/ 中的文件覆盖
   - 所有修改应在 src/ 中进行

2. **不要手动编辑 RcppExports.cpp**
   - 该文件由 `Rcpp::compileAttributes()` 自动生成
   - 修改会在下次生成时被覆盖

3. **添加新函数时**
   - 在 src/*.cpp 中添加 `// [[Rcpp::export]]`
   - 运行 `Rcpp::compileAttributes(".")`
   - 同步到 inst/
   - 重新安装包

4. **版本控制**
   - 只将 src/ 中的 .cpp 文件提交到 Git
   - inst/ 中的 .cpp 文件可以添加到 .gitignore（可选）
   - 或保持同步并一起提交

## 🧪 测试修改

### 单元测试
```r
# 测试 genotype_qc 函数
library(Rcpp)
sourceCpp("src/genotype_qc.cpp")

# 运行测试脚本
source("inst/genovieweR/test_consistency.R")
```

### Shiny 应用测试
```r
# 测试 genovieweR
shiny::runApp("inst/genovieweR")

# 测试 pedivieweR
shiny::runApp("inst/pedivieweR")

# 测试 easyblup
shiny::runApp("inst/easyblup")
```

## 📝 开发工作流

```bash
# 1. 创建功能分支
git checkout -b feature/new-qc-function

# 2. 修改 src/ 中的源文件
vim src/genotype_qc.cpp

# 3. 重新生成导出
R -e "Rcpp::compileAttributes('.')"

# 4. 同步到 inst/
cp src/genotype_qc.cpp inst/genovieweR/

# 5. 测试
R CMD INSTALL .
R -e "source('inst/genovieweR/test_consistency.R')"

# 6. 提交更改
git add src/genotype_qc.cpp R/RcppExports.R inst/genovieweR/genotype_qc.cpp
git commit -m "Add new QC function"

# 7. 合并到主分支
git checkout main
git merge feature/new-qc-function
```

## 🔧 故障排除

### 编译错误
```bash
# 清理编译文件
rm -f src/*.o src/*.so

# 重新安装
R CMD INSTALL . --preclean
```

### 函数未导出
```r
# 检查是否有 [[Rcpp::export]]
grep -n "Rcpp::export" src/genotype_qc.cpp

# 重新生成导出
Rcpp::compileAttributes(".")
```

### inst/ 和 src/ 不同步
```bash
# 批量同步所有文件
./sync_cpp_files.sh  # 见下方脚本
```

## 📜 自动化脚本

创建 `sync_cpp_files.sh` 用于批量同步：

```bash
#!/bin/bash
# 同步所有 C++ 源文件从 src/ 到 inst/

echo "同步 C++ 源文件..."

cp src/genotype_qc.cpp inst/genovieweR/genotype_qc.cpp
echo "✓ genotype_qc.cpp"

cp src/pedigree_qc.cpp inst/pedivieweR/pedigree_qc.cpp
echo "✓ pedigree_qc.cpp"

cp src/plink_blup_convert.cpp inst/easyblup/plink_blup_convert.cpp
echo "✓ plink_blup_convert.cpp"

echo "同步完成！"
```

使用方法：
```bash
chmod +x sync_cpp_files.sh
./sync_cpp_files.sh
```

## 📚 相关文档

- [Rcpp 文档](https://cran.r-project.org/package=Rcpp)
- [Writing R Extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html)
- [genovieweR 修复报告](inst/genovieweR/RCPP_FIX_REPORT.md)
- [快速参考指南](inst/genovieweR/QUICK_REFERENCE.md)

---

**最后更新**: 2026-02-17  
**维护者**: easybreedeR 开发团队
