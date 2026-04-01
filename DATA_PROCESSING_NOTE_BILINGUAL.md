# Bilingual Note on New Data Processing / 新数据处理说明（中英文）

## 1. Purpose / 目的

### English
This note explains how the new workbook `data/raw/Name your insight (4).xlsx` is processed in the current project. The dataset is used as the shared time-series source for three modules:

- Time Series Explorer
- Time Series Clustering
- Time Series Forecasting

The common analytical backbone is **monthly visitor arrivals by country**, while hotel occupancy, average length of stay, and total room revenue are retained as supporting indicators for interpretation.

### 中文
这份说明用于解释当前项目是如何处理新数据文件 `data/raw/Name your insight (4).xlsx` 的。该数据集现在作为整个项目的共享时间序列数据源，支撑三个模块：

- 时间序列探索（Time Series Explorer）
- 时间序列聚类（Time Series Clustering）
- 时间序列预测（Time Series Forecasting）

项目的统一主线数据是**按国家划分的月度游客到达量**，而酒店入住率、平均停留天数、总房费收入等指标则作为辅助解释变量，用来说明旅游需求恢复是否反映到更广泛的旅游业表现上。

## 2. Raw Data Structure / 原始数据结构

### English
The workbook is read from the sheet `My Series`. It contains:

- metadata rows near the top of the worksheet
- monthly time-series values below the metadata block
- multiple tourism indicators in a wide-column format

The project does not assume a simple rectangular CSV-style table. Instead, it first extracts metadata and then reshapes the time-series values into a tidy format.

### 中文
该工作簿的数据来自 `My Series` 工作表。它的结构包括：

- 工作表顶部的多行元数据
- 元数据下方的月度时间序列数值
- 多个旅游指标以宽表（wide format）形式按列存放

因此，项目并不是把它当作普通的二维表直接读取，而是先提取元数据，再把时间序列数值整理成长表（tidy long format）。

## 3. Metadata Extraction / 元数据提取

### English
The function `read_tourism_metadata()` reads the top metadata block from the workbook and extracts:

- `label`
- `region`
- `frequency`
- `unit`
- `source`
- `series_id`

It then:

- trims whitespace
- removes empty labels
- creates a normalized `series_key` using `make.names()`

This step is important because later plotting, filtering, and model selection all depend on reliable series labels and frequency information.

### 中文
函数 `read_tourism_metadata()` 会先读取工作簿顶部的元数据区域，并提取以下字段：

- `label`：指标名称
- `region`：区域信息
- `frequency`：频率
- `unit`：单位
- `source`：来源
- `series_id`：系列编号

之后会进一步进行：

- 去除多余空格
- 删除空白标签
- 使用 `make.names()` 生成标准化的 `series_key`

这一步很关键，因为后续的筛选、可视化和模型选择都依赖清晰稳定的指标标签和频率信息。

## 4. Converting Wide Data to Long Format / 宽表转长表

### English
The function `load_tourism_long_data()` reads the value section of the sheet, then converts the workbook from a wide format into a tidy long table with the following core fields:

- `date`
- `label`
- `series_key`
- `frequency`
- `unit`
- `source`
- `series_id`
- `value`

Main processing steps:

1. Read the worksheet after the metadata rows.
2. Rename columns as `col_1`, `col_2`, etc.
3. Parse the first column into `date`.
4. Pivot all remaining series columns into one `value` column.
5. Join the value records back to the metadata table.
6. Drop rows with missing `date`, `label`, or `value`.

This creates a standard time-series table that can be reused by all modules.

### 中文
函数 `load_tourism_long_data()` 会读取数值区域，并把原始宽表转换成长表，形成统一的时间序列表，主要字段包括：

- `date`
- `label`
- `series_key`
- `frequency`
- `unit`
- `source`
- `series_id`
- `value`

主要处理步骤如下：

1. 跳过元数据行，读取真正的数值区域。
2. 将列名重命名为 `col_1`、`col_2` 等。
3. 把第一列解析为 `date`。
4. 将其他所有指标列收成长表中的 `value` 列。
5. 把数值记录与前面提取的元数据重新关联。
6. 删除缺失 `date`、`label` 或 `value` 的记录。

这样就得到了一个标准化的时间序列表，供后续所有模块共用。

## 5. Keeping Only Monthly Series / 保留月度序列

### English
For the current analytical direction, the project focuses on **monthly** tourism series. Therefore, `load_tourism_data()` calls `load_tourism_long_data(..., only_monthly = TRUE)` and removes non-monthly data from the main analytical workflow.

This makes the time index consistent for:

- trend comparison
- clustering over aligned months
- rolling train/test forecasting splits

### 中文
根据当前项目方向，主要分析对象是**月度**旅游时间序列。因此 `load_tourism_data()` 会调用 `load_tourism_long_data(..., only_monthly = TRUE)`，把非月度频率的数据排除在主分析流程之外。

这样做的好处是可以保证时间索引一致，便于：

- 比较趋势变化
- 基于共同月份窗口进行聚类
- 进行时序训练集 / 测试集划分和预测

## 6. Building the Monthly Feature Table / 构建月度特征表

### English
The function `build_monthly_feature_table()` creates a curated monthly feature table by selecting a fixed group of important tourism indicators from the long dataset, including:

- total visitor arrivals
- visitor arrivals from selected countries
- ASEAN and North Asia arrivals
- hotel room occupancy rate
- average length of stay
- number of hotels
- total room revenue

It also derives:

- `period`: `pre_covid`, `covid_shock`, `recovery`
- `year`
- `month`
- `china_share`
- `avg_stay_monthly_capped`

This feature table is mainly used to support the app and to provide consistent derived fields for cross-module comparisons.

### 中文
函数 `build_monthly_feature_table()` 会从长表中挑选出一组关键旅游指标，构建一个整理好的月度特征表，主要包括：

- 总游客到达量
- 若干重点国家的游客到达量
- ASEAN 和 North Asia 的游客到达量
- 酒店入住率
- 平均停留天数
- 酒店数量
- 总房费收入

同时还会衍生出以下字段：

- `period`：`pre_covid`、`covid_shock`、`recovery`
- `year`
- `month`
- `china_share`
- `avg_stay_monthly_capped`

这个特征表主要用于支持 Shiny 应用和跨模块的一致性分析。

## 7. Country-Level Visitor Arrivals as the Core Dataset / 以国家游客到达量作为统一主数据

### English
The revised project direction treats **monthly visitor arrivals by country** as the common analytical backbone. The helper functions:

- `is_country_arrival_label()`
- `list_country_arrival_series()`

identify valid country-level arrival series and exclude aggregate regional categories such as:

- ASEAN
- West Asia
- North Asia
- Americas
- Africa

This ensures that the forecasting and clustering modules work with country series rather than mixed region groups or unrelated stay-length categories.

### 中文
根据新的分析方向，项目将**按国家划分的月度游客到达量**作为统一的核心数据。辅助函数：

- `is_country_arrival_label()`
- `list_country_arrival_series()`

会识别哪些序列属于有效的国家级游客到达量，并排除一些汇总性区域类别，例如：

- ASEAN
- West Asia
- North Asia
- Americas
- Africa

这样可以保证 forecasting 和 clustering 模块处理的是国家级时间序列，而不是把区域汇总序列或停留时长类别混在一起。

## 8. Supporting Indicators for Interpretation / 作为解释层的辅助指标

### English
The following indicators are retained as supporting tourism-performance measures:

- `Hotel Room Occupancy Rate`
- `Average Length of Stay`
- `Total Room Revenue`

They are not treated as the primary forecasting target. Instead, they are used to interpret whether the recovery in source-market demand is associated with:

- stronger hotel occupancy
- longer or shorter stays
- higher tourism revenue

### 中文
以下指标在当前项目中被保留为旅游业表现的辅助解释变量：

- `Hotel Room Occupancy Rate`（酒店入住率）
- `Average Length of Stay`（平均停留天数）
- `Total Room Revenue`（总房费收入）

它们不再作为主要的预测目标，而是用于帮助解释：来源市场需求恢复之后，是否进一步反映为：

- 更高的酒店入住率
- 更长或更短的停留时间
- 更高的旅游收入

## 9. Processing for Forecasting / 用于预测模块的数据处理

### English
The forecasting pipeline now follows a Chapter 19 / 20 style workflow:

1. Select one country-level arrivals series.
2. Prepare a clean monthly sequence with `prepare_forecast_series()`.
3. Compare the selected country series with supporting indicators using `prepare_country_context_panel()`.
4. Split the time series into training and testing windows.
5. Compare:
   - Seasonal Naive
   - ETS
   - ARIMA
6. Refit the best model and generate future forecasts.

The project uses `modeltime`, `tidymodels`, `timetk`, `tsibble`, and `feasts` to support this workflow.

### 中文
预测模块的数据处理现在遵循 Chapter 19 / 20 风格的流程：

1. 选择一个国家级游客到达量序列。
2. 使用 `prepare_forecast_series()` 生成干净的月度预测序列。
3. 使用 `prepare_country_context_panel()` 将该国家序列与辅助指标放在一起对照。
4. 按时间顺序划分训练集和测试集。
5. 比较以下模型：
   - Seasonal Naive
   - ETS
   - ARIMA
6. 用最佳模型重新拟合全量数据，并向未来进行预测。

这个流程主要依赖 `modeltime`、`tidymodels`、`timetk`、`tsibble` 和 `feasts` 等 R 包。

## 10. Processing for Clustering / 用于聚类模块的数据处理

### English
The clustering module no longer clusters monthly rows as “market states”. Instead, it now clusters **country trajectories**. The function `prepare_time_series_cluster_data()` does the following:

1. Select multiple country-arrivals series.
2. Restrict them to a shared lookback window.
3. Align them by common monthly dates.
4. Drop incomplete overlapping dates.
5. Convert the aligned data into a country-by-month matrix.
6. Optionally normalize each country trajectory.
7. Compute per-country summary metadata:
   - mean arrivals
   - volatility
   - latest arrivals
   - start and end dates

This transforms the problem into a proper time-series clustering setup.

### 中文
聚类模块不再像旧版那样对每个月份记录做“市场状态聚类”，而是改成对**国家游客到达轨迹**做聚类。函数 `prepare_time_series_cluster_data()` 主要完成以下处理：

1. 选择多个国家游客到达量序列。
2. 将它们限制在共同的回看时间窗口内。
3. 按相同月份对齐各国家序列。
4. 删除不能共同对齐的缺失月份。
5. 将结果转换成“国家 × 月份”的矩阵。
6. 可选地对每个国家的轨迹做标准化。
7. 为每个国家计算摘要指标：
   - 平均到达量
   - 波动性
   - 最新到达量
   - 开始与结束日期

这样才真正形成了时间序列聚类所需要的数据结构。

## 11. Output Objects Used Across the Project / 项目中形成的主要数据对象

### English
After processing, the project mainly works with three shared objects:

- `metadata`: cleaned series-level metadata
- `long_monthly`: tidy monthly long-format series table
- `monthly_features`: curated wide monthly feature table

These objects support the Quarto prototype pages, Shiny modules, and validation scripts.

### 中文
完成处理后，项目主要使用三个共享数据对象：

- `metadata`：清洗后的指标级元数据
- `long_monthly`：整理好的月度长表时间序列
- `monthly_features`：整理后的月度宽表特征数据

这些对象同时服务于 Quarto 原型页面、Shiny 模块和验证脚本。

## 12. Summary / 总结

### English
In summary, the new dataset is not used as a raw spreadsheet dump. It is transformed into a clean monthly time-series framework with:

- extracted metadata
- tidy long-format observations
- a curated monthly feature table
- country-level arrivals as the core forecasting and clustering series
- hotel and stay indicators as supporting context

This processing design keeps the three modules consistent and aligns the project more closely with the instructor's time-series recommendation.

### 中文
总的来说，新数据并不是被直接当作原始 Excel 表格使用，而是经过了以下整理与转换：

- 提取并清洗元数据
- 转换为规范的月度长表时间序列
- 构建整理后的月度特征表
- 以国家级游客到达量作为 forecasting 和 clustering 的统一主线
- 将酒店与停留相关指标保留为辅助解释层

这样的处理方式让三个模块能够共享同一套数据逻辑，也更符合老师建议的时间序列分析方向。
