## 2024-03-24 - Added alt text to Quarto ggplots
**Learning:** When using Quarto/RMarkdown, `ggplot2` plots need alternative text added directly within the R code using `labs(alt = '...')` to ensure the generated HTML images are accessible to screen readers.
**Action:** Always include `labs(alt = '...')` in `ggplot2::ggplot()` calls when authoring accessible Quarto documents.
