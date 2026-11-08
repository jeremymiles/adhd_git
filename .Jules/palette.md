## 2024-11-08 - Accessible Alternative Text for Data Visualizations in ggplot2
**Learning:** In projects utilizing data visualization heavily, such as Quarto documents with ggplot2 for meta-analyses, plots often lack essential alternative text (alt text). This renders the main analytical findings inaccessible to users with visual impairments relying on screen readers. While `metafor` plots use base R graphics, ggplot2 visualizations natively support dynamic alt text via the `alt` argument within the `labs()` function (e.g., `labs(alt = "...")`).

**Action:** Ensure all non-decorative `ggplot2` visualizations have meaningful `alt` text set in the `labs()` function.

## 2024-11-08 - Accessible Alternative Text for Data Visualizations in ggplot2
**Learning:** In projects utilizing data visualization heavily, such as Quarto documents with ggplot2 for meta-analyses, plots often lack essential alternative text (alt text). This renders the main analytical findings inaccessible to users with visual impairments relying on screen readers. While `metafor` plots use base R graphics, ggplot2 visualizations natively support dynamic alt text via the `alt` argument within the `labs()` function (e.g., `labs(alt = "...")`).

**Action:** Ensure all non-decorative `ggplot2` visualizations have meaningful `alt` text set in the `labs()` function.
