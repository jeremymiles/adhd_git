## 2026-03-17 - [Use Accessible Colors for Plots]
**Learning:** Hardcoded base colors like 'red' in plots (e.g., metafor forest plots) can be inaccessible or difficult to read for colorblind users. The Okabe-Ito palette provides a robust set of colors that are distinguishable by all types of color vision.
**Action:** Replace basic color assignments like `col = "red"` with an accessible equivalent, such as the Okabe-Ito vermilion (`col = "#D55E00"`), across data visualizations to ensure accessibility.
