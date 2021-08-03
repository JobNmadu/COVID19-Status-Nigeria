
 library(flexpivot)
 library(magrittr)
 nobel_laureates %>%
     pivot_table("category") %>%
     pivot_format()
 nobel_laureates %>%
     pivot_table("category", "gender") %>%
     pivot_format()
 nobel_laureates %>%
     pivot_table(c("category", "gender"), total = FALSE) %>%
     pivot_format()
 nobel_laureates %>%
     pivot_table(c("category", "gender"), total = TRUE) %>%
     pivot_format()
 nobel_laureates %>%
 pivot_table("category") %>%
 pivot_format(
       background = "#D8DEE9",
       color = "#3B4252",
       labels = pivot_labels(
             n = "Count",
             p = "Percentage",
             rows = "Nobel category"
         )
   )
 pt <- pivot_table(
     data = nobel_laureates,
     "category", "gender",
     total = FALSE,
     stats = c("n", "p")
 )

 # PowerPoint
 export_pptx(pt, "my-presentation.pptx")

 # Word
 export_docx(pt, "my-document.docx")

 # Excel
 export_xlsx(pt, "my-workbook.xlsx")