library(openxlsx)
report_objects <- readRDS("C:\\Users\\teres\\Desktop\\Data work\\IPEDS report\\report_objects.rds")

ranked_enrollments <- report_objects$ranked_enrollments
undergrad_demo <- report_objects$undergrad_demographics_table
grad_demo <- report_objects$grad_demographics_table

wb <- createWorkbook()

# Sheet 1: Ranked Enrollment

addWorksheet(wb, "Enrollment Ranking")

writeDataTable(wb, sheet = "Enrollment Ranking", x = ranked_enrollments,
               tableStyle = "TableStyleMedium2")

header_style <- createStyle(textDecoration = "bold", halign = "center")
addStyle(wb, sheet = "Enrollment Ranking", style = header_style,
         rows = 1, cols = 1:ncol(ranked_enrollments), gridExpand = TRUE)

number_style <- createStyle(numFmt = "#,##0")
addStyle(wb, sheet = "Enrollment Ranking", style = number_style,
         rows = 2:(nrow(ranked_enrollments) + 1),
         cols = which(names(ranked_enrollments) == "Total_Enrollment"),
         gridExpand = TRUE)

setColWidths(wb, sheet = "Enrollment Ranking",
             cols = 1:ncol(ranked_enrollments), widths = "auto")


# Sheet 2: Undergraduate and Graduate Demographics

addWorksheet(wb, "Demographic Comparison")

writeDataTable(wb, sheet = "Demographic Comparison", x = undergrad_demo,
               startRow = 1, tableStyle = "TableStyleMedium2")

grad_start_row <- nrow(undergrad_demo) + 4
writeDataTable(wb, sheet = "Demographic Comparison", x = grad_demo,
               startRow = grad_start_row, tableStyle = "TableStyleMedium2")

addStyle(wb, sheet = "Demographic Comparison", style = header_style,
         rows = 1, cols = 1:ncol(undergrad_demo), gridExpand = TRUE)
addStyle(wb, sheet = "Demographic Comparison", style = header_style,
         rows = grad_start_row, cols = 1:ncol(grad_demo), gridExpand = TRUE)

diff_col <- which(names(undergrad_demo) == "Difference")  # same position in both tables

red_style <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
green_style <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")

conditionalFormatting(wb, sheet = "Demographic Comparison",
                       cols = diff_col,
                       rows = 2:(nrow(undergrad_demo) + 1),
                       rule = "<0", style = red_style)
conditionalFormatting(wb, sheet = "Demographic Comparison",
                       cols = diff_col,
                       rows = 2:(nrow(undergrad_demo) + 1),
                       rule = ">=0", style = green_style)

conditionalFormatting(wb, sheet = "Demographic Comparison",
                       cols = diff_col,
                       rows = (grad_start_row + 1):(grad_start_row + nrow(grad_demo)),
                       rule = "<0", style = red_style)
conditionalFormatting(wb, sheet = "Demographic Comparison",
                       cols = diff_col,
                       rows = (grad_start_row + 1):(grad_start_row + nrow(grad_demo)),
                       rule = ">=0", style = green_style)

setColWidths(wb, sheet = "Demographic Comparison",
             cols = 1:ncol(undergrad_demo), widths = "auto")


# Save
saveWorkbook(wb, "IPEDS_tables.xlsx", overwrite = TRUE)

