## Attorney stuff w/ Dave
## MVH

library(dplyr)
library(igraph)
library(readr)
library(stringr)

bor <- rstringrbor <- read_csv("./inputs/Board_of_Review_Appeal_Decision_History_20250817.csv")

ccao <- read_csv("./inputs/Assessor_-_Appeals_20250817.csv")

bor |>
  filter(tax_year == 2022) |>
  group_by(`Attorney ID`) |>
  summarize(n = n()) |>
  arrange(desc(n))

bor |>
  filter(tax_year == 2022) |>
  filter(MajorClass %in% c("Industrial Incentive and Industrial Brownfield", "Commercial/Industrial Incentive",
                           "Commercial Incentive")) |>
  filter(`Attorney ID` == 7668) |>
  group_by(Appellant) |>
  reframe(Appellant, n = n(), first(Attorney_FirmName), diff = sum(Assessor_TotalValue - BOR_TotalValue, na.rm = T)) |>
  arrange(desc(diff)) |>
  distinct()

bor |>
  group_by(ChangeReasonDescription) |>
  summarize(n = n()/100)

bor_2022 <- bor |>
  filter(tax_year == 2022) |>
  rename(attorneyid = `Attorney ID`)

bor_2022 |>
  group_by(Class) |>
  summarize(n = n())

bor |>
  filter(str_detect(Appellant, regex("AMAZON", ignore_case = TRUE))) |>
  group_by(`Attorney ID`) |>
  group_by(tax_year) |>
  reframe(tax_year, Appellant, n = n(), first(Attorney_FirmName), diff = sum(Assessor_TotalValue - BOR_TotalValue, na.rm = T)) |>
              arrange(desc(tax_year)) |>
              distinct()


# 1) Edgelist directly
edges <- bor_2022 %>%
  transmute(
    attorney  = trimws(Attorney_FirmName),
    appellant = trimws(Appellant)
  ) %>%
  filter(!is.na(attorney), attorney != "",
         !is.na(appellant), appellant != "") %>%
  distinct()

# 2) Combine attorneys + appellants into one node set
attorneys  <- unique(edges$attorney)
appellants <- unique(edges$appellant)

all_nodes <- c(attorneys, appellants)

# 3) Map names to indices
edges_idx <- data.frame(
  from = match(edges$attorney, all_nodes),
  to   = match(edges$appellant, all_nodes)
)

# 4) Build bipartite graph from edgelist
g <- graph_from_data_frame(
  d = edges_idx,
  vertices = data.frame(
    name = all_nodes,
    type = c(rep(FALSE, length(attorneys)),  # FALSE = attorneys
             rep(TRUE,  length(appellants))) # TRUE  = appellants
  ),
  directed = FALSE
)

# 5) Plot
plot(
  g,
  layout       = layout_as_bipartite,
  vertex.size  = 2,
  vertex.label = NA,
  vertex.color = ifelse(V(g)$type, "darkorange", "steelblue"),
  edge.color   = adjustcolor("gray50", alpha.f = 0.3)
)