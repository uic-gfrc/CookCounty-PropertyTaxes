
# ==============================================
# Two-mode SNA of Appeals × Firms, per year
# ==============================================

# Packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(knitr)

# ---------------------------
# CONFIG
# ---------------------------
CSV_PATH <- "./inputs/Board_of_Review_Appeal_Decision_History.csv"                 # path to your CSV
FIRM_SPLIT_PATTERN <- "\\s*[;|,]\\s+" # split multiple firms on ; | or comma
MIN_DEGREE_FIRMS <- 2                 # keep firms with degree >= this (per year) for plotting
TOP_N_FIRMS <- 30                     # cap plotted firms to top-N by degree (per year)
KEEP_GIANT_COMPONENT <- TRUE          # optionally restrict graphs to GCC for readability

# Plot export (set TRUE to save PNG/PDFs)
SAVE_PLOTS <- FALSE
OUT_DIR    <- "sna_year_plots"
IMG_WIDTH  <- 10
IMG_HEIGHT <- 7
IMG_DPI    <- 300

if (SAVE_PLOTS) dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---------------------------
# LOAD + PREP  (AppealID is numeric)
# ---------------------------
bor_raw <- read_csv(CSV_PATH, show_col_types = FALSE)

bor <- bor_raw %>%
  mutate(
    AppealID_num = as.numeric(AppealID),        # preserve numeric id
    AppealID_chr = as.character(AppealID_num),  # igraph vertex 'name'
    Attorney_FirmName = coalesce(Attorney_FirmName, "")
  ) %>%
  separate_rows(Attorney_FirmName, sep = FIRM_SPLIT_PATTERN, convert = FALSE) %>%
  mutate(
    Attorney_FirmName = str_squish(Attorney_FirmName),
    Attorney_FirmName = na_if(Attorney_FirmName, "")
  ) %>%
  filter(!is.na(Attorney_FirmName)) %>%
  select(tax_year, AppealID_num, AppealID_chr, Attorney_FirmName) %>%
  distinct()

stopifnot(all(c("tax_year", "AppealID_chr", "AppealID_num", "Attorney_FirmName") %in% names(bor)))

# ---------------------------
# BUILD YEARLY BIPARTITE GRAPHS
# ---------------------------
make_graph_for_year <- function(df_year) {
  edges <- df_year %>%
    select(AppealID_chr, Attorney_FirmName) %>%
    rename(AppealID = AppealID_chr, Firm = Attorney_FirmName)
  
  appeal_nodes <- tibble(name = unique(edges$AppealID), type = FALSE) # FALSE = Appeal
  firm_nodes   <- tibble(name = unique(edges$Firm),     type = TRUE)  # TRUE  = Firm
  nodes <- bind_rows(appeal_nodes, firm_nodes)
  
  g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  
  # attach numeric appeal id back
  id_map <- df_year %>% distinct(AppealID_chr, AppealID_num) %>% tibble::deframe()
  V(g)$AppealID_num <- NA_real_
  appeal_idx <- which(V(g)$type == FALSE)
  V(g)$AppealID_num[appeal_idx] <- id_map[V(g)$name[appeal_idx]]
  
  # degree centrality
  V(g)$deg <- degree(g, mode = "all")
  
  # giant component for readability (optional)
  if (KEEP_GIANT_COMPONENT && gorder(g) > 0) {
    comps <- components(g)
    g <- induced_subgraph(g, which(comps$membership == which.max(comps$csize)))
  }
  
  g
}

graphs_by_year <- bor %>%
  group_split(tax_year, .keep = TRUE) %>%
  set_names(bor %>% distinct(tax_year) %>% arrange(tax_year) %>% pull()) %>%
  map(make_graph_for_year)

# ---------------------------
# CENTRALITY TABLES (degree)
# ---------------------------
centrality_tables <- imap(graphs_by_year, function(g, yr) {
  if (gorder(g) == 0) return(tibble())
  as_tibble(vertex_attr(g)) %>%
    transmute(
      tax_year = yr,
      node     = name,
      type     = if_else(type, "Firm", "Appeal"),
      degree   = deg,
      AppealID_num
    ) %>%
    arrange(desc(degree), node)
})

centrality_all <- bind_rows(centrality_tables)

# Firms: top 10 by year
cat("\n### Firm centrality by year (degree)\n")
centrality_all %>%
  filter(type == "Firm") %>%
  group_by(tax_year) %>%
  slice_max(order_by = degree, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  select(tax_year, node, degree) %>%
  kable()

# Appeals: top 10 by year
cat("\n### Appeal centrality by year (degree)\n")
centrality_all %>%
  filter(type == "Appeal") %>%
  group_by(tax_year) %>%
  slice_max(order_by = degree, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  select(tax_year, AppealID_num, degree) %>%
  kable()

# ---------------------------
# PLOT: ONE YEAR AT A TIME (no faceting)
# ---------------------------
plot_year_graph <- function(g, yr) {
  if (gorder(g) == 0) {
    message("Year ", yr, ": empty graph after filtering; skipping.")
    return(invisible(NULL))
  }
  
  tg <- as_tbl_graph(g) %>%
    mutate(
      node_type = if_else(type, "Firm", "Appeal"),
      size_var  = scales::rescale(deg, to = c(3, 12))
    )
  
  p <- ggraph(tg, layout = "bipartite") +
    geom_edge_link(alpha = 0.2) +
    geom_node_point(aes(shape = node_type, size = size_var)) +
    scale_shape_manual(values = c(Appeal = 16, Firm = 15)) +
    guides(size = "none") +
    labs(title = paste0("Two-Mode SNA: Appeals × Firms — ", yr),
         subtitle = "Bipartite layout (Appeal = circle, Firm = square)") +
    theme_void() +
    theme(plot.title = element_text(face = "bold"))
  
  print(p)
  
  if (SAVE_PLOTS) {
    fn_png <- file.path(OUT_DIR, paste0("two_mode_", yr, ".png"))
    fn_pdf <- file.path(OUT_DIR, paste0("two_mode_", yr, ".pdf"))
    ggsave(fn_png, p, width = IMG_WIDTH, height = IMG_HEIGHT, dpi = IMG_DPI)
    ggsave(fn_pdf, p, width = IMG_WIDTH, height = IMG_HEIGHT)
  }
  
  invisible(p)
}

# Filter each yearly graph to a readable subset
filtered_graphs <- imap(graphs_by_year, function(g, yr) {
  if (gorder(g) == 0) return(g)
  
  # keep firms above degree threshold
  firm_ids <- V(g)[V(g)$type & V(g)$deg >= MIN_DEGREE_FIRMS]
  
  # if too many, keep top-N by degree
  if (length(firm_ids) > TOP_N_FIRMS) {
    firm_ids <- V(g)[V(g)$type][order(-V(g)$deg[V(g)$type])][seq_len(TOP_N_FIRMS)]
  }
  
  # keep those firms + their incident appeals
  keep_vids <- union(firm_ids, neighbors(g, firm_ids, mode = "all"))
  
  # if nothing passes filter, fall back to GCC so you still see something
  if (length(keep_vids) == 0) {
    comps <- components(g)
    keep_vids <- which(comps$membership == which.max(comps$csize))
  }
  
  induced_subgraph(g, vids = keep_vids)
})

# Show available years and plot all sequentially
available_years <- names(filtered_graphs)
message("Years available: ", paste(available_years, collapse = ", "))
invisible(imap(filtered_graphs, plot_year_graph))

# Helper to plot a single year on demand:
# plot_year_graph(filtered_graphs[["2022"]], "2022")

# ---------------------------
# (Optional) One-mode firm–firm projection stats
# ---------------------------
proj_tables <- imap(graphs_by_year, function(g, yr) {
  if (gorder(g) == 0) return(tibble())
  pr <- bipartite_projection(g, which = "both")
  firms_g <- pr$proj2
  tibble(
    tax_year = as.integer(yr),
    firm     = V(firms_g)$name,
    degree   = degree(firms_g),
    weighted_degree = strength(firms_g)
  ) %>%
    arrange(desc(degree))
})

proj_all <- bind_rows(proj_tables)

cat("\n### Firm–Firm projection (degree, by year)\n")
proj_all %>%
  group_by(tax_year) %>%
  slice_max(order_by = degree, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  kable()