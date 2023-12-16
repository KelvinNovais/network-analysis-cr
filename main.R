library(igraph)

# GLOBAL VARIABLES
# Set if it's going to plot in interactive mode
tk <- F
# number of answers
answers <<- 0

# PATHS
raw_data_path           <- "./csv/raw-data.csv"
nodes_template_path     <- "./csv/nodes-template.csv"
nodes_path              <- "./csv/nodes.csv"
edges_template_path     <- "./csv/edges-template.csv"
edges_path              <- "./csv/edges.csv"

# RANGE OF CATEGORIES NUMBERS
# beginning of each category
bar <- 1      # age_range
bsk <- 7      # skin_color
bg  <- 13     # gender
be  <- 15     # education
brl <- 18     # role
bpm <- 24     # preferred_mg
bum <- 44     # unpreferred_mg
br  <- 64     # residents
bi  <- 69     # income

# a matrix that points the beginning and the end of each category, on 1st and 2nd column respectively
category_range <- matrix(
  data = c(
    bar,bsk-1,			# age_range
    bsk,bg-1,		    # skin_color
    bg,be-1,		    # gender
    be,brl-1,		    # education
    brl,bpm-1,		  # role
    bpm,bum-1,	   	# preferred_mg
    bum,br-1,		    # unpreferred_mg
    br,bi-1,	     	# residents
    bi,73           # income
  ),
  nrow = 9,
  ncol = 2,
  byrow = T
)

# A function to get the raw data and generate the edges incidence matrix and the nodes table
treat_data <- function() {
  # Copy templates
  file.copy(
    from = edges_template_path,
    to = edges_path,
    overwrite = T
  )
  
  file.copy(
    from = nodes_template_path,
    to = nodes_path,
    overwrite = T
  )
  
  # Read csv files
  raw_data <- read.csv(
    file = raw_data_path,
    header = F,
    skip = 1                     # skip the first row (with the form questions)
  )
  
  nodes_df <- read.csv(
    file = nodes_path,
    header = T,
    as.is = T
  )
  
  edges_df <- read.csv(
    file = edges_template_path,
    header = T,
    as.is = T
  )
  
  if (category_range[nrow(category_range), ncol(category_range)] != nrow(nodes_df)) {
    message("O número de categorias no arquivo 'edges.csv' difere do listado no código!")
    quit(status = 1)
  }
  
  # A data frame that will append nodes soon
  nodes_to_append <- data.frame()
  
  # Get the number of answers
  answers <<- nrow(raw_data)
  
  # TREAT EDGES:
  # generate a incidence matrix
  for (row in 1:nrow(raw_data)) {
    # Add the name of the edge on the first column
    edges_df[row, 1] <- sprintf("U%03d", row)
    
    # Generate node to append the data frame; "10" refers to a category that will be used to set vertex color
    nodes_to_append <- rbind(nodes_to_append, c(sprintf("U%03d", row), NA, 10, NA, NA, NA))
    
    # for each value on the raw_data, insert 1 for pointed categories and 0 for non pointed categories
    for (col in 3:11) {
      for (x in category_range[col-2, 1]:category_range[col-2, 2]) {
        if (raw_data[row, col] == nodes_df[x, 2]) {
          edges_df[row, nodes_df[x, 1]] <- 1
        } else {
          edges_df[row, nodes_df[x, 1]] <- 0
        }
      }
    }
  }
  
  # TREAT NODES:
  # append the table with previous generated values
  write.table(
    nodes_to_append,  
    file = nodes_path, 
    append = T, 
    sep=',', 
    row.names=F, 
    col.names=F
  )
  
  write.csv(
    edges_df,
    file = edges_path,
    row.names = F
  )
  
  rm(raw_data)
  rm(edges_df)
  rm(nodes_df)
}

main <- function() {
  message("Starting...")
  message("Coded by Kelvin Novais - 2023 <https://github.com/KelvinNovais>")
  message("No license code, feel free to use!")
  
  set.seed(1234)
  
  # Color palette
  nodes_colors <- rainbow(10, alpha=0.65)
  nodes_colors <- sample(nodes_colors)    # randomize the vector
  
  legends <- c(
    "Faixa etária",
    "Raça ou cor",
    "Sexo",
    "Escolaridade",
    "Curso/Função",
    "Gênero mais consumido",
    "Gênero menos consumido",
    "Habitantes", 
    "Renda"
  )
  
  treat_data()
  
  # File format to save
  svg(
    width = 15,
    height = 15
  )
  
  nodes <- read.csv(nodes_path, header = T, as.is = T)
  links <- read.csv(edges_path, header = T, row.names = 1)
  
  links <- as.matrix(links)
  
  # The generated edges csv is a inverted matrix, so just transpose it
  links <- t(links)
  
  net <- graph_from_incidence_matrix(
    links,
    directed = T,
    mode = "in" # vertexes go from "U" to "c"
  )
  
  # Count the number of degree for each node:
  net.deg <- degree(net, mode = "in")
  
  plot(
    net,
    
    layout = layout_on_sphere(net) * 1.075,
    
    vertex.label = nodes$vertex.label,
    vertex.label.color = "black",
    vertex.size = (log(net.deg^3 + 2) + 1), # a function to print a good vertex size
    vertex.color = nodes_colors[nodes$category.type],
    vertex.frame.color = "gray50",
    rescale = F,
    margin = 0,
    
    edge.arrow.size = 0.1,
    
    main = "Grafo geral",
  )
  
  legend(
    x = "bottomright",
    legend = legends,
    fill = nodes_colors[2:11],
    title = "Legenda",
    horiz = F
  )
  
  sink("output.txt")
  cat("VÉRTICES: ", vcount(net))
  cat("\n==========================================================\n")
  cat("ARESTAS: ", ecount(net))
  cat("\n==========================================================\n")
  cat("DENSIDADE: ", graph.density(net, loop = F))
  cat("\n==========================================================\n")
  cat("RECIPROCIDADE: ", reciprocity(net))
  cat("\n==========================================================\n")
  cat("TRANSITIVIDADE: ", triad_census(net))
  cat("\n==========================================================\n")
  cat("DIÂMETRO: ", diameter(net, directed=T, weights=NA))
  cat("\n==========================================================\n")
  print("CENTRALIDADE: ")
  print(centr_degree(net, mode="in", normalized=T))
  cat("\n==========================================================\n")
  cat("PROXIMIDADE: ", closeness(net, mode="in", weights=NA))
  cat("\n==========================================================\n")
  cat("INTERMEDIAÇÃO: ", betweenness(net, directed=T, weights=NA))
  cat("\n==========================================================\n")
  cat("CAMINHO MÉDIO: ", mean_distance(net, directed=T))
  cat("\n==========================================================\n")
  cat("DISTÂNCIAS: ", distances(net, weights=NA))
  sink()
  
  # Interactive plot for the full graph
  if (tk) {
    id_tkplot <- tkplot(
      net,

      layout = layout_on_sphere(net) * 1.075,

      vertex.label = nodes$vertex.label,
      vertex.label.color = "black",
      vertex.size = (log((net.deg)^3 + 2) + 1), # a function to print a good vertex size
      vertex.color = nodes_colors[nodes$category.type],
      vertex.frame.color = "gray50",
      rescale = F,
      margin = 0,

      edge.curved = 0.3,
      edge.arrow.size = 0.7,

      main = "Grafo geral",
    )
  }

  # On the nodes table, get the range of people ids
  people_ids <- (nrow(nodes) -answers +1):nrow(nodes)
  
  # PLOT SUBGRAPHS IN PAIRS, ACCORDING TO CATEGORIES
  for (main_category in c(6, 7)) {
    for (row in c(1, 2, 3, 4, 5, 8, 9)) {
      main_category.range <- category_range[main_category, 1]:category_range[main_category, 2]
      combinant.range     <- category_range[row, 1]:category_range[row, 2]
      
      if (main_category < row) {
        nodes.range <- append(main_category.range, combinant.range)
      } else {
        nodes.range <- append(combinant.range, main_category.range)
      }
      
      category.vids <- append(nodes.range, people_ids)
      
      category.net <- subgraph(net, category.vids)
      category.deg <- degree(category.net, mode = "in")
      
      plot(
        category.net,
        
        layout = layout_on_sphere(category.net) * 1.075,
        
        vertex.label = nodes$vertex.label[category.vids],
        vertex.label.color = "black",
        vertex.label.font = 2,
        vertex.label.family = "Arial",
        vertex.size = (log(category.deg ^ 3 + 2) + 1),
        # access node_colors vector, according to the column "category.type" on the nodes table, matching only the current ids
        vertex.color = nodes_colors[nodes$category.type[category.vids]],
        vertex.frame.color = "gray50",
        rescale = F,

        edge.arrow.size = 0.5,
        
        main = paste(legends[row], legends[main_category], sep = " & "),
      )
      
      legend(
        x = "bottomright",
        legend = paste(nodes$vertex.label[combinant.range], nodes$category.legend[combinant.range], sep = ":  "),
        title = "Legenda",
        horiz = F
      )
    }
  }

  # Close the graphics device
  dev.off()
  
  message("Finishing...")
}

main()

# Plot:     https://r-coder.com/save-plot-r/
# Tkplot:   https://r.igraph.org/reference/tkplot.html
# Legend:   https://r-coder.com/add-legend-r/

# https://robwiederstein.github.io/network_analysis/igraph.html
# https://r.igraph.org/reference/
# https://igraph.org/r/html/latest/

# edge    ==    link
# vertex  ==    node
