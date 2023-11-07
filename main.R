library(igraph)

# GLOBAL VARIABLES
tk <- T

# PATHS
raw_data_path <- "./csv/raw-data.csv"
nodes_template_path <- "./csv/nodes-template.csv"
nodes_path <- "./csv/nodes.csv"
edges_template_path <- "./csv/edges-template.csv"
edges_path <- "./csv/edges.csv"

# TODO: improve
# verificar cada linha da categora; quando houver mudança de categoria, marcar como início; gerar matiz automaticamente
# RANGE OF CATEGORIES NUMBERS
bar <- 1      # age_range
bsk <- 7      # skin_color
bg  <- 13     # gender
be  <- 17     # education
br  <- 18     # role
bpm <- 26     # preferred_mg
bum <- 46     # unpreferred_mg
br  <- 66     # residents
bi  <- 71     # income

# a matrix that points the beginning and the end of each category, on 1st and 2nd column respectively
id_key_ranges <- matrix(
  data = c(
    bar,bsk-1,			# age_range
    bsk,bg-1,		    # skin_color
    bg,be-1,		    # gender
    be,br-1,		    # education
    br,bpm-1,		    # role
    bpm,bum-1,	   	# preferred_mg
    bum,br-1,		    # unpreferred_mg
    br,bi-1,	     	# residents
    bi,75           # income
  ),
  nrow = 9,
  ncol = 2,
  byrow = T
)

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
  
  if (id_key_ranges[nrow(id_key_ranges), ncol(id_key_ranges)] != nrow(nodes_df)) {
    message("O número de categorias no arquivo 'edges.csv' difere do listado no código!")
    quit(status = 1)
  }
  
  # A data frame that will append nodes soon
  nodes_to_append <- data.frame()
  
  # TREAT EDGES:
  # generate a incidence matrix
  for (row in 1:nrow(raw_data)) {
    # Add the name of the edge on the first column
    edges_df[row, 1] <- sprintf("U%03d", row)
    
    # Generate node to append the data frame; 12 refers to a category that will be used to set vertex color
    nodes_to_append <- rbind(nodes_to_append, c(sprintf("U%03d", row), NA, 10, NA, NA))
    
    # for each value on the raw_data, insert 1 for pointede categories and 0 for non pointed categories
    for (col in 3:11) {
      for (x in id_key_ranges[col-2, 1]:id_key_ranges[col-2, 2]) {
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

treat_data_old <- function() {
  message("Treating raw data...")
  
  # IMPORTING THE RAW DATA CSV
  raw_data <- read.csv(
    file = raw_data_path,
    header = F,
    skip = 1                     # skip the first row (with the form questions)
  )
  # Columns names:
  columns.c <- c(
    "timestamp", "name", "age_range", "skin_color", "gender", "education",
    "role", "preferred_mg", "unprefered_mg", "residents", "income"
  )
  # Naming columns
  colnames(raw_data) <- columns.c
  # Replacing answers by node IDs
  nodes <- read.csv(file = nodes_path, header = T)
  for (col in 1:length((columns.c))) {
    for (row in 1:nrow(raw_data)) {
      for (x in 1:nrow(nodes)) {
        # TODO genero menos consumido
        if (raw_data[row, col] == nodes[x, 2]) {
          raw_data[row, col] = nodes[x, 1]
          break
        }
      }
    }
  }
  
  # TREATING EDGES
  if (file.exists(edges_path)) {
    file.remove(edges_path)
  }
  
  edges_df <- data.frame()
  people <- c(paste("p", 1:nrow(raw_data), sep=''))
  for (i in 3:11) {
    if (!i==5) {
      # temporary data frame with all attributes
      tmp = data.frame(
        from = people,
        to = raw_data[[i]]
      )
      # append the relations data frame
      edges_df <- rbind(edges_df, tmp)
      # discard the tmp data frame
      rm(tmp)
    }
  }
  
  write.csv(
    edges_df,
    file = edges_path,
    row.names = FALSE
  )
  
  # TREATING NODES
  nodes_df <- data.frame(id=people)
  write.table(nodes_df,  
              file=nodes_path, 
              append = T, 
              sep='\n', 
              row.names=F, 
              col.names=F
  )
  
  # discard unused
  rm(edges_df)
  rm(raw_data)
  rm(nodes)
  rm(people)
  
  message("Finished treating data")
}

main <- function() {
  message("Starting...")
  message("Coded by Kelvin Novais - 2023 <https://github.com/KelvinNovais>")
  message("No license code, feel free to use!")
  
  set.seed(1234)
  
  # Color palette
  nodes_colors <- rainbow(10, alpha=0.7)
  
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
  
  net1 <- graph_from_incidence_matrix(
    links,
    directed = T,
    mode = "in" # vertexes go from "U" to "c"
  )
  
  # Count the number of degree for each node:
  deg1 <- degree(net1, mode = "in")
  
  # Setting some layouts
  lc <- layout_in_circle(net1)
  
  plot(
    net1,
    
    layout = lc,
    
    vertex.label = nodes$vertex.label,
    vertex.label.color = "black",
    vertex.label.dist = 1.5,
    vertex.size = (deg1 + 1.5) * 1.05,
    vertex.color = nodes_colors[nodes$category.type],
    rescale = F,
    margin = 0,
    
    #edge.curved=0.2,
    edge.arrow.size = 0.1,
    
    main = "Grafo geral",
    #sub = "Subtítulo",
  )
  
  legend(
    x = "bottomright",
    legend = c(
      "Faixa etária",
      "Raça ou cor",
      "Sexo",
      "Escolaridade",
      "Curso/Função",
      "Gênero mais consumido",
      "Gênero menos consumido",
      "Habitantes", 
      "Renda"
    ),
    fill = nodes_colors[2:11],
    title = "Legenda",
    horiz = F
  )
  
  if (tk) {
    id_tkplot_1 <- tkplot(
      net1,
      
      # canvas.width = 450,
      # canvas.height = 450,
      
      layout = lc,
      
      vertex.label = nodes$vertex.label,
      vertex.label.color = "black",
      vertex.label.dist = 1.2,
      vertex.size = (deg1 + 1.5) * 1.05,
      vertex.color = nodes_colors[nodes$category.type],
      rescale = F,
      margin = 0,
      
      edge.curved = 0.3,
      edge.arrow.size = 0.7,
      
      main = "Grafo geral",
      #sub = "Subtítulo",
    )
    
    legend(
      x = "bottomright",
      legend = c(
        "Faixa etária",
        "Raça ou cor",
        "Sexo",
        "Escolaridade",
        "Curso/Função",
        "Gênero mais consumido",
        "Gênero menos consumido",
        "Habitantes", 
        "Renda"
      ),
      fill = nodes_colors[1:9],
      title = "Legenda",
      horiz = F
    )
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
