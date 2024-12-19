from graphviz import Digraph

# Define fixed positions for the nodes
fixed_positions = {
    "anh": (4, 3),
    "dep": (2, 3.2),
    "slp": (2, 1),
    "ene": (3, 4),
    "app": (1, 4),
    "glt": (2.5, 2),
    "con": (0, 2.7),
    "mot": (0.8, 2),
    "sui": (0, 1),
    "P.emp": (3, 0),
    "P.soc": (4, 1),
    "P.hou": (5, 0),
    "S.rel": (5, 2.2),
    "S.fin": (6, 1),
}

# List of differing edges (to be made dashed and gray)
differing_edges = [
    ("anh", "mot"), ("dep", "anh"), ("dep", "glt"), ("dep", "con"),
    ("slp", "con"), ("slp", "P.emp"), ("slp", "S.rel"), ("ene", "anh"),
    ("ene", "dep"), ("ene", "slp"), ("ene", "app"), ("ene", "con"),
    ("app", "anh"), ("app", "slp"), ("app", "glt"), ("app", "con"),
    ("glt", "slp"), ("glt", "S.rel"), ("con", "anh"), ("mot", "anh"),
    ("mot", "con"), ("sui", "S.rel"), ("P.emp", "slp"), ("P.emp", "S.fin"),
    ("P.soc", "glt"), ("S.rel", "slp"), ("S.rel", "glt"), ("S.rel", "sui"),
    ("S.rel", "S.fin"), ("S.fin", "P.soc")
]

# Function to create the graph
def create_graph_with_dashed_edges(matrix, title, filename):
    dot = Digraph(format='png', engine='neato')
    dot.attr(size="13,13", dpi="300", splines="true", ratio="fill")
    dot.attr(label=title, labelloc="top", fontsize="20", fontname="Palatino")

    # Add nodes with fixed positions
    for node, pos in fixed_positions.items():
        dot.node(node, node, shape="circle", pos=f"{pos[0]},{pos[1]}!",
                 fixedsize="true", width="1", height="1", fontsize="12")

    # Add edges based on the matrix
    nodes = list(fixed_positions.keys())
    for i, src in enumerate(nodes):
        for j, dest in enumerate(nodes):
            if matrix[i][j] > 0:  # Only process non-zero edges
                arrowhead = "odot" if matrix[i][j] == 1 else "normal"
                arrowtail = "odot" if matrix[j][i] == 1 else "inv" if matrix[j][i] == 3 else "none"
                edge_style = "dashed" if (src, dest) in differing_edges or (dest, src) in differing_edges else "solid"
                edge_color = "gray" if edge_style == "dashed" else "black"
                
                # Add the edge
                dot.edge(
                    src, dest,
                    style=edge_style,
                    color=edge_color,
                    arrowhead=arrowhead,
                    arrowtail=arrowtail,
                    dir="both" if matrix[i][j] == 2 and matrix[j][i] == 2 else "forward"
                )

    # Save and render the graph
    output_file = f"{filename}.gv"
    dot.render(output_file, view=True)
    print(f"Graph saved to {output_file}")

# Define the adjacency matrix
matrix2 = [
    [0, 2, 2, 2, 2, 2, 2, 0, 0, 0, 2, 0, 0, 0],
    [3, 0, 2, 2, 2, 3, 1, 2, 2, 0, 0, 0, 0, 0],
    [2, 2, 0, 2, 2, 2, 3, 2, 0, 3, 0, 0, 0, 0],
    [3, 3, 3, 0, 3, 0, 3, 2, 0, 0, 0, 0, 0, 0],
    [3, 2, 3, 2, 0, 3, 3, 2, 0, 0, 0, 0, 0, 0],
    [2, 2, 3, 0, 2, 0, 2, 2, 2, 0, 2, 0, 2, 0],
    [3, 2, 2, 2, 2, 3, 0, 2, 2, 0, 0, 0, 0, 0],
    [0, 2, 2, 2, 2, 2, 3, 0, 2, 0, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 3, 0],
    [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3],
    [2, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 2],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 3],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 0, 2, 0],
]

# Plot the graph for matrix2
create_graph_with_dashed_edges(matrix2, "RCoT only", "matrix2_graph_dashed")
