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

# Add nodes and edges
def create_graph_with_differing_edges(matrix, title, filename, differing_edges):
    dot = Digraph(format="png", engine="neato")
    dot.attr(size="13,13", dpi="300", splines="true", ratio="fill")
    dot.attr(label=title, labelloc="top", fontsize="20", fontname="Palatino")

    # Add nodes with fixed positions
    for node, pos in fixed_positions.items():
        dot.node(
            node,
            node,
            shape="circle",
            pos=f"{pos[0]},{pos[1]}!",
            fixedsize="true",
            width="1",
            height="1",
            fontsize="12",
        )

    # Add edges based on the matrix and differing_edges
    nodes = list(fixed_positions.keys())
    for i, src in enumerate(nodes):
        for j, dest in enumerate(nodes):
            if i < j:  # Avoid duplicate edge processing
                style = "solid"
                color = "black"
                if (src, dest) in differing_edges or (dest, src) in differing_edges:
                    style = "dashed"
                    color = "gray"

                if matrix[i][j] == 2 and matrix[j][i] == 2:
                    dot.edge(src, dest, dir="both", arrowtail="normal", arrowhead="normal", color=color, style=style)
                elif matrix[i][j] == 2 and matrix[j][i] == 1:
                    dot.edge(dest, src, arrowtail="odot", arrowhead="normal", color=color, style=style)
                elif matrix[i][j] == 1 and matrix[j][i] == 2:
                    dot.edge(src, dest, arrowtail="odot", arrowhead="normal", color=color, style=style)
                elif matrix[i][j] == 1 and matrix[j][i] == 1:
                    dot.edge(src, dest, dir="none", arrowhead="odot", arrowtail="odot", color=color, style=style)
                elif matrix[i][j] == 2:
                    dot.edge(src, dest, arrowhead="normal", color=color, style=style)
                elif matrix[j][i] == 2:
                    dot.edge(dest, src, arrowhead="normal", color=color, style=style)
                elif matrix[i][j] == 1:
                    dot.edge(src, dest, dir="none", arrowhead="odot", color=color, style=style)

    # Save and render the graph
    output_file = f"{filename}.gv"
    dot.render(output_file, view=True)
    print(f"Graph saved to {output_file}")


# Define differing edges
differing_edges = [
    ("anh", "mot"),
    ("dep", "anh"),
    ("dep", "glt"),
    ("dep", "con"),
    ("slp", "con"),
    ("slp", "P.emp"),
    ("slp", "S.rel"),
    ("ene", "anh"),
    ("ene", "dep"),
    ("ene", "slp"),
    ("ene", "app"),
    ("ene", "con"),
    ("app", "anh"),
    ("app", "slp"),
    ("app", "glt"),
    ("app", "con"),
    ("glt", "slp"),
    ("glt", "S.rel"),
    ("con", "anh"),
    ("mot", "anh"),
    ("mot", "con"),
    ("sui", "S.rel"),
    ("P.emp", "slp"),
    ("P.emp", "S.fin"),
    ("P.soc", "glt"),
    ("S.rel", "slp"),
    ("S.rel", "glt"),
    ("S.rel", "sui"),
    ("S.rel", "S.fin"),
    ("S.fin", "P.soc"),
]


# Define the adjacency matrices
matrix1 = [
    [0, 2, 2, 2, 2, 2, 2, 0, 0, 0, 2, 0, 0, 0],
    [2, 0, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0],
    [2, 2, 0, 2, 2, 2, 2, 2, 0, 2, 0, 0, 2, 0],
    [2, 2, 3, 0, 2, 0, 2, 2, 0, 0, 0, 0, 0, 0],
    [3, 2, 2, 2, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0],
    [2, 2, 2, 0, 2, 0, 2, 2, 2, 0, 2, 0, 2, 0],
    [1, 2, 2, 2, 2, 3, 0, 2, 2, 0, 0, 0, 0, 0],
    [0, 2, 2, 2, 2, 2, 2, 0, 2, 0, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 2, 0],
    [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2],
    [2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 2],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 2, 0],
]



# Plot the graphs
create_graph_with_differing_edges(matrix1, "Both", "matrix1_graph", differing_edges)
