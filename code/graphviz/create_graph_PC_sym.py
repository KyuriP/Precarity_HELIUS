from graphviz import Digraph

# Define the fixed positions for the nodes
fixed_positions = {
    "anh": (0, 3),
    "dep": (1, 4),
    "slp": (2, 3),
    "ene": (3, 4),
    "app": (4, 3),
    "glt": (2, 2),
    "con": (2, 1),
    "mot": (1.7, 0),
    "sui": (1, 1),
    # "Prec": (4,1)
    "P.emp": (3.5, 0),
    "P.soc": (4, 1),
    "P.hou": (5, 0),
    "S.rel": (5, 2),
    "S.fin": (6, 1)
}

# Function to create a graph with fixed layout and bidirectional edges
def create_curvy_graph_with_bidirectional_edges(matrix1, matrix2, title, positions, filename):
    dot = Digraph(format='png', engine='neato')
    dot.attr(size="13,13", dpi="300", splines="true", ratio="fill")
    dot.attr('node', fixedsize='true', width='1', height='1')

    # Add nodes with fixed positions and uniform sizes
    for node, pos in positions.items():
        dot.node(node, node, shape="circle", pos=f"{pos[0]},{pos[1]}!",
                 fixedsize="true", width='1', height='1', fontsize="30")

    # Add edges based on the matrix and highlight the differences with dashed lines
    nodes = list(positions.keys())
    for i, src in enumerate(nodes):
        for j, dest in enumerate(nodes):
            if i < j:  # Avoid duplicate processing
                style = "solid"
                color = "black"  # Default color

                if (matrix1[i][j] == 0 and matrix1[j][i] == 0 and (matrix2[i][j] == 1 or matrix2[j][i] == 1)):
                    style = "dashed"  # Highlight with dashed line
                    color = "#000080"
                if (matrix1[i][j] == 1 and matrix1[j][i] == 1) or (matrix2[i][j] == 1 and matrix2[j][i] == 1):
                    dot.edge(src, dest, dir="both", style=style, color =  color)
                elif matrix1[i][j] == 1 or matrix2[i][j] == 1:
                    dot.edge(src, dest, dir="forward", style=style, color = color)
                elif matrix1[j][i] == 1 or matrix2[j][i] == 1:
                    dot.edge(dest, src, dir="forward", style=style, color = color)

    # Save and render the graph
    output_file = f"{filename}"
    dot.render(output_file, view=True)
    print(f"Graph saved to {output_file}")



PC_rcot = [
    [0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1],
    [0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1],
    [0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0],
    [0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0],
    [0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
]

PC_gauss = [
    [0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0],
    [0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0],
    [0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0],
    [0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0],
    [0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0],
    [0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
]

# Generate graphs for both matrices
create_curvy_graph_with_bidirectional_edges(PC_rcot, PC_gauss, "PC_symptom_graph", fixed_positions, "PC_symptom_graph")



