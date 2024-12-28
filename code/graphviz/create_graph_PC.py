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
    "mot": (2, 0),
    "sui": (1, 1),
    "Prec": (4,1)
    # "P.emp": (3, 0),
    # "P.soc": (4, 1),
    # "P.hou": (5, 0),
    # "S.rel": (5, 2),
    # "S.fin": (6, 1)
}

# Function to create a graph with fixed layout and bidirectional edges
def create_curvy_graph_with_bidirectional_edges(matrix, title, positions, filename):
    dot = Digraph(format='png', engine='neato')
    dot.attr(size="13,13", dpi="300", splines="true", ratio="fill")
    #dot.attr(label=title, labelloc="top", fontsize="20", fontname="Palatino")
    # Set global node attributes for uniform size
    dot.attr('node', fixedsize='true', width='1', height='1')
    
    # Add nodes with fixed positions and uniform sizes
    for node, pos in positions.items():
        dot.node(node, node, shape="circle", pos=f"{pos[0]},{pos[1]}!",
                 fixedsize="true", width="1", height="1", fontsize="30")
    
    # Add edges based on the matrix
    nodes = list(positions.keys())
    for i, src in enumerate(nodes):
        for j, dest in enumerate(nodes):
            if i < j:  # Avoid duplicate processing
                if matrix[i][j] == 1 and matrix[j][i] == 1:
                    # Bidirectional edge
                    dot.edge(src, dest, dir="both")
                elif matrix[i][j] == 1:
                    # Directed edge i -> j
                    dot.edge(src, dest, dir="forward")
                elif matrix[j][i] == 1:
                    # Directed edge j -> i
                    dot.edge(dest, src, dir="forward")
    
    # Save and render the graph
    output_file = f"{filename}"
    dot.render(output_file, view=True)
    print(f"Graph saved to {output_file}")

# Define the adjacency matrices
# ## individual symptom case PC
# sym_pc1 = [
#     [0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1],
#     [0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 1],
#     [0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0],
#     [0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1],
#     [0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1],
#     [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1],
#     [0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1],
#     [0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
# ]

# sym_pc2 = [
#     [0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0],
#     [0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0],
#     [0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0],
#     [0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0],
#     [0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0],
#     [0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0],
#     [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
#     [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
#     [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
# ]

# # Generate graphs for both matrices
# create_curvy_graph_with_bidirectional_edges(sym_pc1, "GaussianCI and RCoT", fixed_positions, "GaussianCI and RCoT")
# create_curvy_graph_with_bidirectional_edges(sym_pc2, "onlyRCoT", fixed_positions, "onlyRCoT")



## precarity sum score case PC
presum_pc1 = [
 [0, 1, 1, 1, 1, 1, 1, 1, 0, 1],
 [0, 0, 1, 1, 1, 1, 1, 1, 1, 1],
 [0, 0, 1, 0, 1, 0, 1, 0, 0, 1],
 [0, 1, 1, 0, 1, 1, 1, 1, 0, 1],
 [0, 0, 0, 0, 0, 1, 1, 1, 0, 1],
 [0, 1, 1, 1, 1, 0, 1, 1, 1, 1],
 [0, 0, 0, 0, 0, 0, 0, 1, 0, 1],
 [0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
 [0, 1, 0, 0, 0, 0, 0, 1, 0, 1],
 [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
 ]

presum_pc2 = [
    [0, 1, 1, 1, 1, 0, 1, 0, 0, 1],
    [0, 0, 1, 1, 0, 1, 1, 1, 0, 1],
    [0, 0, 0, 1, 1, 0, 1, 0, 0, 1],
    [0, 0, 1, 0, 1, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 1, 1, 1, 0, 1],
    [0, 0, 0, 0, 0, 0, 1, 1, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 1],
    [0, 1, 0, 0, 0, 1, 0, 1, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
 ]

# Generate graphs for both matrices
create_curvy_graph_with_bidirectional_edges(presum_pc1, "pc_presum_GaussianCIRCoT", fixed_positions, "pc_presum_GaussianCIRCoT")
create_curvy_graph_with_bidirectional_edges(presum_pc2, "pc_presum_onlyRCoT", fixed_positions, "pc_presum_onlyRCoT")

