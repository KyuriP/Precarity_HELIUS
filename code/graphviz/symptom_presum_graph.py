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
    "Precarity": (3, 0)
}

# Create the directed graph
dot = Digraph(format='png', engine='neato')
dot.attr(size="14,15", dpi="300", splines="true", ratio="fill")
dot.attr(label="", labelloc="top", fontsize="20", fontname="Palatino")

# Add nodes with fixed positions
for node, pos in fixed_positions.items():
    dot.node(node, node, shape="circle", pos=f"{pos[0]},{pos[1]}!",
             fixedsize="true", width="1", height="1", fontsize="25")


# Manually define edges based on the adj.matrices
dot.edge("anh", "dep", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="#a9a9a9")
dot.edge("anh", "slp", dir="both", arrowhead="odot", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("anh", "ene", dir="both", arrowhead="odot", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("anh", "app", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("anh", "con", dir="both", arrowhead="odot", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("anh", "Precarity", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")

dot.edge("dep", "slp", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("dep", "app", dir="both", arrowhead="odot", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("dep", "glt", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("dep", "con", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("dep", "mot", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("dep", "sui", dir="both", arrowhead="odot", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("dep", "Precarity", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")

dot.edge("slp", "ene", dir="both", arrowhead="odot", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("slp", "app", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("slp", "con", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("slp", "mot", dir="both", arrowhead="odot", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("slp", "Precarity", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")

dot.edge("ene", "app", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("ene", "mot", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")

dot.edge("app", "glt", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("app", "con", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("app", "mot", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("app", "Precarity", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")

dot.edge("glt", "con", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("glt", "mot", dir="both", arrowhead="odot", arrowtail="odot", style="dashed", color="#a9a9a9")
dot.edge("glt", "sui", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="#a9a9a9")
dot.edge("glt", "Precarity", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")

dot.edge("con", "mot", dir="both", arrowhead="normal", arrowtail="normal")

dot.edge("mot", "sui", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("mot", "Precarity", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="#a9a9a9")

dot.edge("sui", "Precarity", dir="both", arrowhead="normal", arrowtail="normal")

# GaussCItest edge
dot.edge("con", "Precarity", dir="both", arrowhead="normal", arrowtail="odot",style="dashed", color="#000080")


# Render and view the graph
dot.render("presum_graph_refined", view=True)
