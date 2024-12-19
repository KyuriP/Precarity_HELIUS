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

# Create the directed graph
dot = Digraph(format='png', engine='neato')
dot.attr(size="15,17", dpi="300", splines="true", ratio="fill")
dot.attr(label="", labelloc="top", fontsize="20", fontname="Palatino")

# Add nodes with fixed positions
for node, pos in fixed_positions.items():
    dot.node(node, node, shape="circle", pos=f"{pos[0]},{pos[1]}!",
             fixedsize="true", width="1", height="1", fontsize="25")

# Manually define edges based on the image
# Solid edges
dot.edge("anh", "dep", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("anh", "slp", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("anh", "glt", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("anh", "app", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("anh", "ene", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("anh", "P.soc", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("app", "ene", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("app", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("app", "slp", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
dot.edge("app", "glt", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
dot.edge("app", "con", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
dot.edge("con", "anh", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("con", "mot", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("dep", "glt", dir="both", arrowhead="odot", arrowtail="odot", style="dashed", color="gray")
dot.edge("dep", "slp", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("dep", "con", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("dep", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("dep", "sui", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("dep", "app", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("ene", "con", dir="both", arrowhead="normal",  arrowtail = "odot", style="dashed", color="gray")
dot.edge("ene", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("ene", "slp", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
dot.edge("ene", "dep", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
dot.edge("glt", "slp", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("glt", "con", dir="both", arrowhead="normal", arrowtail="none")
dot.edge("glt", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("glt", "S.rel", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
dot.edge("glt", "P.soc", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("slp", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("slp", "con", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("slp", "P.emp", dir="both", arrowhead="normal", arrowtail = "odot", style="dashed", color="#000080")
dot.edge("slp", "S.rel", dir="both", arrowhead="normal", arrowtail="normal", style="dashed", color="#000080")

dot.edge("con", "sui", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("sui", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("sui", "glt", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("S.rel", "S.fin", dir="both", arrowhead="odot", arrowtail="normal",  style="dashed", color="gray")
dot.edge("S.fin", "P.soc", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
dot.edge("S.fin", "P.emp", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
dot.edge("S.rel", "sui", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")

# Render and view the graph
dot.render("both_manual_graph_complete", view=True)
