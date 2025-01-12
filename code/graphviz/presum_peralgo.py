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
    "Precarity": (3.5, 0.5)
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
# FCI
dot.edge("anh", "dep", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("anh", "slp", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("anh", "ene", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("anh", "app", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("anh", "glt", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("anh", "con", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("anh", "Precarity", dir="both", arrowhead="normal", arrowtail="odot")

dot.edge("dep", "slp", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("dep", "ene", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("dep", "app", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("dep", "glt", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("dep", "con", dir="both", arrowhead="odot", arrowtail="odot")
dot.edge("dep", "mot", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("dep", "sui", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("dep", "Precarity", dir="both", arrowhead="odot", arrowtail="none")

dot.edge("slp", "ene", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("slp", "app", dir="both", arrowhead="odot", arrowtail="odot")
dot.edge("slp", "glt", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("slp", "con", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("slp", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("slp", "Precarity", dir="both", arrowhead="normal", arrowtail="odot")

dot.edge("ene", "app", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("ene", "con", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("ene", "mot", dir="both", arrowhead="normal", arrowtail="normal")

dot.edge("app", "glt", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("app", "con", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("app", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("app", "Precarity", dir="both", arrowhead="normal", arrowtail="odot")

dot.edge("glt", "con", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("glt", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("glt", "sui", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("glt", "Precarity", dir="both", arrowhead="normal", arrowtail="odot")

dot.edge("con", "mot", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("con", "sui", dir="both", arrowhead="odot", arrowtail="normal")

dot.edge("mot", "sui", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("mot", "Precarity", dir="both", arrowhead="normal", arrowtail="odot")

dot.edge("sui", "Precarity", dir="both", arrowhead="normal", arrowtail="normal")

# GaussCItest edge
dot.edge("con", "Precarity", dir="both", arrowhead="odot", arrowtail="odot",style="dashed", color="#000080")


# Render and view the graph
dot.render("FCI_presum", view=True)



# # CCI
# dot.edge("anh", "dep", dir="both", arrowhead="normal", arrowtail="none")
# dot.edge("anh", "slp", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("anh", "ene", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("anh", "app", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("anh", "glt", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("anh", "con", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("anh", "Precarity", dir="both", arrowhead="normal", arrowtail="normal")

# dot.edge("dep", "slp", dir="both", arrowhead="none", arrowtail="normal")
# dot.edge("dep", "ene", dir="both", arrowhead="none", arrowtail="normal")
# dot.edge("dep", "app", dir="both", arrowhead="none", arrowtail="normal")
# dot.edge("dep", "glt", dir="both", arrowhead="none", arrowtail="normal")
# dot.edge("dep", "con", dir="both", arrowhead="none", arrowtail="normal")
# dot.edge("dep", "mot", dir="both", arrowhead="none", arrowtail="normal")
# dot.edge("dep", "sui", dir="both", arrowhead="odot", arrowtail="normal")
# dot.edge("dep", "Precarity", dir="both", arrowhead="none", arrowtail="none")

# dot.edge("slp", "ene", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("slp", "app", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("slp", "glt", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("slp", "con", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("slp", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("slp", "Precarity", dir="both", arrowhead="normal", arrowtail="none")

# dot.edge("ene", "app", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("ene", "con", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("ene", "mot", dir="both", arrowhead="normal", arrowtail="normal")

# dot.edge("app", "glt", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("app", "con", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("app", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("app", "Precarity", dir="both", arrowhead="normal", arrowtail="none")

# dot.edge("glt", "con", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("glt", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("glt", "sui", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("glt", "Precarity", dir="both", arrowhead="normal", arrowtail="none")

# dot.edge("con", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("con", "sui", dir="both", arrowhead="odot", arrowtail="normal")

# dot.edge("mot", "sui", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("mot", "Precarity", dir="both", arrowhead="normal", arrowtail="none")

# dot.edge("sui", "Precarity", dir="both", arrowhead="normal", arrowtail="normal")

# # GaussCItest edge
# dot.edge("con", "Precarity", dir="both", arrowhead="odot", arrowtail="none",style="dashed", color="#000080")

# # Render and view the graph
# dot.render("CCI_presum", view=True)