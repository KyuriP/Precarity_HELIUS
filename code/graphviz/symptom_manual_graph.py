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
dot.attr(size="14,15", dpi="300", splines="true", ratio="fill")
dot.attr(label="", labelloc="top", fontsize="20", fontname="Palatino")

# Add nodes with fixed positions
for node, pos in fixed_positions.items():
    dot.node(node, node, shape="circle", pos=f"{pos[0]},{pos[1]}!",
             fixedsize="true", width="1", height="1", fontsize="25")

# Manually define edges based on the image
# Solid edges
# dot.edge("anh", "dep", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("anh", "slp", dir="both", arrowhead="normal", arrowtail="odot")
# #dot.edge("anh", "glt", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("anh", "app", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("anh", "ene", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("anh", "P.soc", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("app", "ene", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("app", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("app", "slp", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("app", "glt", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
# dot.edge("app", "con", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("con", "anh", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
# dot.edge("con", "mot", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("dep", "glt", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("dep", "slp", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("dep", "con", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("dep", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("dep", "sui", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("dep", "app", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("ene", "con", dir="both", arrowhead="normal",  arrowtail = "normal")
# dot.edge("ene", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("ene", "slp", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
# dot.edge("ene", "dep", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
# #dot.edge("glt", "slp", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("glt", "con", dir="both", arrowhead="normal", arrowtail="none")
# dot.edge("glt", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("glt", "S.rel", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
# dot.edge("glt", "P.soc", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("slp", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("slp", "con", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("slp", "P.emp", dir="both", arrowhead="normal", arrowtail = "odot", style="dashed", color="#000080")
# dot.edge("slp", "S.rel", dir="both", arrowhead="normal", arrowtail="normal", style="dashed", color="#000080")

# #dot.edge("con", "sui", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("sui", "mot", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("sui", "glt", dir="both", arrowhead="normal", arrowtail="normal")
# dot.edge("S.rel", "S.fin", dir="both", arrowhead="odot", arrowtail="normal",  style="dashed", color="gray")
# dot.edge("S.fin", "P.soc", dir="both", arrowhead="odot", arrowtail="normal", style="dashed", color="gray")
# dot.edge("S.fin", "P.emp", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")
# dot.edge("S.rel", "sui", dir="both", arrowhead="normal", arrowtail="odot", style="dashed", color="gray")

## FCI only (RCoT and Gaussian dashed)
dot.edge("anh", "dep", dir="both", arrowhead="normal", arrowtail="none")
dot.edge("anh", "slp", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("anh", "ene", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("anh", "app", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("anh", "glt", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("anh", "con", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("anh", "P.soc", dir="both", arrowhead="normal", arrowtail="tee", style="dashed")


dot.edge("dep", "slp", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("dep", "ene", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("dep", "app", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("dep", "glt", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("dep", "con", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("dep", "mot", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("dep", "sui", dir="both", arrowhead="tee", arrowtail="normal")
dot.edge("dep", "P.soc", dir="both", arrowhead="normal", arrowtail="none")
dot.edge("dep", "S.fin", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")


dot.edge("slp", "ene", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("slp", "app", dir="both", arrowhead="odot", arrowtail="tee")
dot.edge("slp", "glt", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("slp", "con", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("slp", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("slp", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("slp", "P.emp", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")
dot.edge("slp", "S.rel", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")


dot.edge("ene", "app", dir="both", arrowhead="normal", arrowtail="tee")
dot.edge("ene", "con", dir="both", arrowhead="normal", arrowtail="tee")
dot.edge("ene", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("ene", "S.rel", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")


dot.edge("app", "glt", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("app", "con", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("app", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("app", "S.rel", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")

dot.edge("glt", "con", dir="both", arrowhead="normal", arrowtail="odot")
dot.edge("glt", "mot", dir="both", arrowhead="normal", arrowtail="normal")
dot.edge("glt", "sui", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("glt", "P.soc", dir="both", arrowhead="odot", arrowtail="odot")
dot.edge("glt", "S.rel", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")

dot.edge("con", "mot", dir="both", arrowhead="tee", arrowtail="normal")
dot.edge("con", "sui", dir="both", arrowhead="tee", arrowtail="normal")
dot.edge("con", "P.emp", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")
dot.edge("con", "P.soc", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")

dot.edge("mot", "sui", dir="both", arrowhead="odot", arrowtail="normal")
dot.edge("mot", "P.emp", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")
dot.edge("mot", "P.soc", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")
dot.edge("mot", "S.fin", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")

dot.edge("sui", "P.soc", dir="both", arrowhead="normal", arrowtail="normal")

dot.edge("P.emp", "S.fin", dir="both", arrowhead="odot", arrowtail="odot")
dot.edge("P.emp", "P.hou", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")

dot.edge("P.soc", "S.rel", dir="both", arrowhead="tee", arrowtail="tee", style="dashed")
dot.edge("P.soc", "S.fin", dir="both", arrowhead="tee", arrowtail="normal", style="dashed")

dot.edge("P.hou", "S.fin", dir="both", arrowhead="normal", arrowtail="tee", style="dashed")
dot.edge("S.fin", "S.rel", dir="both", arrowhead="normal", arrowtail="normal")



# Render and view the graph
dot.render("symptom_graph_refined", view=True)
