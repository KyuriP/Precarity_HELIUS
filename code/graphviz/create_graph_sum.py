from graphviz import Digraph

# Create the directed graph
# dot = Digraph(format='png', engine='neato')
dot = Digraph(format='png', engine='dot')

# Set global font for the graph, nodes, and edges
dot.attr(graph_fontname='Palatino', node_fontname='Palatino', edge_fontname='Palatino')


# Set graph size and resolution
dot.attr(size="7,7")  # Increase graph dimensions: Width, Height
dot.attr(dpi="300")   # Set resolution to 300 DPI
dot.attr(ratio="fill")

# Add title
# dot.attr(label="RCoT", labelloc="top", fontsize="20")
dot.attr(label="", labelloc="top", fontsize="20")

# Set global node attributes for uniform size
dot.attr('node', fixedsize='true', width='0.8', height='0.8')

# Enable splines for curved edges
dot.attr(splines="true")

# Define nodes
# dot.node("PHQsum", "PHQsum", shape="circle", pos="1,4!", style="filled", fillcolor="#f2f0ef")
dot.node("PHQsum", "PHQsum", shape="circle", pos="1,4!")
dot.node("P.emp", "P.emp", shape="circle", pos="0,3!")
dot.node("P.hou", "P.hou", shape="circle", pos="0,1!")
dot.node("P.soc", "P.soc", shape="circle", pos="2,3!")
dot.node("S.rel", "S.rel", shape="circle", pos="1,2!")
dot.node("S.fin", "S.fin", shape="circle", pos="1,0!")

# Define nodes with manual positions
# dot.node("PHQsum", "PHQsum", shape="circle", pos="0,2!")
# dot.node("P.emp", "P.emp", shape="circle", pos="-2,0!")
# dot.node("P.hou", "P.hou", shape="circle", pos="-4,0!")
# dot.node("P.soc", "P.soc", shape="circle", pos="2,0!")
# dot.node("S.rel", "S.rel", shape="circle", pos="0,-2!")
# dot.node("S.fin", "S.fin", shape="circle", pos="0,-4!")

# Define edges in one go with style
# edges = [
#     ("PHQsum", "P.emp", "odot", "normal"),  # o->
#     ("PHQsum", "P.soc", "odot", "normal"),  # o->
#     ("PHQsum", "S.fin", "normal", "none"),  # <-
#     ("PHQsum", "S.rel", "normal", "normal"),  # <->

#     ("P.emp", "P.hou", "normal", "odot"),  # <-o
#     ("P.emp", "S.fin", "normal", "odot", color="green"),  # <-o

#     ("S.rel", "S.fin", "normal", "normal"),  # <->
#     ("S.rel", "P.soc", "odot", "normal"),  # o->

#     ("P.soc", "S.fin", "normal", "odot"),  # <-o
#     ("P.hou", "S.fin", "odot", "normal"),  # o->
# ]
# # Add all edges with respective arrowhead and arrowtail styles
# for src, dest, tail, head in edges:
#     dot.edge(src, dest, dir="both", arrowtail=tail, arrowhead=head)

# Define edges with custom colors
# rcot only situation
# dot.edge("PHQsum", "P.emp", dir="both", arrowtail="odot", arrowhead="normal")  # o->
# dot.edge("PHQsum", "P.soc", dir="both", arrowtail="odot", arrowhead="normal")  # o->
# dot.edge("PHQsum", "S.fin", dir="both", arrowtail="normal", arrowhead="odot", color="gray", style ="dashed", constraint="false")  # <-
# dot.edge("PHQsum", "S.rel", dir="both", arrowtail="normal", arrowhead="odot", color="gray", style ="dashed")  # <->

# dot.edge("P.emp", "P.hou", dir="both", arrowtail="normal", arrowhead="odot")  # <-o
# dot.edge("P.emp", "S.fin", dir="both", arrowtail="normal", arrowhead="normal")  # <-o

# dot.edge("S.rel", "S.fin", dir="both", arrowtail="odot", arrowhead="normal", color="gray", style ="dashed")  # <->
# # dot.edge("S.rel", "P.soc", dir="both", arrowtail="odot", arrowhead="normal")  # o->

# dot.edge("P.soc", "S.fin", dir="both", arrowtail="normal", arrowhead="odot")  # <-o
# dot.edge("P.hou", "S.fin", dir="both", arrowtail="odot", arrowhead="normal", color="gray", style ="dashed")  # o->

# # both gaussian and rcot situation
# dot.edge("PHQsum", "P.emp", dir="both", arrowtail="odot", arrowhead="normal")  # o->
# dot.edge("PHQsum", "P.soc", dir="both", arrowtail="odot", arrowhead="normal")  # o->
# dot.edge("PHQsum", "S.fin", dir="both", arrowtail="odot", arrowhead="odot", color="gray", style ="dashed", constraint="false")  # <-
# dot.edge("PHQsum", "S.rel", dir="both", arrowtail="normal", arrowhead="odot", color="gray", style ="dashed")  # <->

# dot.edge("P.emp", "P.hou", dir="both", arrowtail="normal", arrowhead="odot")  # <-o
# dot.edge("P.emp", "S.fin", dir="both", arrowtail="normal", arrowhead="odot", color="gray", style ="dashed")  # <-o

# dot.edge("S.rel", "S.fin", dir="both", arrowtail="normal", arrowhead="normal")  # <->
# dot.edge("S.rel", "P.soc", dir="both", arrowtail="odot", arrowhead="normal")  # o->

# dot.edge("P.soc", "S.fin", dir="both", arrowtail="normal", arrowhead="odot")  # <-o
# dot.edge("P.hou", "S.fin", dir="both", arrowtail="odot", arrowhead="normal")  # o->


## both gaussian and rcot situation for FCI only
# dot.edge("PHQsum", "P.emp", dir="both", arrowtail="odot", arrowhead="normal")  # o->
# dot.edge("PHQsum", "P.soc", dir="both", arrowtail="odot", arrowhead="normal")  # o->
# dot.edge("PHQsum", "S.fin", dir="both", arrowtail="normal", arrowhead="none")  # <-
# dot.edge("PHQsum", "S.rel", dir="both", arrowtail="normal", arrowhead="normal")  # <->

# dot.edge("P.emp", "P.hou", dir="both", arrowtail="normal", arrowhead="odot")  # <-o
# dot.edge("P.emp", "S.fin", dir="both", arrowtail="normal", arrowhead="odot")  # <-o

# dot.edge("S.rel", "S.fin", dir="both", arrowtail="normal", arrowhead="normal")  # <->
# dot.edge("S.rel", "P.soc", dir="both", arrowtail="odot", arrowhead="normal")  # o->

# dot.edge("P.soc", "S.fin", dir="both", arrowtail="normal", arrowhead="tee", style="dashed")  # <-o
# dot.edge("P.hou", "S.fin", dir="both", arrowtail="odot", arrowhead="normal")  # o->
# # Render and view the graph
# dot.render("FCI_depsum", view=True)


## both gaussian and rcot situation for CCI only
dot.edge("PHQsum", "P.emp", dir="both", arrowtail="tee", arrowhead="normal", style="dashed")  # o->
dot.edge("PHQsum", "P.soc", dir="both", arrowtail="odot", arrowhead="normal")  # o->
dot.edge("PHQsum", "S.fin", dir="both", arrowtail="tee", arrowhead="normal", style="dashed")  # <-
dot.edge("PHQsum", "S.rel", dir="both", arrowtail="normal", arrowhead="tee", style="dashed")  # <->

dot.edge("P.emp", "P.hou", dir="both", arrowtail="normal", arrowhead="odot")  # <-o
dot.edge("P.emp", "S.fin", dir="both", arrowtail="normal", arrowhead="normal")  # <-o

dot.edge("S.rel", "S.fin", dir="both", arrowtail="normal", arrowhead="normal")  # <->
dot.edge("S.rel", "P.soc", dir="both", arrowtail="odot", arrowhead="normal")  # o->

dot.edge("P.soc", "S.fin", dir="both", arrowtail="normal", arrowhead="odot")  # <-o
dot.edge("P.hou", "S.fin", dir="both", arrowtail="tee", arrowhead="normal", style="dashed")  # o->
# Render and view the graph
dot.render("CCI_depsum", view=True)