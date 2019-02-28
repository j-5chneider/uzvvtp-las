library(DiagrammeR)


grViz("
  digraph boxes_and_circles {

    # a 'graph' statement
    graph [overlap = true, fontsize = 10]

    # several 'node' statements
    node [shape = circle,
          fontname = Helvetica]
    f1; f2; f3; f4; f5; f6; f7
    {rank = same; f1; f2; f3; f4; f5; f6; f7}

    node [shape = box,
          fixedsize = true,
          width = 0.9] // sets as boxes
    i1_1; i1_2; i1_3; i1_4; i1_5; i2_1; i2_2; i2_3; i2_4; i2_5; i3_1; i3_2; i3_3; i3_4; i3_5; i4_1; i4_2; i4_3; i4_4; i4_5; i5_1; i5_2; i5_3; i5_4; i5_5; i6_1; i6_2; i6_3; i6_4; i6_5; i7_1; i7_2; i7_3; i7_4; i7_5
    {rank = same; i1_1; i1_2; i1_3; i1_4; i1_5; i2_1; i2_2; i2_3; i2_4; i2_5; i3_1; i3_2; i3_3; i3_4; i3_5; i4_1; i4_2; i4_3; i4_4; i4_5; i5_1; i5_2; i5_3; i5_4; i5_5; i6_1; i6_2; i6_3; i6_4; i6_5; i7_1; i7_2; i7_3; i7_4; i7_5}

    # several 'edge' statements
    f1-> {i1_1 i1_2 i1_3 i1_4 i1_5} [label='.573']
    f2-> {i2_1 i2_2 i2_3 i2_4 i2_5} [label='.573']
    f3-> {i3_1 i3_2 i3_3 i3_4 i3_5} [label='.573']
    f4-> {i4_1 i4_2 i4_3 i4_4 i4_5} [label='.573']
    f5-> {i5_1 i5_2 i5_3 i5_4 i5_5} [label='.573']
    f6-> {i6_1 i6_2 i6_3 i6_4 i6_5} [label='.573']
    f7-> {i7_1 i7_2 i7_3 i7_4 i7_5} [label='.573']

}
")

grViz("
  digraph boxes_and_circles {

    # a 'graph' statement
    graph [overlap = true, fontsize = 10]

     # several 'node' statements
    node [shape = circle,
          fontname = Helvetica]
    f1 [group=g1]; f2 [group=g2]; f3 [group=g3]; f4 [group=g4]; f5 [group=g5]; f6 [group=g6]; f7 [group=g7]
    {rank = same; f1; f2; f3; f4; f5; f6; f7}

    node [shape = box,
          fixedsize = true,
          width = 0.9] // sets as boxes
    i1_1 [group=g1]; i1_2 [group=g1]; i1_3 [group=g1]; i1_4 [group=g1]; i1_5 [group=g1]; 
    i2_1 [group=g2]; i2_2 [group=g2]; i2_3 [group=g2]; i2_4 [group=g2]; i2_5 [group=g2]; 
    i3_1 [group=g3]; i3_2 [group=g3]; i3_3 [group=g3]; i3_4 [group=g3]; i3_5 [group=g3]; 
    i4_1 [group=g4]; i4_2 [group=g4]; i4_3 [group=g4]; i4_4 [group=g4]; i4_5 [group=g4]; 
    i5_1 [group=g5]; i5_2 [group=g5]; i5_3 [group=g5]; i5_4 [group=g5]; i5_5 [group=g5]; 
    i6_1 [group=g6]; i6_2 [group=g6]; i6_3 [group=g6]; i6_4 [group=g6]; i6_5 [group=g6]; 
    i7_1 [group=g7]; i7_2 [group=g7]; i7_3 [group=g7]; i7_4 [group=g7]; i7_5 [group=g7]
    {rank = same; i1_1; i1_2; i1_3; i1_4; i1_5; i2_1; i2_2; i2_3; i2_4; i2_5; i3_1; i3_2; i3_3; i3_4; i3_5; i4_1; i4_2; i4_3; i4_4; i4_5; i5_1; i5_2; i5_3; i5_4; i5_5; i6_1; i6_2; i6_3; i6_4; i6_5; i7_1; i7_2; i7_3; i7_4; i7_5}

    # several 'edge' statements
    f1-> {i1_1 i1_2 i1_3 i1_4 i1_5} [label='.573']
    f2-> {i2_1 i2_2 i2_3 i2_4 i2_5} [label='.573']
    f3-> {i3_1 i3_2 i3_3 i3_4 i3_5} [label='.573']
    f4-> {i4_1 i4_2 i4_3 i4_4 i4_5} [label='.573']
    f5-> {i5_1 i5_2 i5_3 i5_4 i5_5} [label='.573']
    f6-> {i6_1 i6_2 i6_3 i6_4 i6_5} [label='.573']
    f7-> {i7_1 i7_2 i7_3 i7_4 i7_5} [label='.573']
    
    f1-> f2 [dir = both label = .6]
    f1-> f3 [dir = both label = .4]
    f1-> f4 [dir = both label = .2]
    f1-> f5 [dir = both label = 0]
    f1-> f6 [dir = both label = -.2]
    f1-> f7 [dir = both label = -.4]
}
")


grViz("
  digraph boxes_and_circles {

    # a 'graph' statement
    graph [overlap = true, fontsize = 10]

    # several 'node' statements
    node [shape = circle,
          fontname = Helvetica]
    f1 [group=g1]; f2 [group=g2]; f3 [group=g3]; f4 [group=g4]; f5 [group=g5]; f6 [group=g6]; f7 [group=g7]
    {rank = same; f1; f2; f3; f4; f5; f6; f7}

    node [shape = box,
          fixedsize = true,
          width = 0.9] // sets as boxes
    i1_1 [group=g1]; i1_2 [group=g1]; i1_3 [group=g1]; i1_4 [group=g1]; i1_5 [group=g1]; 
    i2_1 [group=g2]; i2_2 [group=g2]; i2_3 [group=g2]; i2_4 [group=g2]; i2_5 [group=g2]; 
    i3_1 [group=g3]; i3_2 [group=g3]; i3_3 [group=g3]; i3_4 [group=g3]; i3_5 [group=g3]; 
    i4_1 [group=g4]; i4_2 [group=g4]; i4_3 [group=g4]; i4_4 [group=g4]; i4_5 [group=g4]; 
    i5_1 [group=g5]; i5_2 [group=g5]; i5_3 [group=g5]; i5_4 [group=g5]; i5_5 [group=g5]; 
    i6_1 [group=g6]; i6_2 [group=g6]; i6_3 [group=g6]; i6_4 [group=g6]; i6_5 [group=g6]; 
    i7_1 [group=g7]; i7_2 [group=g7]; i7_3 [group=g7]; i7_4 [group=g7]; i7_5 [group=g7]
    {rank = same; i1_1; i1_2; i1_3; i1_4; i1_5; i2_1; i2_2; i2_3; i2_4; i2_5; i3_1; i3_2; i3_3; i3_4; i3_5; i4_1; i4_2; i4_3; i4_4; i4_5; i5_1; i5_2; i5_3; i5_4; i5_5; i6_1; i6_2; i6_3; i6_4; i6_5; i7_1; i7_2; i7_3; i7_4; i7_5}

    # several 'edge' statements
    f1-> {i1_1 i1_2 i1_3 i1_4 i1_5} [label='.573']
    f2-> {i2_1 i2_2 i2_3 i2_4 i2_5} [label='.573']
    f3-> {i3_1 i3_2 i3_3 i3_4 i3_5} [label='.573']
    f4-> {i4_1 i4_2 i4_3 i4_4 i4_5} [label='.573']
    f5-> {i5_1 i5_2 i5_3 i5_4 i5_5} [label='.573']
    f6-> {i6_1 i6_2 i6_3 i6_4 i6_5} [label='.573']
    f7-> {i7_1 i7_2 i7_3 i7_4 i7_5} [label='.573']
    
    f1-> f2 [dir = both label = .6]
    f1-> f3 [dir = both label = .4]
    f1-> f4 [dir = both label = .2]
    f1-> f5 [dir = both label = 0]
    f1-> f6 [dir = both label = -.2]
    f1-> f7 [dir = both label = -.4]
    
    f2-> f3 [dir = both label = .6]
    f2-> f4 [dir = both label = .4]
    f2-> f5 [dir = both label = .2]
    f2-> f6 [dir = both label = 0]
    f2-> f7 [dir = both label = -.2]

    f3-> f4 [dir = both label = .6]
    f3-> f5 [dir = both label = .4]
    f3-> f6 [dir = both label = .2]
    f3-> f7 [dir = both label = 0]
    
    f4-> f5 [dir = both label = .6]
    f4-> f6 [dir = both label = .4]
    f4-> f7 [dir = both label = .2]
    
    f5-> f6 [dir = both label = .6]
    f5-> f7 [dir = both label = .4]
    
    f6-> f7 [dir = both label = .6]
    
    
}
")
