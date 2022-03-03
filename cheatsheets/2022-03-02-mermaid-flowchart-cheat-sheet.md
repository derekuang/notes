---
tags: mermaid flowchart
---

# Flowchart

## 1. Grammar

**flowchart** `Orientation`

​	**subgraph** `Name`

​	    **direction** `Orientation`

​		`Node` `Link` `Node`

​	**end**

​	`Node` `Link` `Node`

## 2. Orientations

| Orientation |        Meaning        |
| :---------: | :-------------------: |
|     TB      |     top to bottom     |
|     TD      | top-down / same as TB |
|     BT      |     bottom to top     |
|     LR      |     left to right     |
|     RL      |     right to left     |

## 3. Name

...

## 4. Node

`Id` `Box(Text)`

| Box Type |      Meaning      |
| :------: | :---------------: |
|    []    |      normal       |
|   ( )    |    round edges    |
|  ([ ])   |      stadium      |
|  [[ ]]   |    subroutine     |
|  [( )]   |    cylindrical    |
|  (( ))   |      circle       |
|   > ]    |    asymmetric     |
|   { }    |      rhombus      |
|  {{ }}   |      hexagon      |
|  [/ /]   |   parallelogram   |
|  [\ \\]  | parallelogram alt |
|  [/ \\]  |     trapezoid     |
|  [\\ /]  |   trapezoid alt   |

e.g.

```mermaid
flowchart LR
  subgraph p3
	11[\parallelogram-alt\]
    12[/trapezoid\]
    13[\trapezoid-alt/]
  end
  subgraph p2
    6((circle))
    7>asymmetric]
    8{rhombus}
    9{{hexagon}}
    10[/parallelogram/]
  end
  subgraph p1
    1[normal]
	2(round-edges)
	3([stadium])
	4[[subroutine]]
	5[(cylindrical)]
  end
```

## 5. Link

`Node1` & `Node2` `Link` `Node3`&`Node4`...

| Normal Link Type |   Meaning   |
| :--------------: | :---------: |
|       -->        | arrow link  |
|       ---        |  open link  |
|       -.->       | dotted link |
|       ==>        | thick link  |

e.g.

```mermaid
flowchart LR
  A --> B
  A ----- B
  A -.-> B
  A ==> B
  C --o D
  C ----x D
  C o--o D
  C <--> D
  C x--x D
```

## 6. Other Example

```mermaid
flowchart TB
    sq[Square shape] --> ci((Circle shape))

    subgraph A
        od>Odd shape]-- Two line<br/>edge comment --> ro
        di{Diamond with <br/> line break} -.-> ro(Rounded<br>square<br>shape)
        di==>ro2(Rounded square shape)
    end

    %% Notice that no text in shape are added here instead that is appended further down
    e --> od3>Really long text with linebreak<br>in an Odd shape]

    %% Comments after double percent signs
    e((Inner / circle<br>and some odd <br>special characters)) --> f(,.?!+-*ز)

    cyr[Cyrillic]-->cyr2((Circle shape Начало));

     classDef green fill:#9f6,stroke:#333,stroke-width:2px;
     classDef orange fill:#f96,stroke:#333,stroke-width:4px;
     class sq,e green
     class di orange
```



