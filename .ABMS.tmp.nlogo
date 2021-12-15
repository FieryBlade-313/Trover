extensions [array csv]

breed [searchers searcher]

searchers-own [heuristics at-peak?]

globals [
  initial-index terrain-points heuristic-permutation best-heuristics-pool debug-current-heuristics current-agent-index
  group-position group-score current-group group-stable? dta? every-group-done? CSV-OUTPUT
]

to-report get-patch-by-index [index]
  report patch index 0
end

to setup-default-searchers-parameters

  let initial-patch get-patch-by-index initial-index

  ask searchers [

    set color red
    set shape "circle"
    move-to initial-patch
    set heuristics []
    set at-peak? false
  ]

end

to setup-searchers-heuristics
  let heuristics-size 3
  if heuristics-size > max-heuristics-value [
    show "Warning: heuristics-size should NOT be greater than max-heuristics-value"
    ask searchers [
      set heuristics ( shuffle n-values max-heuristics-value [i -> i] )
    ]
    stop
  ]


  ask searchers [

    foreach n-values heuristics-size [i -> i] [
      index ->

      let new-heuristics-val ( random max-heuristics-value + 1 )
      while [ member? new-heuristics-val heuristics ] [
        set new-heuristics-val ( random max-heuristics-value + 1 )
      ]

      set heuristics insert-item index heuristics new-heuristics-val
    ]
  ]

end

to setup-searcher [amount]
  create-searchers amount
  setup-default-searchers-parameters
  setup-searchers-heuristics
end

to setup
  clear-all
  set initial-index 0
  set current-group []
  set dta? false
  set every-group-done? false
  setup-terrain
  set group-score first terrain-points
  set-patch-shades
  ;; setup-searcher searcher-amount
  reset-ticks
end

to setup-terrain
  set terrain-points n-values terrain-size [0]
  let nextmove 0
  let current 0
  let next 0
  let ran 0
  let nval 0
  let nex 0
  let diff 0

  while [ current < terrain-size - 1]  [
   ifelse (current + nextmove) < terrain-size
    [
      set terrain-points replace-item (current + nextmove) terrain-points (random-float height)
      if(next - current) > 1[
        set ran next - current - 1
       set nval item current terrain-points
        set nex current  + 1
        set diff (item next terrain-points - item current terrain-points) / (next - current)
        foreach n-values ran [j -> j] [
          j -> set terrain-points  replace-item nex  terrain-points (nval + diff )
          set nval item nex terrain-points
          set nex nex + 1
        ]
      ]
      set current (current + nextmove)
    ]
    [
       set ran (terrain-size) - current - 1
       set nval item current terrain-points
       set nex current  + 1
       set diff (item 0 terrain-points - item current terrain-points) / ((terrain-size) - current)
       foreach n-values ran [j -> j] [
         j -> set terrain-points  replace-item nex  terrain-points (nval + diff )
         set nval item nex terrain-points
         set nex nex + 1
         set current (terrain-size - 1)
    ]]
    set nextmove random (2 * smoothing-factor) + 1
    set next (current + nextmove)
  ]

end

to set-patch-shades
  resize-world (0) (terrain-size - 1 ) 0 0
  foreach n-values terrain-size [k -> k][
   k -> ask patch k 0 [ set pcolor scale-color black (item k terrain-points) 0 height]
  ]
end

to move-searcher [target-index]
  move-to get-patch-by-index target-index
end

to go-searcher
  if not at-peak?
  [
    let curr-index pxcor
  let found false
  let heuristics-index 0
  let heuristics-size length heuristics

  while [ not found ] [

    let jump-size item heuristics-index heuristics


    let forward-index ( curr-index + jump-size ) mod terrain-size
    let backward-index ( curr-index - jump-size ) mod terrain-size

    let max-val-index forward-index


    if item max-val-index terrain-points < item backward-index terrain-points [
      set max-val-index backward-index

    ]

    if item max-val-index terrain-points > item curr-index terrain-points or stochasticity? and ( random 100 + 1 ) <= error-probability  [
      move-searcher max-val-index
      set found true
    ]

    set heuristics-index heuristics-index + 1

    if heuristics-index = heuristics-size [
      set found true
      set at-peak? true
      ;;show "DEBUG: No Better point in sight"
    ]


  ]
  ]
end

to-report check-for-repeat [ lst ]
  let prev-len length lst
  set lst remove-duplicates lst
  ifelse prev-len != length lst
    [ report true]
    [ report false]
end

to-report increment-heuristics [current-heuristics]
  let carry 1
  let list-size length current-heuristics
  let i list-size - 1
  while [ i >= 0 ]
  [
    let new-value item i current-heuristics + carry
    set carry 0
    if new-value = max-heuristics-value + 1
    [
      set new-value 1
      set carry 1
    ]

    set current-heuristics replace-item i current-heuristics new-value
    set i i - 1
  ]

  report current-heuristics
end

to-report generate-next-heuristics [current-heuristics]
  set current-heuristics increment-heuristics current-heuristics
  loop
  [
    if not check-for-repeat current-heuristics
    [
      report current-heuristics
    ]
    set current-heuristics increment-heuristics current-heuristics
  ]
end

to-report take [n xs]
  report sublist xs 0 min list n (length xs)
end

to-report get-terminal-heuristics-by-size [list-size]
  report n-values ifelse-value list-size < max-heuristics-value [list-size] [max-heuristics-value] [i -> max-heuristics-value - i]
end

to-report get-initial-heuristics-by-size [list-size]
  report n-values ifelse-value list-size < max-heuristics-value [list-size] [max-heuristics-value] [i -> i + 1]
end

to-report list-equal [list1 list2]
  if length list1 != length list2 [report false]
  let i length list1 - 1
  while [ i >= 0 ]
  [
    if item i list1 != item i list2 [report false]
    set i i - 1
  ]
  report true
end

to update-searchers-heuristics [target-heuristics]
  ask searchers [ set heuristics target-heuristics ]
end

to reset-searchers-position [searchers-list]
  let list-size length searchers-list
  let i 0
  while[ i < list-size ]
  [
    ask item i searchers-list [
      move-searcher i
      set at-peak? false
   ]
    set i i + 1
  ]
end

to-report get-heuristics-average-score [current-heuristics]
  update-searchers-heuristics current-heuristics
  while [count searchers with [at-peak?] != count searchers]
  [
    ask searchers [ go-searcher ]
  ]
  report mean [item pxcor terrain-points] of searchers
end

to insert-score [current-heuristics score]
  let best-pool-size length best-heuristics-pool
  let i 0
  while [i < best-pool-size]
  [
    if last item i best-heuristics-pool < score
    [
      set best-heuristics-pool insert-item i best-heuristics-pool (list current-heuristics score)
      if best-pool-size = team-size
      [
        set best-heuristics-pool remove-item team-size best-heuristics-pool
      ]
      stop
    ]

    set i i + 1
  ]
end

to-report get-best-pool-mean
  let sum-value 0
  let i 0
  let pool-size length best-heuristics-pool
  while [ i < pool-size ]
  [
    set sum-value sum-value + last item i best-heuristics-pool
    set i i + 1
  ]
  report sum-value / pool-size
end

to generate-best-heuristics
  set best-heuristics-pool (list list ([]) (0))
  let temp-stochasticity? stochasticity?
  set stochasticity? false
  setup-searcher terrain-size
  let searchers-list [self] of searchers
  let list-size 3
  let current-heuristics get-initial-heuristics-by-size list-size
  let terminal-heuristics get-terminal-heuristics-by-size list-size

  loop [
    set debug-current-heuristics current-heuristics
    reset-searchers-position searchers-list
    let score get-heuristics-average-score current-heuristics
    insert-score current-heuristics score
    ;; update-plots

    if list-equal current-heuristics terminal-heuristics
    [
      set stochasticity? temp-stochasticity?
      show "Best Heuristics calculated"
      stop
    ]

    set current-heuristics generate-next-heuristics current-heuristics
  ]
end

to create-group [elite-amount]
  ask searchers [die]
  set current-agent-index 0
  set group-position 0
  set group-score first terrain-points
  set group-stable? false
  ask searchers [ set size 0 ]
  setup-searcher team-size
  let group [self] of searchers
  let i 0
  while [i < elite-amount]
  [
    ask item i group [set heuristics first item i best-heuristics-pool]
    set i i + 1
  ]

  ask first group [set size 1]
  set current-group shuffle group
end

to create-and-go-every-groups
  set dta? false
  set CSV-OUTPUT []
  set expert-amount 0
  let diverse-score 0
  let expert-score 0
  let csv-end-index 0
  foreach n-values (team-size + 1) [i -> i]
  [
    i ->
    set expert-amount i
    create-group expert-amount
    set CSV-OUTPUT insert-item csv-end-index CSV-OUTPUT (list group-score expert-amount)
    set csv-end-index csv-end-index + 1
    while[not group-stable?] [
      go-group current-group
      set CSV-OUTPUT insert-item csv-end-index CSV-OUTPUT (list group-score expert-amount)
      set csv-end-index csv-end-index + 1
    ]
    if i = 0
    [
      set diverse-score group-score
    ]
    if i = team-size
    [
      set expert-score group-score
    ]
    show (word "group " i " done")
  ]

  if diverse-score >= expert-score
  [
    set dta? true
  ]
  csv:to-file (word "./Exports/in_between_values_" remove ":" date-and-time ".csv") CSV-OUTPUT
  set every-group-done? true

end

to go-relay [agent]
  ask agent [go-searcher]
end

to-report get-agent-best-location [agent]
  let i 0
  let list-size length [heuristics] of agent
  let curr-index [pxcor] of agent
  while [i < list-size]
  [
    let jump-size item i [heuristics] of agent


    let forward-index ( curr-index + jump-size ) mod terrain-size
    let backward-index ( curr-index - jump-size ) mod terrain-size

    let max-val-index forward-index


    if item max-val-index terrain-points < item backward-index terrain-points [
      set max-val-index backward-index
    ]

    if item max-val-index terrain-points > item curr-index terrain-points or stochasticity? and ( random 100 + 1 ) <= error-probability [
      report max-val-index
    ]

    set i i + 1
  ]
  report [pxcor] of agent
end

to go-tournament [group]
  let i 0
  let best-suggestion group-position
  while [i < team-size]
  [
    let agent-best-point get-agent-best-location item i group
    if item best-suggestion terrain-points < item agent-best-point terrain-points
    [
      set best-suggestion agent-best-point
    ]
    set i i + 1
  ]

  set group-position best-suggestion
  ask searchers [move-searcher group-position]
end

to go-group [group]
  ifelse relay-mode?
  [
    if current-agent-index < team-size
    [
      let current-agent item current-agent-index group
      go-relay current-agent
      set group-position [pxcor] of current-agent
      set group-score item [pxcor] of current-agent terrain-points
      if [at-peak?] of current-agent
      [
        set current-agent-index current-agent-index + 1
        if current-agent-index = team-size
        [
          set group-stable? true
          stop
        ]
        ask current-agent [set size 0]
        let next-agent item current-agent-index group
        ask next-agent [
          set size 1
          move-searcher group-position
        ]
      ]
    ]
  ]
  [
    let prev-position group-position
    go-tournament group
    set group-score item group-position terrain-points
    if prev-position = group-position
    [
      set group-stable? true
      stop
    ]
  ]
  tick
end
@#$#@#$#@
GRAPHICS-WINDOW
13
11
3321
31
-1
-1
11.0
1
10
1
1
1
0
1
1
1
0
299
0
0
0
0
1
ticks
30.0

SLIDER
12
40
184
73
terrain-size
terrain-size
0
2000
300.0
50
1
NIL
HORIZONTAL

BUTTON
3
235
107
268
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
13
82
186
115
smoothing-factor
smoothing-factor
0
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
13
125
186
158
height
height
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
211
40
383
73
max-heuristics-value
max-heuristics-value
1
15
12.0
1
1
NIL
HORIZONTAL

MONITOR
242
190
355
235
Searchers at Peak
count searchers with [ at-peak? ]
0
1
11

MONITOR
365
190
528
235
Current Heuristics [DEBUG]
debug-current-heuristics
17
1
11

SLIDER
396
40
568
73
team-size
team-size
3
9
9.0
1
1
NIL
HORIZONTAL

BUTTON
4
275
171
308
Generate Best Heuristics
generate-best-heuristics
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
244
247
355
292
Best Agents Count
length best-heuristics-pool
17
1
11

PLOT
623
191
959
422
plot 1
iterations
Score
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"group-score" 1.0 0 -2674135 true "" "plot group-score"
"expert-no" 1.0 1 -13840069 true "" "plot expert-amount * 10"

MONITOR
542
190
612
235
Pool mean
get-best-pool-mean
0
1
11

SWITCH
12
190
136
223
relay-mode?
relay-mode?
1
1
-1000

MONITOR
143
190
232
235
Current Mode
ifelse-value relay-mode? [\"Relay\"] [\"Tournament\"]
0
1
11

MONITOR
617
43
731
88
Group Score Value
group-score
2
1
11

BUTTON
5
361
88
394
Go Grouo
go-group current-group
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
399
89
571
122
expert-amount
expert-amount
0
team-size
9.0
1
1
NIL
HORIZONTAL

BUTTON
5
319
125
352
Generate Group
create-group expert-amount
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
133
319
274
352
Generate all Groups
create-and-go-every-groups
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
402
131
533
164
stochasticity?
stochasticity?
0
1
-1000

SLIDER
539
131
711
164
error-probability
error-probability
0
100
8.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="param-sweep-1" repetitions="1" runMetricsEveryStep="true">
    <setup>steup
generate-best-heuristics</setup>
    <go>create-and-go-every-groups</go>
    <metric>group-score</metric>
    <metric>dta?</metric>
    <steppedValueSet variable="max-heuristics-value" first="5" step="5" last="15"/>
    <enumeratedValueSet variable="height">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain-size">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="error-probability" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="team-size">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relay-mode?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expert-amount">
      <value value="6"/>
    </enumeratedValueSet>
    <steppedValueSet variable="smoothing-factor" first="0" step="4" last="20"/>
  </experiment>
  <experiment name="param-sweep-test" repetitions="2" runMetricsEveryStep="false">
    <setup>setup
generate-best-heuristics</setup>
    <go>create-and-go-every-groups</go>
    <exitCondition>every-group-done?</exitCondition>
    <metric>group-score</metric>
    <metric>dta?</metric>
    <metric>expert-amount</metric>
    <enumeratedValueSet variable="max-heuristics-value">
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="height">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="terrain-size">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="error-probability">
      <value value="0"/>
      <value value="2"/>
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="team-size">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochasticity?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relay-mode?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="expert-amount">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="smoothing-factor">
      <value value="0"/>
      <value value="5"/>
      <value value="10"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
