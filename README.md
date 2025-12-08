# AoC-2025

This is my personal rule for the 12 days:
Every single puzzle of Advent of Code 2025 will be solved exclusively in Neo4j Cypher, made easier by the brand-new CYPHER 25 and the occasional APOC / GDS helper when it really shines. No Python, no JavaScript, no external code — the database does everything.

## Day 1

[blog](https://medium.com/@pierre.halftermeyer/aoc-2025-in-cypher-day-1-secret-entrance-0e8a747edafd)

### Setup

```cypher
:params {data: "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"}
```

### Part 1

```cypher
CYPHER 25
LET rots = [rot IN split($data, '\n') WHERE rot <> "" |
  CASE left(rot,1)
    WHEN 'L' THEN 100 - toInteger(substring(rot,1))
    ELSE toInteger(substring(rot,1))
  END
]
RETURN reduce(
  s = {dial: 50, counter: 0},
  delta IN rots |
  {
    dial: (s.dial + delta) % 100,
    counter: s.counter + CASE WHEN (s.dial + delta) % 100 = 0 THEN 1 ELSE 0 END
  }
).counter AS part1
```

### Part 2

```cypher
CYPHER 25
LET rots = [rot IN split($data, '\n') WHERE rot <> "" |
  {
    dir        : left(rot,1),
    distance   : toInteger(substring(rot,1)),
    full_rots  : toInteger(substring(rot,1)) / 100,
    clean_dist : toInteger(substring(rot,1)) % 100,
    motion     : CASE left(rot,1)
                   WHEN 'L' THEN 100 - (toInteger(substring(rot,1)) % 100)
                   ELSE toInteger(substring(rot,1)) % 100
                 END
  }
]
RETURN reduce(
  s = {dial: 50, counter: 0},
  r IN rots |
  {
    dial: (s.dial + r.motion) % 100,
    counter: s.counter
      + r.full_rots
      + CASE WHEN r.dir = 'L' AND 0 < s.dial < r.clean_dist THEN 1 ELSE 0 END
      + CASE WHEN r.dir = 'R' AND s.dial + r.clean_dist > 100 THEN 1 ELSE 0 END
      + CASE WHEN (s.dial + r.motion) % 100 = 0 THEN 1 ELSE 0 END
  }
).counter AS part2
```

## Day 2

[blog](https://medium.com/@pierre.halftermeyer/aoc-2025-in-cypher-day-2-gift-shop-advent-of-code-2025-ed9e57557c52)

### Setup

```cypher
:params {data: "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"}
```

### Part 1

```cypher
CYPHER 25
LET ranges_t = [rg IN split($data, ',') | [bd IN split(rg, '-') | bd] ]
LET ranges = [rg IN ranges_t | {
  first_id: toInteger(rg[0]),
  last_id:  toInteger(rg[1]),
  first_search_bd: CASE size(rg[0]) % 2
                     WHEN 0 THEN toInteger(left(rg[0], size(rg[0])/2))
                     ELSE toInteger(10^(size(rg[0])/2)) END,
  last_search_bd:  CASE size(rg[1]) % 2
                     WHEN 0 THEN toInteger(left(rg[1], size(rg[1])/2))
                     ELSE toInteger(10^(size(rg[0])/2) - 1) END
}]

RETURN reduce(part1 = 0,
  rg IN ranges |
  part1 +
  reduce(sum_doubles = 0,
    ix IN range(rg.first_search_bd, rg.last_search_bd) |
    sum_doubles +
      CASE WHEN rg.first_id <= toInteger(toString(ix) + toString(ix)) <= rg.last_id
           THEN toInteger(toString(ix) + toString(ix))
           ELSE 0 END
  )
) AS part1
```

### Part 2

```cypher
CYPHER 25
// parse input
LET ranges_t = [rg IN split($data, ',') | [bd IN split(rg, '-') | bd] ]
LET ranges = [rg IN ranges_t | {
  first_id: toInteger(rg[0]),
  last_id:  toInteger(rg[1])
}]

RETURN ranges

NEXT

// compute max bound
LET max_bd = head(collect { UNWIND ranges AS rg RETURN rg.last_id AS max_bd ORDER BY max_bd DESC LIMIT 1 })
RETURN max_bd, ranges

NEXT

// bound search space
RETURN
  max_bd,
  ranges,
  CASE size(toString(max_bd)) % 2
    WHEN 0 THEN toInteger(left(toString(max_bd), size(toString(max_bd))/2))
    ELSE toInteger(10^(size(toString(max_bd))/2) - 1) END AS last_search_bd,
  size(toString(max_bd)) AS max_length

NEXT

// brute-force find invalid keys
RETURN reduce(invalid_keys = [],
  k IN range(1, last_search_bd) |
    invalid_keys +
    reduce(k_invalid_keys = [],
      times IN range(2, toInteger(ceil(max_length / toFloat(size(toString(k)))))) |
        k_invalid_keys +
        CASE WHEN any(rg IN ranges
                 WHERE rg.first_id <= toInteger(reduce(inval="", _ IN range(1,times) | inval + toString(k))) <= rg.last_id)
          THEN [toInteger(reduce(inval="", _ IN range(1,times) | inval + toString(k)))]
          ELSE [] END
    )
) AS invalid_keys

NEXT

// sum unique keys
UNWIND invalid_keys AS ik
WITH DISTINCT ik
RETURN sum(ik) AS part2
```

## Day 3

[blog](https://medium.com/@pierre.halftermeyer/advent-of-code-2025-day-3-lobby-76281355e6ae)

### Setup

```cypher
:param {data: "987654321111111
811111111111119
234234234234278
818181911112111"}
```

### Part 1

```cypher
// Split input into individual banks (one string per line)
UNWIND reduce(
  out_joltage = [],
  bank IN [bank IN split($data, '\n') | 
    [battery IN split(bank, '') | toInteger(battery)]   // → list of integers
  ] |
  out_joltage + [
    // For each bank, find the two highest digits, preserving left-most order
    reduce(
      state = {first: 0, second: 0},                     // running best pair
      ix IN range(0, size(bank) - 2) |                  // scan all but last pos
      CASE 
        WHEN bank[ix] > state.first                  // new absolute maximum?
          THEN {
            first:  bank[ix],                             // promote it to tens place
            second: reduce(
                      sec = 0,
                      j IN range(ix+1, size(bank)-1) |    // look ahead for new second
                      CASE WHEN bank[j] > sec THEN bank[j] ELSE sec END
                    )
          }
        ELSE state                                      // nothing better found
      END
    )
  ]
) AS bank_target_digit

// Reconstruct two-digit number and sum everything
WITH bank_target_digit.first * 10 + bank_target_digit.second AS bank_joltage
RETURN sum(bank_joltage) AS part1
```

### Part 2

```cypher
// Same preprocessing: one list of integers per bank
UNWIND reduce(
  out_joltage = [],
  bank IN [bank IN split($data, '\n') | 
    [battery IN split(bank, '') | toInteger(battery)]
  ] |
  out_joltage + [
    // Build list of the 12 greedily chosen digits (with their positions)
    reduce(
      target_digit_list = [{digit: 0, ix: -1}],        // start sentinel
      jx IN range(11, 0, -1) |                         // 12 → 1 (12 times)
      target_digit_list + 
        reduce(
          target_digit = {
            digit: bank[target_digit_list[-1].ix + 1],    // first candidate after previous pick
            ix:    target_digit_list[-1].ix + 1
          },
          ix IN range(target_digit_list[-1].ix + 1, size(bank) - jx - 1) |
          CASE 
            WHEN bank[ix] > target_digit.digit            // found a better digit further right?
              THEN {digit: bank[ix], ix: ix}
            ELSE target_digit
          END
        )
    )
  ]
) AS bank_target_digit

// Convert the 12 chosen digits back into a real integer
WITH reduce(
       bank_joltage = 0,
       ix IN range(1, size(bank_target_digit) - 1) |      // skip sentinel
       bank_joltage + bank_target_digit[ix].digit * toInteger(10^(12 - ix))
     ) AS bank_joltage
RETURN sum(bank_joltage) AS part2
```

## Day 4

[blog](https://medium.com/@pierre.halftermeyer/advent-of-code-2025-day-4-printing-department-in-cypher-abaf356f6dcc?postPublishedType=initial)

### Setup

```cypher
:params {data: "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."}
```

```cypher
CREATE CONSTRAINT pos_row_coll IF NOT EXISTS FOR (p:Position) REQUIRE (p.row_ix, p.coll_ix) IS NODE KEY;
CREATE INDEX pos_mark IF NOT EXISTS FOR (p:Position) ON (p.mark);
```

### Part 1

```cypher
CYPHER 25

LET grid = [row IN split($data, '\n') | split(row, '')]
CALL (grid) {
  UNWIND range(0, size(grid)-1) AS row_ix
  UNWIND range(0, size(grid[0])-1) AS coll_ix
  MERGE (p:Position {row_ix:row_ix, coll_ix:coll_ix})
    SET p.mark = grid[row_ix][coll_ix]
  }

MATCH (p:Position)
CALL (p) {
    MATCH (neigh:Position)
    WHERE p.row_ix-1 <= neigh.row_ix <= p.row_ix+1
    AND p.coll_ix-1 <= neigh.coll_ix <= p.coll_ix+1
    AND p <> neigh
    AND elementId(p) < elementId(neigh)
    MERGE (p)-[:NEIGHBOR]->(neigh)
  }

MATCH (p:Position {mark: '@'})
WITH p, count {(p)-[:NEIGHBOR]-(x WHERE x.mark = '@')} AS neigh_num
WHERE neigh_num < 4
RETURN count(p) AS part1
```

### Part 2

```cypher
CYPHER 25
UNWIND range(1,1_000_000) AS loopx
CALL (loopx) {
  MATCH (pos:Position {mark: '@'})
  WITH pos, count {(pos)-[:NEIGHBOR]-(x WHERE x.mark = '@')} AS neigh_num
  WHERE neigh_num < 4
  SET pos.mark = 'x'
  WITH count(pos) AS num_x
  RETURN CASE num_x WHEN 0 THEN 1/0 ELSE num_x END AS num_x
} IN TRANSACTIONS OF 1 ROW
  ON ERROR BREAK
  REPORT STATUS AS s
FILTER s.committed = true
RETURN sum(num_x) AS part2
```

### Alternative GDS-based Part 2

```cypher
CYPHER 25

MATCH (source:Position {mark: '@'})-[r:NEIGHBOR]->(target:Position{mark: '@'})
RETURN gds.graph.project(
  'graph',
  source,
  target,
  {},
  { undirectedRelationshipTypes: ['*'] }
) AS g


NEXT

CALL gds.kcore.stream('graph')
YIELD nodeId, coreValue
FILTER coreValue < 4
RETURN gds.util.asNode(nodeId) AS cell, coreValue
ORDER BY coreValue ASC, cell DESC

NEXT

RETURN count(cell) AS part2
```

## Day 5

[blog](https://medium.com/@pierre.halftermeyer/advent-of-code-2025-day-5-cafeteria-in-cypher-ecdc2808cb17)

### Setup

```cypher
:params {data: "3-5
10-14
16-20
12-18

1
5
8
11
17
32"}
```

```cypher
CREATE INDEX fresh_id_range_start_end IF NOT EXISTS FOR (n:FreshIDRange) ON (n.start, n.end);
```

### Part 1

```cypher
CYPHER 25
UNWIND split($data, '\n') AS line
CALL (line) {
  WHEN line CONTAINS '-' THEN {
    LET rg = split(line, '-')
    CREATE (:FreshIDRange {start:toInteger(rg[0]), end:toInteger(rg[1])})
  }
}
RETURN count(line) AS _

NEXT

UNWIND split($data, '\n') AS line
CALL (line) {
  WHEN NOT line CONTAINS '-' AND size(line) > 0 THEN {
    MATCH ANY (rg:FreshIDRange WHERE rg.start <= toInteger(line) <= rg.end)
    RETURN line AS num, rg
    LIMIT 1
  }
}
RETURN count(rg) AS part1
```

### Part 2

#### Setup

```cypher
CYPHER 25
MATCH (rg1:FreshIDRange)
CALL (rg1) {
  MATCH (rg2:FreshIDRange)
  WHERE rg2.start <= rg1.start <= rg2.end
  AND rg1 <> rg2
  MERGE (rg1)-[:STARTS_IN]->(rg2)
}
RETURN count(rg1) AS _
```

#### pure CYPHER alternative

```cypher
CYPHER 25
UNWIND range(1,1_000_000) AS loopx
CALL (loopx) {
  MATCH (rg1)-[:STARTS_IN]->(rg2)
  FILTER rg1.start <> rg2.start OR rg1.end <> rg2.end
  LIMIT 1
  CALL (rg1, rg2) {
    UNWIND [rg1.start, rg1.end, rg2.start, rg2.end] AS bound
    WITH rg1, rg2, bound ORDER BY bound ASC
    WITH rg1, rg2, collect(bound) AS bounds
    SET
      rg1.start = bounds[0],
      rg2.start = bounds[0],
      rg1.end = bounds[3],
      rg2.end = bounds[3]
  }
  RETURN CASE count(*) WHEN 0 THEN 1/0 ELSE 1 END AS processed
} IN TRANSACTIONS OF 1 ROW
  ON ERROR BREAK
  REPORT STATUS AS s
FILTER s.committed = true
RETURN sum(processed) AS _

NEXT

MATCH (rg:FreshIDRange)
WITH DISTINCT rg.start AS start, rg.end AS end
RETURN sum(end - start + 1) AS part2
```

#### GDS alternative

```cypher
CYPHER 25

MATCH (rg1:FreshIDRange)
OPTIONAL MATCH (rg1)-[:STARTS_IN]->(rg2)
RETURN gds.graph.project('ranges', rg1, rg2, {}) AS g

NEXT

CALL gds.wcc.stream('ranges')
YIELD nodeId, componentId
WITH
  min(gds.util.asNode(nodeId).start) AS start,
  max(gds.util.asNode(nodeId).end) AS end,
  componentId
WITH componentId, end-start+1 AS fresh_id_qty
RETURN sum(fresh_id_qty) AS part2
```

## Day 6

[blog](https://medium.com/@pierre.halftermeyer/advent-of-code-2025-day-6-trash-compactor-ccffec374bf0)

### Setup

```cypher
:params {data: "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  "}
```

### Part 1

```cypher
CYPHER 25
// Split into lines and remove empty cells
LET data = [line IN split($data, '\n') 
            | [val IN split(line, ' ') WHERE size(val) > 0]]
// Convert each column of numbers into a list of integers
LET num_lists = [l IN data[0..-1] 
                 | [num IN l | toInteger(num)]]
// The bottom row contains the operators
LET sym_list = data[-1]
// Fold over problems: for each column, apply + or * across its vertical numbers
RETURN reduce(acc = 0, ix IN range(0, size(sym_list)-1) |
  acc + reduce(acc_ix = CASE sym_list[ix] WHEN '*' THEN 1 ELSE 0 END,
               num_list IN num_lists |
    CASE sym_list[ix] 
      WHEN '*' THEN acc_ix * num_list[ix] 
      ELSE acc_ix + num_list[ix] 
    END
  )
) AS part1
```

### Part 2

```cypher
CYPHER 25
// Treat each character as a separate cell
LET data = [line IN split($data, '\n') | split(line, '')]
// Reconstruct numbers by reading columns right-to-left, top-to-bottom
LET num_list = reduce(l = [],
     ix IN range(size(data[0])-1, 0, -1) |
  l + [toInteger(reduce(num = "",
        num_line IN data[0..-1] |
    num + CASE WHEN num_line[ix] =~ "[0-9]"  
          THEN num_line[ix] ELSE "" END
  ))]
)
// Operators are the bottom row, but read right-to-left
LET sym_list = reverse(data[-1])
// Build a flat list: number, number, ..., operator, number, number, ..., operator...
LET homework = [ix IN range(0, size(num_list)-1) 
                | CASE WHEN num_list[ix] IS NULL 
                  THEN sym_list[ix-1] 
                  ELSE num_list[ix] END] + [sym_list[-1]]
// Single-pass stack machine: push numbers, apply operator when seen
RETURN reduce(state = {acc: 0, stack: []},
     el IN homework |
  CASE el
    WHEN "*" THEN {acc: state.acc + reduce(ac=1,  n IN state.stack | ac * n), stack: []}
    WHEN "+" THEN {acc: state.acc + reduce(ac=0,  n IN state.stack | ac + n), stack: []}
    ELSE          {acc: state.acc,               stack: state.stack + [el]}
  END
).acc AS part2
```

## Day 7

[blog](https://medium.com/@pierre.halftermeyer/advent-of-code-2025-in-cypher-day-7-laboratories-cf68d1dc3fd5)

### Setup

```cypher
:params {data: ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."}
```

```cypher
CREATE CONSTRAINT loc_row_coll IF NOT EXISTS FOR (l:Location) REQUIRE (l.row_ix, l.coll_ix) IS NODE KEY;
CREATE INDEX loc_mark IF NOT EXISTS FOR (l:Location) ON (l.mark);
```

```cypher
// Create Nodes
CYPHER 25

LET grid = [row IN split($data, '\n') | [cell IN split(row, '')]]
CALL (grid) {
  UNWIND range(0, size(grid)-1) AS row_ix
  UNWIND range(0, size(grid[0])-1) AS coll_ix
  CREATE (l:Location {row_ix:row_ix, coll_ix:coll_ix, mark: grid[row_ix][coll_ix]})
  }
RETURN count(*) AS one_row
```

```cypher
// Create Relationships (part 1 only so better run next one)
CYPHER 25

UNWIND range(1,1_000_000) AS loopx
CALL (loopx) {
  MATCH (l:Location&!Processed)
  WHERE (l.mark = 'S' OR EXISTS {()-[:BEAM_CONTINUES_TO]->(l)})
  CALL (l) {
      WHEN l.mark IN ['S', '.'] THEN {
      SET l:Processed
      MATCH (neigh:Location {row_ix:l.row_ix+1, coll_ix:l.coll_ix})
      CREATE (l)-[:BEAM_CONTINUES_TO]->(neigh)
      
      }
      ELSE {
      SET l:Processed
      MATCH (neigh:Location WHERE neigh.row_ix = l.row_ix+1 AND neigh.coll_ix IN [l.coll_ix-1, l.coll_ix+1])
      CREATE (l)-[:BEAM_CONTINUES_TO]->(neigh)
      }
  }
  RETURN CASE count(l) WHEN 0 THEN 1/0 ELSE count(l) END AS processed
}  IN TRANSACTIONS OF 1 ROW
ON ERROR BREAK
RETURN sum(processed) AS locations_processed
```

```cypher
// Create Relationships and count upstream splitted words (for part 1 anc part 2)
CYPHER 25

UNWIND range(1,1_000_000) AS loopx
CALL (loopx) {
  MATCH (l:Location&!Processed)
  WHERE (l.mark = 'S' OR EXISTS {()-[:BEAM_CONTINUES_TO]->(l)})
  CALL (l) {
      WHEN l.mark='S' THEN {
      SET l:Processed
      MATCH (neigh:Location {row_ix:l.row_ix+1, coll_ix:l.coll_ix})
      CREATE (l)-[:BEAM_CONTINUES_TO {splitted: 1}]->(neigh)
      }
      WHEN l.mark='.' THEN {
      SET l:Processed
      MATCH (neigh:Location {row_ix:l.row_ix+1, coll_ix:l.coll_ix})
      CREATE (l)-[:BEAM_CONTINUES_TO {
        splitted: reduce ( acc=0, s IN COLLECT {
          MATCH ()-[b:BEAM_CONTINUES_TO]->(l) RETURN b.splitted
        } | acc+s)
      }]->(neigh)
      }
      ELSE {
      SET l:Processed
      MATCH (neigh:Location WHERE neigh.row_ix = l.row_ix+1 AND neigh.coll_ix IN [l.coll_ix-1, l.coll_ix+1])
      CREATE (l)-[:BEAM_CONTINUES_TO {
        splitted: reduce ( acc=0, s IN COLLECT {
          MATCH ()-[b:BEAM_CONTINUES_TO]->(l) RETURN b.splitted
        } | acc+s)
      }]->(neigh)
      }
  }
  RETURN CASE count(l) WHEN 0 THEN 1/0 ELSE count(l) END AS processed
}  IN TRANSACTIONS OF 1 ROW
ON ERROR BREAK
RETURN sum(processed) AS locations_processed
```

### Part 1

```cypher
MATCH(l:Location {mark:'^'})
WHERE l:Processed
RETURN count(l) AS part1
```

### Part 2

```cypher
CYPHER 25
MATCH (l:Location)
RETURN max(l.row_ix) AS max_row_ix

NEXT

MATCH (end:Location {row_ix: max_row_ix})
RETURN sum (reduce ( acc=0, s IN COLLECT {
          MATCH ()-[b:BEAM_CONTINUES_TO]->(end) RETURN b.splitted
        } | acc+s) ) AS part2
```

## Day 8

[blog](https://medium.com/@pierre.halftermeyer/advent-of-code-2025-in-cypher-day-8-playground-173f473bd9fa)

### Setup

```cypher
// iter = 1000 on non-test input
:params {iter: 10, data: "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"}
```

```cypher
CREATE POINT INDEX jbox_point_index IF NOT EXISTS
FOR (j:JBox) ON (j.position)
```

```cypher
CYPHER 25
LET points = [row IN split($data, '\n') | [coord IN split(row, ',')| toInteger(coord)]]
LET points_point = [xyz_t IN points | point({x:xyz_t[0], y:xyz_t[1], z:xyz_t[2]})]
UNWIND points_point AS jbox_pos
CREATE (:JBox {pos: jbox_pos});
```

```cypher
CYPHER 25
MATCH (jb1:JBox), (jb2:JBox)
WHERE elementId(jb1) < elementId(jb2)
ORDER BY point.distance(jb1.pos, jb2.pos) ASC
LIMIT $iter
CALL (jb1, jb2){
MATCH (jb1)-[:SAME_CC_AS]->*(root1)
  WHERE NOT EXISTS {(root1)-[:SAME_CC_AS]->()}
MATCH (jb2)-[:SAME_CC_AS]->*(root2)
  WHERE NOT EXISTS {(root2)-[:SAME_CC_AS]->()}
  CALL (root1, root2) {
    WHEN root1 <> root2 THEN {
      CREATE (root1)-[:SAME_CC_AS]->(root2)
    }
  }
}
```

### Part 1

```cypher
MATCH (jb:JBox)-[:SAME_CC_AS]->*(root)
WHERE NOT EXISTS {(root)-[:SAME_CC_AS]->()}
WITH root, count(jb) AS cc_size
ORDER BY cc_size DESC LIMIT 3
RETURN reduce(acc=1, s IN collect(cc_size) | acc*s) AS part1
```

### Part 2

```cypher
CYPHER 25
MATCH (jb1:JBox), (jb2:JBox)
WHERE elementId(jb1) < elementId(jb2)
ORDER BY point.distance(jb1.pos, jb2.pos) ASC
CALL (jb1, jb2){
  WHEN COUNT {
      MATCH (cc:JBox) WHERE NOT EXISTS {(cc)-[:SAME_CC_AS]->()}
    } = 1
    THEN {
      CREATE (:X {err_prop: 1/0})
    }
    ELSE {
      MERGE (xs:LastXsSeen)
        SET xs.x1 = toInteger(jb1.pos.x), xs.x2 = toInteger(jb2.pos.x)
      MATCH (jb1)-[:SAME_CC_AS]->*(root1)
        WHERE NOT EXISTS {(root1)-[:SAME_CC_AS]->()}
      MATCH (jb2)-[:SAME_CC_AS]->*(root2)
        WHERE NOT EXISTS {(root2)-[:SAME_CC_AS]->()}
      CALL (root1, root2) {
          WHEN root1 <> root2 THEN {
            CREATE (root1)-[:SAME_CC_AS]->(root2)
          }
        }
      }
    } IN TRANSACTIONS OF 1 ROW
  ON ERROR BREAK
  RETURN count(*) AS _
  
  NEXT  

  MATCH (xs:LastXsSeen)
  RETURN xs.x1 * xs.x2 AS part2
```


