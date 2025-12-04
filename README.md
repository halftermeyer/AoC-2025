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

MATCH (n)
CALL(n) {
  DETACH DELETE n
} IN TRANSACTIONS OF 1000 ROWS
RETURN collect (n) AS _

NEXT

LET grid = reduce(rows = [],
  row IN split($data, '\n') | rows + [[cell IN split(row, '') | cell]])
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

