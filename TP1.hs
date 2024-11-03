import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- This function uses a list comprehension to extract all cities in the roadmap, collecting them from both the first and second positions in each tuple (road). 
-- To remove duplicates that may appear we use the function: `Data.List.nub`.
-- Arguments: `xs` - roadmap
cities :: RoadMap -> [City]
cities xs = Data.List.nub ([a | (a,_,_) <- xs] ++ [b | (_,b,_) <- xs])

-- This function leverages the `distance` function to determine if there is a road between the cities, returning `True` if there is a distance, and `False` otherwise.
-- Arguments: `xs` - roadmap, `c1` - city 1, `c2` - city 2
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent xs c1 c2
  | distance xs c1 c2 /= Nothing = True
  | otherwise = False

-- `distance` uses a list comprehension to locate roads between two given cities. If a road exists, it returns the distance between the cities using `Just`; otherwise, it returns `Nothing`. 
-- The `where aux` clause filters the roadmap for roads between `c1` and `c2`.
-- Arguments: `xs` - roadmap, `c1` - city 1, `c2` - city 2
distance :: RoadMap -> City -> City -> Maybe Distance
distance xs c1 c2
  | length aux /= 0 = Just (head [d | (a,b,d) <- xs, (a,b) == (c1,c2) || (a,b) == (c2,c1)])
  | otherwise = Nothing
  where aux = [(a,b) | (a,b,_) <- xs, (a,b) == (c1,c2) || (a,b) == (c2,c1)]

-- `adjacent` finds all cities directly connected to a given city (`c1`) in the roadmap.
-- It returns a list of tuples where each tuple contains a neighboring city and the distance to it.
-- The function combines two list comprehensions:
-- One for roads that start from `c1` and connect to other cities.
-- Another for roads that end at `c1`, representing connections from other cities to `c1`.
-- This approach ensures that connections to `c1` are included, regardless of direction.
-- Arguments: `xs` - roadmap, `c1` - source city
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent xs c1 = [(b,c) | (a,b,c) <- xs, a == c1 ] ++ [(a,c) | (a,b,c) <- xs, b == c1]

-- `pathDistance` calculates the total distance of a given path.
-- The function pairs consecutive cities in the path using `zip` and then recursively calculates the sum of distances between each pair.
-- If all cities in the path are connected, it returns `Just` the total distance.
-- If any two consecutive cities are not connected by a road, it returns `Nothing`.
-- Arguments: `xs` - roadmap, `p` - path
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance xs p = sumDistances auxZip
  where
    auxZip = zip p (tail p)
    
    sumDistances :: [(City, City)] -> Maybe Distance
    sumDistances [] = Just 0
    sumDistances ((a, b):pairs) =
      case distance xs a b of
        Just dist  -> case sumDistances pairs of
                     Just total -> Just (dist + total)
                     Nothing    -> Nothing
        Nothing -> Nothing

-- `rome` identifies the cities with the highest number of direct road connections in the roadmap.
-- It first extracts all city names from the roadmap (both start and end cities),
-- counts the occurrences of each city, and returns those with the maximum count.
-- Arguments: `xs` - roadmap                         
rome :: RoadMap -> [City]
rome xs = 
  let 
    -- Extracts all cities from the roadmap, treating both start and end cities equally.
    extractCities :: RoadMap -> [City]
    extractCities roads = [a | (a, _, _) <- roads] ++ [b | (_, b, _) <- roads]

    -- Counts the frequency of each city in the list.
    countCityOccurrences :: [City] -> [(City, Int)]
    countCityOccurrences cities = map (\group -> (head group, length group)) 
                                      (Data.List.group (Data.List.sort cities))

    cityList = extractCities xs
    cityCounts = countCityOccurrences cityList
    maxCount = maximum (map snd cityCounts)

  in [city | (city, count) <- cityCounts, count == maxCount]

-- `pathExists` determines if there is a path between two cities (`start` and `end`) in a roadmap.
-- It uses a depth-first search (DFS) approach to explore potential paths.
-- Arguments: `xs` - roadmap, `start` - starting city, `end` - target city
pathExists :: RoadMap -> City -> City -> Bool
pathExists xs start end = dfs xs [start] []  -- Start DFS with the initial city in the stack
  where
    -- Helper function `dfs` implements depth-first search with a stack.
    -- Arguments: `xs` - roadmap, `stack` - list of cities to visit next, `visited` - list of cities already visited
    dfs :: RoadMap -> [City] -> [City] -> Bool
    dfs _ [] _ = False  -- Base case: stack is empty, path not found
    dfs xs (current:stack) visited
      | current == end = True  -- Found the target city
      | current `elem` visited = dfs xs stack visited  -- Skip already visited city
      | otherwise = dfs xs (adjacentCities xs current ++ stack) (current : visited)
          -- Add unvisited adjacent cities to stack and continue DFS

    -- `adjacentCities` finds all cities directly connected to the given city in the roadmap.
    -- Returns a list of adjacent cities without distances.
    -- Arguments: `xs` - roadmap, `city` - source city
    adjacentCities :: RoadMap -> City -> [City]
    adjacentCities xs city = [neighbor | (neighbor, _) <- adjacent xs city]

-- `isStronglyConnected` checks if the roadmap is strongly connected, meaning there is a path between every pair of distinct cities.
-- It does this by ensuring `pathExists` is true for all pairs of cities.
-- Arguments: `xs` - roadmap
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected xs = and [pathExists xs a b | a <- cities xs, b <- cities xs, a /= b]

-- `shortestPath` finds all shortest paths between two cities (`start` and `end`) in a roadmap.
-- If the starting city is the same as the ending city, it returns a path containing only that city.
-- Otherwise, it explores all possible paths, finding those with the minimum distance.
-- Arguments: `xs` - roadmap, `start` - starting city, `end` - target city
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath xs start end
  | start == end = [[start]]
  | otherwise = searchShortestPaths [([start], 0)] []
  where
    -- `searchShortestPaths` explores paths to find the shortest ones.
    -- Arguments: `list` - a list of tuples representing paths to explore, `res`- a list of tuples representing completed paths that reach the target city
    searchShortestPaths :: [(Path, Distance)] -> [(Path, Distance)] -> [Path]
    searchShortestPaths [] res = [p | (p, d) <- res, d == minDist]
      where minDist = if null res then 0 else minimum [d | (_, d) <- res]  -- Find the minimum distance among completed paths
    
    searchShortestPaths ((path, dist):list) res
      | current == end = searchShortestPaths list ((path, dist) : res)  -- If the current path reaches `end`, add it to results
      | otherwise = searchShortestPaths newList res
      where
        current = last path  -- Get the last city in the current path
        -- `neighbors` generates new paths by extending the current path with adjacent cities
        -- and accumulating the distance.
        neighbors = [(path ++ [adjacentCity], dist + d) | (adjacentCity, d) <- adjacent xs current, adjacentCity `notElem` path]
        -- Sort paths by distance to prioritize exploring shorter paths first
        newList = Data.List.sortOn snd (list ++ neighbors)

-- `travelSales` finds the shortest path that visits all cities in the roadmap.
-- If the roadmap is empty, disconnected, or no path exists to complete the graph, it returns an empty path.
-- Arguments: `xs` - roadmap
travelSales :: RoadMap -> Path
travelSales xs
    | null allCities = []
    | not (isStronglyConnected xs) = []
    | dpArray Data.Array.! (initialMask, startPos) >= infinity = []
    | otherwise = constructPath initialMask startPos
  where
    allCities = cities xs

    n = length allCities

    startCity = head allCities

    infinity = 1000000000

    startPos = 0

    initialMask = Data.Bits.bit startPos

    cityToIndex = zip allCities [0..]

    indexToCity = zip [0..] allCities

    -- `toIdx` returns the index of a given city.
    -- Arguments: `city` - the city whose index is being retrieved
    toIdx :: City -> Int
    toIdx city = case lookup city cityToIndex of
                   Just idx -> idx
                   Nothing -> 0

    -- `toCity` returns the city corresponding to a given index.
    -- Arguments: `idx` - the index whose corresponding city is being retrieved
    toCity :: Int -> City
    toCity idx = case lookup idx indexToCity of
                   Just city -> city
                   Nothing -> ""

    -- `distArray` is an array of distances between city indices.
    -- If two cities are not directly connected, the distance is set to infinity.
    distArray = Data.Array.array ((0, 0), (n-1, n-1))
        [((i, j), case distance xs (toCity i) (toCity j) of
                      Just d -> d
                      Nothing -> infinity)
        | i <- [0..n-1], j <- [0..n-1]]

    -- `dpArray` is a memoization array storing the minimum distances for subsets of cities.
    dpArray = Data.Array.array ((0, 0), ((2^n)-1, n-1))
        [((mask, pos), dp mask pos) | mask <- [0..(2^n)-1], pos <- [0..n-1]]

    -- `dp` recursively computes the minimum distance to complete a tour starting from the initial city and visiting all cities in the subset represented by `mask`.
    -- Arguments: `mask` - bitmask representing the subset of cities visited so far, `pos` - the current city position (index) in the subset
    dp :: Int -> Int -> Distance
    dp mask pos
        | mask == (2^n)-1 = distArray Data.Array.! (pos, 0)
        | otherwise = minimum [if Data.Bits.testBit mask next then infinity
                               else distArray Data.Array.! (pos, next) + dpArray Data.Array.! (Data.Bits.setBit mask next, next)
                               | next <- [0..n-1]]

    -- `constructPath` reconstructs the shortest path using `dpArray` by tracing back the optimal choices.
    -- Arguments: `mask` - bitmask representing the subset of cities visited so far, `pos` - the current city position (index) in the subset
    constructPath :: Int -> Int -> Path
    constructPath mask pos
        | mask == (2^n)-1 = [toCity pos, startCity]
        | otherwise = toCity pos : constructPath newMask bestNextCity
      where
        possibleNextCities = [(next, distArray Data.Array.! (pos, next) + dpArray Data.Array.! (Data.Bits.setBit mask next, next))
                       | next <- [0..n-1], not (Data.Bits.testBit mask next)]
        (bestNextCity, _) = Data.List.minimumBy (\(_, x) (_, y) -> compare x y) possibleNextCities
        newMask = Data.Bits.setBit mask bestNextCity
    
tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
