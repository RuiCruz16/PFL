import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities xs = Data.List.nub ([a | (a,_,_) <- xs] ++ [b | (_,b,_) <- xs])

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent xs c1 c2
  | distance xs c1 c2 /= Nothing = True
  | otherwise = False

distance :: RoadMap -> City -> City -> Maybe Distance
distance xs c1 c2
  | length aux /= 0 = Just (head [d | (a,b,d) <- xs, (a,b) == (c1,c2) || (a,b) == (c2,c1)])
  | otherwise = Nothing
  where aux = [(a,b) | (a,b,_) <- xs, (a,b) == (c1,c2) || (a,b) == (c2,c1)]

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent xs c1 = [(b,c) | (a,b,c) <- xs, a == c1 ] ++ [(a,c) | (a,b,c) <- xs, b == c1]

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
                          
rome :: RoadMap -> [City] -- check da função e variáveis
rome xs = 
  let 
    extractStrings :: RoadMap -> [String]
    extractStrings l = [a | (a,_,_) <- l] ++ [b | (_,b,_) <- l]

    occurrencesAll :: [String] -> [(String, Int)]
    occurrencesAll c = map (\g -> (head g, length g)) (Data.List.group (Data.List.sort c))

    listStr = extractStrings xs
    listCount = occurrencesAll listStr
    max = maximum (map snd listCount)

  in [a | (a,b) <- listCount, b == max]

pathExists :: RoadMap -> City -> City -> Bool
pathExists xs start end = dfs xs [start] []
  where
    dfs :: RoadMap -> [City] -> [City] -> Bool
    dfs _ [] _ = False 
    dfs roadmap (current:stack) visited
      | current == end = True
      | current `elem` visited = dfs roadmap stack visited
      | otherwise = dfs roadmap (adjacentCities roadmap current ++ stack) (current : visited)
    
    adjacentCities :: RoadMap -> City -> [City]
    adjacentCities roadmap city = [neighbor | (neighbor, _) <- adjacent roadmap city]

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected xs = and [pathExists xs a b | a <- cities xs, b <- cities xs, a /= b]

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start end
  | start == end = [[start]]
  | otherwise = searchShortestPaths [([start], 0)] []
  where
    searchShortestPaths :: [(Path, Distance)] -> [(Path, Distance)] -> [Path]
    searchShortestPaths [] res = [p | (p, d) <- res, d == minDist]
      where minDist = if null res then 0 else minimum [d | (_, d) <- res]
    
    searchShortestPaths ((path, dist):list) res
      | current == end = searchShortestPaths list ((path, dist) : res)
      | otherwise = searchShortestPaths newList res
      where
        current = last path
        neighbors = [(path ++ [adjacentCity], dist + d) | (adjacentCity, d) <- adjacent roadmap current, adjacentCity `notElem` path]
        newList = Data.List.sortOn snd (list ++ neighbors)

-- Marcar sessão com o professor

travelSales :: RoadMap -> Path
travelSales roadmap
    | null allCities = []
    | not (isStronglyConnected roadmap) = []
    | dpArray Data.Array.! (initialMask, startPos) >= infinity = []
    | otherwise = constructPath initialMask startPos
  where
    -- Parâmetros e valores iniciais
    allCities = cities roadmap
    n = length allCities
    startCity = head allCities
    infinity = 1000000000

    -- Mapeamento de cidades para índices e vice-versa
    cityToIndex = zip allCities [0..]
    indexToCity = zip [0..] allCities
    
    -- Conversão de City para Int (índice) e vice-versa sem `fromMaybe`
    toIdx city = case lookup city cityToIndex of
                   Just idx -> idx
                   Nothing -> 0  -- Fallback para 0 se a cidade não for encontrada

    toCity idx = case lookup idx indexToCity of
                   Just city -> city
                   Nothing -> ""  -- Fallback para string vazia se o índice não for encontrado

    -- Matriz de distâncias
    distArray = Data.Array.array ((0, 0), (n-1, n-1))
        [((i, j), case distance roadmap (toCity i) (toCity j) of
                      Just d -> d
                      Nothing -> infinity)
        | i <- [0..n-1], j <- [0..n-1]]

    -- Tabela de programação dinâmica
    dpArray = Data.Array.array ((0, 0), ((2^n)-1, n-1))
        [((mask, pos), dp mask pos) | mask <- [0..(2^n)-1], pos <- [0..n-1]]

    -- Função de DP que calcula o custo mínimo
    dp :: Int -> Int -> Distance
    dp mask pos
        | mask == (2^n)-1 = distArray Data.Array.! (pos, 0)  -- Retorna ao ponto inicial
        | otherwise = minimum [if Data.Bits.testBit mask next then infinity
                               else distArray Data.Array.! (pos, next) + dpArray Data.Array.! (Data.Bits.setBit mask next, next)
                               | next <- [0..n-1]]

    -- Função para reconstruir o caminho
    constructPath :: Int -> Int -> Path
    constructPath mask pos
        | mask == (2^n)-1 = [toCity pos, startCity]
        | otherwise = toCity pos : constructPath newMask bestNext
      where
        possibleNext = [(next, distArray Data.Array.! (pos, next) + dpArray Data.Array.! (Data.Bits.setBit mask next, next))
                       | next <- [0..n-1], not (Data.Bits.testBit mask next)]
        (bestNext, _) = Data.List.minimumBy (\(_, x) (_, y) -> compare x y) possibleNext
        newMask = Data.Bits.setBit mask bestNext

    -- Parâmetros iniciais para o caminho final
    startPos = 0
    initialMask = Data.Bits.bit startPos


tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: Path
gTest4 = ["7","6","5","4"]

a = travelSales gTest1

b = travelSales gTest2

c = travelSales gTest3