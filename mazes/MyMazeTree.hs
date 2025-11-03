
module MyMazeTree (
  Maze(..), 
  makeMaze, -- :: Size -[Wall] -Maze
  hasWall,  -- :: Maze -Place -Direction -Bool
  sizeOf    -- :: Maze -Size
)
where
import Geography

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf deriving Show

data Maze = Maze Size (BinaryTree Place) (BinaryTree Place) (BinaryTree Place) (BinaryTree Place)
    deriving (Show)
--NSEW


makeMaze :: Size -> [Wall] -> Maze
makeMaze (x,y) walls = 
  let boundaries = -- the four boundaries
        [((0,j),   W) | j <- [0..y-1]] ++ -- westerly boundary
        [((x-1,j), E) | j <- [0..y-1]] ++ -- easterly boundary
        [((i,0),   S) | i <- [0..x-1]] ++ -- southerly boundary
        [((i,y-1), N) | i <- [0..x-1]] -- northerly boundary
      allWalls = walls ++ boundaries ++ map reflect (walls ++ boundaries)
      filterDir dir walls = map (\(pos,dir) -> pos) (filter (\(_, direction) -> dir == direction) walls)
 in Maze (x,y) (makeTree (filterDir N allWalls)) (makeTree (filterDir S allWalls)) (makeTree (filterDir E allWalls)) (makeTree (filterDir W allWalls))

makeTree :: [Place] -> BinaryTree Place
makeTree places = treeMaker sortedPlaces
  where sortedPlaces = quicksort places

searchTree Leaf _ = False
searchTree (Node (x,y) t1 t2) (a,b) = if (x,y) == (a,b) then True else if (a,b)<(x,y) then searchTree t1 (a,b) else searchTree t2 (a,b)

treeMaker [] = Leaf
treeMaker places = Node (x,y) (treeMaker (filter (\(a,b) -> (a,b)<(x,y)) rest)) (treeMaker (filter (\(a,b) -> (a,b)>(x,y)) rest))
    where (x,y) = places !! (div (length places) 2)
          rest = filter (\(a,b) -> (a,b)/=(x,y)) places

quicksort :: [Place] -> [Place]
quicksort [] = []
quicksort (first: rest) = quicksort (filter (\x -> x < first) rest) ++ (first : quicksort (filter (\x -> x >= first) rest))

reflect :: Wall -> Wall
reflect ((i,j), d) = (move d (i,j), opposite d)


hasWall :: Maze -> Place -> Direction -> Bool
hasWall (Maze _ walls _ _ _) pos N = searchTree walls pos
hasWall (Maze _ _ walls _ _) pos S = searchTree walls pos
hasWall (Maze _ _ _ walls _) pos E = searchTree walls pos
hasWall (Maze _ _ _ _ walls) pos W = searchTree walls pos


sizeOf :: Maze -> Size
sizeOf (Maze size _ _ _ _) = size

