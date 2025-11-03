module MyMaze (
  Maze(..), 
  makeMaze, -- :: Size -[Wall] -Maze
  hasWall,  -- :: Maze -Place -Direction -Bool
  sizeOf    -- :: Maze -Size
)
where
import Geography


data Maze = Maze Size [Place] [Place] [Place] [Place] 
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
 in Maze (x,y) (filterDir N allWalls) (filterDir S allWalls) (filterDir E allWalls) (filterDir W allWalls)


reflect :: Wall -> Wall
reflect ((i,j), d) = (move d (i,j), opposite d)


hasWall :: Maze -> Place -> Direction -> Bool
hasWall (Maze _ walls _ _ _) pos N = pos `elem` walls
hasWall (Maze _ _ walls _ _) pos S = pos `elem` walls
hasWall (Maze _ _ _ walls _) pos E = pos `elem` walls
hasWall (Maze _ _ _ _ walls) pos W = pos `elem` walls


sizeOf :: Maze -> Size
sizeOf (Maze size _ _ _ _) = size

