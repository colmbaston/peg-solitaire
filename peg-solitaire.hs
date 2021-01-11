{-# LANGUAGE BangPatterns #-}

import           Data.List
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Foldable

{-
    A hexagonal tile, represented using cube coordinates.
-}

data Tile = Tile !Int !Int !Int deriving (Eq, Ord)

instance Show Tile where
  show (Tile x _ z) = show (x * (x + 1) `div` 2 + 1 - z)

{-
    A list of all of the tiles of the triangle and a function
    to check whether a given tile is part of the triangle.
-}

size :: Int
size = 4

fullTriangle :: [Tile]
fullTriangle = [Tile x y z | x <- [0 .. size - 1], y <- [-x .. 0], let z = -x-y]

inTriangle :: Tile -> Bool
inTriangle (Tile x y z) = and [-x <= y, y <= 0, 0 <= x, x < size, z == -x-y]

{-
    A list of the six symmetry transformation functions.
-}

symmetries :: [Tile -> Tile]
symmetries = [f . g | f <- [id, reflect], g <- [id, rotate, rotate . rotate]]
  where
    reflect :: Tile -> Tile
    reflect (Tile x y z) = Tile x z y

    rotate :: Tile -> Tile
    rotate (Tile x y z) = Tile (y + size - 1) z (x - size + 1)

{-
    A board is a set of live tiles; tiles which are part of the triangle and
    are not present in the set are considered dead. Generate the initial
    boards by calculating the symmetry classes of tiles in the triangle.
-}

type Board = Set Tile

initialBoards :: [Board]
initialBoards = map (flip S.delete (S.fromAscList fullTriangle) . head) (symmetryClasses fullTriangle)
  where
    symmetryClasses :: [Tile] -> [[Tile]]
    symmetryClasses []     = []
    symmetryClasses (a:as) = let (bs, cs) = partition (\b -> any ((== a) . ($ b)) symmetries) as
                             in (a:bs) : symmetryClasses cs

{-
    A move is an ordered sequence of tiles. Generate all of the unique moves available
    from a board state, along with the board state after performing each move.
-}

type Move = [Tile]

moves :: Board -> [(Move, Board)]
moves b = unique S.empty (tail . go b =<< S.elems b)
  where
    go :: Board -> Tile -> [(Move, Board)]
    go b t@(Tile x y z) = ([t], S.insert t b) : [(t:ms,b') | [dx, dy, dz] <- permutations [-1, 0, 1],
                                                             let o@(Tile ox oy oz) = Tile  (x+dx)  (y+dy)  (z+dz),
                                                             let l                 = Tile (ox+dx) (oy+dy) (oz+dz),
                                                             S.member    o b,
                                                             inTriangle  l,
                                                             S.notMember l b,
                                                             (ms,b') <- go (S.delete t (S.delete o b)) l]

    unique :: Set (Move, Board) -> [(Move, Board)] -> [(Move, Board)]
    unique _ []          = []
    unique s ((m,b):mbs) = let x = minimum (map (\f -> (map f m, S.map f b)) symmetries)
                           in if S.member x s then unique s mbs else (m,b) : unique (S.insert x s) mbs

{-
    A game is an ordered sequence of valid moves that ends with a single
    live tile remaining. Generate all unique games from a board state.
-}

type Game = [Move]

games :: Board -> [Game]
games b | S.size b == 1 = [[]]
        | otherwise     = [m:ms | (m, b') <- moves b, ms <- games b']

{-
    The program entry-point and other utilities related to IO and printing.
-}

main :: IO ()
main = do (c, g, _) <- foldlM update (0 :: Int, [], maxBound) (games =<< initialBoards)
          putStrLn ("\nGenerated " ++ show c ++ " games on a size " ++ show size ++ " board.")
          putStrLn ("Minimum-length game: " ++ showGame g)

update :: (Int, Game, Int) -> Game -> IO (Int, Game, Int)
update (!c, m, lm) g = do putStrLn (showGame g)
                          let lg = length g
                          if lg < lm
                            then pure (c+1, g, lg)
                            else pure (c+1, m, lm)

showGame :: Game -> String
showGame g = '[' : intercalate ", " (map (intercalate "-" . map show) g) ++ "]"
