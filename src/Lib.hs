module Lib (
  run
) where

  import Data.List            (sortBy)
  import Data.Function        (on)
  import Control.Applicative

  type Move = (Int, Int)      -- From one Peg to another Peg
  type Moves = [Move]         -- History of Moves
  type Height = Int           -- The height of a stack
  type Peg = Int              -- Location of Peg
  type Disc = Int             -- Size of Disc
  type Stack = (Int, [Disc])  -- Stack of Discs at a Location
  type State = [Stack]        -- State of the Tower at a given point in time
  type Solution = (Moves, State)  -- The History of Moves and the State of the Tower at a given point in time
  data Result = Invalid | Valid Solution deriving (Show)

  apply :: Move -> Disc -> Stack -> Stack
  apply (from, to) disc (index, discs)
    | index == from && top == disc  = (index, stack)
    | index == to                   = (index, disc: discs)
    | index == from && top /= disc  = error "Attempt to remove incorrect disc!"
    | otherwise                     = (index, discs)
    where (top: stack) = discs

  canPop :: Peg -> Stack -> Bool
  canPop source (peg, discs) = isSource && isNotEmpty
    where isSource    = peg == source
          isNotEmpty  = not $ null discs

  canPush :: Disc -> Peg -> Stack -> Bool
  canPush disc destination (peg, discs) = isDestination && (isEmpty || canFit)
    where isDestination = peg == destination
          isEmpty       = null discs
          canFit        = disc < top
          (top: _)      = discs

  getDisc :: Peg -> Stack -> Disc
  getDisc source (peg, discs)
    | isSource && isNotEmpty  = top
    | otherwise               = 0
    where isSource    = peg == source
          isNotEmpty  = not $ null discs
          (top: _)    = discs

  single :: [a] -> a
  single []   = error "Empty!"
  single [x]  = x
  single _    = error "More than one!"

  singleMatch :: (a -> Bool) -> [a] -> a
  singleMatch predicate list = single $ filter predicate list

  moveDisc :: State -> Move -> State
  moveDisc state (from, to) = apply (from, to) disc <$> state
    where disc = singleMatch (/= 0) $ getDisc from <$> state

  isFull :: Height -> Stack -> Bool
  isFull heightOfFullStack (_, discs) = length discs == heightOfFullStack

  isComplete :: State -> Height -> Bool
  isComplete state heightOfFullStack = hasFullPeg && peg == 2
    where fullPeg     = filter (isFull heightOfFullStack) state
          hasFullPeg  = not $ null fullPeg
          (peg, _)    = single fullPeg

  isValidResult :: Result -> Bool
  isValidResult (Valid _) = True
  isValidResult Invalid   = False

  move :: Height -> Moves -> State -> Move -> Result
  move heightOfFullStack history state delta
    | length updatedMoves > maximumMoves  = Invalid
    | complete                            = Valid (updatedMoves, updatedState)
    | null validResults                   = Invalid
    | otherwise                           = best
    where complete      = isComplete updatedState heightOfFullStack
          updatedMoves  = delta: history
          updatedState  = moveDisc state delta
          options       = calculateOptions updatedMoves updatedState
          results       = move heightOfFullStack updatedMoves updatedState <$> options
          validResults  = filter isValidResult results
          best          = single $ validResults
          maximumMoves  = heightOfFullStack ^ 2 - 1

  isValidMove :: State -> Move -> Bool
  isValidMove state (from, to) = isValidPop && isValidPush && movesDisc
    where isValidPop  = (length $ filter (== True) $ canPop from <$> state) == 1
          isValidPush = (length $ filter (== True) $ canPush disc to <$> state) == 1
          movesDisc   = from /= to
          disc        = singleMatch (/= 0) $ getDisc from <$> state

  isForward :: Move -> Move -> Bool
  isForward (lastFrom, lastTo) (from, to) = differentSource || differentDestination
    where differentSource       = from /= lastTo
          differentDestination  = to /= lastFrom

  calculateOptions :: Moves -> State -> Moves
  calculateOptions history state
    | firstMove = valid
    | otherwise = smart
    where firstMove = null history
          options   = (,) <$> [0..2] <*> [0..2]
          valid     = filter (isValidMove state) $ options
          smart     = filter (isForward $ head history) valid

  createPeg :: Height -> Peg -> Stack
  createPeg heightOfFullStack peg
    | firstPeg  = (peg, [1..heightOfFullStack])
    | otherwise = (peg, [])
    where firstPeg = peg == 0

  calculateInitialState :: Height -> State
  calculateInitialState heightOfFullStack = createPeg heightOfFullStack <$> [0..2]

  run :: Height -> Result
  run heightOfFullStack = best
    where initialState  = calculateInitialState heightOfFullStack
          options       = calculateOptions [] initialState
          results       = move heightOfFullStack [] initialState <$> options
          validResults  = filter isValidResult results
          best          = single $ validResults
