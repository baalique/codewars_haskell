-- https://www.codewars.com/kata/53223653a191940f2b000877

module Graph where

type Node = Char
type Arc = (Node, Node)

solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph start end _ | start == end = True
solveGraph start end arcs             = move arcs startArcs [] end where startArcs = filter (\a -> fst a == start) arcs

move :: [Arc] -> [Arc] -> [Arc] -> Node -> Bool
move _ [] _ _                 = False
move graph stack visited goal | snd (head stack) == goal = True
move graph stack visited goal = move graph (tail stack ++ possibleArcs) (head stack : visited) goal
    where possibleArcs = filter (\a -> a `notElem` (stack ++ visited)) $ getAdjacentArcs (head stack) graph

getAdjacentArcs :: Arc -> [Arc] -> [Arc]
getAdjacentArcs (_, y) = filter (\(a, _) -> a == y)
