type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up:: MyState -> MyState
up (S (0,y) l a m) = Null
up (S (x,y) l a m ) = (S ((x-1),y) l "up" (S (x,y) l a m)) 

down:: MyState -> MyState
down (S (5,y) l a m) = Null
down (S (x,y) l a m ) = (S ((x+1),y) l "down" (S (x,y) l a m)) 

right:: MyState -> MyState
right (S (x,5) l a m) = Null
right (S (x,y) l a m ) = (S (x,(y+1)) l "right" (S (x,y) l a m)) 

left:: MyState -> MyState
left (S (x,0) l a m) = Null
left (S (x,y) l a m ) = (S (x,(y-1)) l "left" (S (x,y) l a m)) 

collectFind _ _ [] = False
collectFind x y ((x1,y1):t) = if x == x1 && y == y1 then True else collectFind x y t 

remove1 x y ((x1,y1):t) = if x == x1 && y == y1 then t else (x1,y1):(remove1 x y t) 

collect:: MyState -> MyState
collect (S (x,y) l a m) = if collectFind x y l then (S (x,y) (remove1 x y l) "collect" (S (x,y) l a m)) else Null

nextMyStatesRemove [] = []
nextMyStatesRemove (h:t) = if h == Null then nextMyStatesRemove t else h:nextMyStatesRemove t   

nextMyStates::MyState->[MyState]
nextMyStates s = nextMyStatesRemove [(up s), (down s), (left s), (right s), (collect s)]

ok Null _ = False
ok (S (x1, y1) ((p1, p2) : t) h j) (S (x2, y2) _ _ _) = (abs (p1 - x1) + abs (p2 - y1)) < (abs (p1 - x2) + abs (p2 - y2))

nextMyStates2 s = nextMyStatesRemove [(if ok (up s) s then up s else Null), 
                                     (if ok (down s) s then down s else Null), 
                                     (if ok (left s) s then left s else Null), 
                                     (if ok (right s) s then right s else Null), 
                                     (collect s)]

isGoal::MyState->Bool
isGoal (S (x,y) [] a m) = True  
isGoal (S (x,y) l a m) = False

search::[MyState]->MyState
search ((S p a s m) : t) = if isGoal (S p a s m) then (S p a s m) else search (t ++ nextMyStates2 (S p a s m))

constructSolution:: MyState ->[String]
constructSolution Null = []
constructSolution (S (x, y) l a m) = if a == "" then constructSolution m else constructSolution m ++ [a]

solve :: Cell->[Cell]->[String]
solve p l = constructSolution (search ([S p l "" Null]))
