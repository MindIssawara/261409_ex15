-- course registration code is wrong find a bug (give a test case), and give a fix
--      The bug occurs because the code doesn't check for duplicate course IDs.
--      Test case:
--              let course1 = Course' 102 4 [2]       
--              let course2 = Course' 102 3 [1,3]  
--              let courses = [course1, course2]    
--              register'' courses 102 4
--      Expected Output:
--              Left "Found multiple courses with this course ID."
--      Actual output:
--              Right [Course' {cid = 102, cap = 4, roster = [4,2]},Course' {cid = 102, cap = 3, roster = [1,3]}]

-- Code --
type CourseID = Int
type Capacity = Int
type StudentID = Int
data CourseInfo' = Course'
  { cid :: CourseID
  , cap :: Capacity
  , roster :: [StudentID]
  } deriving (Show)

enroll'' :: CourseInfo' -> StudentID -> Either String CourseInfo'
enroll'' c sid
  | sid `elem` rs =
      Left "student already registered"
  | length rs >= seats =
      Left "course full"
  | otherwise = Right $
      Course' (cid c) seats (sid:rs)
  where seats = cap c
        rs = roster c

-- Fix --
register'' :: [CourseInfo'] -> CourseID -> StudentID -> Either String [CourseInfo']
register'' courses cid' sid
  -- Check for duplicate CourseID
  | length (filter (\c -> cid c == cid') courses) > 1 =
      Left "Found multiple courses with this course ID."
  | otherwise = registerHelper courses cid' sid
  where
    -- Original logic for registering students
    registerHelper [] _ _ = Left "no such course"
    registerHelper (c : cs) cid' sid
      | cid c == cid' =
          case enroll'' c sid of
            Left msg -> Left msg
            Right c' -> Right (c' : cs)
      | otherwise = fmap (c :) (registerHelper cs cid' sid)


-- define function maybeAp
maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp Nothing _ = Nothing  
maybeAp _ Nothing = Nothing  
maybeAp (Just f) (Just x) = Just (f x)  

-- define initMaybe
initMaybe :: a -> Maybe a
initMaybe x = Just x

-- define listAp
listAp :: [a -> b] -> [a] -> [b]
listAp fs xs = [f x | f <- fs, x <- xs]

-- define initList
initList :: a -> [a]
initList x = [x]

-- explain fmap (*3) (+100)
--      When fmap is applied to a function, it performs function composition.
--      fmap (*3) (+100) means apply (*3) to the result of (+100). This results in a new function: \x -> (*3) ((+100) x).