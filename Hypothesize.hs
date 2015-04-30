{-
 - Hypothesize
 - ?
 -
 - Inspired by Veritasium (insert link here)
 -
 - Jonatan H Sundqvist
 - April 29 2015
 -

 - TODO | - Distinguish between possible but incorrect rules and impossible rules (eg. ascending for '1 2 5 3 3')
 -        - Handle empty and singleton sample lists
 -        - More creative dialogue

 - SPEC | -
 -        -
 -
 -}



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import Data.List (intercalate)    --  
import Text.Regex                 --  
import Text.Printf (printf)       --  
import System.IO (hFlush, stdout) -- 
import Text.Read (readMaybe)      -- 
import Control.Monad (liftM)
import System.Random



---------------------------------------------------------------------------------------------------
-- Types and data
---------------------------------------------------------------------------------------------------
-- |
type Rule = Int -> Int -> Bool
data Tag  = Ascending | Descending | Multiply Int deriving (Show, Read, Eq) -- TODO: Rename (identifies or describes a rule function) (?)


-- |
rules :: [(Tag, Rule)]
rules = [(Descending, (>)),
         (Ascending,  (<)),
         (Multiply 5, (==) . (*5)),
         (Multiply 3, (==) . (*3))]



---------------------------------------------------------------------------------------------------
-- Logic
---------------------------------------------------------------------------------------------------
-- Core -------------------------------------------------------------------------------------------

-- | 
-- TODO: Misleading name, change (?)
-- suggest :: Rule -> Rule -> Bool
-- suggest _ _ = error "Uhm"
suggest :: Tag -> Tag -> Bool
suggest = (==)


-- |
experiment :: Rule -> [Int] -> Bool
experiment rule sample = and $ zipWith rule sample (tail sample)


---------------------------------------------------------------------------------------------------

-- |
-- TODO: Allow rules directly (not Tag) (?)
play :: Tag -> [Int] -> IO ()
play tag initial = do
    -- TODO: Polish the writing
    printf "The numbers %s follow my rule.\n" $ enumerate ", " " and " initial
    printf "What do you think the rule might be?\n"
    printf "I'll give you some options to make it easier.\n"
    printf "The rule is either %s.\n" $ enumerate ", " " or " (map fst rules) -- TODO: IO utilities (formatting output, etc.) (...)

    printf "Perhaps you'd like to test your hypothesis before you guess. You can do so by giving me another\n"
    printf "list of numbers (eg. 1, 5, 2, 6) and I'll tell you whether they follow my rule.\n"

    printf "Suggest a rule or give me a list of numbers: " >> hFlush stdout -- TODO: Flush stdout
    move


    -- TODO: Refactor, meaningful names, break up into functions, handle errors
    -- TODO: No character class shorthands in Haskell regexes (?)
    where Just rule = lookup tag rules
          integers  = mkRegex "^[\\t ]*([0-9]+[\\t ]*,[\\t ]*)*[0-9]+$" -- Matches a single line of comma-separated integers (with optional spacing)
          cvs       = mkRegex "[\\t ]*,[\\t ]*"                         -- Comma with optional whitespace
          parse     = map read . splitRegex cvs                         -- Parses a list of comma-separated integers
    
          move = do
              reply <- getLine
              case matchRegex integers reply of
                       Just _  -> if experiment rule $ parse reply
                                     then printf "Yes, those numbers follow my rule.\n"      >> move --
                                     else printf "No, those numbers don't follow my rule.\n" >> move --
                       Nothing -> case liftM (suggest tag) (readMaybe reply) of -- TODO: Fix this ugliness
                                      Just True  -> printf "That's right! All hail the empirical genius!\n"                              -- 
                                      Just False -> printf "They say you learn the most from being wrong. So congratulations.\n" >> move -- 
                                      Nothing    -> printf "That's not one of the possible rules.\n"                             >> move -- 



-- | Enumerates a list of showable items with the specified item separator and conjunction
enumerate :: Show s => String -> String -> [s] -> String
enumerate sep conj items = (intercalate sep . map show $ init items) ++ conj ++ (show . last $ items)



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    -- TODO: Meta-gameplay (settings, menu, continue, etc.)
    putStrLn "Uhm"
    play (Multiply 5) [3, 15, 75, 375] -- TODO: Make sure the initial list actually satisfies the rule