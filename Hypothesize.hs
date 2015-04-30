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
	printf "The numbers %s follow my rule.\n" $ enumerate ", " " and " initial
	printf "What do you think the rule might be?\n"
	printf "I'll give you some options to make it easier.\n"
	printf "The rule is either %s.\n" $ enumerate ", " " or " (map fst rules) -- TODO: IO utilities (formatting output, etc.) (...)

	printf "Perhaps you'd like to test your hypothesis before you guess. You can do so by giving me another\n"
	printf "list of numbers (eg. 1, 5, 2, 6) and I'll tell you whether they follow my rule.\n"

	printf "Suggest a rule or give me a list of numbers: " -- TODO: Flush stdout
	hFlush stdout -- TODO: Fix this temporary 'hack'
	reply <- getLine

	-- TODO: Refactor, meaningful names, break up into functions, handle errors
	-- TODO: No character class shorthands in Haskell regexes (?)
	case matchRegex integers reply of
		Just _  -> if experiment rule $ parse reply
		              then printf "Yes, those numbers follow my rule\n"      --
		              else printf "No, those numbers don't follow my rule\n" --
		Nothing -> if liftM (suggest tag) (readMaybe reply) == Just True
		              then printf "That's right! All hail the empirical genius!\n"                      -- 
		              else printf "They say you learn the most from being wrong. So congratulations.\n" -- 

	where Just rule = lookup tag rules
	      integers  = mkRegex "^([0-9]+,\\s*)*[0-9]+$"        -- Matches a single line of comma-separated integers (with optional spacing)
	      parse     = map read . splitRegex (mkRegex ",\\s*") -- Parses a list of comma-separated integers (use library)


-- | Enumerates a list of showable items with the specified item separator and conjunction
enumerate :: Show s => String -> String -> [s] -> String
enumerate sep conj items = (intercalate sep . map show $ init items) ++ conj ++ (show . last $ items)



---------------------------------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
	putStrLn "Uhm"
	play (Multiply 5) [3, 15, 75, 375]