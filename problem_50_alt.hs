-- i wish i'd thought of this.

import Primes
import Data.List

problem50 = sum . head . filter (isPrime . sum) $ allTails
            where allInits = takeWhile ((<1000000) . sum) . inits $ primes
                  allTails = concatMap tails . reverse $ allInits
