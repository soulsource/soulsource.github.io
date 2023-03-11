module Main where

import Control.Monad.Free (Free(..), liftF)

data BeerQuality = Lukewarm | Refreshing deriving (Eq, Show)
data Saturday next = GoToBar String next | DrinkABeer (BeerQuality -> next)

instance Functor Saturday where
    fmap f (GoToBar s next) = GoToBar s (f next)
    fmap f (DrinkABeer next) = DrinkABeer (f . next)

goToBar :: String -> Free Saturday ()
goToBar s = liftF (GoToBar s ())
drinkABeer :: Free Saturday BeerQuality
drinkABeer = liftF (DrinkABeer id)

-- Only keep drinking if the beer was refreshing. If a lukewarm brew is served, go home.
aNiceEvening :: Free Saturday ()
aNiceEvening = do _ <- drinkABeer
                  goToBar "Drunken Norwegian"
                  x <- drinkABeer
                  if x /= Lukewarm then do
                    drinkABeer
                    goToBar "Le Rieur Sanglier"
                    x <- drinkABeer
                    if x /= Lukewarm then do
                        x <- drinkABeer
                        if x /= Lukewarm then do
                            goToBar "Coyote Ugly"
                            x <- drinkABeer
                            if x /= Lukewarm then do
                                drinkABeer
                                return ()
                            else return ()
                        else return()
                    else return()
                  else return()


data Location = Home | Bar String deriving (Eq, Show)

countBeersConsumedPerBar :: Free Saturday () -> [(Location, Int)]
countBeersConsumedPerBar = reverse . interpretEveningStep Home []
    where getBeerQualityOfLocation :: Location -> BeerQuality
          getBeerQualityOfLocation (Bar "Le Rieur Sanglier") = Lukewarm
          getBeerQualityOfLocation _ = Refreshing

          interpretEveningStep :: Location -> [(Location, Int)] -> Free Saturday () -> [(Location, Int)]
          interpretEveningStep _ r (Pure _) = r
          interpretEveningStep l [] (Free (DrinkABeer next)) = interpretEveningStep l [(l,1)] (next $ getBeerQualityOfLocation l)
          interpretEveningStep l ((lo,c):cs) (Free (DrinkABeer next)) | lo == l = interpretEveningStep l ((l,c+1):cs) (next $ getBeerQualityOfLocation l)
          interpretEveningStep l cs (Free (DrinkABeer next)) = interpretEveningStep l ((l,1):cs) (next $ getBeerQualityOfLocation l)
          interpretEveningStep l cs (Free (GoToBar name next)) = interpretEveningStep (Bar name) cs next

main :: IO ()
main = print (countBeersConsumedPerBar aNiceEvening)