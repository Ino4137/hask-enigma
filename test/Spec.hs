import           Test.Hspec
import           Encoding
import qualified Data.Vector as V
import qualified Data.IntMap as M

main :: IO ()
main = hspec $ do
  describe "Encoding.Function.encodeCharacter" $ do
    -- w/o proper "enigma building" getting the parts is annoying
    let stats = V.fromList $ map V.fromList $ replicate 3 (zipWith Connection [0..25] [0..25])
        rotorIT (n,vec) = Right $ Rotor n 99 vec
        rotors = V.map rotorIT (V.zip (V.fromList [1,2,3]) stats)
        refl = Left . Reflector . M.fromList $ zip [0..25] ([1..24] ++ [0])  -- A -> B, B -> A
        parts :: V.Vector Part
        parts = V.snoc rotors refl

    it "encodes A to B with zero rotor movements and shifting by one position with reflector" $ do
      encodeCharacter 'A' parts `shouldBe` 'B'
    
    it "can recieve lowercase letters" $ do
      encodeCharacter 'b' parts `shouldBe` 'C'
