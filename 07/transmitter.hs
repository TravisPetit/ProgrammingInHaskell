-- Exercises 7 and 8
import Data.Char

type Bit = Int
type Byte = [Bit] -- 8 bits
type ByteWithParity = (Byte, Bit)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

byteWithParity2int :: ByteWithParity -> Int
byteWithParity2int (byte, _) = bin2int byte

make8 :: [Bit] -> Byte
make8 bits = take 8 (bits ++ repeat 0)

parity :: Byte -> Bit
parity = (`mod` 2) . sum

addParity :: Byte -> ByteWithParity
addParity byte = (byte, parity byte)

flatten :: ByteWithParity -> [Bit]
flatten (byte, p) = byte ++ [p]

encode :: String -> [Bit]
encode = concat . map (flatten . addParity . make8 . int2bin . ord)

-- previously chop8
unflatten :: [Bit] -> [ByteWithParity]
unflatten [] = []
unflatten bits = (take 8 bits, head (drop 8 bits)) : unflatten (drop 9 bits)

correct :: [ByteWithParity] -> Bool
correct = foldr (\(byte, p) xs -> parity byte == p && xs) True

-- By 'forgetting' the first bit I just set it to 0
channel :: [Bit] -> [Bit]
channel bits = [if i `mod` 9 == 0 then 0 else x | (x, i) <- zip bits [0..]]

decode :: [Bit] -> String
decode = map (chr . byteWithParity2int) . unflatten

decodeOk :: String -> Bool
decodeOk = correct . unflatten . channel . encode

transmit :: String -> String
transmit msg =
  if decodeOk msg
  then decode (channel (encode msg))
  else error "--URGENT-- [!!] FATAL ERROR [!!] parity check *FAILED*! Aborting ..."

