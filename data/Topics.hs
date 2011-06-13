module Topics 
(
  Topic(..)
)
where


data Topic = Math | Economics | Statistics | Physics | Cryptography | Cryptovirology | Programming | Sports | Movies | Books 
             deriving (Eq, Ord, Show, Read, Bounded, Enum) 

