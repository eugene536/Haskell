module Equipment where

data Sword = StandardSword
           | SilverSword
           | GoldSword
    deriving (Show, Eq, Ord)

sword2int :: Sword -> Int
sword2int StandardSword = 0
sword2int SilverSword   = 5
sword2int GoldSword     = 10

data Armor = StandardArmor
           | SilverArmor
           | GoldArmor
    deriving (Show, Eq, Ord)

armor2int :: Armor -> Int
armor2int StandardArmor = 0
armor2int SilverArmor   = 5
armor2int GoldArmor     = 10

data Medicine = Drag 
              | Potion

med2int :: Medicine -> Int
med2int Drag   = 5
med2int Potion = 15

data EquipmentState = EquipmentState 
    { sword   :: Sword
    , armor   :: Armor }
    deriving (Show, Eq, Ord)

initEquipment = EquipmentState 
    { sword = StandardSword
    , armor = StandardArmor }
        
    

