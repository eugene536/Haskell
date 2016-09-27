{-# LANGUAGE MultiParamTypeClasses, InstanceSigs, RecordWildCards #-}

module Game where

import           Equipment
import           Debug.Trace(trace)

data State = State
    { health  :: Int
    , attack  :: Int
    , defence :: Int 
    } deriving (Show)

data Player = Player 
    { playerStat :: State
    , playerEquipment  :: EquipmentState 
    } deriving (Show)

--foo (Player State {..} equi)  = health
initPlayer = Player 
    { playerStat = State 
               { health  = 100
               , attack  = 10
               , defence = 2 } 
    , playerEquipment = initEquipment }

data Monster = Monster
    { monsterStat :: State 
    , monsterEquipment   :: EquipmentState 
    } deriving (Show)

initMonster = Monster 
    { monsterStat = State 
               { health  = 100
               , attack  = 5
               , defence = 1 } 
    , monsterEquipment = initEquipment }

data Result = Result
    { restMonsters :: [Monster]
    , player       :: Player
    , cntHits      :: Int 
    } deriving (Show)

p_hit :: (Player, Monster) -> (Player, Monster)
p_hit (p, m) = (p, n_m)
    where p_stat     = playerStat p
          p_sword    = sword2int $ sword $ playerEquipment p
          p_attack_s = p_sword + attack p_stat

          m_stat     = monsterStat m
          m_health   = health m_stat
          m_defence  = defence m_stat

          n_health   = m_health + min 0 (m_defence - p_attack_s)
          n_m        = m { monsterStat = m_stat { health = n_health } }

m_hit :: (Player, Monster) -> (Player, Monster)
m_hit (p, m) = (n_p, m)
    where p_stat    = playerStat p
          p_armor   = armor2int $ armor $ playerEquipment p
          p_armor_s = p_armor + defence p_stat
          p_health  = health p_stat

          m_stat    = monsterStat m
          m_attack  = attack m_stat

          n_health  = p_health + min 0 (p_armor_s - m_attack)
          n_p = p { playerStat = p_stat { health = n_health } }


gloriousBattle :: Player -> [Monster] -> Result
gloriousBattle p monss = gloriousHelper 0 p monss 0
    where 
        gloriousHelper 0 p [] cnt = error "can't happen"
        gloriousHelper 1 p [] cnt = trace "win" (Result [] p) $ (cnt + 1) `div` 2

        gloriousHelper 0 p monss cnt = let (n_p, n_m) = p_hit (p, head monss) in
            if (health $ monsterStat $ n_m) <= 0 then
                gloriousHelper 1 (n_p { playerEquipment = max (monsterEquipment n_m) (playerEquipment n_p)}) (tail monss) $ cnt + 1
            else
                gloriousHelper 1 n_p (n_m:(tail monss)) $ cnt + 1

        gloriousHelper 1 p monss cnt = let (n_p, n_m) = m_hit (p, head monss) in
            if (health $ playerStat $ n_p) <= 0 then
                trace "died" $ Result monss (n_p {playerStat = (playerStat n_p) {health = 0}}) $ cnt + 1
            else
                gloriousHelper 0 n_p monss $ cnt + 1


