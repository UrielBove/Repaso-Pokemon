module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Pokemon = UnPokemon{
    nombre :: String,
    tipo :: TipoPokemon
}deriving (Show, Eq)

data TipoPokemon = Planta | Agua | Fuego deriving(Show, Eq)

tieneVentaja :: TipoPokemon -> TipoPokemon -> Bool
tieneVentaja Planta Agua = True
tieneVentaja Agua Fuego = True
tieneVentaja Fuego Planta = True
tieneVentaja _ _ = False


--1
aQuienesAventaja :: Pokemon -> [Pokemon] -> [Pokemon]
aQuienesAventaja pokemon = filter(leGana  pokemon)

leGana :: Pokemon -> Pokemon -> Bool
leGana p1 p2 = tieneVentaja (tipo p1) (tipo p2)

--2

aCuantosPuedeGanar :: Pokemon -> [Pokemon] -> Number
aCuantosPuedeGanar p1 = length . aQuienesAventaja p1

--4

data Destino = UnGimnasio {nombreGym:: String, siguiente:: Destino}
                   | UnaLiga {contrincantes:: [Pokemon] } deriving (Show, Eq)

estaAlHorno :: Pokemon -> Destino -> Bool
estaAlHorno _ (UnGimnasio _ _) = True
estaAlHorno pokemon (UnaLiga contrincantes) = todosPuedenGanarle pokemon contrincantes

todosPuedenGanarle :: Pokemon -> [Pokemon] -> Bool
todosPuedenGanarle pokemon = not . all (leGana pokemon)

--5

puedoViajar :: Destino -> Destino -> Bool
puedoViajar (UnaLiga _) _ = False
puedoViajar origen destino = origen == destino || puedoViajar (siguiente origen) destino

--2

elMasPicante :: [Pokemon] -> Pokemon
elMasPicante pokemones = foldl1 (elMejorDeDos pokemones) pokemones

elMejorDeDos :: [Pokemon] -> Pokemon -> Pokemon -> Pokemon
elMejorDeDos pokes p1 p2 | aCuantosPuedeGanar p1 pokes >= aCuantosPuedeGanar p2 pokes = p1
    |otherwise = p2