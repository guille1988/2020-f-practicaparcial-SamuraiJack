module Lib where
import Text.Show.Functions

laVerdad = True

data Elemento = UnElemento { 
tipo :: String,
ataque :: (Personaje-> Personaje),
defensa :: (Personaje-> Personaje) 
} deriving (Show)

data Personaje = UnPersonaje { 
nombre :: String,
salud :: Float,
elementos :: [Elemento],
anioPresente :: Int 
} deriving (Show)

--PUNTO 1--

{-
Lo esperado es poder usar el efecto de ataque de un elemento sobre el rival y el de defensa sobre el personaje que lo tiene. 
En caso de que no se indique cuál es el efecto defensivo o el ofensivo, significa que no se altera de ninguna forma al personaje recibido.

Empecemos por algunas transformaciones básicas:
mandarAlAnio: lleva al personaje al año indicado.
meditar: le agrega la mitad del valor que tiene a la salud del personaje.
causarDanio: le baja a un personaje una cantidad de salud dada.
Hay que tener en cuenta al modificar la salud de un personaje que ésta nunca puede quedar menor a 0.
-}

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

meditar :: Personaje -> Personaje
meditar personaje = personaje {salud = alterarSalud (/2) (salud personaje)}

causarDanio :: Float -> Personaje -> Personaje
causarDanio valorDeSalud personaje = personaje {salud = alterarSalud (subtract valorDeSalud) (salud personaje)}

alterarSalud :: (Float -> Float) -> Float -> Float
alterarSalud operacion valor = max (operacion valor) 0


--PUNTO 2--

{-
Queremos poder obtener algo de información extra sobre los personajes. Definir las siguientes funciones:
esMalvado, que retorna verdadero si alguno de los elementos que tiene el personaje en cuestión es de tipo “Maldad”.
danioQueProduce :: Personaje -> Elemento -> Float, que retorne la diferencia entre la salud inicial del personaje 
y la salud del personaje luego de usar el ataque del elemento sobre él.
enemigosMortales que dado un personaje y una lista de enemigos, devuelve la lista de los enemigos que pueden 
llegar a matarlo con un solo elemento. Esto sucede si luego de aplicar el efecto de ataque del elemento, 
el personaje queda con salud igual a 0.
-}

type Enemigo = Personaje

type Enemigos = [Enemigo]

esMalvado :: Personaje -> Bool
esMalvado personaje = any ((=="Maldad").tipo) (elementos personaje) 

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = (salud personaje) - (salud ((ataque elemento) personaje))

enemigosMortales :: Personaje -> Enemigos -> Enemigos
enemigosMortales personaje enemigos = filter (puedenLlegarAmatarlo personaje) enemigos

puedenLlegarAmatarlo :: Personaje -> Enemigo -> Bool
puedenLlegarAmatarlo personaje enemigo = salud ((ataque (head (elementos enemigo))) personaje) == 0

--PUNTO 3--

{-
 Definir los siguientes personajes y elementos:
Definir concentracion de modo que se pueda obtener un elemento cuyo efecto defensivo sea aplicar meditar tantas 
veces como el nivel de concentración indicado y cuyo tipo sea "Magia".
Definir esbirrosMalvados que recibe una cantidad y retorna una lista con esa cantidad de esbirros 
(que son elementos de tipo “Maldad” cuyo efecto ofensivo es causar un punto de daño).
Definir jack de modo que permita obtener un personaje que tiene 300 de salud, que tiene como elementos 
concentración nivel 3 y una katana mágica (de tipo "Magia" cuyo efecto ofensivo es causar 1000 puntos de daño) y vive en el año 200.
Definir aku :: Int -> Float -> Personaje que recibe el año en el que vive y la cantidad de salud con la que debe ser construido. 
Los elementos que tiene dependerán en parte de dicho año. Los mismos incluyen:
Concentración nivel 4
Tantos esbirros malvados como 100 veces el año en el que se encuentra.
Un portal al futuro, de tipo “Magia” cuyo ataque es enviar al personaje al futuro 
(donde el futuro es 2800 años después del año indicado para aku), y su defensa genera un nuevo aku para el año futuro 
correspondiente que mantenga la salud que tenga el personaje al usar el portal.
-}



