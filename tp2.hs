import Text.Show.Functions()
import Data.List(delete)
import Distribution.SPDX (LicenseId(RPL_1_5))

-- TP 1
-- Punto 1
data Planta = Planta {
    especie :: String,
    puntoDeVida :: Int,
    soles :: Int,
    poderAtaque :: Int
}deriving (Show, Eq)

data Zombie = Zombie {
    nombre :: String,
    accesorios :: [String],
    danio :: Int
}deriving (Show, Eq)

-- Nivel de muerte de los Zombies

nivelDeMuerte :: Zombie-> Int
nivelDeMuerte  zombie = length (nombre zombie)

-- Plantas

peaShooter :: Planta
peaShooter = Planta { 
    especie = "PeaShooter",
    puntoDeVida = 5,
    soles = 0,
    poderAtaque = 2
}

repeater :: Planta
repeater = Planta {
    especie = "repeater",
    puntoDeVida = 5,
    soles = 0,
    poderAtaque = 4
}

sunflower :: Planta
sunflower = Planta {
    especie = "Sunflower",
    puntoDeVida = 7,
    soles = 1,
    poderAtaque = 0
}

nut :: Planta
nut = Planta {
    especie = "Nut",
    puntoDeVida = 100,
    soles = 0,
    poderAtaque = 0
}

petaGranada :: Planta 
petaGranada = Planta {
    especie = "PetaGranada",
    puntoDeVida = 25,
    soles = 10,
    poderAtaque = 100
}

setaSolar :: Planta 
setaSolar = Planta {
    especie = "SetaSolar",
    puntoDeVida = 25,
    soles = 15,
    poderAtaque = 75
}

-- Zombies

zombieBase :: Zombie
zombieBase = Zombie {
    nombre = "Basico",
    accesorios = [],
    danio =1
}

balloon :: Zombie
balloon = Zombie {
    nombre = "Ballon",
    accesorios = ["globo"],
    danio =1
}

newspaper :: Zombie
newspaper = Zombie {
    nombre = "Newspaper",
    accesorios = ["Diario"],
    danio =2
}

gargantuar :: Zombie
gargantuar = Zombie {
    nombre = "Gargantuar Hulk Smash Puny God",
    accesorios = ["posteElectrico", "Zombie Enano"],
    danio =30
}

-- Punto 2
-- a
especialidadPlanta :: Planta -> [Char]
especialidadPlanta planta | soles planta > 0 = "Proveedora"
                        | poderAtaque planta > puntoDeVida planta = "Atacante"
                        | otherwise = "Defensiva"   

-- b

zombiePeligroso :: Zombie -> Bool
zombiePeligroso zombie = length (accesorios zombie) > 1 || danio zombie > 10

-- Punto 3
--a
data LineaDeDefensa = LineaDeDefensa {
    plantas :: [Planta],
    zombies :: [Zombie]
} deriving (Show, Eq)

agregarPlanta :: LineaDeDefensa -> Planta -> LineaDeDefensa
agregarPlanta lineaDefensa plantaNueva = lineaDefensa { plantas = plantas lineaDefensa ++ [plantaNueva] }

agregarZombie :: LineaDeDefensa -> Zombie -> LineaDeDefensa
agregarZombie lineaDefensa zombieNuevo = lineaDefensa { zombies = zombies lineaDefensa ++ [zombieNuevo] }

-- b 
-- Para hacer esta funcion voy a tener que delegar 5 funciones
-- danioDePlantas hayZombies danioMordiscos cantidadZombiesPeligrosos y todosZombiesPeligrosos

danioDePlantas :: [Planta] -> Int
danioDePlantas [] = 0
danioDePlantas (planta:resto) = poderAtaque planta + danioDePlantas resto

danioMordiscos :: [Zombie] -> Int
danioMordiscos [] = 0
danioMordiscos (zombie:resto) = danio zombie + danioMordiscos resto


hayZombies::[Zombie]->Bool
hayZombies zombies = length zombies > 0

cantidadZombiesPeligrosos :: [Zombie] -> Int
cantidadZombiesPeligrosos [] = 0
cantidadZombiesPeligrosos (zombie:resto)  | (zombiePeligroso zombie) = 1 +  cantidadZombiesPeligrosos resto 
                                    | otherwise = 0 + cantidadZombiesPeligrosos resto

todosZombiesPeligrosos :: [Zombie] -> Bool
todosZombiesPeligrosos zombies = cantidadZombiesPeligrosos zombies == length zombies


estaEnPeligro :: LineaDeDefensa -> Bool
estaEnPeligro linea = ((danioDePlantas.plantas) linea < (danioMordiscos.zombies) linea) || ((hayZombies . zombies) linea && (todosZombiesPeligrosos .zombies) linea)

--c 
cantidadDePlantasProveedoras::[Planta] -> Int
cantidadDePlantasProveedoras [] = 0
cantidadDePlantasProveedoras (planta:resto) | especialidadPlanta planta == "Proveedora" = 1 + cantidadDePlantasProveedoras resto
                                         | otherwise = 0 + cantidadDePlantasProveedoras resto

necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = cantidadDePlantasProveedoras (plantas linea) == length (plantas linea)

-- casos para prueba, llevar a la consola

-- linea1 = LineaDeDefensa { plantas = [sunflower, sunflower, sunflower], zombies = []}

-- linea2 = LineaDeDefensa { plantas = [peaShooter, peaShooter, sunflower, nut], zombies = [zombieBase, newspaper]}

-- linea3 = LineaDeDefensa { plantas = [sunflower, peaShooter], zombies = [gargantuar, zombieBase, zombieBase]}

--Punto 4


lineaMixta :: LineaDeDefensa  -> Bool
lineaMixta linea = head (plantas linea) /= (head. tail) (plantas linea)

-- punto 5

-- daniarZombie :: Planta -> Zombie -> Zombie
-- daniarZombie planta zombie =
--   let ataqueDePlanta = poderAtaque planta
--       nombreReducido = drop ataqueDePlanta (nombre zombie)
--   in zombie { nombre = nombreReducido }

daniarPlanta :: Planta -> Zombie -> Planta
daniarPlanta planta zombie =
    let ataqueDeZombie = danio zombie
        nuevosPuntosDeVida = puntoDeVida planta - ataqueDeZombie
    in planta { puntoDeVida = nuevosPuntosDeVida }


-- TP 2

-- Punto 1
{--
Responder (agregar un comentario con la respuesta en el archivo con el código).

i. ¿Qué pasaría al consultar si una línea está en peligro si hubiera
una cantidad infinita de zombies bases en la misma?
i. Una de las funciones que utiliza "estaEnPeligro" que es la funcion "cantidadZombiesPeligrosos" entraria en un 
loop infinito haciendo que esta nunca llegue a tocar el caso base, ya que utilizamos recursividad para manejar
la cantidad de zombies que hay en una linea

ii. ¿Qué pasaría al consultar si una línea con una cantidad infinita
de PeaShooters necesita ser defendida? ¿Y con una cantidad
infinita de Sunflowers?
ii. Pasaria lo mismo que antes, esta entraria en un loop infinito haciendo que el programa se trabe haciendo que
no podamos salir de la funcion que esta ejecutando para saber cuantas plantas hay en la linea para saber si 
tiene que ser defendida o no.

iii. Justificar las respuestas conceptualmente.
iii. 
Como utilizamos recursividad, siempre vamos a iterar sobre una cantidad de X items hasta llegar a un paso base
ese paso base seria que items = [] y con eso sabriamos que llegamos al final del bloque que tendriamos que iterar,
el problema con que tengamos una cantidad infinita de plantas o zombies hace que ese paso base nunca se cumpla
por lo que jamas llegariamos a tener algo como items = [] siempre vamos a tener mas y mas, entonces
como no podriamos cumplir un paso base el programa seguiria iterando y iterando hasta tratar de encontrar ese caso
haciendo que este entre en el bucle infinito que mencionabamos antes
-}


-- Punto 2

cactus :: Planta 
cactus = Planta {
    especie = "Cactus",
    puntoDeVida = 9,
    soles = 0,
    poderAtaque = 0
}

-- Para esto vamos a tener que armar un nuevo caso en daniarZombie para que cuando sea un catus el que ataque
-- y se una ballon el zombie atacado, le saque su ballon del inventario

-- Creo una funcion que toma un zombie y un accesorio y devuelve si contiene o no un accesorio en sus accesorios 
tieneAccesorio :: Zombie -> String -> Bool
tieneAccesorio zombie accesorio = elem accesorio (accesorios zombie)

quitarAccesorio :: Zombie -> String -> [String]
quitarAccesorio zombie accesorio = delete accesorio (accesorios zombie)

revisarTipoDePlanta :: Planta -> String -> Bool
revisarTipoDePlanta planta tipo = especie planta == tipo

obtenerNuevosAccesorios :: Planta -> Zombie -> [String]
obtenerNuevosAccesorios planta zombie
  | revisarTipoDePlanta planta "Cactus" && tieneAccesorio zombie "globo" = quitarAccesorio zombie "globo"
  | otherwise = accesorios zombie

daniarZombie :: Planta -> Zombie -> Zombie
daniarZombie planta zombie =
  let ataqueDePlanta = poderAtaque planta
      nombreReducido = drop ataqueDePlanta (nombre zombie)
      nuevosAccesorios = obtenerNuevosAccesorios planta zombie
  in zombie { nombre = nombreReducido, accesorios = nuevosAccesorios }


-- Punto 3 
-- lineas
linea1 :: LineaDeDefensa
linea1 = LineaDeDefensa { plantas = [sunflower, sunflower, sunflower], zombies = []}

linea2 :: LineaDeDefensa
linea2 = LineaDeDefensa { plantas = [peaShooter, peaShooter, sunflower, nut], zombies = [zombieBase, newspaper]}

linea3 :: LineaDeDefensa
linea3 = LineaDeDefensa { plantas = [sunflower, peaShooter], zombies = [gargantuar, zombieBase, zombieBase]}

septimoRegimiento :: [(Zombie, LineaDeDefensa)]
septimoRegimiento = [(newspaper, linea1), (balloon, linea2), (balloon, linea3)]

region :: [(Zombie, LineaDeDefensa)]
region = [(gargantuar, linea1),(gargantuar,linea2),(gargantuar, linea3),(gargantuar, linea1),(gargantuar,linea2),(gargantuar, linea3)]

jardin :: [LineaDeDefensa]
jardin = [linea1, linea2, linea3]

lineaDeLaHorda ::  [(Zombie, LineaDeDefensa)] -> [LineaDeDefensa]
lineaDeLaHorda unaHorda = map ((snd.head) unaHorda)

agregarHoradaAJardin :: [LineaDeDefensa] ->  [(Zombie, LineaDeDefensa)] ->  [LineaDeDefensa]
agregarHoradaAJardin unJardin hordaZombie = unJardin ++ lineaDeLaHorda


-- Punto 4
-- Esta funcion lo que hace es tomar una planta, zombie 
-- y una cantidad X de veces que el zombie va a atacar a la planta
-- Lo que hago es, uso un fold, con una lambda que usa daniarPlanta y una semilla
-- que sea el valor inicial de mi planta, despues uso replicate para que genere una lista del estilo
-- [1,1,1...] que tenga la cantidad de veces atacado para que se aplique X veces esta funcion 
multiplesAtaquesAPlanta :: Planta -> Zombie -> Int -> Planta
multiplesAtaquesAPlanta planta zombie vecesAtacado =
  foldl (\planta _ -> daniarPlanta planta zombie) planta (replicate vecesAtacado 1)


fuegoCruzado :: Planta -> Zombie -> Int -> (Planta, Zombie)
fuegoCruzado planta zombie vecesAtacado =
  let zombieAtacado = daniarZombie planta zombie
      plantaAtacada = multiplesAtaquesAPlanta planta zombieAtacado vecesAtacado
  in (plantaAtacada, zombieAtacado)


-- Punto 5
estaMuertaLaPlanta::Planta->Bool
estaMuertaLaPlanta planta = puntoDeVida planta <= 0

estaMuertoElZombie::Zombie->Bool
estaMuertoElZombie zombie = nombre zombie == ""

resultadoDelCombate::(Planta,Zombie)->String
resultadoDelCombate (planta, zombie)
  | estaMuertaLaPlanta planta && estaMuertoElZombie zombie = "Ambos murieron"
  | estaMuertaLaPlanta planta = "Murio la planta"
  | estaMuertoElZombie zombie = "Murio el zombie"
  | otherwise = "Nadie murio"

