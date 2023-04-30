import Text.Show.Functions()
import Data.List()


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
                        |poderAtaque planta > puntoDeVida planta = "Atacante"
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

cantidadZombiesPeligrosos::[Zombie] -> Int
cantidadZombiesPeligrosos [] = 0
cantidadZombiesPeligrosos (zombie:resto) | zombiePeligroso zombie = 1 +  cantidadZombiesPeligrosos resto
                                        | otherwise 0 + cantidadZombiesPeligrosos resto

todosZombiesPeligrosos::[Zombie]->Bool
todosZombiesPeligrosos zombies = cantidadZombiesPeligrosos zombies == length zombies


estaEnPeligro :: LineaDeDefensa -> Bool
estaEnPeligro linea = (danioDePlantas.plantas linea < danioMordiscos.zombies linea) || (hayZombies && todosZombiesPeligrosos)

--c 
cantidadDePlantasProveedoras::[Planta] -> Int
cantidadDePlantasProveedoras [] = 0
cantidadDePlantasProveedoras (planta:resto) | especialidadPlanta planta == "Proveedora" = 1 + cantidadDePlantasProveedoras resto
                                            | otherwise 0 + cantidadDePlantasProveedoras resto

necesitaSerDefendida :: LineaDeDefensa -> Bool
necesitaSerDefendida linea = cantidadDePlantasProveedoras (plantas linea) == length (plantas linea)