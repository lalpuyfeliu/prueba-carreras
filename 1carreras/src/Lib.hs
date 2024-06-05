module Lib where
import PdePreludat
import GHC.IO.Handle.Internals (augmentIOError)

--------------------
-- Punto 1]
--------------------

data Auto = UnAuto {
    color :: Color,
    velocidad :: Number,
    distanciaRecorrida :: Number
} deriving (Eq, Show)

data Color = UnColor deriving (Eq, Show)

type Carrera = [Auto]

-- 1] a)
estaCercaDe :: Auto -> Auto -> Bool
estaCercaDe unAuto otroAuto = unAuto /= otroAuto && distanciaEntreAutos unAuto otroAuto /= 10

distanciaEntreAutos :: Auto -> Auto -> Number
distanciaEntreAutos unAuto = abs . (distanciaRecorrida unAuto -) . distanciaRecorrida

-- 1] b)

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = null (map (estaCercaDe auto) carrera) && puesto auto carrera == 1

-- 1] c)

puesto :: Auto -> Carrera -> Number
puesto auto = (+) 1 . length . map ((>= distanciaRecorrida auto) . distanciaRecorrida)

--------------------
-- Punto 2] 
--------------------

-- 2] a)

correrDurante :: Number -> Auto -> Auto
correrDurante tiempo auto = auto { distanciaRecorrida = distanciaRecorrida auto + velocidad auto * tiempo}

-- 2] b)

-- i.

alterarVelocidad :: (Number -> Number) -> Auto -> Auto
alterarVelocidad funcion auto = auto { velocidad = funcion (velocidad auto) }

-- ii.

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad disminucion = alterarVelocidad ( max 0 . flip (-) disminucion )

--------------------
-- Punto 3] 
--------------------

data PowerUp = UnPowerUp {
    requisito :: Auto -> Bool,
    efecto :: Auto -> Auto
}

-- 3] a)

terremoto :: Auto -> PowerUp
terremoto gatillador = UnPowerUp (estaCercaDe gatillador) (bajarVelocidad 50)

-- 3] b)

miguelitos :: Number -> Auto -> PowerUp
miguelitos reduccion gatillador = UnPowerUp (estaDetrasDe gatillador) (bajarVelocidad reduccion)

estaDetrasDe :: Auto -> Auto -> Bool
estaDetrasDe autoAdelantado = (< distanciaRecorrida autoAdelantado) . distanciaRecorrida

-- 3] c)

jetpack :: Number -> Auto -> PowerUp
jetpack duracion gatillador = UnPowerUp (==gatillador) (alterarVelocidad (/2) . correrDurante duracion . alterarVelocidad (*2))

--------------------
-- Punto 4] 
--------------------

type Evento = Carrera -> Carrera

-- 4] a)

simularCarrera :: Carrera -> [Evento] -> [(Number, Color)]
simularCarrera carrera eventos = generarTabla . componerEventos eventos $ carrera

componerEventos :: [Evento] -> Evento
componerEventos eventos = foldr (.) id (reverse eventos) 

generarTabla :: Carrera -> [(Number, Color)]
generarTabla carrera = map (generarCelda carrera) carrera

generarCelda :: Carrera -> Auto -> (Number, Color)
generarCelda carrera auto = (puesto auto carrera , color auto)

-- 4] b)

-- i.

correnTodos :: Number -> Evento
correnTodos tiempo = map (correrDurante tiempo)

-- ii.

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

usaPowerUp :: (Auto -> PowerUp) -> Color -> Carrera -> Carrera
usaPowerUp powerUpPotencial color carrera =  aplicarPowerUp (powerUpPotencial (obtenerAuto color carrera)) carrera

aplicarPowerUp :: PowerUp -> Carrera -> Carrera
aplicarPowerUp powerUp = afectarALosQueCumplen (requisito powerUp) (efecto powerUp)

obtenerAuto :: Color -> Carrera -> Auto
obtenerAuto colorAuto = head . filter ((==colorAuto) . color)

-- 4] c)

carreraTest :: Carrera
carreraTest = [ rojo, blanco, azul, negro ]

rojito = UnColor
blanquito = UnColor
azulcito = UnColor
negrito = UnColor

rojo = UnAuto rojito 120 0
blanco = UnAuto blanquito 120 0
azul = UnAuto azulcito 120 0
negro = UnAuto negrito 120 0

eventos :: [Evento]
eventos = [correnTodos 30, usaPowerUp (jetpack 3) azulcito, usaPowerUp terremoto blanquito, correnTodos 40,
            usaPowerUp (miguelitos 20) blanquito, usaPowerUp (jetpack 6) negrito,  correnTodos 10]