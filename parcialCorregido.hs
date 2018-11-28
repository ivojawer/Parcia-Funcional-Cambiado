import Data.List
--------------------------------------------------------------------------------------------------------



type Pieza = (Int,Int)

data Personaje = UnPersonaje{
vida :: Int,
armadura:: [Pieza],
arma::Arma
}deriving Show

durabilidad (_ ,dur)=dur
defensa (def, _)=def

--Parte 1
filtroPartesNoRotas armadura= filter ((>0).durabilidad) armadura
defensaPartes personaje= map defensa (filtroPartesNoRotas (armadura personaje))
poderDeDefensa personaje= (vida personaje)+ (sum (defensaPartes personaje))

--Parte 2   MAL

data Arma = Baculo {nombre:: String, inteligencia:: Int} | Arco {rangoMaximo:: Int, longitudHilo:: Int, danioBase:: Int}  | Espada {almasCosechadas:: Int, materialForjado:: String} deriving (Show)


--data Arma = UnArma {tipoArma :: String ,
--                    inteligencia::Int,
--                    nombre::String,
--                    materialForjado::String,
--                    rangoMaximo::Int ,
--                    longitudHilo::Int,
--                    danioBase::Int ,
--                    almasCosechadas::Int}deriving Show



poderArma (Baculo nombre inteligencia)= (inteligencia) + (length(nombre))
poderArma (Arco rangoMaximo longitudHilo danioBase)=((rangoMaximo)*(longitudHilo))+(danioBase)
poderArma (Espada  almasCosechadas materialForjado) = (almasCosechadas) * (coeficienteDeMaterial materialForjado)
coeficienteDeMaterial "metal" = 3
coeficienteDeMaterial "madera" = 2
coeficienteDeMaterial _ = 1


--Parte 3
setDef valor (def,dur)= (valor,dur)
defCambiada cambio (def,dur) = (cambio def, dur)
frenesi personaje = personaje{armadura= map (defCambiada (*2)) (armadura personaje)}

mantoEtereo personaje = personaje{armadura= map(defCambiada (+3)) (armadura personaje), vida = (vida personaje) - 100}

berserker personaje =cambioMadera personaje{armadura = map (setDef 2) (armadura personaje)}
cambioMadera (UnPersonaje vida arm (Espada almas "madera")) = UnPersonaje vida arm (Espada almas "metal")
cambioMadera personaje=personaje

espejoKarma buff personaje = (buff.buff) personaje

sucesionDeBuffs buffs personaje= foldr ($) personaje buffs

buffCreativo  personaje = personaje{vida=99999, armadura=[(99999,100),(99999,100)], arma = Baculo "Baculo de chuck norris" 9999} --Convierte a tu personaje en chuck norris

pjPrueba = UnPersonaje {vida= 100, armadura= [(200,10),(50,50)],arma= Espada 10 "madera"}

-- 1)Prueba con personaje:>> berserker pjPrueba

-- 2)
poder tipoDePoder personaje = tipoDePoder personaje
personajeIntacto buff personaje =  ((poder (poderArma.arma) personaje) == (poder (poderArma.arma.buff) personaje)) && ((poder poderDeDefensa personaje) == (poder (poderDeDefensa.buff) personaje))
detectoSiEsInofensivo personajes buff= all (personajeIntacto buff) personajes

-- 3) Utilizando orden superior para cambiar la defensa evite repetir logica y en sucesion de buffs utilizo foldl1 para poder aplicar personaje a cada funcion. para saber si
-- un buff es inofensivo paso el tipo de poder (una funcion) que quiero saber a la funcion poder y asi simplifico el codigo y no repito logica.
--Parte 4

desgastePersonaje personaje desgaste =personaje{ armadura=desgasteArmadura (armadura personaje) desgaste}

desgastePieza:: Pieza-> Int->Pieza
desgastePieza pieza desgaste=((defensa pieza) , max ((durabilidad pieza) - desgaste) 0)

--desgasteArmadura:: [Pieza]->Int->[Pieza]
desgasteArmadura [] desgaste= []
desgasteArmadura [pieza] desgaste=[desgastePieza pieza desgaste]
desgasteArmadura (pieza:restoArmadura) desgaste=  (desgastePieza pieza desgaste) : (desgasteArmadura restoArmadura (div desgaste 2))


--Para pensar: En el caso del desgaste como la lista de armadura es infinita nunuca sucederia el caso no recursuvo, por lo tanto nunca saldria del bucle y no tendriamos una respuesta.
--en los otros casos dado que haskell utiliza el Lazy Evaluation aplicaria el buff a las partes sin tener que ver la lista entera primero, pero el programa nunca terminaria porque va
--a intentar aplicar el buff a todas las partes. Lo que si veriamos es una lista de las partes ya buffeadas que se expande infinitamente

--Parte 5 REGULAR

data Clan = UnClan {
 miembros::[Personaje],
 buffs :: [Personaje -> Personaje]}


maximoSegun criterio lista = foldl1 (maxSegun criterio) lista

maxSegun criterio b c
 | criterio b > criterio c = b
 | otherwise = c


--buffosAClan clan buffs = map ($clan) buffs
--poderesDeClanesBuffados clan buffs tipoDePoder=map sum (soloAtributo tipoDePoder (buffosAClan clan buffs))
--mejorPoderClanSegun tipoDePoder buffs clan= maximum (poderesDeClanesBuffados clan buffs tipoDePoder)
--soloAtributo atributo=  map atributo
--poderClan tipoDePoder buffs clan=mejorPoderClanSegun tipoDePoder buffs  clan
--ganaAlAtacar clanAtacante buffsAtacante clanDefensivo buffsDefensivo = (poderClan (poderArma.arma) buffsAtacante clanAtacante) > (poderClan poderDeDefensa buffsDefensivo clanDefensivo)

poderDeAtaque = (poderArma.arma)

ganaAlAtacar clanAtacante clanDefensivo = poderClan (poderArma.arma) clanAtacante > poderClan poderDeDefensa clanDefensivo

poderClan tipoDePoder clan = (sum.map (poderConElMejorBuff tipoDePoder clan).miembros) clan

poderConElMejorBuff tipoDePoder clan personaje = (tipoDePoder. ($personaje).mejorBuffPara tipoDePoder clan) personaje


aplicarBuff personaje buff  = buff personaje

mejorBuffPara tipoDePoder clan personaje = maximoSegun (tipoDePoder.aplicarBuff personaje) (buffs clan)
