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

--Parte 2

data Arma = UnArma {tipoArma :: String ,
                    inteligencia::Int,
                    nombre::String,
                    materialForjado::String,
                    rangoMaximo::Int ,
                    longitudHilo::Int,
                    danioBase::Int ,
                    almasCosechadas::Int}deriving Show



poderArma (UnArma "baculo" inteligencia nombre _ _ _ _ _)= (inteligencia) + (length(nombre))
poderArma (UnArma "arco" _ _ _ rangoMaximo longitudHilo danioBase _)=((rangoMaximo)*(longitudHilo))+(danioBase)
poderArma (UnArma "espada" _ _ materialForjado _ _ _ almasCosechadas) = (almasCosechadas) * (coeficienteDeMaterial materialForjado)
coeficienteDeMaterial materialForjado | materialForjado == "metal" =3
                                      | materialForjado == "madera"=2


--Parte 3
setDef valor (def,dur)= (valor,dur)
defCambiada cambio (def,dur) = (cambio def, dur)
frenesi personaje =UnPersonaje{armadura= map (defCambiada (*1.2)) (armadura personaje)}

mantoEtereo personaje = UnPersonaje{armadura= map(defCambiada (+3)) (armadura personaje), vida = (vida personaje) - 100}

berserker personaje =UnPersonaje{armadura = map (setDef 2) (armadura personaje),arma= UnArma{ materialForjado=cambioMadera ((materialForjado.arma) personaje)}}
cambioMadera "madera" = "metal"
cambioMadera material=material

espejoKarma buff personaje = (buff.buff) personaje

sucesionDeBuffs buffs personaje= foldl1 personaje $buffs

buffCreativo  personaje = UnPersonaje{vida=99999, armadura=[(99999,100),(99999,100)], arma = UnArma "baculo" 200 "Baculo de chuck norris" "fuego" 0 0 0 0} --Convierte a tu personaje en chuck norris

miPersonajeDePrueba1 = UnPersonaje {vida= 100, armadura= [(200,10),(50,50)],arma= UnArma "espada"  0 "espada del bien" "madera" 0 0 0 10}
--miPersonajeDePrueba2 = UnPersonaje {vida= 150, armadura= [(200,10),(4,50)],arma= UnArma "espada"  _ _ "metal" _ _ _ 10}
--miPersonajeDePrueba3 = UnPersonaje {vida= 80, armadura= [(200,10)],arma= UnArma "espada"  _ _ "madera" _ _ _ 80}
-- 1)Prueba con personaje:>> berserker miPersonajeDePrueba1

-- 2)
poder tipoDePoder personaje = tipoDePoder personaje
personajeIntacto buff personaje =  ((poder (poderArma.arma) personaje) == (poder (poderArma.arma.buff) personaje)) && ((poder poderDeDefensa personaje) == (poder (poderDeDefensa.buff) personaje))
detectoSiEsInofensivo personajes buff= all (personajeIntacto buff) personajes

-- 3) Utilizando orden superior para cambiar la defensa evite repetir logica y en sucesion de buffs utilizo foldl1 para poder aplicar personaje a cada funcion. para saber si
-- un buff es inofensivo paso el tipo de poder (una funcion) que quiero saber a la funcion poder y asi simplifico el codigo y no repito logica.
--Parte 4

desgastePersonaje personaje desgaste =UnPersonaje{ armadura=desgasteArmadura (armadura personaje) desgaste}

desgastePieza:: Pieza-> Int->Pieza
desgastePieza pieza desgaste=((defensa pieza) , max ((durabilidad pieza) - desgaste) 0)

--desgasteArmadura:: [Pieza]->Int->[Pieza]
desgasteArmadura [] desgaste= []
desgasteArmadura [pieza] desgaste=[desgastePieza pieza desgaste]
desgasteArmadura (pieza:restoArmadura) desgaste=  (desgastePieza pieza desgaste) : (desgasteArmadura restoArmadura (div desgaste 2))


--Para pensar: En el caso del desgaste como la lista de armadura es infinita nunuca sucederia el caso no recursuvo, por lo tanto nunca saldria del bucle y no tendriamos una respuesta.
--en los otros casos dado que haskell utiliza el Lazy Evaluation aplicaria el buff a las partes sin tener que ver la lista entera primero, pero el programa nunca terminaria porque va
--a intentar aplicar el buff a todas las partes. Lo que si veriamos es una lista de las partes ya buffeadas que se expande infinitamente

--Parte 5

buffosAClan clan buffs = map ($clan) buffs
poderesDeClanesBuffados clan buffs tipoDePoder=map sum (soloAtributo tipoDePoder (buffosAClan clan buffs))
mejorPoderClanSegun tipoDePoder buffs clan= maximum (poderesDeClanesBuffados clan buffs tipoDePoder)
soloAtributo atributo=  map atributo
poderClan tipoDePoder buffs clan=mejorPoderClanSegun tipoDePoder buffs  clan
--ganaAlAtacar clanAtacante buffsAtacante clanDefensivo buffsDefensivo = (poderClan (poderArma.arma) buffsAtacante clanAtacante) > (poderClan poderDeDefensa buffsDefensivo clanDefensivo)
