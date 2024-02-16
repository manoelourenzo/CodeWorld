{-# LANGUAGE OverloadedStrings#-} 
import CodeWorld
import System.Random
import Data.Text

rndmLst :: Int -> IO [Int]
rndmLst n = sequence (Prelude.replicate n (randomRIO (-6, 9::Int)))


main = do
       xs <- rndmLst 10000
       activityOf  (mundoInicial xs) update visualization

mundoInicial :: [Int] -> World
mundoInicial xs = World  {posBaloes = [(0,-9)] ,
                      tempo = 2,
                    randomList= xs,
                    numeroBaloes = 0,
                    posXdard = -9,
                    posYdard = 0,
                    velocidadeYdard = 2,
                    placar = 0} 
periodo = 2

data World = World {posBaloes :: [Point] ,
                    tempo :: Double,
                    randomList :: [Int],
                    numeroBaloes :: Int,
                    posXdard :: Double,
                    posYdard :: Double,
                    velocidadeYdard :: Double,
                    placar :: Int}

visualization :: World -> Picture
visualization mundo@World {posBaloes = qs,posXdard = xd,posYdard=yd,placar = score} = viewScore score & fundoPlacar & molduraPlacar & nuvem & baloesVisualization (qs) &
                                             translated xd yd dard & grama & sol  &fundo 
                                             

baloesVisualization :: [Point] -> Picture 
baloesVisualization [] = blank
baloesVisualization ((x,y) : qs) = translated x y balao & baloesVisualization qs

balao = curva & fio
  where
    curva = colored green (solidClosedCurve [(0,-1), (-0.7,0.3), (0,1), (0.7,0.3), (0,-1)])
    fio =colored (brighter 0.5 yellow) (curve [(0,-1), (0.1, -1.1), (0, -1.2), (-0.1, -1.3), (0,-1.4), 
                     (0.1, -1.5), (0, -1.6), (-0.1, -1.7), (0, -1.8), (0.1, -1.9),
                     (0, -2)])
velocidadeBalao = 6 :: Double


balaoUpdate :: Double ->World -> World
balaoUpdate t mundo@World {posBaloes = qs,tempo = tm,numeroBaloes = nb,randomList = nxs}
   | ntm <= 0  = mundo { posBaloes = ( [fromIntegral x | x <- nxs] !! nb, -9) : nqs,tempo=
                                      periodo, numeroBaloes = nb+ 1} 
   | otherwise = mundo {posBaloes = nqs,tempo = ntm}
  where
    ntm = tm - t
    nqs = atualizaBaloes t qs

update :: Event -> World -> World 
update (KeyPress "Up") mundo@World    {posXdard = -9, posYdard = yd}   =mundo {posXdard = -9,posYdard = yd + 1 }
update (KeyRelease "Up") mundo@World  {posXdard = -9, posYdard = yd}   =mundo {posXdard = -9,posYdard = yd }
update (KeyPress "Down") mundo@World  {posXdard = -9, posYdard = yd}  =mundo {posXdard = -9,posYdard = yd -1}
update (KeyRelease "Down")mundo@World {posXdard = -9, posYdard = yd}   =mundo {posXdard = -9,posYdard = yd }
update (KeyPress " ")  mundo@World    {posXdard = xd}   =mundo {posXdard = xd + 0.1 }
update (TimePassing t) w = balaoUpdate t . updateDardo t $ w 
 
update _ w                          = w

atualizaBaloes t qs = [ (x, y + velocidadeBalao * t) | (x, y) <- qs  ]

lançamento :: Double -> World -> World
lançamento t  mundo@World  {posXdard = xd,posYdard = yd,velocidadeYdard = velDard} =
              mundo{ posXdard = xd + velH * t,posYdard = yd + velDard * t + 1/2 * g * t^2,velocidadeYdard = velDard + g * t}
  
  
updateDardo :: Double -> World -> World 
updateDardo t mundo@World  {placar = score,numeroBaloes = nb,posBaloes = qs,posXdard = xd,posYdard =yd,velocidadeYdard =velDard}
     |xd >= 11  = mundo {posXdard = -9,posYdard =0,velocidadeYdard =2}
     |xd >= (-8.9) =lançamento t mundo {posBaloes= restantes,placar= score + quantidadeEstourados}
     |otherwise = mundo 
     where 
      restantes = Prelude.filter(baloesSobrando xd yd ) qs
      quantidadeEstourados =Prelude.length qs - Prelude.length restantes  
dard :: Picture
dard = corpo & cauda & cabeca & ponta
  where
    cauda = solidPolygon [(-1,0.5), (-1, -0.5),(-0.5,0) ]
    corpo =  colored red ( solidRectangle 2 0.3) 
    cabeca = blank
    ponta = solidPolygon  [(1,0.15), (1, -0.15),(2,0) ]

baloesSobrando :: Double -> Double -> Point -> Bool
baloesSobrando xd yd (x,y) = not (estouro xd yd (x,y))

estouro :: Double -> Double -> Point -> Bool
estouro xd yd (x,y) =pontoColisao <= x+0.7 && pontoColisao >= x - 0.7 && yd >= y - 1 && yd <= y + 1
       where
        pontoColisao = xd + 2
        
viewScore :: Int -> Picture
viewScore sc=  (translated 0 8)(lettering $ pack ("score :" ++ show sc))

fundo = colored ( lighter 0.05 blue) (solidRectangle 20 20)
grama = colored (darker 0.27 green) (translated 0 (-10)  (solidRectangle 20 10))
sol = colored yellow ((translated 10 10) (solidCircle 4))
fundoPlacar = translated 0 8 (colored white (solidRectangle 4 2))
molduraPlacar = translated 0 8 (colored brown (solidRectangle 4.18 2.18))
nuvem = translated (-8) 8.5 (colored white (solidCircle 1.3)) &  translated (-6.5) 8.5 (colored white (solidCircle 1.3)) & translated (-5) 8.5 (colored white (solidCircle 1.3))

g = -5
velH = 20
velVIni = 2

