module Mecanics where


import Data 

atualizarPosicoes :: Direcao -> Cobra -> Cobra
atualizarPosicoes direcao (Cobra (cabeca:corpo) (vx,vy)) = 
    let novacabeca = calcularNovaCabeca direcao cabeca (vs,vy)
        novocorpo = init (cabeca:corpo)
    in Cobra (novacabeca:novocorpo ) (vx,vy)

calcularNovaCabeca ::Direcao -> Posicao -> Velocidade -> Posicao
calcularNovaCabeca Up (x, y) (vx, vy) = (_, y +vy)
calcularNovaCabeca Down (x, y) ( vx, vy) = (_, y-vy)
calcularNovaCabeca Left (x, y) (vx, vy) = (x-vx, _)
calcularNovaCabeca Right (x, y) (vx. vy) =(x+vx, _)



