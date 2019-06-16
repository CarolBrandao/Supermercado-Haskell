tamanhoLinha :: Int
tamanhoLinha = 30

type Codigo = Int
type Nome = String
type Preco = Int

type Produtos = [(Codigo, Nome, Preco)]
type Carrinho = [Codigo]
type Conta = [(Nome, Preco)]

tabelaProdutos :: Produtos
tabelaProdutos = [ (001, "Chocolate", 121),
                   (002, "Biscoito", 1010),
                   (003, "Laranja", 56),
                   (004, "Sabao", 21),
                   (005, "Batata Chips", 133),
                   (006, "Doritos", 450)]
                
formataCentavos::Codigo -> String 
formataCentavos codigo
    | mod codigo 100 > 9 = show (div codigo 100) ++"."++ show (mod codigo 100)
    | otherwise = show (div codigo 100) ++".0"++ show (mod codigo 100)

    
formataLinha :: (Nome,Preco) -> String 
formataLinha (n,p) =  n++ (replicate (tamanhoLinha - length n - length (show p)) '.') ++ formataCentavos p ++ "\n"

formataLinhas :: [(Nome,Preco)] -> String 
formataLinhas [] = []
formataLinhas x = formataLinha (head x) ++ formataLinhas (tail x)

    
formataTotal :: Preco -> String 
formataTotal p = formataLinha ("Total",p)

soma :: (Nome,Preco) -> Preco
soma (n,p) = p

calculoTotal :: Conta -> Preco 
calculoTotal x
    |  length x == 0 = 0
    | otherwise = soma (head x) + calculoTotal (tail x)

formataConta :: Conta -> String 
formataConta x = "Comercial Haskell\n\n" ++ formataLinhas x ++ "\n" ++formataTotal (calculoTotal x)++"\n\n"

confereCodigo :: (Codigo,Nome,Preco) -> Codigo -> Bool
confereCodigo (c,n,p) c1 
    | c == c1  = True
    | otherwise = False
retornaCodigo :: (Codigo,Nome,Preco) -> (Nome,Preco)
retornaCodigo (c,n,p) = (n,p)

procuraCodigo :: Produtos -> Codigo -> (Nome,Preco)
procuraCodigo [] y = ("Não encontrado",0)
procuraCodigo (x:xs) y = if (confereCodigo x y) then retornaCodigo x else procuraCodigo xs y

criarConta :: Produtos -> Carrinho -> Conta
criarConta prods car = map (procuraCodigo prods) car

fazCompra :: Produtos -> Carrinho -> String 
fazCompra prods car = formataConta (criarConta prods car)
