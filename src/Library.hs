module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas
    deriving (Eq, Show)

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

esCarne :: Ingrediente -> Bool
esCarne ingrediente = Carne == ingrediente

esPollo :: Ingrediente -> Bool
esPollo ingrediente = Pollo == ingrediente

tieneCarne :: Hamburguesa -> Bool
tieneCarne hamburguesa = any esCarne (ingredientes hamburguesa)

tienePollo :: Hamburguesa -> Bool
tienePollo hamburguesa = any esPollo (ingredientes hamburguesa)

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    | tieneCarne hamburguesa = hamburguesa {ingredientes = Carne : ingredientes hamburguesa}
    | tienePollo hamburguesa = hamburguesa {ingredientes = Pollo : ingredientes hamburguesa}
    | otherwise = hamburguesa

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingredienteAAgregar hamburguesa = hamburguesa {ingredientes = ingredienteAAgregar : ingredientes hamburguesa}

agregarIngredientes :: [Ingrediente] -> Hamburguesa -> Hamburguesa
agregarIngredientes [] hamburguesa = hamburguesa
agregarIngredientes (ingrediente : otrosIngredientes) hamburguesa =
    agregarIngredientes otrosIngredientes (agregarIngrediente ingrediente hamburguesa)


calcularPrecio :: Hamburguesa -> Number
calcularPrecio hamburguesa = precioBase hamburguesa + sum (map precioIngrediente (ingredientes hamburguesa))

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa {precioBase = precioBase hamburguesa * (1 - porcentaje/100)}

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa {precioBase = 20, ingredientes = [Pan, Carne, Cheddar, Pan]}

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 (agrandar (agrandar (agregarIngredientes [Panceta, Cheddar] cuartoDeLibra)))


-- >>> calcularPrecio pdepBurger
-- 110

dobleCuarto :: Hamburguesa
dobleCuarto = agrandar (agregarIngrediente Cheddar cuartoDeLibra)

-- >>> calcularPrecio dobleCuarto
-- 84

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

-- >>> calcularPrecio bigPdep
-- 89

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 30 (agregarIngrediente Papas hamburguesa)

-- >>> calcularPrecio (delDia bigPdep)