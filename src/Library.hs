module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | BaconDeTofu | PanIntegral
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
precioIngrediente PatiVegano = 10
precioIngrediente BaconDeTofu = 12
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

-- Punto 1

esCarne :: Ingrediente -> Bool
esCarne ingrediente = Carne == ingrediente

esPollo :: Ingrediente -> Bool
esPollo ingrediente = Pollo == ingrediente

esCheddar :: Ingrediente -> Bool
esCheddar ingrediente = ingrediente == Cheddar

tieneCarne :: Hamburguesa -> Bool
tieneCarne hamburguesa = any esCarne (ingredientes hamburguesa)

tienePollo :: Hamburguesa -> Bool
tienePollo hamburguesa = any esPollo (ingredientes hamburguesa)

tieneCheddar :: Hamburguesa -> Bool
tieneCheddar hamburguesa = any esCheddar (ingredientes hamburguesa)

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa
    | tieneCarne hamburguesa = hamburguesa {ingredientes = Carne : ingredientes hamburguesa}
    | tienePollo hamburguesa = hamburguesa {ingredientes = Pollo : ingredientes hamburguesa}
    | tienePatiVegano hamburguesa = hamburguesa {ingredientes = PatiVegano : ingredientes hamburguesa}
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

-- Punto 2

dobleCuarto :: Hamburguesa
dobleCuarto = agrandar (agregarIngrediente Cheddar cuartoDeLibra)

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 30 (agregarIngrediente Papas hamburguesa)

-- Punto 3

tienePatiVegano :: Hamburguesa -> Bool
tienePatiVegano hamburguesa = any (== PatiVegano) (ingredientes hamburguesa)

reemplazarIngredienteVeggie :: Ingrediente -> Ingrediente
reemplazarIngredienteVeggie Carne = PatiVegano
reemplazarIngredienteVeggie Pollo = PatiVegano
reemplazarIngredienteVeggie Cheddar = QuesoDeAlmendras
reemplazarIngredienteVeggie Panceta = BaconDeTofu
reemplazarIngredienteVeggie ingrediente = ingrediente

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = hamburguesa {ingredientes = map reemplazarIngredienteVeggie (ingredientes hamburguesa)
}
-- >>> :t filter
-- filter :: (a -> Bool) -> [a] -> [a]

reemplazarPan :: Ingrediente -> Ingrediente
reemplazarPan Pan = PanIntegral
reemplazarPan ingrediente = ingrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = hamburguesa {
    ingredientes = map reemplazarPan (ingredientes hamburguesa)
}

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati (hacerVeggie dobleCuarto)