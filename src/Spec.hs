module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "Hamburguesas" $ do -- bien
        it "calcularPrecio de cuartoDeLibra debería ser 54" $ do
            calcularPrecio cuartoDeLibra `shouldBe` 54

        it "calcularPrecio de pdepBurger debería ser 110" $ do
            calcularPrecio pdepBurger `shouldBe` 110

        it "calcularPrecio de dobleCuarto debería ser 84" $ do
            calcularPrecio dobleCuarto `shouldBe` 84

        it "calcularPrecio de bigPdep debería ser 89" $ do
            calcularPrecio bigPdep `shouldBe` 89

        it "calcularPrecio de delDia dobleCuarto debería ser 88" $ do
            calcularPrecio (delDia dobleCuarto) `shouldBe` 88

        it "hacerVeggie convierte cuartoDeLibra en veggie" $ do
            ingredientes (hacerVeggie cuartoDeLibra) `shouldBe` [Pan, PatiVegano, QuesoDeAlmendras, Pan]

        it "cambiarPanDePati convierte Pan en PanIntegral" $ do
            ingredientes (cambiarPanDePati cuartoDeLibra) `shouldBe` [PanIntegral, Carne, Cheddar, PanIntegral]

        it "dobleCuartoVegano tiene ingredientes veggie y pan integral" $ do
            ingredientes dobleCuartoVegano `shouldBe` [PatiVegano,QuesoDeAlmendras,PanIntegral,PatiVegano,QuesoDeAlmendras,PanIntegral]

        it "calcularPrecio de dobleCuartoVegano debería ser 76" $ do
            calcularPrecio dobleCuartoVegano `shouldBe` 76