{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Todo.Finance
Description : Todo module to manage finance data.
Copyright   : (c) Simon Goller, 2016
License     : BSD

The module will provide the following features:
 * Provides many accounts which can store money.
 * Provide a wallet which can store n accounts.
 * Transfer value from one account to another.
-}

module Todo.Finance (
        Wallet,
        createAccountM,
        createAccount,
        transferM,
        transfer,
        getBalanceM,
        getBalance
        ) where

import Control.Lens
import Control.Monad.State.Lazy

data Wallet = Wallet {
    _prAccount :: [Account]
} deriving (Read, Show)

data Account = Account {
    _accTitle :: String,
    _accBalance :: Int,
    _accDecimalPlace :: Int
} deriving (Read, Show)


makeLenses ''Wallet
makeLenses ''Account

newAccount :: String -> Int -> Account
newAccount msg dec = Account msg 0 dec

createAccountM :: String -> Int -> State Wallet ()
createAccountM msg dec = do
    let account = newAccount msg dec
    prAccount %= \accs -> account : accs

createAccount :: String -> Int -> Wallet -> Wallet
createAccount title dec wallet = execState (createAccountM title dec) wallet

transferUnchecked :: String -> String -> Int -> State Wallet ()
transferUnchecked from to amount = do
    let fromAccT = accountTraversal from
        toAccT = accountTraversal to
    fromAccT.accBalance -= amount
    toAccT.accBalance += amount

transferM :: String -> String -> Int -> State Wallet Bool
transferM from to amount = do
    stat <- get
    if accountExists from stat && accountExists to stat
     then do
            transferUnchecked from to amount
            return True
     else return False

transfer :: String -> String -> Int -> Wallet -> Wallet
transfer from to amount wallet = execState (transferM from to amount) wallet

accountExists :: String -> Wallet -> Bool
accountExists title wallet =
    let accountTitles = map accountTitle $ wallet^.prAccount
        accountTitle acc = acc^.accTitle
    in title `elem` accountTitles

accountTraversal :: String -> Traversal' Wallet Account
accountTraversal title = prAccount.traverse.(filtered (\acc ->
        acc^.accTitle == title))

getBalanceM :: String -> State Wallet (Maybe Float)
getBalanceM title = do
    state <- get
    let account = map accountBalance $ state^..(accountTraversal title)
    if length account == 0 then return Nothing
                           else return $ Just $ head account

getBalance :: String -> Wallet -> Maybe Float
getBalance title wallet = evalState (getBalanceM title) wallet

accountBalance :: Account -> Float
accountBalance acc = (fromIntegral $ acc^.accBalance) /
                     (fromIntegral $ (acc^.accDecimalPlace) ^ 10)
