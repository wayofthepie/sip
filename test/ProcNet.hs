{-# LANGUAGE
    OverloadedStrings
    #-}
module ProcNet where

import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString

import Test.Tasty
import Test.Tasty.HUnit

import Linux.Parser.Internal.ProcNet


unitTests :: TestTree
unitTests =
    testGroup "Unit Tests"
        [ testCaseSteps "Test /proc/net/dev parser" $ \step -> do
            step "Parse file"
            runNetDevp >>= \res -> case res of
                Right (first, second) -> do
                    step "Verifying first row of parsed data"
                    verifyNetDevData first netDevpExpectedOne
                    verifyNetDevData second netDevpExpectedTwo
                Left _ -> assertFailure "Parsing test/data/proc_net_dev failed!"
        ]

-- Assert the actual net dev data is equal to the
-- expected net dev data.
verifyNetDevData :: NetDeviceInfo -> NetDeviceInfo -> IO ()
verifyNetDevData actual expected = do
    assertEqual "Device name incorrect"
        (_deviceName actual) (_deviceName expected)

    assertEqual "Received bytes incorrect"
        (_rcvBytes actual) (_rcvBytes expected)

    assertEqual "Received packets incorrect"
        (_rcvPackets actual) (_rcvPackets expected)

    assertEqual "Received errors incorrect"
        (_rcvErrors actual) (_rcvErrors expected)

    assertEqual "Received drop incorrect"
        (_rcvDrop actual) (_rcvDrop expected)

    assertEqual "Received fifo incorrect"
        (_rcvFifo actual) (_rcvFifo expected)

    assertEqual "Received frame incorrect"
        (_rcvFrame actual) (_rcvFrame expected)

    assertEqual "Receieved compressed incorrect"
        (_rcvCompressed actual) (_rcvCompressed expected)

    assertEqual "Received multicast incorrect"
        (_rcvMulticast actual) (_rcvMulticast expected)

    assertEqual "Transmitted bytes incorrect"
        (_transBytes actual) (_transBytes expected)

    assertEqual "Transmitted packets incorrect"
        (_transPackets actual) (_transPackets expected)

    assertEqual "Transmitted errors incorrect"
        (_transErrors actual) (_transErrors expected)

    assertEqual "Transmitted drop incorrect"
        (_transDrop actual) (_transDrop expected)

    assertEqual "Transmitted fifo incorrect"
        (_transFifo actual) (_transFifo expected)

    assertEqual "Transmitted colls incorrect"
        (_transColls actual) (_transColls expected)

    assertEqual "Transmitted carrier incorrect"
        (_transCarrier actual) (_transCarrier expected)

    assertEqual "Transmitted compressed incorrect"
        (_transCompressed actual) (_transCompressed expected)

runNetDevp ::IO ( Either () (NetDeviceInfo, NetDeviceInfo) )
runNetDevp = do
    bs <- BS.readFile "test/data/proc_net_dev"
    let out = parseOnly netDevp bs
    case out of
        Right list-> return $ Right (head list, head $ tail list)
        Left _    -> return $ Left ()

netDevpExpectedOne :: NetDeviceInfo
netDevpExpectedOne = NetDeviceInfo
    { _deviceName = "eth0"
    , _rcvBytes = "5428767"
    , _rcvPackets = "5115"
    , _rcvErrors = "0"
    , _rcvDrop = "0"
    , _rcvFifo = "0"
    , _rcvFrame = "0"
    , _rcvCompressed = "0"
    , _rcvMulticast = "0"
    , _transBytes = "345242"
    , _transPackets = "4195"
    , _transErrors = "0"
    , _transDrop = "0"
    , _transFifo = "0"
    , _transColls = "0"
    , _transCarrier = "0"
    , _transCompressed = "0"
    }

netDevpExpectedTwo :: NetDeviceInfo
netDevpExpectedTwo = NetDeviceInfo
    { _deviceName = "lo"
    , _rcvBytes = "0"
    , _rcvPackets = "0"
    , _rcvErrors = "0"
    , _rcvDrop = "0"
    , _rcvFifo = "0"
    , _rcvFrame = "0"
    , _rcvCompressed = "0"
    , _rcvMulticast = "0"
    , _transBytes = "0"
    , _transPackets = "0"
    , _transErrors = "0"
    , _transDrop = "0"
    , _transFifo = "0"
    , _transColls = "0"
    , _transCarrier = "0"
    , _transCompressed = "0"
    }


