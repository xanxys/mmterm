{-# LANGUAGE ViewPatterns, TupleSections #-}
-- | Easy logging of images and matrices(palnned) for CV research.
-- messages are send via TCP connection as MessagePack objects.
--
-- * ["str", utf8 string]: string
--
-- * ["uni", "u8", shape, raw]: uniform array
--
module Main where
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.IORef
import Data.List
import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events as Ev
import Graphics.Rendering.Cairo
import System.IO
import qualified Data.ByteString as BS
import Text.Printf
import Network
import Data.Word
import Data.Array.MArray
import Data.MessagePack as MP
import Data.Attoparsec.ByteString as Atto
import qualified Data.Array.Base as Unsafe
import qualified Codec.Binary.UTF8.String
import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

type Model = (MVar (Seq.Seq Message), MVar (Set.Set String))

data Message =
    StringMessage String |
    UniformArrayMessage [Int] BS.ByteString
    deriving(Show)

main = do
    let port = 8001
    (mseq, mClients) <- runServer port
    
    initGUI
    window <- windowNew
    vb <- vBoxNew False 1
    status <- hBoxNew False 0
    lb <- labelNew (Just $ printf "listening on port %d" port)
    lbClients <- labelNew (Just "")
    clear <- buttonNewFromStock "gtk-clear"
    on clear buttonActivated $ void $ swapMVar mseq Seq.empty
    hb <- hBoxNew False 0
    tb <- vBoxNew False 0
    refill mseq tb 0
    adj <- adjustmentNew 0 1 100 1 20 20
    onScroll window $ \ev->do
        v0 <- adjustmentGetValue adj
        delta <- adjustmentGetPageIncrement adj
        case Ev.eventDirection ev of 
            ScrollDown -> adjustmentSetValue adj (v0+delta/3)
            ScrollUp -> adjustmentSetValue adj (v0-delta/3)
            _ -> return ()
        return True
    scr <- vScrollbarNew adj
    on scr valueChanged (adjustmentGetValue adj >>= refill mseq tb)
    boxPackStart hb tb PackGrow 0
    boxPackStart hb scr PackNatural 0
    boxPackStart status lb PackGrow 0
    boxPackStart status clear PackNatural 0
    boxPackStart vb status PackNatural 0
    boxPackStart vb lbClients PackNatural 0
    boxPackStart vb hb PackGrow 0
    set window [containerChild := vb]
    
    nmseq <- newIORef 0
    nmcls <- newIORef 0
    let
        updateLog=do
            prev_n <- readIORef nmseq
            curr_n <- withMVar mseq (return . Seq.length)
            when (curr_n/=prev_n) $ do
                adjustmentSetUpper adj $ fromIntegral curr_n
                adjustmentGetValue adj >>= refill mseq tb
            writeIORef nmseq curr_n
        updateClients = do
            prev_n <- readIORef nmcls
            curr_n <- withMVar mClients (return . Set.size)
            when (curr_n/=prev_n) $ do
                str <- withMVar mClients (return . intercalate " / " . Fold.toList)
                labelSetText lbClients str
            writeIORef nmcls curr_n
        
    timeoutAdd (updateLog >> updateClients >> return True) 50
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI


-- seems ok
refill mseq box fromd=do
    let from=floor $ realToFrac fromd-1
    mapM_ widgetDestroy =<< containerGetChildren box
    msgs <- withMVar mseq $ return . Fold.toList . fst . Seq.splitAt 20 . snd . Seq.splitAt from
    mapM_ (\msg->do{l<-instantiateView msg; boxPackStart box l PackNatural 0}) msgs
    widgetShowAll box


instantiateView :: Message -> IO Widget
instantiateView (StringMessage x)=do
    l <- labelNew (Just x)
    set l [miscXalign := 0]
    return $ castToWidget l

instantiateView (UniformArrayMessage shape raw)=do
    l <- drawingAreaNew
    surf <- arrayToSurface shape raw
    widgetSetSizeRequest l (-1) $ max 25 (head shape)
    onExpose l $ \ev -> do
        dw <- widgetGetDrawWindow l
        w <- imageSurfaceGetWidth surf
        renderWithDrawable dw $ do
            save
            when (w<15) $ scale 3 3
            setSourceSurface surf 0 0
            getSource >>= flip patternSetFilter FilterNearest
            paint
            restore
        return True
    
    return $ castToWidget l

arrayToSurface :: [Int] -> BS.ByteString -> IO Surface
arrayToSurface [h,w] raw = arrayToSurface [h,w,1] raw
arrayToSurface [h,w,c] raw = do
    surf <- createImageSurface FormatRGB24 w h
    arr <- imageSurfaceGetPixels surf -- BGRA packing
    case c of
        1 -> mapM_ (\i -> transfer1 arr i) [0..w*h-1]
        3 -> mapM_ (\i -> transfer3 arr i) [0..w*h-1]
        _ -> mapM_ (\i -> transfer arr i) [0..w*h-1]
    return surf
    where
        transfer arr i = do
            mapM_ (\d -> Unsafe.unsafeWrite arr (i*4+d) $ BS.index raw (i*c+d)) [0..min c 3-1]
        transfer1 arr i = do
            let v = BS.index raw i
            Unsafe.unsafeWrite arr (i*4+0) v
            Unsafe.unsafeWrite arr (i*4+1) v
            Unsafe.unsafeWrite arr (i*4+2) v
        transfer3 arr i = do
            Unsafe.unsafeWrite arr (i*4+0) $ BS.index raw (i*3+2)
            Unsafe.unsafeWrite arr (i*4+1) $ BS.index raw (i*3+1)
            Unsafe.unsafeWrite arr (i*4+2) $ BS.index raw (i*3+0)
arrayToSurface shape _ = do
    putStrLn "unknown shape"
    print shape
    createImageSurface FormatRGB24 1 1



runServer :: Int -> IO Model
runServer port = do
    mLog <- newMVar Seq.empty
    mClients <- newMVar Set.empty
    
    ch <- newChan
    forkIO $ forever $ do
        x <- readChan ch
        modifyMVar_ mLog $ return . (Seq.|> x)
    
    sock <- listenOn (PortNumber $ fromIntegral port)
    putStrLn "started listening"
    forkIO $ forever $ do
        (h, host, port) <- accept sock
        let clientName = printf "%s %s" host (show port)
        modifyMVar_ mClients $ return . Set.insert clientName
        
        hSetBuffering h NoBuffering
        forkIO $ do
            handleConn ch h (Atto.parse MP.get)
            modifyMVar_ mClients $ return . Set.delete clientName
    
    return (mLog, mClients)

handleConn ch h parse_cont = BS.hGet h 1024 >>= resolveFull parse_cont
    where
        resolveFull parse_cont x = do
            case parse_cont x of
                Fail _ ctx err -> putStrLn "wrong packet"
                Partial f -> handleConn ch h f
                Done rest packet -> do
                    case translateMessage packet of
                        Nothing -> return () -- ignore
                        Just msg -> writeChan ch msg
                    resolveFull (Atto.parse MP.get) rest
        
translateMessage :: [MP.Object] -> Maybe Message
translateMessage
    [ObjectRAW (decodeUTF8 -> "str"),
    (ObjectRAW msg)]=Just $ StringMessage $ decodeUTF8 msg
translateMessage
    [ObjectRAW (decodeUTF8 -> "uni"),
    ObjectRAW (decodeUTF8 -> "u8"),
    shape_raw,
    ObjectRAW raw]=do
    shape <- MP.fromObject shape_raw
    if BS.length raw==product shape
        then return $ UniformArrayMessage shape raw
        else Nothing
translateMessage _=Nothing

decodeUTF8 :: BS.ByteString -> String
decodeUTF8 = Codec.Binary.UTF8.String.decode . BS.unpack

