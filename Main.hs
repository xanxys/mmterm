{-# LANGUAGE ViewPatterns #-}
module Main where
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.IORef
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
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq

main = do
    print $ Fold.toList $ fst $ Seq.splitAt 5 $ snd $ Seq.splitAt 1 (Seq.empty :: Seq.Seq Int)
    mseq <- runServer
    
    initGUI
    window <- windowNew
    vb <- vBoxNew False 1
    lb <- labelNew (Just $ "192.168.24.15 "++show port++" ok")
    hb <- hBoxNew False 0
    tb <- vBoxNew False 0
    refill mseq tb 0
    adj <- adjustmentNew 1 1 100 1 20 20
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
    boxPackStart vb lb PackNatural 0
    boxPackStart vb hb PackGrow 0
    set window [containerChild := vb]
    
    nmseq <- newIORef 0
    let update=do
        prev_n <- readIORef nmseq
        curr_n <- withMVar mseq (return . Seq.length)
        when (curr_n/=prev_n) $ adjustmentGetValue adj >>= refill mseq tb
        writeIORef nmseq curr_n
    timeoutAdd (update >> return True) 50
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI


arrayToSurface [h,w,c] raw = do
    surf <- createImageSurface FormatRGB24 w h
    arr <- imageSurfaceGetPixels surf -- BGRA packing
    case c of
        1 -> mapM_ (transfer1 arr) [0..w*h-1]
        3 -> mapM_ (transfer3 arr) [0..w*h-1]
        _ -> mapM_ (transfer arr) [0..w*h-1]
    return surf
    where
        transfer arr i = do
            mapM_ (\d -> writeArray arr (i*4+d) $ BS.index raw (i*c+d)) [0..min c 3-1]
        transfer1 arr i = do
            let v = BS.index raw i
            writeArray arr (i*4+0) v
            writeArray arr (i*4+1) v
            writeArray arr (i*4+2) v
        transfer3 arr i = do
            writeArray arr (i*4+0) $ BS.index raw (i*3+2)
            writeArray arr (i*4+1) $ BS.index raw (i*3+1)
            writeArray arr (i*4+2) $ BS.index raw (i*3+0)
            

data Message =
    StringMessage String |
    UniformArrayMessage [Int] BS.ByteString
    deriving(Show)

port=8001

runServer :: IO (MVar (Seq.Seq Message))
runServer=do
    ch <- newChan
    mseq <- newMVar Seq.empty
    forkIO $ forever $ do
        x <- readChan ch
        modifyMVar_ mseq $ return . (Seq.|> x)
    
    sock <- listenOn (PortNumber port)
    putStrLn "started listening"
    forkIO $ forever $ do
        (h, host, port) <- accept sock
        printf "received connection from %s %s" host (show port)
        hSetBuffering h NoBuffering
        forkIO $ handleConn ch h (Atto.parse MP.get)
    return mseq

handleConn ch h parse_cont = BS.hGet h 16 >>= resolveFull parse_cont
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
    [ObjectRAW (UTF8.decode . BS.unpack -> "str"),
    (ObjectRAW msg)]=Just $ StringMessage $ UTF8.decode $ BS.unpack msg
translateMessage
    [ObjectRAW (UTF8.decode . BS.unpack -> "uni"),
    ObjectRAW (UTF8.decode . BS.unpack -> "u8"),
    shape_raw,
    ObjectRAW raw]=do
    shape <- MP.fromObject shape_raw
    if BS.length raw==product shape
        then return $ UniformArrayMessage shape raw
        else Nothing
translateMessage _=Nothing





instantiateView (StringMessage x)=do
--    l <- labelNew (Just $ show x)
--    labelSetSelectable l True
    l <- drawingAreaNew
    widgetSetSizeRequest l (-1) 20
    onExpose l $ \ev -> do
        dw <- widgetGetDrawWindow l
        renderWithDrawable dw $ do
            setSourceRGB 0.5 0.5 1
            translate 0 10
            showText x
            fill
        return True
    
    return l

instantiateView (UniformArrayMessage shape raw)=do
--    l <- labelNew (Just $ show x)
--    labelSetSelectable l True
    l <- drawingAreaNew
    surf <- arrayToSurface shape raw
    widgetSetSizeRequest l (-1) 25
    onExpose l $ \ev -> do
        dw <- widgetGetDrawWindow l
        renderWithDrawable dw $ do
            setSourceSurface surf 0 0
            paint
        return True
    
    return l

-- seems ok
refill mseq box fromd=do
    let from=floor $ realToFrac fromd
    mapM_ widgetDestroy =<< containerGetChildren box
    msgs <- withMVar mseq $ return . Fold.toList . fst . Seq.splitAt 20 . snd . Seq.splitAt from
    mapM_ (\msg->do{l<-instantiateView msg; boxPackStart box l PackNatural 0}) msgs
    widgetShowAll box



