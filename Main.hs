
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization  -- drawpixels
import Data.Array.Storable
import Codec.Image.PNG   -- PNG: http://hackage.haskell.org/packages/archive/pngload/0.1/doc/html/Codec-Image-PNG.html
import Control.Monad        --liftM
import Data.Word            -- Word8
import Graphics.UI.GLUT.Initialization -- initialWindowSize
import Data.IORef
import MyModule   
import Stations 




img::IO (StorableArray (Int,Int) Word8)
img = getImageData "/export/roxar591/nilscb/line5vestli.png"
  

main = do
 (progName,_) <- getArgsAndInitialize

 initialWindowSize $= (Size 500 850)
 initialDisplayMode $= [DoubleBuffered]
 createWindow (progName++"MyWindow")
 reshapeCallback $= Just reshape
 mypos <- newIORef 0.0
 idleCallback $= Just (idle mypos)
 displayCallback $= (display mypos)
 projection (0) (500) (0) 850 (-10) (10)
 
 GLUT.mainLoop
 
 
idle mypos = do
  x <- get mypos 
  mypos $= if x < 500 
            then
              x + 2
            else
              0.0
  postRedisplay Nothing
 
 
reshape s = return ()

projection xl xu yl yu zl zu = do
   matrixMode $= Projection
   loadIdentity
   ortho xl xu yl yu zl zu
   matrixMode $= Modelview 0
   loadIdentity
   
display mypos = do
   clearColor $= Color4 1.0 1.0 1.0 1
   clear [ColorBuffer]
   currentColor $= Color4 1.0 0.3 0.2 0.5
   displayImage
   x <- get mypos
   displayTrain x 150.0 
   GLUT.swapBuffers
   postRedisplay Nothing
   flush

    
displayTrain::Float -> Float -> IO ()
displayTrain x y = do
                     matrixMode $= Modelview 0
                     loadIdentity
                     translate $ GL.Vector3 x y 0.0
                     displayPoints myPoints Quads 

displayImage = do 
           loadIdentity
           pixels <- img
           withStorableArray pixels $ \ptr -> do
             GL.rowAlignment GL.Unpack $= 1
             GL.rasterPos (GL.Vertex2 (0) (0)::GL.Vertex2 GL.GLint)
             GL.drawPixels (GL.Size 408 844) (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
            



getImageData::String -> IO (StorableArray (Int,Int) Word8)
getImageData impath = liftM (imageData . myF) (loadPNGFile impath) 
                         
myF::Either String PNGImage -> PNGImage
myF (Left str ) =  error str                           
myF (Right im) =  im       


displayPoints points primitiveShape = do
 renderAs primitiveShape points
 flush
  
renderAs figure ps = renderPrimitive figure $ makeVertexes ps

makeVertexes = mapM_ (\(x,y,z)->vertex$Vertex3 x y z)

myPoints
 = [(5,5,0::GLfloat)
   ,(-5,5,0)
   ,(-5,-5,0)
   ,(5,-5,0) ]






--displayImage = do 
--           pixels <- img
--           withStorableArray pixels $ \ptr -> do
--             GL.rowAlignment GL.Unpack $= 0
--             GL.rasterPos (GL.Vertex2 (-1) 0::GL.Vertex2 GL.GLint)
--             GL.drawPixels (GL.Size 640 480) (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
--             flush
              
-- FourBytes UnsignedByte
