module Graphics.Rendering.OpenGL.GL.CoordTrans where

import Control.Monad ( zipWithM_ )
import Foreign.Ptr ( castPtr )
import Foreign.Storable ( Storable(..) )

data GLcolumn4 a = GLcolumn4 a a a a

data GLmatrix a = GLmatrix (GLcolumn4 a) (GLcolumn4 a) (GLcolumn4 a) (GLcolumn4 a)

instance Storable a => Storable (GLmatrix a) where
   sizeOf    ~(GLmatrix (GLcolumn4 x _ _ _) _ _ _) = 16 * sizeOf x
   alignment ~(GLmatrix (GLcolumn4 x _ _ _) _ _ _) = alignment x

   peek ptr = do
      [ a00, a01, a02, a03,
        a04, a05, a06, a07,
        a08, a09, a10, a11,
        a12, a13, a14, a15 ] <- mapM (peekElemOff (castPtr ptr)) [ 0 .. 15 ]
      return $ GLmatrix (GLcolumn4 a00 a01 a02 a03)
                        (GLcolumn4 a04 a05 a06 a07)
                        (GLcolumn4 a08 a09 a10 a11)
                        (GLcolumn4 a12 a13 a14 a15)

   poke ptr (GLmatrix (GLcolumn4 a00 a01 a02 a03)
                      (GLcolumn4 a04 a05 a06 a07)
                      (GLcolumn4 a08 a09 a10 a11)
                      (GLcolumn4 a12 a13 a14 a15)) =
      zipWithM_ (pokeElemOff (castPtr ptr)) [ 0 .. ]
                [ a00, a01, a02, a03,
                  a04, a05, a06, a07,
                  a08, a09, a10, a11,
                  a12, a13, a14, a15 ]
