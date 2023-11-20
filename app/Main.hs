module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


--
-- Constants of the equations (change them to obtain different results!)
--
initialPoint :: (Float, Float, Float)
initialPoint = (0.01, 0.7, 0)

sigma :: Float
sigma = 10
rho :: Float
rho = 49
beta :: Float
beta = 8/3

-- Additionally here you can change the colour of the plotted line (generates a gradient between both colours)
startColor :: Color
startColor = makeColor 0 0.012 0.78 1
endColor :: Color
endColor = makeColor 0.973 0.427 0.992 1


--
-- Lorenz equations
--
dt :: Float
dt = 0.01

dx :: Point3D -> Float
dx (x, y, z) = (sigma * (y - x)) * dt
dy :: Point3D -> Float
dy (x, y, z) = (x * (rho - z) - y) * dt
dz :: Point3D -> Float
dz (x, y, z) = (x*y - beta * z) * dt



--
-- Different Data and Types Definitions
--
data AppState = AppState
  { drawnLines :: [(Picture, Color)]
  , clicked :: Bool
  , viewPosition :: (Float, Float)
  , zoomFactor :: Float
  , titleText :: String
  , titleScale :: Float
  }

type Point3D = (Float, Float, Float)
type Point2D = (Float, Float)


--
-- Main
--
main :: IO ()
main = playIO
  FullScreen
  black
  300
  initialAppState
  render
  handleEvent
  update


--
-- Initial State
--
initialAppState :: AppState
initialAppState = AppState
                  { drawnLines = []               -- Initial empty plot
                  , clicked = False               -- Default value
                  , viewPosition = (0, 0)         -- Default camera position
                  , zoomFactor = 1.0              -- Default camera zoom
                  , titleText = "XY Axis View"    -- Initial text
                  , titleScale = 0.0              -- Initial hidden text
                  }


--
-- Render
--
render :: AppState -> IO Picture
render state = do
  let scaledTitle = translate (-650) (-400) $ Scale tScale tScale $ Color white $ text (titleText state)
      scaledLines = Pictures $ map (\(pic, col) -> translate x y $ Color col pic) (drawnLines state)
      tScale = max 0.0 (titleScale state - 0.4)
      (x, y) = viewPosition state
      zoomedLines = Scale (zoomFactor state) (zoomFactor state) scaledLines
  return $ Pictures [scaledTitle, zoomedLines]


--
-- Interactive Events
--
handleEvent :: Event -> AppState -> IO AppState

-- Change Perspective (Space bar)
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state = return initialAppState { clicked = not (clicked state) }

-- Move Camera (Arrows)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = return $ moveCamera (-50, 0) state
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state = return $ moveCamera (50, 0) state
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state = return $ moveCamera (0, 50) state
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state = return $ moveCamera (0, -50) state

-- Zoom View (+, -)
handleEvent (EventKey (Char '+') Down _ _) state = return $ state { zoomFactor = zoomFactor state + 0.1 } 
handleEvent (EventKey (Char '-') Down _ _) state = return $ state { zoomFactor = max 0.1 (zoomFactor state - 0.1) }

-- Other
handleEvent _ state = return state

-- Move Camera
moveCamera :: (Float, Float) -> AppState -> AppState
moveCamera (dxx, dyy) state = state { viewPosition = (x + dxx, y + dyy) }
  where
    (x, y) = viewPosition state


--
-- Update
--

-- XZ Axis View
update :: Float -> AppState -> IO AppState
update _ state@(AppState { clicked = True, drawnLines = lines' }) = do
  let numLinesToAdd = 1  -- Number of lines to add per frame
      linesToAdd = take numLinesToAdd (drop (length lines') (linesToDraw True))

  return $ state { drawnLines = lines' ++ linesToAdd
                  , titleText = "XZ Axis View"

                  -- Title Animation
                  , titleScale = if length (lines' ++ linesToAdd) > 1500 then 0
                            else if length (lines' ++ linesToAdd) > 900 then max 0.0 (1.0 - (fromIntegral (length (lines' ++ linesToAdd)) - 900) / 900)
                            else if length (lines' ++ linesToAdd) > 600 then 1
                            else fromIntegral (length (lines' ++ linesToAdd)) / 600
                  }

-- XY Axis View
update _ state@(AppState { clicked = False, drawnLines = lines' }) = do
  let numLinesToAdd = 1  -- Number of lines to add per frame
      linesToAdd = take numLinesToAdd (drop (length lines') (linesToDraw False))

  return $ state { drawnLines = lines' ++ linesToAdd
                  , titleText = "XY Axis View"

                  -- Title Animation
                  , titleScale = if length (lines' ++ linesToAdd) > 1500 then 0
                            else if length (lines' ++ linesToAdd) > 900 then max 0.0 (1.0 - (fromIntegral (length (lines' ++ linesToAdd)) - 900) / 900)
                            else if length (lines' ++ linesToAdd) > 600 then 1
                            else fromIntegral (length (lines' ++ linesToAdd)) / 600
                  }


--
-- Generating the Lorenz System
--
linesToDraw :: Bool -> [(Picture, Color)]
linesToDraw clicked | not clicked = zip (drawLines (allProjectedPoints False (allPoints3D initialPoint 12000))) (gradientColors startColor endColor)
                    | otherwise = zip (drawLines (allProjectedPoints True (allPoints3D initialPoint 12000))) (gradientColors startColor endColor)
                      
-- Gradient Colors
gradientColors :: Color -> Color -> [Color]
gradientColors startColor endColor =
  let step = 0.00008 
      blendColors t = mixColors' t startColor endColor
  in [blendColors t | t <- [0, step .. 1]]

  where
    mixColors' :: Float -> Color -> Color -> Color
    mixColors' t startColor' endColor' =
      let (r1, g1, b1, a1) = rgbaOfColor startColor'
          (r2, g2, b2, a2) = rgbaOfColor endColor'
          r = r1 + t * (r2 - r1)
          g = g1 + t * (g2 - g1)
          b = b1 + t * (b2 - b1)
          a = a1 + t * (a2 - a1)
      in makeColor r g b a


-- Generate all the points from the Lorenz Equations
allPoints3D :: Point3D -> Int -> [Point3D]
allPoints3D _ 0 = []
allPoints3D (x, y, z) n = nextPoint : allPoints3D nextPoint (n-1)
  where
    nextPoint = (x + dx (x, y, z), y + dy (x, y, z), z + dz (x, y, z))

-- 2D Perspective Projection Functions (XZ axis & XY axis)
perspectiveScaleFactor :: Float
perspectiveScaleFactor = 15

perspectiveProjectionXZ :: Point3D -> Point2D
perspectiveProjectionXZ (x, y, z) | y == 0 = (x,z)
                                | otherwise = (perspectiveScaleFactor * x, perspectiveScaleFactor * z)

perspectiveProjectionXY :: Point3D -> Point2D
perspectiveProjectionXY (x, y, z) | z == 0 = (x,y)
                                  | otherwise = (perspectiveScaleFactor * x, perspectiveScaleFactor * y)

-- Project all 3D points into a 2D perspective
allProjectedPoints :: Bool -> [Point3D] -> [Point2D]
allProjectedPoints perspective vs | perspective = map perspectiveProjectionXZ vs
                                  | otherwise = map perspectiveProjectionXY vs

-- Generate the lines between points
drawLines :: [Point2D] -> [Picture]
drawLines [] = []
drawLines (_:[]) = []
drawLines (x:xs) = Line [x, head xs] : drawLines xs