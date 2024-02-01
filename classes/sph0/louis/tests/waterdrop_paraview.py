#### import the simple module from the paraview
from paraview.simple import *
import glob, os.path

#### disable automatic camera reset on 'Show'
paraview.simple._DisableFirstRenderCameraReset()

print("cwd=", os.getcwd())
dirname = '/home/boman/dev/Projet_MP/waves/sph/louis/workspace/tests_waterdrop'

# create a new 'XML Structured Grid Reader'
gridvts = XMLStructuredGridReader(FileName=os.path.join(dirname,'grid.vts'))
RenameSource('Grid', gridvts)

# create a new 'XML Unstructured Grid Reader'
inpfiles = sorted(glob.glob(os.path.join(dirname,'resFP_*.vtu')))
#print inpfiles
resFP_00000 = XMLUnstructuredGridReader(FileName=inpfiles)
resFP_00000.PointArrayStatus = ['max(mu_ab)', 'Pressure', 'Speed of sound', 'Mass density', 'Velocity', 'Smoothing length', 'Mass', 'Nb of neighbours']
RenameSource('Fixed particles', resFP_00000)

# get animation scene
animationScene1 = GetAnimationScene()

# update animation scene based on data timesteps
animationScene1.UpdateAnimationUsingDataTimeSteps()

# create a new 'XML Unstructured Grid Reader'
inpfiles = sorted(glob.glob(os.path.join(dirname,'resMP_*.vtu')))
resMP_00000 = XMLUnstructuredGridReader(FileName=inpfiles)
resMP_00000.PointArrayStatus = ['max(mu_ab)', 'Pressure', 'Speed of sound', 'Mass density', 'Velocity', 'Smoothing length', 'Mass', 'Nb of neighbours']
RenameSource('Mobile particles', resMP_00000)

# get active view
renderView1 = GetActiveViewOrCreate('RenderView')
# uncomment following to set a specific view size
# renderView1.ViewSize = [997, 849]

# show data in view
gridvtsDisplay = Show(gridvts, renderView1)
# trace defaults for the display properties.
gridvtsDisplay.Representation = 'Outline'
gridvtsDisplay.ColorArrayName = ['POINTS', '']
gridvtsDisplay.OSPRayScaleFunction = 'PiecewiseFunction'
gridvtsDisplay.SelectOrientationVectors = 'None'
gridvtsDisplay.ScaleFactor = 0.2
gridvtsDisplay.SelectScaleArray = 'None'
gridvtsDisplay.GlyphType = 'Arrow'
gridvtsDisplay.ScalarOpacityUnitDistance = 0.21650635094610968

# reset view to fit data
renderView1.ResetCamera()

# show data in view
resFP_00000Display = Show(resFP_00000, renderView1)
# trace defaults for the display properties.
resFP_00000Display.ColorArrayName = [None, '']
resFP_00000Display.OSPRayScaleArray = 'Mass'
resFP_00000Display.OSPRayScaleFunction = 'PiecewiseFunction'
resFP_00000Display.SelectOrientationVectors = 'Mass'
resFP_00000Display.ScaleFactor = 0.19512200355529785
resFP_00000Display.SelectScaleArray = 'Mass'
resFP_00000Display.GlyphType = 'Arrow'
resFP_00000Display.ScalarOpacityUnitDistance = 0.18843877254666028
resFP_00000Display.GaussianRadius = 0.09756100177764893
resFP_00000Display.SetScaleArray = ['POINTS', 'Mass']
resFP_00000Display.ScaleTransferFunction = 'PiecewiseFunction'
resFP_00000Display.OpacityArray = ['POINTS', 'Mass']
resFP_00000Display.OpacityTransferFunction = 'PiecewiseFunction'

# show data in view
resMP_00000Display = Show(resMP_00000, renderView1)
# trace defaults for the display properties.
resMP_00000Display.ColorArrayName = [None, '']
resMP_00000Display.OSPRayScaleArray = 'Mass'
resMP_00000Display.OSPRayScaleFunction = 'PiecewiseFunction'
resMP_00000Display.SelectOrientationVectors = 'Mass'
resMP_00000Display.ScaleFactor = 0.04545450210571289
resMP_00000Display.SelectScaleArray = 'Mass'
resMP_00000Display.GlyphType = 'Arrow'
resMP_00000Display.ScalarOpacityUnitDistance = 0.07157227916349206
resMP_00000Display.GaussianRadius = 0.022727251052856445
resMP_00000Display.SetScaleArray = ['POINTS', 'Mass']
resMP_00000Display.ScaleTransferFunction = 'PiecewiseFunction'
resMP_00000Display.OpacityArray = ['POINTS', 'Mass']
resMP_00000Display.OpacityTransferFunction = 'PiecewiseFunction'

# set active source
SetActiveSource(gridvts)

# change representation type
gridvtsDisplay.SetRepresentationType('Wireframe')


# Properties modified on gridvtsDisplay
gridvtsDisplay.Opacity = 0.2

# set active source
SetActiveSource(resFP_00000)

# change representation type
resFP_00000Display.SetRepresentationType('Points')

# Properties modified on resFP_00000Display
resFP_00000Display.PointSize = 5.0

# set active source
SetActiveSource(resMP_00000)

# create a new 'Glyph'
glyph1 = Glyph(Input=resMP_00000, GlyphType='Sphere')
glyph1.Scalars = ['POINTS', 'Smoothing length']
glyph1.Vectors = ['POINTS', 'None']
glyph1.ScaleMode = 'scalar'
glyph1.ScaleFactor = 1.0
glyph1.GlyphTransform = 'Transform2'
glyph1.GlyphMode = 'All Points'



# get color transfer function/color map for 'Smoothinglength'
smoothinglengthLUT = GetColorTransferFunction('Smoothinglength')

# show data in view
glyph1Display = Show(glyph1, renderView1)
# trace defaults for the display properties.
glyph1Display.ColorArrayName = ['POINTS', 'Smoothing length']
glyph1Display.LookupTable = smoothinglengthLUT
glyph1Display.OSPRayScaleArray = 'Smoothing length'
glyph1Display.OSPRayScaleFunction = 'PiecewiseFunction'
glyph1Display.SelectOrientationVectors = 'Mass'
glyph1Display.ScaleFactor = 0.051454496383666996
glyph1Display.SelectScaleArray = 'Smoothing length'
glyph1Display.GlyphType = 'Arrow'
glyph1Display.GaussianRadius = 0.025727248191833498
glyph1Display.SetScaleArray = ['POINTS', 'Smoothing length']
glyph1Display.ScaleTransferFunction = 'PiecewiseFunction'
glyph1Display.OpacityArray = ['POINTS', 'Smoothing length']
glyph1Display.OpacityTransferFunction = 'PiecewiseFunction'

# show color bar/color legend
glyph1Display.SetScalarBarVisibility(renderView1, True)

# get opacity transfer function/opacity map for 'Smoothinglength'
smoothinglengthPWF = GetOpacityTransferFunction('Smoothinglength')

# hide data in view
Hide(resMP_00000, renderView1)

# set scalar coloring
ColorBy(glyph1Display, ('POINTS', 'Velocity'))

# Hide the scalar bar for this color map if no visible data is colored by it.
HideScalarBarIfNotNeeded(smoothinglengthLUT, renderView1)

# rescale color and/or opacity maps used to include current data range
glyph1Display.RescaleTransferFunctionToDataRange(True, False)

# show color bar/color legend
glyph1Display.SetScalarBarVisibility(renderView1, True)


#### saving camera placements for all active views

# current camera placement for renderView1
renderView1.CameraPosition = [2.242394659003803, -5.320849062052243, 2.8132656553463735]
renderView1.CameraFocalPoint = [1.0000000000000002, 0.9999999999999994, 0.9999999999999989]
renderView1.CameraViewUp = [-0.2489518711661434, 0.22153968976360192, 0.9428378077391271]
renderView1.CameraParallelScale = 1.7320508075688772

#### uncomment the following to render all views
RenderAllViews()
# alternatively, if you want to write images, you can use SaveScreenshot(...).